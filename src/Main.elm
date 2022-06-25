module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Set exposing (Set)
import Html exposing (Html, button, div, input, text, span)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (..)

main =
  Browser.sandbox { init = initModel, update = update, view = view }

-- MODEL
type alias Model =
    { players : Set String
    , tab : Tab
    , pastTeams : List Team
    , newPlayer : String
    , matches : List Match
    , scores : Dict String Int
    }


initModel : Model
initModel =
    { players = Set.empty
    , tab = Players
    , pastTeams = []
    , newPlayer = ""
    , matches = []
    , scores = Dict.empty
    }


type Tab
    = Players
    | Matches
    | Scores


type Team
    = SinglesTeam String
    | DoublesTeam String String


type alias Match =
    { court : Int
    , team1 : Team
    , team2 : Team
    }


-- UPDATE
type Msg
    = AddPlayer
    | InputPlayer String
    | RemovePlayer String
    | FinishMatch
    | SelectTab Tab


update : Msg -> Model -> Model
update msg model =
  case msg of
    SelectTab t ->
        { model | tab = t }

    AddPlayer ->
        case model.newPlayer of
            "" -> model
            player ->
                { model
                | newPlayer = ""
                , players = Set.insert player model.players
                , scores = Dict.insert player 0 model.scores
                }

    RemovePlayer player ->
        { model | players = Set.remove player model.players }

    InputPlayer name ->
        { model | newPlayer = name }

    FinishMatch -> model -- XXX



-- Create random assignment of people to partners/courts
startMatches : Model -> Model
startMatches model =
  let
    players = Set.toList <| model.players
    matches = mkMatches players
  in
    { model | tab = Matches, matches = matches }


-- This could use some work
-- * try to pair players with lots of wins with those with fewer
-- * try to pair singles players together of similar ability
-- * add some randomization
mkMatches : List String -> List Match
mkMatches players =
  let
    go ps courtNum =
      case ps of
        [] -> []
        [p] ->
          [{court = courtNum, team1 = SinglesTeam p, team2 = SinglesTeam "Sub"}]
        [p1, p2] ->
          let
            team1 = SinglesTeam p1
            team2 = SinglesTeam p2
          in
            [{ court = courtNum, team1 = team1, team2 = team2 }]
        [p1, p2, p3] ->
          let
            team1 = DoublesTeam p1 p2
            team2 = DoublesTeam p3 "Sub"
            match = { court = courtNum, team1 = team1, team2 = team2 }
          in
            [ match ]
        (p1 :: p2 :: p3 :: p4 :: rest) ->
          let
            team1 = DoublesTeam p1 p2
            team2 = DoublesTeam p3 p4
            match = { court = courtNum, team1 = team1, team2 = team2 }
          in
            match :: go rest (courtNum + 1)

  in
    go players 1


-- VIEW
view : Model -> Html Msg
view model = withHeader model <|
  case model.tab of
    Players ->
      div [] <|
        List.map viewPlayer (Set.toList model.players) ++
        [ input [ placeholder "name", value (model.newPlayer), onInput InputPlayer ] []
        , button [ onClick AddPlayer ] [ text "Add Player" ]
        ]

    Matches ->
      div [] <|
        List.map viewMatch model.matches

    Scores -> viewScores model.scores


viewScores : Dict String Int -> Html Msg
viewScores scores = text "TODO: Scores"


-- XXX Highlight the selected state
withHeader : Model -> Html Msg -> Html Msg
withHeader model html =
    div []
      [ button [ onClick (SelectTab Players) ] [ text "Players "]
      , button [ onClick (SelectTab Matches) ] [ text "Matches" ]
      , button [ onClick (SelectTab Scores) ] [ text "Scores" ]
      , div [] [ html ]
      ]


viewMatch : Match -> Html Msg
viewMatch match =
  let
    viewTeam team =
      case team of
        SinglesTeam p -> p
        DoublesTeam p1 p2 -> p1 ++ "/" ++ p2
    msg = String.concat
      [ "Court "
      , String.fromInt match.court
      , ":"
      , viewTeam match.team1
      , " vs "
      , viewTeam match.team2
      ]
    finishButton = button [ onClick FinishMatch ] [ text "Finish" ]
  in
    -- XXX Should show score once finished
    -- XXX Have an 'edit' button to adjust score afterwards
    -- XXX finish should somehow be able to say what the scores were
    div [] <|
      [ text msg
      , finishButton
      ]

viewPlayer : String -> Html Msg
viewPlayer name =
    div [] <|
      [ button [ onClick (RemovePlayer name) ] [ text "X" ]
      , text name
      ]