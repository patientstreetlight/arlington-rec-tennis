module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Modal as Modal
import Bootstrap.Text as Text
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Browser
import Dict exposing (Dict)
import Set exposing (Set)
import Html exposing (Html, div, input, text, span, h3)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (..)
import Char exposing (isDigit)

main =
  Browser.sandbox { init = initModel, update = update, view = view }

-- MODEL
type alias Model =
    { players : Set String
    , tab : Tab
    , pastTeams : List Team
    , newPlayer : String
    , matches : Dict Int Match
    , scores : Dict String Int
    , scoreModal : Maybe ScoreModal
    }


initModel : Model
initModel =
    { players = Set.empty
    , tab = Players
    , pastTeams = []
    , newPlayer = ""
    , matches = Dict.empty
    , scores = Dict.empty
    , scoreModal = Nothing
    }


type Tab
    = Players
    | Matches
    | Scores


type Team
    = SinglesTeam String

    -- A doubles team is canonically represented as 2 players with the
    -- first being lexicographically lower/first.
    | DoublesTeam String String


mkDoublesTeam : String -> String -> Team
mkDoublesTeam p1 p2 =
  if p1 < p2 then
    DoublesTeam p1 p2
  else
    DoublesTeam p2 p1

type alias Match =
    { team1 : Team
    , team2 : Team
    }


type alias ScoreModal =
    { match : Match
    , team1Score : String
    , team2Score : String
    }


-- UPDATE
type Msg
    = AddPlayer
    | InputPlayer String
    | RemovePlayer String
    | SelectTab Tab
    | CreateMatches
    | CloseScoreModal
    | OpenScoreModel Match
    | ScoreModalSubmitScores
    | ScoreModalInputScore1 String
    | ScoreModalInputScore2 String


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
        { model
        | players = Set.remove player model.players
        , scores = Dict.remove player model.scores
        }

    InputPlayer name ->
        { model | newPlayer = name }

    CreateMatches ->
        startMatches model
    
    CloseScoreModal ->
        { model | scoreModal = Nothing }
    
    OpenScoreModel match ->
        { model
        | scoreModal = Just { match = match, team1Score = "", team2Score = "" }
        }
    
    ScoreModalSubmitScores ->
        case model.scoreModal of
            Nothing -> model
            Just scoreModal ->
                { model
                | scoreModal = Nothing
                -- XXX also update scores
                }
    
    ScoreModalInputScore1 score ->
        updateScoreModal setModalScore1 score model
    
    ScoreModalInputScore2 score ->
        updateScoreModal setModalScore2 score model

updateScoreModal : (String -> ScoreModal -> ScoreModal) -> String -> Model -> Model
updateScoreModal setter newScore model =
  let
    setModalScore : ScoreModal -> ScoreModal
    setModalScore modal =
        if String.all isDigit newScore then
            setter newScore modal
        else
            modal
  in
    { model
    | scoreModal = Maybe.map setModalScore model.scoreModal
    }


setModalScore1 : String -> ScoreModal -> ScoreModal
setModalScore1 newScore modal =
    { modal | team1Score = newScore }

setModalScore2 : String -> ScoreModal -> ScoreModal
setModalScore2 newScore modal =
    { modal | team2Score = newScore }



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
mkMatches : List String -> Dict Int Match
mkMatches players =
  let
    go : List String -> Int -> Dict Int Match -> Dict Int Match
    go ps courtNum matches =
      case ps of
        [] -> matches
        [p] ->
          Dict.insert
              courtNum
              { team1 = SinglesTeam p, team2 = SinglesTeam "Coach" }
              matches
        [p1, p2] ->
          let
            team1 = SinglesTeam p1
            team2 = SinglesTeam p2
          in
            Dict.insert
                courtNum
                { team1 = team1, team2 = team2 }
                matches
        [p1, p2, p3] ->
          let
            team1 = mkDoublesTeam p1 p2
            team2 = mkDoublesTeam p3 "Coach"
            match = { team1 = team1, team2 = team2 }
          in
            Dict.insert courtNum match matches
        (p1 :: p2 :: p3 :: p4 :: rest) ->
          let
            team1 = mkDoublesTeam p1 p2
            team2 = mkDoublesTeam p3 p4
            match = { team1 = team1, team2 = team2 }
            newMatches = Dict.insert courtNum match matches
          in
            go rest (courtNum + 1) newMatches

  in
    go players 1 Dict.empty


-- VIEW
view : Model -> Html Msg
view model = withBootstrap <| withHeader model <|
  case model.tab of
    Players ->
      div [] <|
        List.map viewPlayer (Set.toList model.players) ++
        [ input [ placeholder "name", value (model.newPlayer), onInput InputPlayer ] []
        , Button.button
            [ Button.primary
            , Button.attrs [ onClick AddPlayer ]
            ]
            [ text "Add Player" ]
        ]

    Matches ->
      div [] <| List.concat
        [ List.map viewMatch <| Dict.toList model.matches
        , viewScoreModal model
        , [ div []
            [ Button.button
                [ Button.success
                , Button.attrs [ onClick CreateMatches ]
                ]
                [ text "Create new matches" ] ] ]
        ]

    Scores -> viewScores model.scores


viewScoreModal : Model -> List (Html Msg)
viewScoreModal model =
  let
    teamName : Team -> String
    teamName team =
        case team of
            SinglesTeam p -> p
            DoublesTeam p1 p2 -> p1 ++ " & " ++ p2
    
    teamInput : Team -> String -> (String -> Msg) -> Html Msg
    teamInput team currentScore mkInputMessage =
        div []
            [ text <| teamName team
            , input
                [ onInput mkInputMessage
                , pattern "[0-9]*"
                , type_ "number"
                , value currentScore
                ]
                []
            ]
  in
    case model.scoreModal of
        Nothing -> []
        Just scoreModal ->
            Modal.config CloseScoreModal
              |> Modal.small
              |> Modal.hideOnBackdropClick True
              |> Modal.body []
                  [ teamInput scoreModal.match.team1 scoreModal.team1Score ScoreModalInputScore1
                  , teamInput scoreModal.match.team2 scoreModal.team2Score ScoreModalInputScore2
                  ]
              |> Modal.footer []
                  [ Button.button
                      [ Button.primary, Button.attrs [ onClick ScoreModalSubmitScores ] ]
                      [ text "Submit" ]
                  ]
              |> Modal.view Modal.shown
              |> List.singleton


withBootstrap : Html a -> Html a
withBootstrap html =
    Grid.container []
        [ CDN.stylesheet
        , html
        ]


viewScores : Dict String Int -> Html Msg
viewScores scores =
  let
    viewScore (player, score) =
        Grid.row []
            [ Grid.col [ Col.md1 ] [ text player ]
            , Grid.col [] [ text <| String.fromInt score ]
            ]
  in
    Dict.toList scores
        |> List.sortBy Tuple.second
        |> List.reverse
        |> List.map viewScore
        |> Grid.container []


-- XXX Highlight the selected state
withHeader : Model -> Html Msg -> Html Msg
withHeader model html =
  let
    mkButton tab label =
        Button.button
            [ Button.primary
            , Button.attrs [ onClick (SelectTab tab) ]
            ]
            [ text label ]
  in
    div []
      [ mkButton Players "Players"
      , mkButton Matches "Matches"
      , mkButton Scores "Scores"
      , div [] [ html ]
      ]


viewMatch : (Int, Match) -> Html Msg
viewMatch (court, match) =
  let
    viewTeam team =
      case team of
        SinglesTeam p ->
            text p
        DoublesTeam p1 p2 ->
            div []
                [ div [] [ text p1 ]
                , div [] [ text p2 ]
                ]
  in
    Card.config
        [ Card.outlineSuccess
        ]
        |> Card.block []
            [ Block.titleH5 [] [ text <| "Court " ++ String.fromInt court ]
            , Block.custom <|
                Grid.container []
                    [ Grid.row []
                        [ Grid.col [] [ viewTeam match.team1 ]
                        , Grid.col [] [ viewTeam match.team2 ]
                        ]
                    ]
            , Block.custom <|
                Button.button
                    [ Button.primary
                    , Button.attrs [ onClick <| OpenScoreModel match ]
                    ]
                    [ text "Finish" ]
            ]
        |> Card.view

viewPlayer : String -> Html Msg
viewPlayer name =
    div [] <|
      [ Button.button
          [ Button.danger
          , Button.small
          , Button.attrs [ onClick (RemovePlayer name) ]
          ]
          [ text "X" ]
      , text name
      ]