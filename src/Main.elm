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
    , state : State
    , pastTeams : List Team
    , scores : Dict String Int
    }


initModel : Model
initModel =
    { players = Set.empty
    , state = Setup ""
    , pastTeams = []
    , scores = Dict.empty
    }


type State
    = Setup String
    | Playing (List Match)
    | Done


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
    | StartMatches


update : Msg -> Model -> Model
update msg model =
  case (msg, model.state) of
    (InputPlayer s, Setup _) ->
        { model | state = Setup s }

    (AddPlayer, Setup "") -> model

    (AddPlayer, Setup player) ->
        { model | state = Setup "", players = Set.insert player model.players }

    (RemovePlayer player, Setup _) ->
        { model | players = Set.remove player model.players }

    (StartMatches, _) ->
        startMatches model

    _ -> model


-- Create random assignment of people to partners/courts
startMatches : Model -> Model
startMatches model =
  let
    players = Set.toList <| model.players
    matches = mkMatches players
  in
    { model | state = Playing matches }


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
view model =
  case model.state of
    Setup name ->
      div [] <|
        List.map viewPlayer (Set.toList model.players) ++
        [ input [ placeholder "name", value name, onInput InputPlayer ] []
        , button [ onClick AddPlayer ] [ text "Add Player" ]
        , div [] [ button [ onClick StartMatches, disabled (Set.isEmpty model.players) ]
            [ text "Start playing!" ] ]
        ]

    Playing matches ->
      div [] <|
        List.map viewMatch matches

    _ -> text "unsupported state"


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
  in
    div [] <|
      [ text msg ]

viewPlayer : String -> Html Msg
viewPlayer name =
    div [] <|
      [ button [ onClick (RemovePlayer name) ] [ text "X" ]
      , text name
      ]