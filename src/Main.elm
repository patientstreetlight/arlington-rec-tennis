module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
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
    | CreateMatches


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
            team1 = mkDoublesTeam p1 p2
            team2 = mkDoublesTeam p3 "Sub"
            match = { court = courtNum, team1 = team1, team2 = team2 }
          in
            [ match ]
        (p1 :: p2 :: p3 :: p4 :: rest) ->
          let
            team1 = mkDoublesTeam p1 p2
            team2 = mkDoublesTeam p3 p4
            match = { court = courtNum, team1 = team1, team2 = team2 }
          in
            match :: go rest (courtNum + 1)

  in
    go players 1


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
        [ List.map viewMatch model.matches
        , [ div []
            [ Button.button
                [ Button.success
                , Button.attrs [ onClick CreateMatches ]
                ]
                [ text "Create new matches" ] ] ]
        ]

    Scores -> viewScores model.scores


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


viewMatch : Match -> Html Msg
viewMatch match =
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
            [ Block.titleH5 [] [ text <| "Court " ++ String.fromInt match.court ]
            , Block.custom <|
                Grid.container []
                    [ Grid.row []
                        [ Grid.col [] [ viewTeam match.team1 ]
                        , Grid.col [] [ viewTeam match.team2 ]
                        ]
                    ]
            , Block.custom <|
                Button.button [ Button.primary ] [ text "Finish" ]
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