module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Modal as Modal
import Bootstrap.Table as Table
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
import Random.List exposing (shuffle)
import Random exposing (generate)

main =
  Browser.element
      { init = \() -> (initModel, Cmd.none)
      , update = update
      , view = view
      , subscriptions = \_ -> Sub.none
      }

-- MODEL
type alias Model =
    { players : Set String
    , tab : Tab
    , pastTeams : List Team
    , newPlayer : String
    , matches : Dict CourtNum Match
    , scores : Dict String Int
    , modal : Maybe Modal
    }


initModel : Model
initModel =
    { players = Set.empty
    , tab = Players
    , pastTeams = []
    , newPlayer = ""
    , matches = Dict.empty
    , scores = Dict.empty
    , modal = Nothing
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
    , scores : Maybe (Int, Int)
    }

newMatch : Team -> Team -> Match
newMatch t1 t2 =
    { team1 = t1
    , team2 = t2
    , scores = Nothing
    }

type Modal
    = ScoreModal ScoreInput

type alias ScoreInput =
    { court : CourtNum
    , match : Match
    , team1Score : String
    , team2Score : String
    }

type alias CourtNum = Int


-- UPDATE
type Msg
    = AddPlayer
    | InputPlayer String
    | RemovePlayer String
    | SelectTab Tab
    | CreateMatches
    | CloseModal
    | OpenScoreModal (CourtNum, Match)
    | ScoreModalSubmitScores
    | ScoreModalInputScore1 String
    | ScoreModalInputScore2 String
    | ShuffledPlayers (List String)


withNoneCmd : Model -> (Model, Cmd Msg)
withNoneCmd model = (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectTab t ->
        { model | tab = t } |> withNoneCmd

    AddPlayer ->
        withNoneCmd <| case model.newPlayer of
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
        } |> withNoneCmd

    InputPlayer name ->
        { model | newPlayer = name } |> withNoneCmd

    CreateMatches ->
        (model, randomPlayersCmd model)
    
    ShuffledPlayers shuffledPlayers ->
        startMatches shuffledPlayers model |> withNoneCmd
    
    CloseModal ->
        { model | modal = Nothing } |> withNoneCmd
    
    OpenScoreModal courtAndMatch ->
        openScoreModal courtAndMatch model |> withNoneCmd
    
    ScoreModalSubmitScores ->
        scoreModalSubmitScores model |> withNoneCmd
    
    ScoreModalInputScore1 score ->
        updateScoreModal setModalScore1 score model |> withNoneCmd
    
    ScoreModalInputScore2 score ->
        updateScoreModal setModalScore2 score model |> withNoneCmd

openScoreModal : (Int, Match) -> Model -> Model
openScoreModal (courtNum, match) model =
  let
    (team1Score, team2Score) =
        case match.scores of
            Nothing -> ("", "")
            Just (score1, score2) ->
                (String.fromInt score1, String.fromInt score2)
  in
    { model
    | modal =
        Just <| ScoreModal
            { court = courtNum
            , match = match
            , team1Score = team1Score
            , team2Score = team2Score
            }
    }

scoreModalSubmitScores : Model -> Model
scoreModalSubmitScores model =
    case model.modal of
        Nothing -> model
        Just (ScoreModal scoreModal) ->
          let
            players : Team -> List String
            players team =
                case team of
                    SinglesTeam p -> [p]
                    DoublesTeam p1 p2 -> [p1, p2]
            
            parseScore : String -> Int
            parseScore score =
                String.toInt score |> Maybe.withDefault 0
            
            team1Players = players scoreModal.match.team1
            team2Players = players scoreModal.match.team2

            team1Score = parseScore scoreModal.team1Score
            team2Score = parseScore scoreModal.team2Score

            (oldTeam1Score, oldTeam2Score) =
                Maybe.withDefault (0, 0) scoreModal.match.scores

            addToScore : (String, Int) -> Dict String Int -> Dict String Int
            addToScore (player, score) scores =
                Dict.update player (Maybe.map ((+) score)) scores

            playersWithScoreDeltas =
                List.concat
                    [ List.map (\p -> (p, team1Score - oldTeam1Score)) team1Players
                    , List.map (\p -> (p, team2Score - oldTeam2Score)) team2Players
                    ]
            
            updatedScores =
                List.foldl addToScore model.scores playersWithScoreDeltas
            
            finishMatch : Match -> Match
            finishMatch match =
                { match | scores = Just (team1Score, team2Score) }
            
            updatedMatches =
                Dict.update scoreModal.court (Maybe.map finishMatch) model.matches
          in
            { model
            | scores = updatedScores
            , modal = Nothing
            , matches = updatedMatches
            }

updateScoreModal : (String -> ScoreInput -> ScoreInput) -> String -> Model -> Model
updateScoreModal setter newScore model =
  let
    setModalScore : ScoreInput -> ScoreInput
    setModalScore modal =
        if String.all isDigit newScore then
            setter newScore modal
        else
            modal
  in
    case model.modal of
        Just (ScoreModal scoreModal) ->
            { model | modal = Just <| ScoreModal <| setModalScore scoreModal }
        _ -> model


setModalScore1 : String -> ScoreInput -> ScoreInput
setModalScore1 newScore modal =
    { modal | team1Score = newScore }

setModalScore2 : String -> ScoreInput -> ScoreInput
setModalScore2 newScore modal =
    { modal | team2Score = newScore }



-- Create random assignment of people to partners/courts
startMatches : List String -> Model -> Model
startMatches randomizedPlayers model =
  let
    matches = mkMatches randomizedPlayers
  in
    { model | tab = Matches, matches = matches }


randomPlayersCmd : Model -> Cmd Msg
randomPlayersCmd model =
    generate ShuffledPlayers <| shuffle <| Set.toList model.players


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
              (newMatch (SinglesTeam p) (SinglesTeam "Coach"))
              matches
        [p1, p2] ->
          let
            team1 = SinglesTeam p1
            team2 = SinglesTeam p2
          in
            Dict.insert
                courtNum
                (newMatch team1 team2)
                matches
        [p1, p2, p3] ->
          let
            team1 = mkDoublesTeam p1 p2
            team2 = mkDoublesTeam p3 "Coach"
            match = newMatch team1 team2
          in
            Dict.insert courtNum match matches
        (p1 :: p2 :: p3 :: p4 :: rest) ->
          let
            team1 = mkDoublesTeam p1 p2
            team2 = mkDoublesTeam p3 p4
            match = newMatch team1 team2
            newMatches = Dict.insert courtNum match matches
          in
            go rest (courtNum + 1) newMatches

  in
    go players 1 Dict.empty


-- VIEW
view : Model -> Html Msg
view model = withHeader model <| withModal model <|
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
        , [ div []
            [ Button.button
                [ Button.success
                , Button.attrs [ onClick CreateMatches ]
                ]
                [ text "Create new matches" ] ] ]
        ]

    Scores -> viewScores model.scores


viewScoreModal : Model -> ScoreInput -> Modal.Config Msg -> Modal.Config Msg
viewScoreModal model scoreModal modalCfg =
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
    modalCfg
      |> Modal.body []
          [ teamInput scoreModal.match.team1 scoreModal.team1Score ScoreModalInputScore1
          , teamInput scoreModal.match.team2 scoreModal.team2Score ScoreModalInputScore2
          ]
      |> Modal.footer []
          [ Button.button
              [ Button.primary, Button.attrs [ onClick ScoreModalSubmitScores ] ]
              [ text "Submit" ]
          ]


viewScores : Dict String Int -> Html Msg
viewScores scores =
  let
    viewScore (player, score) =
        Table.tr []
            [ Table.td [] [ text player ]
            , Table.td [] [ text <| String.fromInt score ]
            ]

    descendingScores =
        Dict.toList scores |> List.sortBy Tuple.second |> List.reverse
  in
    Table.table
        { options = []
        , thead = Table.simpleThead
            [ Table.th [] [ text "Player" ]
            , Table.th [] [ text "Score" ]
            ]
        , tbody =
            Table.tbody [] <| List.map viewScore descendingScores
        }


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

withModal : Model -> Html Msg -> Html Msg
withModal model html =
    case model.modal of
        Nothing -> html
        Just modal ->
          let
            mkModal =
                case modal of
                    ScoreModal scoreModal -> viewScoreModal model scoreModal
          in
            div []
                [ html
                , Modal.config CloseModal
                    |> Modal.small
                    |> Modal.hideOnBackdropClick True
                    |> mkModal
                    |> Modal.view Modal.shown
                ]

viewMatch : (Int, Match) -> Html Msg
viewMatch (court, match) =
    case match.scores of
        Nothing -> viewInProgressMatch court match
        Just scores ->
            viewFinishedMatch court match scores

viewFinishedMatch : Int -> Match -> (Int, Int) -> Html Msg
viewFinishedMatch court match (score1, score2) =
  let
    viewTeam team score =
      case team of
        SinglesTeam p ->
            div []
                [ div [] [ text p ]
                , div [] [ text <| "(" ++ String.fromInt score ++ ")"]
                ]
        DoublesTeam p1 p2 ->
            div []
                [ div [] [ text p1 ]
                , div [] [ text p2 ]
                , div [] [ text <| "(" ++ String.fromInt score ++ ")" ]
                ]

    editButton =
        Button.button
            [ Button.secondary
            , Button.attrs [ onClick <| OpenScoreModal (court, match) ]
            ]
            [ text "Edit" ]
  in
    Card.config
        [ Card.outlineSecondary
        ]
        |> Card.block []
            [ Block.titleH5 [] [ text <| "Court " ++ String.fromInt court ]
            , Block.custom <|
                Grid.container []
                    [ Grid.row []
                        [ Grid.col [] [ viewTeam match.team1 score1 ]
                        , Grid.col [] [ viewTeam match.team2 score2 ]
                        ]
                    ]
            , Block.custom editButton
            ]
        |> Card.view

viewInProgressMatch : Int -> Match -> Html Msg
viewInProgressMatch court match =
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
    
    finishButton =
        Button.button
            [ Button.primary
            , Button.attrs [ onClick <| OpenScoreModal (court, match) ]
            ]
            [ text "Finish" ]
                
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
            , Block.custom finishButton
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