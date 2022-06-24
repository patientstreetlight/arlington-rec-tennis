module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Set exposing (Set)
import Html exposing (Html, button, div, input, text)
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


update : Msg -> Model -> Model
update msg model =
  case (msg, model.state) of
    (InputPlayer s, Setup _) ->
        { model | state = Setup s }

    (AddPlayer, Setup "") -> model

    (AddPlayer, Setup player) ->
        { model | state = Setup "", players = Set.insert player model.players }

    _ -> model


-- VIEW
view : Model -> Html Msg
view model =
  case model.state of
    Setup name ->
      div [] <|
        List.map viewPlayer (Set.toList model.players) ++
        [ input [ placeholder "name", value name, onInput InputPlayer ] []
        , button [ onClick AddPlayer ] [ text "Add Player" ]
        ]
    _ -> text "unsupported state"


viewPlayer : String -> Html Msg
viewPlayer name = div [] [ text name ]