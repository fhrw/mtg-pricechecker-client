module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, p, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)


orderTable : List String -> Html Msg
orderTable names =
    div []
        (List.map (\ele -> p [] [ text ele ]) names)


type Msg
    = SetSearchInput String
    | AddToList String


type alias Model =
    { searchInput : String
    , cardsList : List String
    }


validCards : List String
validCards =
    [ "island", "forest", "plains" ]


initialModel : Model
initialModel =
    { searchInput = ""
    , cardsList = []
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Buy Optimizer" ]
        , input [ placeholder "name", onInput SetSearchInput ] []
        , button [ onClick (AddToList model.searchInput) ] [ text "add" ]
        , orderTable model.cardsList
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetSearchInput newText ->
            { model | searchInput = newText }

        AddToList newCard ->
            if List.member newCard validCards then
                { model | cardsList = List.append model.cardsList [ newCard ] }

            else
                model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
