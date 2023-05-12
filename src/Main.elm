module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, input, p, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (remove)
import Platform.Cmd as Cmd



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }



-- MODEL


validCards : List String
validCards =
    [ "island", "forest", "plains" ]


type alias Model =
    { searchInput : String
    , cardsList : List String
    , optimizedOrder : OptOrder
    }


type OptOrder
    = Loading
    | Success
        { price : Float
        , arrangement : List ShopOrder
        }
    | Failure
    | Unloaded


type alias ShopOrder =
    { shopName : String
    , cards : List Card
    }


type alias Card =
    { name : String
    , price : Float
    }


initialModel : Model
initialModel =
    { searchInput = ""
    , cardsList = []
    , optimizedOrder = Unloaded
    }



-- UPDATE


type Msg
    = SetSearchInput String
    | AddToList String
    | RemoveFromList String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSearchInput newText ->
            ( { model | searchInput = newText }, Cmd.none )

        AddToList newCard ->
            if List.member newCard validCards && not (List.member newCard model.cardsList) then
                ( { model | cardsList = List.append model.cardsList [ newCard ] }, Cmd.none )

            else
                ( model, Cmd.none )

        RemoveFromList card ->
            ( { model | cardsList = removeFromTable model.cardsList card }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Optimizer" ]
        , input [ placeholder "name", onInput SetSearchInput ] []
        , button [ onClick (AddToList model.searchInput) ] [ text "add" ]
        , orderTable model.cardsList
        , viewOptimized model.optimizedOrder
        , button [] [ text "scrape and optimize!" ]
        ]


removeFromTable : List String -> String -> List String
removeFromTable list item =
    remove item list


viewCardsList : List Card -> Html Msg
viewCardsList cards =
    div []
        (List.map
            (\ele ->
                div []
                    [ p [] [ text ele.name ]
                    , p [] [ text (String.fromFloat ele.price) ]
                    ]
            )
            cards
        )


orderTable : List String -> Html Msg
orderTable names =
    div []
        (List.map
            (\ele ->
                div []
                    [ p [] [ text ele ]
                    , button [ onClick (RemoveFromList ele) ] [ text "x" ]
                    ]
            )
            names
        )


viewShop : ShopOrder -> Html Msg
viewShop shop =
    div []
        [ h3 [] [ text shop.shopName ]
        , viewCardsList shop.cards
        ]


viewOptimized : OptOrder -> Html Msg
viewOptimized orderList =
    case orderList of
        Success data ->
            div []
                (List.map viewShop data.arrangement)

        Loading ->
            div [] [ p [] [ text "Calculating the perfect order..." ] ]

        Failure ->
            div [] [ p [] [ text "Failed somehow. Guess it it wasn't so perfect after all..." ] ]

        Unloaded ->
            div [] [ p [] [ text "waiting for you to press optimize!" ] ]
