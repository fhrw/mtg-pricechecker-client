module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, input, p, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)


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


removeFromTable : List String -> String -> List String
removeFromTable list item =
    List.filter (\x -> not (String.contains item x)) list


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


viewShop : ShopOrder -> Html Msg
viewShop shop =
    div []
        [ h3 [] [ text shop.shopName ]
        , viewCardsList shop.cards
        ]


viewOptimized : List ShopOrder -> Html Msg
viewOptimized orderList =
    div []
        (List.map viewShop orderList)


type Msg
    = SetSearchInput String
    | AddToList String
    | RemoveFromList String


type alias ShopOrder =
    { shopName : String
    , cards : List Card
    }


type alias Card =
    { name : String
    , price : Float
    }


type alias Model =
    { searchInput : String
    , cardsList : List String
    , optimizedOrder :
        { price : Float
        , arrangement : List ShopOrder
        }
    }


validCards : List String
validCards =
    [ "island", "forest", "plains" ]


initialModel : Model
initialModel =
    { searchInput = ""
    , cardsList = []
    , optimizedOrder =
        { price = 0
        , arrangement = []
        }
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Optimizer" ]
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
            if List.member newCard validCards && not (List.member newCard model.cardsList) then
                { model | cardsList = List.append model.cardsList [ newCard ] }

            else
                model

        RemoveFromList card ->
            { model | cardsList = removeFromTable model.cardsList card }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
