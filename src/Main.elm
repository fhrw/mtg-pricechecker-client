module Main exposing (main)

import Browser
import Fuzzy exposing (match)
import Html exposing (Html, button, div, h1, h3, input, p, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, Error)
import Json.Encode as Encode exposing (object)
import List.Extra exposing (remove)
import String



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
    [ "Sol Ring", "Badlands", "Cryptic Command", "Island", "Mountain", "Wasteland", "Rhox War Monk", "Polymorph" ]


type alias Model =
    { searchInput : String
    , searchResults : SearchResults
    , cardsList : List String
    , optimizedOrder : OptOrder
    }


type OptOrder
    = Loading
    | Success
        { price : Float
        , arrangement : ShopOrder
        }
    | Failure
    | Unloaded


type alias SearchResults =
    List String


type alias Payload =
    { data : ShopOrder }


type alias Data =
    List ShopOrder


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
    , searchResults = []
    , cardsList = []
    , optimizedOrder = Unloaded
    }



-- UPDATE


type Msg
    = SetSearchInput String
    | AddToList String
    | RemoveFromList String
    | GetOptimize
    | GotOptimized (Result Http.Error Payload)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSearchInput newText ->
            ( { model
                | searchInput = newText
                , searchResults = setSearchResults newText validCards
              }
            , Cmd.none
            )

        AddToList newCard ->
            if List.member newCard validCards && not (List.member newCard model.cardsList) then
                ( { model
                    | searchInput = ""
                    , cardsList = List.append model.cardsList [ newCard ]
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        RemoveFromList card ->
            ( { model | cardsList = removeFromTable model.cardsList card }, Cmd.none )

        GetOptimize ->
            ( { model | optimizedOrder = Loading }, loadOpti model )

        GotOptimized result ->
            case result of
                Err _ ->
                    ( { model
                        | optimizedOrder = Failure
                      }
                    , Cmd.none
                    )

                Ok payload ->
                    ( { model
                        | optimizedOrder =
                            Success
                                { price =
                                    List.map (\x -> x.price) payload.data.cards
                                        |> List.foldl (\x a -> x + a) 0
                                , arrangement = payload.data
                                }
                      }
                    , Cmd.none
                    )


encodeCardJson cardName =
    Encode.object [ ( "name", Encode.string cardName ) ]


encodeCardList : List String -> List Encode.Value
encodeCardList list =
    List.map (\x -> encodeCardJson x) list


encodeListArray list =
    Encode.list identity list


encodePayload array =
    Encode.object [ ( "List", array ) ]


loadOpti : Model -> Cmd Msg
loadOpti model =
    Http.post
        { url = "http://localhost:8080/best"
        , body =
            encodeCardList model.cardsList
                |> encodeListArray
                |> encodePayload
                |> Http.jsonBody
        , expect = Http.expectJson GotOptimized decodePayload
        }


decodeCard : Decoder Card
decodeCard =
    Decode.map2 Card
        (Decode.field "name" Decode.string)
        (Decode.field "price" Decode.float)


decodeShop : Decoder ShopOrder
decodeShop =
    Decode.map2 ShopOrder
        (Decode.field "Name" Decode.string)
        (Decode.field "Cards" (Decode.list decodeCard))


decodeData : Decoder Data
decodeData =
    Decode.list decodeShop


decodePayload : Decoder Payload
decodePayload =
    Decode.map Payload
        (Decode.field "data" decodeShop)


setSearchResults : String -> List String -> SearchResults
setSearchResults string cards =
    if string == "" then
        []

    else
        let
            simpleMatch config separators needle hay =
                match config separators needle hay |> .score
        in
        List.sortBy (simpleMatch [] [] (String.toLower string)) (List.map (\x -> String.toLower x) cards)
            |> List.map (\x -> capitalizeCard x)
            |> topSuggestion


capitalizeCard : String -> String
capitalizeCard string =
    String.words string
        |> List.map (\x -> String.join "" [ String.toUpper (String.left 1 x), String.right (String.length x - 1) x ])
        |> String.join " "


topSuggestion : SearchResults -> SearchResults
topSuggestion list =
    List.take 3 list



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Optimizer" ]
        , input [ placeholder "name", value model.searchInput, onInput SetSearchInput ] []
        , viewSuggestedResults model.searchResults
        , button [ onClick (AddToList model.searchInput) ] [ text "add" ]
        , orderTable model.cardsList
        , viewOptimized model.optimizedOrder
        , button [ onClick GetOptimize ] [ text "scrape and optimize!" ]
        ]


removeFromTable : List String -> String -> List String
removeFromTable list item =
    remove item list


viewSuggestedResults : List String -> Html Msg
viewSuggestedResults list =
    div []
        (List.map
            (\ele ->
                div []
                    [ p [] [ text ele ]
                    , button [ onClick (AddToList ele) ] [ text "quick add" ]
                    ]
            )
            list
        )


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


viewShop : ShopOrder -> Float -> Html Msg
viewShop shop price =
    div []
        [ h3 [] [ text shop.shopName ]
        , p [] [ text ("total price: " ++ String.fromFloat price) ]
        , viewCardsList shop.cards
        ]


viewOptimized : OptOrder -> Html Msg
viewOptimized orderList =
    case orderList of
        Success data ->
            div []
                [ viewShop
                    data.arrangement
                    data.price
                ]

        Loading ->
            div [] [ p [] [ text "Calculating the perfect order..." ] ]

        Failure ->
            div [] [ p [] [ text "Failed somehow. Guess it it wasn't so perfect after all..." ] ]

        Unloaded ->
            div [] [ p [] [ text "waiting for you to press optimize!" ] ]
