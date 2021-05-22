module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Json.Decode exposing (Decoder, at, list, string, succeed)
import Http exposing (expectJson)


{-

Bitcoin Hype:

You want to get rich, and the best way to get rich fast is to invest all your money in bitcoins.
To figure out if you are a millionaire yet, you want to build a small web application retrieving
the current bitcoin value from a REST API and displaying it.


You are given the skeleton below. Fill it in to retrieve the current bitcoin price when the
button is clicked.


Optional:
Load the bitcoin price immediately when the application is loaded.


-}



main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }


type alias Model =
  { currentPrice : Maybe String
  }


init : ( Model, Cmd Msg )
init =
  ( { currentPrice = Nothing }, getBitcoinPrice )


type Msg
  -- TODO add a second Msg constructor for the result returned by getBitcoinPrice
  = GetBitcoinPrice | BitcoinPriceResult (Result Http.Error String)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    -- TODO1: Execute the API request when the button is pressed
    GetBitcoinPrice ->
      ( model, getBitcoinPrice )
    BitcoinPriceResult result  -> case result of
       Ok price -> ({model | currentPrice = Just price}, Cmd.none)
       Err httpError -> ({model | currentPrice = Nothing}, Cmd.none )
    -- TODO2: Handle the result of the API request



-- TODO: Take a look at https://package.elm-lang.org/packages/elm/http/latest/Http#expectJson to see how
-- you can parse a JSON response. The actual JSON decoder is already provided for you, see decodeContent.
--
-- The API URL is http://api.coindesk.com/v1/bpi/currentprice.json
getBitcoinPrice : Cmd Msg
getBitcoinPrice = Http.get 
                        { url = "http://api.coindesk.com/v1/bpi/currentprice.json", 
                          expect = Http.expectJson BitcoinPriceResult decodeContent 
                        }

decodeContent : Decoder String
decodeContent =
  at [ "bpi", "USD", "rate" ] string


---------------------------------------------------------------------------------------------------------------------------------
-- Feel free to take a look below, but no changes should be necessary
---------------------------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
  div [ class "container text-center jumbotron" ]
    -- Responsive fixed width container
    [ viewMainContent model ]

viewMainContent : Model -> Html Msg
viewMainContent model =
  div []
    [ p [] [ h1 [] [ text "Bitcoin Price" ], h5 [] [ text "It costs right\n        now:" ] ]
    , p [ class "alert alert-success" ]
      [ h4 []
        [ text
          (Maybe.withDefault
            "Not yet!"
            model.currentPrice
          )
        , text " USD"
        ]
      ]
    , button [ type_ "button", class "btn btn-primary", onClick GetBitcoinPrice ]
      [ text "Update it now" ]
    , div [ class "text-right small" ]
      [ a [ href "https://github.com/dailydrip/elm-bitcoin-price" ]
        [ text
          "Source Code"
        ]
      ]
    ]


