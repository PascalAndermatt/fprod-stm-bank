module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Json.Decode exposing (Decoder, at, list, string, succeed)
import Http exposing (expectJson)

import Json.Encode
import Json.Decode exposing (field)

type alias BankAccount =
    { balance : Int
    , active : Bool
    , owner : String
    , ibanNr : String
    }

decodeBankAccounts : Json.Decode.Decoder (List BankAccount)
decodeBankAccounts = Json.Decode.list decodeBankAccount

decodeBankAccount : Json.Decode.Decoder BankAccount
decodeBankAccount =
    Json.Decode.map4 BankAccount
        (field "balance" Json.Decode.int)
        (field "active" Json.Decode.bool)
        (field "owner" Json.Decode.string)
        (field "ibanNr" Json.Decode.string)

encodeBankAccount : BankAccount -> Json.Encode.Value
encodeBankAccount record =
    Json.Encode.object
        [ ("balance",  Json.Encode.int <| record.balance)
        , ("active",  Json.Encode.bool <| record.active)
        , ("owner",  Json.Encode.string <| record.owner)
        , ("ibanNr",  Json.Encode.string <| record.ibanNr)
        ]


main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }


type alias Model =
  { bankAccounts : List BankAccount
  }

init : ( Model, Cmd Msg )
init =
  ( { bankAccounts = []}, Cmd.none )

type Msg
  -- TODO add a second Msg constructor for the result returned by getBitcoinPrice
  = GetAllBankAccounts | BankAccountsResult (Result Http.Error (List BankAccount))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    -- TODO1: Execute the API request when the button is pressed
    GetAllBankAccounts ->
      ( model, getAllBankAccounts )
    BankAccountsResult result  -> case result of
       Ok listWithAccounts -> ({model | bankAccounts = listWithAccounts}, Cmd.none)
       Err httpError -> ({model | bankAccounts = []}, Cmd.none )
    -- TODO2: Handle the result of the API request

getAllBankAccounts : Cmd Msg
getAllBankAccounts = Http.get 
                        { url = "http://localhost:4000/accounts", 
                          expect = Http.expectJson BankAccountsResult decodeBankAccounts 
                        }

view : Model -> Html Msg
view model =
  div [ class "container text-center jumbotron" ]
    -- Responsive fixed width container
    [ 
      button [ type_ "button", class "btn btn-primary", onClick GetAllBankAccounts ] [ text "get all accounts" ],
      (viewMainContent model)
    ]

viewMainContent : Model -> Html Msg
viewMainContent model =
  div [] (List.map createBankAccountElement model.bankAccounts)
    

createBankAccountElement : BankAccount -> Html Msg
createBankAccountElement acc = 
          div []
          [
            p [] [text acc.ibanNr],
            p [] [text acc.owner],
            p [] [text (String.fromInt acc.balance)]
          ]

