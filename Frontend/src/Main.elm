module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Json.Decode exposing (Decoder, at, list, string, succeed)
import Http exposing (expectJson)

import Json.Encode
import Json.Decode exposing (field)
import Html.Events exposing (onInput)
import Http

type alias BankAccountRequest =
    { owner : String
    , balance : Int
    }

decodeBankAccountRequest : Json.Decode.Decoder BankAccountRequest
decodeBankAccountRequest =
    Json.Decode.map2 BankAccountRequest
        (field "owner" Json.Decode.string)
        (field "balance" Json.Decode.int)

encodeBankAccountRequest : BankAccountRequest -> Json.Encode.Value
encodeBankAccountRequest record =
    Json.Encode.object
        [ ("owner",  Json.Encode.string <| record.owner)
        , ("balance",  Json.Encode.int <| record.balance)
        ]

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
  { bankAccounts : List BankAccount,
    newOwner : String,
    newBalance : Int
  }

init : ( Model, Cmd Msg )
init =
  ( { bankAccounts = [], newOwner = "", newBalance = 0}, Cmd.none )

type Msg = GetAllBankAccounts | 
           BankAccountsResult (Result Http.Error (List BankAccount)) | 
           SetOwner String | 
           SetBalance Int |
           CreateBankAccount |
           CreateBankAccountResult (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetOwner owner -> ({model | newOwner = owner}, Cmd.none)
    SetBalance bal -> ({model | newBalance = bal}, Cmd.none)
    CreateBankAccount -> (model, createBankAccount model)
    GetAllBankAccounts ->
      ( model, getAllBankAccounts )
    BankAccountsResult result  -> case result of
       Ok listWithAccounts -> ({model | bankAccounts = listWithAccounts}, Cmd.none)
       Err httpError -> ({model | bankAccounts = []}, Cmd.none )
    CreateBankAccountResult result -> case result of
       Ok _ -> (model, Cmd.none)
       Err httpError -> (model, Cmd.none)

getAllBankAccounts : Cmd Msg
getAllBankAccounts = Http.get 
                        { url = "http://localhost:4000/accounts",
                          expect = Http.expectJson BankAccountsResult decodeBankAccounts 
                        }

createBankAccount : Model -> Cmd Msg
createBankAccount model = Http.post 
                        { url = "http://localhost:4000/accounts",
                          body = Http.jsonBody (encodeBankAccountRequest {owner = model.newOwner, balance = model.newBalance}),
                          expect = Http.expectWhatever CreateBankAccountResult
                        }

view : Model -> Html Msg
view model = div [] [
    createNavBar,
    div [ class "container" ]
      [ 
        h3 [class "mb-4"] [text "Bankaccounts"],
        table [class "table"] [
          thead [] [
            tr [] [
              th [scope "col"] [text "owner"],
              th [scope "col"] [text "IBAN"],
              th [scope "col"] [text "active"],
              th [scope "col"] [text "balance"]
            ]
          ],
          tbody [] (List.map createTableRowFromBankAccount model.bankAccounts)
        ],
        button [ type_ "button", class "btn btn-primary", onClick GetAllBankAccounts ] [ text "get all accounts" ]
      ],
      newAccountView
  ]
  
newAccountView : Html Msg
newAccountView = div [class "container mt-5"] [
    h3 [] [text "create new Bankaccount"],
    div [] [
      label [for "owner-input", class "form-label"] [text "Owner"],
      input [type_ "text", class "form-control", id "owner-input", placeholder "Peter", onInput SetOwner] []
    ],
    div [class "mb-4"] [
      label [for "balance-input", class "form-label"] [text "Balance"],
      input [type_ "text", class "form-control", id "balance-input", placeholder "1000", onInput (\str -> SetBalance (Maybe.withDefault 0 (String.toInt str)))] []
    ],
    button [ type_ "button", class "btn btn-primary", onClick CreateBankAccount] [ text "create" ]
  ]

getAccountStatus : BankAccount -> String
getAccountStatus acc = if acc.active then "active" else "inactive"

createTableRowFromBankAccount : BankAccount -> Html Msg
createTableRowFromBankAccount acc = tr [] [
    th [scope "row"] [text acc.owner],
    td [] [text acc.ibanNr],
    td [] [text (getAccountStatus acc)],
    td [] [text (String.fromInt acc.balance)]
  ]

createNavBar : Html Msg
createNavBar = div [class "mb-4", style "background-color" "#5E5184", style "height" "100px"] [
      div [class "container-fluid"] [
        div [class "row align-items-center gx-5"] [
            
            div [class "col-1 p-3"] [
                img [src "/src/assets/haskell.png", width 50, height 50] []
            ],
            h1 [class "col-3 p-3"] [text "STM Bank"]
            
          ]
      ]
  ]