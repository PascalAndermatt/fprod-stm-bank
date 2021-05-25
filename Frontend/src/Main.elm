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
import Http
import Http exposing (expectWhatever)
import Http
import Platform.Cmd exposing (Cmd)
import Http
import Http

type alias TransferRequest =
    { from : String
    , to : String
    , amount : Int
    }

decodeTransferRequest : Json.Decode.Decoder TransferRequest
decodeTransferRequest =
    Json.Decode.map3 TransferRequest
        (field "from" Json.Decode.string)
        (field "to" Json.Decode.string)
        (field "amount" Json.Decode.int)

encodeTransferRequest : TransferRequest -> Json.Encode.Value
encodeTransferRequest record =
    Json.Encode.object
        [ ("from",  Json.Encode.string <| record.from)
        , ("to",  Json.Encode.string <| record.to)
        , ("amount",  Json.Encode.int <| record.amount)]

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
    newBalance : Int,
    ibanForUpdate : String,
    balanceForUpdate : Int,
    updateMethod : String,
    ibanFrom : String,
    ibanTo : String,
    amountForTransfer : Int,
    error : String
  }

init : ( Model, Cmd Msg )
init =
  ( { bankAccounts = [], 
      newOwner = "", 
      newBalance = 0,
      ibanForUpdate = "",
      balanceForUpdate = 0,
      updateMethod = "",
      ibanFrom = "",
      ibanTo = "",
      amountForTransfer = 0,
      error = ""}, Cmd.none )

type Msg = GetAllBankAccounts | 
           BankAccountsResult (Result Http.Error (List BankAccount)) | 
           SetOwner String | 
           SetBalance Int |
           CreateBankAccount |
           CreateBankAccountResult (Result Http.Error ()) |
           SetIbanForUpdate String |
           SetBalanceForUpdate Int |
           SetUpdateMethod String |
           UpdateBalance |
           UpdateBalanceResult (Result Http.Error BankAccount) |
           SetIbanFrom String |
           SetIbanTo String |
           SetAmountForTransfer Int |
           Transfer |
           TransferResult (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetIbanForUpdate iban -> ({model | ibanForUpdate = iban}, Cmd.none)
    SetBalanceForUpdate bal -> ({model | balanceForUpdate = bal}, Cmd.none)
    SetOwner owner -> ({model | newOwner = owner}, Cmd.none)
    SetBalance bal -> ({model | newBalance = bal}, Cmd.none)
    SetUpdateMethod str -> ({model | updateMethod = str}, Cmd.none)
    CreateBankAccount -> (model, createBankAccount model)
    UpdateBalance -> (model, updateBalance model)
    GetAllBankAccounts ->
      ( model, getAllBankAccounts )
    BankAccountsResult result  -> case result of
       Ok listWithAccounts -> ({model | bankAccounts = listWithAccounts}, Cmd.none)
       Err httpError -> ({model | bankAccounts = []}, Cmd.none )
    CreateBankAccountResult result -> case result of
       Ok _ -> (model, Cmd.none)
       Err httpError -> (model, Cmd.none)
    UpdateBalanceResult result -> case result of
       Ok _ -> (model, Cmd.none)
       Err httpError -> case httpError of
          Http.BadBody _ -> ({model | error = "Test1"}, Cmd.none)
          Http.BadUrl _ -> ({model | error = "Test2"}, Cmd.none)
          Http.NetworkError -> ({model | error = "Test3"}, Cmd.none)
          Http.Timeout -> ({model | error = "Test4"}, Cmd.none)
          Http.BadStatus _ -> ({model | error = "Test5"}, Cmd.none)
    SetIbanTo to -> ({model | ibanTo = to}, Cmd.none)
    SetIbanFrom from -> ({model | ibanFrom = from}, Cmd.none)
    SetAmountForTransfer amount -> ({model | amountForTransfer = amount}, Cmd.none)
    Transfer -> (model, transfer model)
    TransferResult result -> case result of
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

updateBalance : Model -> Cmd Msg
updateBalance model = Http.post
                      {
                        url = "http://localhost:4000/accounts/" ++ model.ibanForUpdate ++ "/" ++ String.toLower model.updateMethod ++ "?amount=" ++ (String.fromInt model.balanceForUpdate),
                        body = Http.emptyBody,
                        expect = Http.expectJson UpdateBalanceResult decodeBankAccount
                      }

transfer : Model -> Cmd Msg
transfer model = Http.post
                  {
                    url = "http://localhost:4000/accounts/transfer",
                    body = Http.jsonBody (encodeTransferRequest {from = model.ibanFrom, to = model.ibanTo, amount = model.amountForTransfer}),
                    expect = Http.expectWhatever TransferResult
                  }

view : Model -> Html Msg
view model = div [class "mb-5"] [
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
        button [ type_ "button", class "btn haskell-btn", onClick GetAllBankAccounts ] [ text "get all accounts" ]
      ],
      newAccountView,
      updateBalanceView,
      transferView,
      if model.error /= "" then errorView model else div [] []
 ]

errorView : Model -> Html Msg
errorView model = div [class "alert alert-danger", attribute "role" "alert"] [
  text model.error]
  
newAccountView : Html Msg
newAccountView = div [class "container mt-5"] [
    h3 [] [text "create new Bankaccount"],
    haskellBorder [
      div [] [
        label [for "owner-input", class "form-label"] [text "Owner"],
        input [type_ "text", class "form-control", id "owner-input", placeholder "Peter", onInput SetOwner] []
      ],
      div [class "mb-4"] [
        label [for "balance-input", class "form-label"] [text "Balance"],
        input [type_ "text", class "form-control", id "balance-input", placeholder "1000", onInput (\str -> SetBalance (Maybe.withDefault 0 (String.toInt str)))] []
      ],
      button [ type_ "button", class "btn haskell-btn", onClick CreateBankAccount] [ text "create" ]
    ]
  ]

updateBalanceView : Html Msg
updateBalanceView = div [class "container mt-5"] [
    h3 [] [text "update balance of Bankaccount"],
    haskellBorder [
      div [] [
        label [for "iban-input", class "form-label"] [text "IBAN"],
        input [type_ "text", class "form-control", id "iban-input", placeholder "CH2707888954202552370TUR", onInput SetIbanForUpdate] []
      ],
      div [class "mb-4"] [
        label [for "balance-input", class "form-label"] [text "Balance"],
        input [type_ "text", class "form-control", id "balance-input", placeholder "1000", onInput (\str -> SetBalanceForUpdate (Maybe.withDefault 0 (String.toInt str)))] []
      ],
      select [class "form-select mb-4", onInput SetUpdateMethod] [
        option [selected True ] [text "choose update action"],
        option [value "withdraw"] [text "withdraw"],
        option [value "deposit"] [text "deposit"]
      ],
      button [ type_ "button", class "btn haskell-btn", onClick UpdateBalance] [ text "update balance" ]
    ]
  ]

transferView : Html Msg
transferView = div [class "container mt-5"] [
    h3 [] [text "transfer"],
    haskellBorder [
      div [] [
        label [for "from-input", class "form-label"] [text "from"],
        input [type_ "text", class "form-control", id "from-input", placeholder "CH2707888954202552370TUR", onInput SetIbanFrom] []
      ],
      div [] [
        label [for "to-input", class "form-label"] [text "to"],
        input [type_ "text", class "form-control", id "to-input", placeholder "CH2707888954202552370TUR", onInput SetIbanTo] []
      ],
      div [class "mb-4"] [
        label [for "amount-transfer-input", class "form-label"] [text "amount"],
        input [type_ "text", class "form-control", id "amount-transfer-input", placeholder "1000", onInput (\str -> SetAmountForTransfer (Maybe.withDefault 0 (String.toInt str)))] []
      ],
      button [ type_ "button", class "btn haskell-btn", onClick Transfer] [ text "transfer" ]
    ]
  ]

haskellBorder : List (Html Msg) -> Html Msg
haskellBorder content = div [class "haskell-border p-4"] content

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
            h1 [class "col-3 p-3"] [text "STM Bank"],
            div [class "col-8 container"] [
              div [class "row justify-content-end"] [
                h6 [class "col-12 align-self-end"] [text "Authors:"],
                h6 [class "col-12 align-self-end"] [text "Pascal Andermatt"],
                h6 [class "col-12 align-self-end"] [text "Turan Ledermann"]
              ]
            ]
            
          ]
      ]
  ]