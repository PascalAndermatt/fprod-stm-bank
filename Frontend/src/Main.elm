module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Json.Decode exposing (Decoder, at, list, string, succeed, decodeString, errorToString)
import Http exposing (expectJson, Expect)

import Json.Encode
import Json.Decode exposing (field, string)
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

type alias LabelTextInputPair = 
    {
      labelId : String,
      labelText : String,
      inputPlaceholder : String,
      onInputHandler : String -> Msg
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

baseUrl : String
baseUrl = "http://localhost:4000/accounts"

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
    newBalance : String,
    ibanForUpdate : String,
    balanceForUpdate : Int,
    updateMethod : String,
    ibanFrom : String,
    ibanTo : String,
    amountForTransfer : Int,
    error : String,
    updateBalanceError : String,
    createAccountError : String,
    transferError : String,
    closeAccountError :String,
    ibanForClosing : String
  }

init : ( Model, Cmd Msg )
init =
  ( { bankAccounts = [], 
      newOwner = "", 
      newBalance = "",
      ibanForUpdate = "",
      balanceForUpdate = 0,
      updateMethod = "",
      ibanFrom = "",
      ibanTo = "",
      amountForTransfer = 0,
      error = "",
      updateBalanceError = "",
      createAccountError = "",
      transferError = "",
      closeAccountError = "",
      ibanForClosing = ""}, Cmd.none )

type Msg = GetAllBankAccounts | 
           BankAccountsResult (Result String (List BankAccount)) | 
           SetOwner String | 
           SetBalance String |
           CreateBankAccount |
           CreateBankAccountResult (Result String BankAccount) |
           SetIbanForUpdate String |
           SetBalanceForUpdate Int |
           SetUpdateMethod String |
           UpdateBalance |
           UpdateBalanceResult (Result String BankAccount) |
           SetIbanFrom String |
           SetIbanTo String |
           SetAmountForTransfer Int |
           Transfer |
           TransferResult (Result String ()) |
           DeleteUpdateBalanceError |
           DeleteCreatAccountError |
           DeleteTransferError |
           SetIbanForClosing String |
           CloseAccount |
           DeleteCloseAccountError |
           CloseAccountResult (Result String BankAccount)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetIbanForUpdate iban -> ({model | ibanForUpdate = iban}, Cmd.none)
    SetBalanceForUpdate bal -> ({model | balanceForUpdate = bal}, Cmd.none)
    SetOwner owner -> ({model | newOwner = owner}, Cmd.none)
    SetBalance bal -> ({model | newBalance = bal}, Cmd.none)
    SetUpdateMethod str -> ({model | updateMethod = str}, Cmd.none)
    CreateBankAccount -> ({model | newBalance = "", newOwner = ""}, createBankAccount model)
    UpdateBalance -> (model, updateBalance model)
    GetAllBankAccounts ->
      ( model, getAllBankAccounts )
    BankAccountsResult result  -> case result of
       Ok listWithAccounts -> ({model | bankAccounts = listWithAccounts}, Cmd.none)
       Err _ -> ({model | bankAccounts = []}, Cmd.none )
    CreateBankAccountResult result -> case result of
       Ok _ -> (model, Cmd.none)
       Err message -> ({model | createAccountError = message}, Cmd.none)
    UpdateBalanceResult result -> case result of
       Ok _ -> (model, Cmd.none)
       Err message -> ({model | updateBalanceError = message}, Cmd.none)

    SetIbanTo to -> ({model | ibanTo = to}, Cmd.none)
    SetIbanFrom from -> ({model | ibanFrom = from}, Cmd.none)
    SetAmountForTransfer amount -> ({model | amountForTransfer = amount}, Cmd.none)
    Transfer -> (model, transfer model)
    TransferResult result -> case result of
       Ok _ -> (model, Cmd.none)
       Err message -> ({model | transferError = message}, Cmd.none)
    DeleteUpdateBalanceError -> ({model | updateBalanceError = ""}, Cmd.none)
    DeleteCreatAccountError -> ({model | createAccountError = ""}, Cmd.none)
    DeleteTransferError -> ({model | transferError = ""}, Cmd.none)
    SetIbanForClosing iban -> ({model | ibanForClosing = iban}, Cmd.none)
    CloseAccount -> (model, closeAccount model)
    DeleteCloseAccountError -> ({model | closeAccountError = ""}, Cmd.none)
    CloseAccountResult result -> case result of
       Ok _ -> (model, Cmd.none)
       Err message -> ({model | closeAccountError = message}, Cmd.none)

getAllBankAccounts : Cmd Msg
getAllBankAccounts = Http.get 
                        { url = baseUrl,
                          expect = expectJson BankAccountsResult decodeBankAccounts 
                        }

createBankAccount : Model -> Cmd Msg
createBankAccount model = Http.post 
                        { url = baseUrl,
                          body = Http.jsonBody (encodeBankAccountRequest {owner = model.newOwner, balance = Maybe.withDefault 0 (String.toInt model.newBalance)}),
                          expect = expectJson CreateBankAccountResult decodeBankAccount 
                        }

updateBalance : Model -> Cmd Msg
updateBalance model = Http.post
                      {
                        url = baseUrl ++ "/" ++ model.ibanForUpdate ++ "/" ++ String.toLower model.updateMethod ++ "?amount=" ++ (String.fromInt model.balanceForUpdate),
                        body = Http.emptyBody,
                        expect = expectJson UpdateBalanceResult decodeBankAccount
                      }

expectJson : (Result String a -> msg) -> Decoder a -> Expect msg
expectJson toMsg decoder =
  Http.expectStringResponse toMsg <|
    \response ->
      case response of
        Http.BadUrl_ url ->
          Err ("Fehler: Http.BadUrl: " ++ url)
        Http.Timeout_ ->
          Err "Fehler: Http.Timeout"
        Http.NetworkError_ ->
          Err "Fehler: Http.NetworkError"
        Http.BadStatus_ metadata body ->
          Err body
        Http.GoodStatus_ metadata body ->
          case decodeString decoder body of
            Ok value ->
              Ok value
            Err err ->
              Err (errorToString err)

transfer : Model -> Cmd Msg
transfer model = Http.post
                  {
                    url = baseUrl ++ "/transfer",
                    body = Http.jsonBody (encodeTransferRequest {from = model.ibanFrom, to = model.ibanTo, amount = model.amountForTransfer}),
                    expect = expectJson TransferResult (succeed ()) 
                  }

closeAccount : Model -> Cmd Msg
closeAccount model = Http.post 
                      {
                        url = (baseUrl ++ "/close/" ++ model.ibanForClosing),
                        body = Http.emptyBody,
                        expect = expectJson CloseAccountResult decodeBankAccount
                      }

view : Model -> Html Msg
view model = div [class "mb-5"] [
    createNavBar,
    div [ class "container" ]
      [ 
        h3 [class "mb-4"] [text "Bankaccounts"],
        table [class "table"] [
          thead [] [
            tr [] (List.map (\str -> th [scope "col"] [text str]) ["owner", "IBAN", "active", "balance"])
          ],
          tbody [] (List.map createTableRowFromBankAccount model.bankAccounts)
        ],
        createHaskellButton "get all accounts" GetAllBankAccounts
      ],
      newAccountView model,
      updateBalanceView model,
      transferView model,
      closeAccountView model
 ]
  
newAccountView : Model -> Html Msg
newAccountView model = div [class "container mt-5"] [
    viewBoxTitle "create new Bankaccount",
    haskellBorder [
      labelInputPairMarginBottom (LabelTextInputPair "ownerInput" "Owner" "Peter" SetOwner),
      labelInputPairMarginBottom (LabelTextInputPair "balanceInput" "Balance" "1000" SetBalance),
      createHaskellButton "create" CreateBankAccount,
      customErrorView model.createAccountError DeleteCreatAccountError
    ]
  ]

ibanPlaceholder : String
ibanPlaceholder = "CH2707888954202552370TUR"

updateBalanceView : Model -> Html Msg
updateBalanceView model = div [class "container mt-5"] [
    viewBoxTitle "update balance of Bankaccount",
    haskellBorder [
      labelInputPairMarginBottom (LabelTextInputPair "ibanForUpdate" "IBAN" ibanPlaceholder SetIbanForUpdate),
      labelInputPairMarginBottom (LabelTextInputPair "amountForUpdate" "Balance" "1000" (\str -> SetBalanceForUpdate (Maybe.withDefault 0 (String.toInt str)))),
      select [class "form-select mb-4", onInput SetUpdateMethod] [
        option [selected True ] [text "choose update action"],
        option [value "withdraw"] [text "withdraw"],
        option [value "deposit"] [text "deposit"]
      ],
      createHaskellButton "update balance" UpdateBalance,
      customErrorView model.updateBalanceError DeleteUpdateBalanceError
    ]
  ]

viewBoxTitle : String -> Html Msg
viewBoxTitle t = h3 [] [text t]

customErrorView : String -> Msg -> Html Msg
customErrorView errorMsg deleteHandler = if (errorMsg /= "") 
      then div [class "container alert alert-danger mt-4", attribute "role" "alert"] [
              div [class "row align-items-center"] [
                  div [class "col-11"] [text errorMsg], 
                  div [class "col-1"] [
                    button [ type_ "button", class "btn haskell-btn", onClick deleteHandler] [ text "X" ]
                  ]
              ]
          
            ] 
      else text ""

transferView : Model -> Html Msg
transferView model = div [class "container mt-5"] [
    viewBoxTitle "transfer",
    haskellBorder [
      labelInputPairMarginBottom (LabelTextInputPair "ibanFrom" "from" ibanPlaceholder SetIbanFrom),
      labelInputPairMarginBottom (LabelTextInputPair "ibanTo" "to" ibanPlaceholder SetIbanTo),
      labelInputPairMarginBottom (LabelTextInputPair "amountTransfer" "amount" "1000" (\str -> SetAmountForTransfer (Maybe.withDefault 0 (String.toInt str)))),
      createHaskellButton "transfer" Transfer,
      customErrorView model.transferError DeleteTransferError
    ]
  ]

closeAccountView : Model -> Html Msg
closeAccountView model = div [class "container mt-5"] [
    viewBoxTitle "close account",
    haskellBorder [
      labelInputPairMarginBottom (LabelTextInputPair "ibanForClosing" "iban" ibanPlaceholder SetIbanForClosing),
      createHaskellButton "close account" CloseAccount,
      customErrorView model.closeAccountError DeleteCloseAccountError
    ]
  ]

createHaskellButton : String -> Msg -> Html Msg
createHaskellButton bText handler = 
              button [ type_ "button", class "btn haskell-btn", onClick handler] [ text bText ]

labelInputPairMarginBottom : LabelTextInputPair -> Html Msg
labelInputPairMarginBottom data = div [class "mb-4"] (createLabelTextInputPair data)

createLabelTextInputPair : LabelTextInputPair -> List (Html Msg)
createLabelTextInputPair data = [
          label [for data.labelId, class "form-label"] [text data.labelText],
          input [type_ "text", class "form-control", id data.labelId, placeholder data.inputPlaceholder, onInput data.onInputHandler] [] 
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