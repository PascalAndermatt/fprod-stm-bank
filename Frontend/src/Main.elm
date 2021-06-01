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
      onInputHandler : String -> Msg,
      valueModel : String
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

-- konstante
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
    balanceForUpdate : String,
    balanceUpdateAction : String,
    ibanFrom : String,
    ibanTo : String,
    amountForTransfer : String,
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
      newOwner            = "", 
      newBalance          = "",
      ibanForUpdate       = "",
      balanceForUpdate    = "",
      balanceUpdateAction = "",
      ibanFrom            = "",
      ibanTo              = "",
      amountForTransfer   = "",
      error               = "",
      updateBalanceError  = "",
      createAccountError  = "",
      transferError       = "",
      closeAccountError   = "",
      ibanForClosing      = ""
    }, getAllBankAccounts )

type Msg = GetAllBankAccounts | 
           BankAccountsResult (Result String (List BankAccount)) | 
           SetOwner String | 
           SetBalanceForCreate String |
           CreateBankAccount |
           CreateBankAccountResult (Result String BankAccount) |
           SetIbanForUpdate String |
           SetBalanceForUpdate String |
           SetBalanceUpdateAction String |
           UpdateBalance |
           UpdateBalanceResult (Result String BankAccount) |
           SetIbanFrom String |
           SetIbanTo String |
           SetAmountForTransfer String |
           Transfer |
           TransferResult (Result String ()) |
           DeleteUpdateBalanceError |
           DeleteCreatAccountError |
           DeleteTransferError |
           SetIbanForClosing String |
           CloseAccount |
           DeleteCloseAccountError |
           CloseAccountResult (Result String BankAccount)


-- ordering & comments
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    -- get all accounts
    GetAllBankAccounts              -> ( model, getAllBankAccounts )
    BankAccountsResult result       -> case result of
       Ok accounts -> ({model | bankAccounts = accounts}, Cmd.none)
       Err _       -> ({model | bankAccounts = []}, Cmd.none )

    -- create account
    SetOwner o                      -> ({model | newOwner = o}, Cmd.none)
    SetBalanceForCreate b           -> validateBalanceForCreate b model
    CreateBankAccount               -> createBankAccount model
    CreateBankAccountResult result  -> case result of
       Ok _        -> ({model | newOwner = "", newBalance = ""}, getAllBankAccounts)
       Err message -> ({model | createAccountError = message, newOwner = "", newBalance = ""}, Cmd.none)
    DeleteCreatAccountError         -> ({model | createAccountError = ""}, Cmd.none)

    -- update balance of account
    SetIbanForUpdate i              -> ({model | ibanForUpdate = i}, Cmd.none)
    SetBalanceForUpdate b           -> validateBalanceForUpdate b model
    SetBalanceUpdateAction a        -> ({model | balanceUpdateAction = a}, Cmd.none)
    UpdateBalance                   -> updateBalance model
    UpdateBalanceResult result      -> case result of
       Ok _        -> ({model | balanceForUpdate = "", ibanForUpdate = ""}, getAllBankAccounts)
       Err message -> ({model | updateBalanceError = message, balanceForUpdate = "", ibanForUpdate = ""}, Cmd.none)
    DeleteUpdateBalanceError        -> ({model | updateBalanceError = ""}, Cmd.none)

    -- transfer
    SetIbanFrom f                   -> ({model | ibanFrom = f}, Cmd.none)
    SetIbanTo t                     -> ({model | ibanTo = t}, Cmd.none)
    SetAmountForTransfer a          -> validateAmountForTransfer a model
    Transfer                        -> transfer model
    TransferResult result           -> case result of
       Ok _        -> ({model | ibanFrom = "", ibanTo = "", amountForTransfer = ""}, getAllBankAccounts)
       Err message -> ({model | transferError = message, ibanFrom = "", ibanTo = "", amountForTransfer = ""}, Cmd.none)
    DeleteTransferError             -> ({model | transferError = ""}, Cmd.none)
    
    -- close account
    SetIbanForClosing i             -> ({model | ibanForClosing = i}, Cmd.none)
    CloseAccount                    -> closeAccount model
    CloseAccountResult result       -> case result of
       Ok _        -> ({model | ibanForClosing = ""}, getAllBankAccounts)
       Err message -> ({model | closeAccountError = message, ibanForClosing = ""}, Cmd.none)
    DeleteCloseAccountError         -> ({model | closeAccountError = ""}, Cmd.none)

-- input field validation
validateBalanceForCreate : String -> Model -> (Model, Cmd Msg)
validateBalanceForCreate bal m = validateBalanceLength m {m | newBalance = String.filter Char.isDigit bal} bal

validateBalanceForUpdate : String -> Model -> (Model, Cmd Msg)
validateBalanceForUpdate bal m = validateBalanceLength m {m | balanceForUpdate = String.filter Char.isDigit bal} bal

validateAmountForTransfer : String -> Model -> (Model, Cmd Msg)
validateAmountForTransfer bal m = validateBalanceLength m {m | amountForTransfer = String.filter Char.isDigit bal} bal

validateBalanceLength : Model -> Model -> String -> (Model, Cmd Msg)
validateBalanceLength oldM newM bal = if String.length bal > 7 
                                        then (oldM, Cmd.none)
                                        else (newM, Cmd.none)
getAllBankAccounts : Cmd Msg
getAllBankAccounts = Http.get 
                        { url = baseUrl,
                          expect = expectJson BankAccountsResult decodeBankAccounts 
                        }

createBankAccount : Model -> (Model, Cmd Msg)
createBankAccount model = case isAnyInputFieldEmpty [model.newOwner, model.newBalance] of
                                Ok _        -> ( model, Http.post 
                                                  { url = baseUrl,
                                                    body = Http.jsonBody (encodeBankAccountRequest {
                                                                                    owner   = model.newOwner, 
                                                                                    balance = Maybe.withDefault 0 (String.toInt model.newBalance)
                                                                                  }),
                                                    expect = expectJson CreateBankAccountResult decodeBankAccount 
                                                  })
                                Err message -> ({model | createAccountError = message}, Cmd.none)

isAnyInputFieldEmpty : List String -> Result String ()
isAnyInputFieldEmpty fields = if List.any String.isEmpty fields
                                    then Err "Fehler: nicht alle Felder ausgefüllt"
                                    else Ok ()

updateBalance : Model -> (Model, Cmd Msg)
updateBalance model = case validateBalanceUpdateRequest model of
                        Ok _        ->  (model, Http.post
                                            { url = baseUrl ++ "/" ++ model.ibanForUpdate ++ "/" ++ String.toLower model.balanceUpdateAction ++ "?amount=" ++ model.balanceForUpdate,
                                              body = Http.emptyBody,
                                              expect = expectJson UpdateBalanceResult decodeBankAccount
                                            })
                        Err message -> ({model | updateBalanceError = message}, Cmd.none)

transfer : Model -> (Model, Cmd Msg)
transfer model = case isAnyInputFieldEmpty [model.ibanFrom, model.ibanTo, model.amountForTransfer] of
                          Ok _ -> (model, Http.post
                                    {
                                      url = baseUrl ++ "/transfer",
                                      body = Http.jsonBody (encodeTransferRequest {from = model.ibanFrom, to = model.ibanTo, amount = (Maybe.withDefault 0 (String.toInt model.amountForTransfer))}),
                                      expect = expectJson TransferResult (succeed ()) 
                                    })
                          Err message -> ({model | transferError = message}, Cmd.none)

validateBalanceUpdateRequest : Model -> Result String ()
validateBalanceUpdateRequest model = case isAnyInputFieldEmpty [model.ibanForUpdate, model.balanceForUpdate] of
                                              Ok _ -> if List.member model.balanceUpdateAction [str_WITHDRAW, str_DEPOSIT]
                                                            then Ok ()
                                                            else Err "Fehler: keine update action ausgewählt"
                                              Err message -> Err message

closeAccount : Model -> (Model, Cmd Msg)
closeAccount model = case isAnyInputFieldEmpty [model.ibanForClosing] of
                            Ok _ -> (model, Http.post 
                                        {
                                          url = (baseUrl ++ "/close/" ++ model.ibanForClosing),
                                          body = Http.emptyBody,
                                          expect = expectJson CloseAccountResult decodeBankAccount
                                        })
                            Err message -> ({model | closeAccountError = message}, Cmd.none)
                

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
          case decodeString decoder (convertBody body) of
            Ok value ->
              Ok value
            Err err ->
              Err (errorToString err)

convertBody : String -> String
convertBody body = if body == "" then "{}" else body



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
      labelInputPairMarginBottom (LabelTextInputPair "ownerInput" "Owner" "Peter" SetOwner model.newOwner),
      labelInputPairMarginBottom (LabelTextInputPair "balanceInput" "Balance" "1000" SetBalanceForCreate model.newBalance),
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
      labelInputPairMarginBottom (LabelTextInputPair "ibanForUpdate" "IBAN" ibanPlaceholder SetIbanForUpdate model.ibanForUpdate),
      labelInputPairMarginBottom (LabelTextInputPair "amountForUpdate" "Balance" "1000" SetBalanceForUpdate model.balanceForUpdate),
      select [class "form-select mb-4", onInput SetBalanceUpdateAction] [
        option [selected True ] [text "choose update action"],
        option [value str_WITHDRAW] [text str_WITHDRAW],
        option [value str_DEPOSIT] [text str_DEPOSIT]
      ],
      createHaskellButton "update balance" UpdateBalance,
      customErrorView model.updateBalanceError DeleteUpdateBalanceError
    ]
  ]

str_WITHDRAW : String
str_WITHDRAW = "withdraw"

str_DEPOSIT : String
str_DEPOSIT = "deposit"

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
      labelInputPairMarginBottom (LabelTextInputPair "ibanFrom" "from" ibanPlaceholder SetIbanFrom model.ibanFrom),
      labelInputPairMarginBottom (LabelTextInputPair "ibanTo" "to" ibanPlaceholder SetIbanTo model.ibanTo),
      labelInputPairMarginBottom (LabelTextInputPair "amountTransfer" "amount" "1000" SetAmountForTransfer model.amountForTransfer),
      createHaskellButton "transfer" Transfer,
      customErrorView model.transferError DeleteTransferError
    ]
  ]

closeAccountView : Model -> Html Msg
closeAccountView model = div [class "container mt-5"] [
    viewBoxTitle "close account",
    haskellBorder [
      labelInputPairMarginBottom (LabelTextInputPair "ibanForClosing" "iban" ibanPlaceholder SetIbanForClosing model.ibanForClosing),
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
          input [value data.valueModel, type_ "text", class "form-control", id data.labelId, placeholder data.inputPlaceholder, onInput data.onInputHandler] [] 
        ]

haskellBorder : List (Html Msg) -> Html Msg
haskellBorder content = div [class "haskell-border p-4"] content

getAccountStatus : BankAccount -> String
getAccountStatus acc = if acc.active then "active" else "inactive"

createTableRowFromBankAccount : BankAccount -> Html Msg
createTableRowFromBankAccount acc = tr [] [
    td [] [text acc.owner],
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