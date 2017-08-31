module CommonAuthenticationComponent exposing (..)

import Html exposing (Html, text, p, div, button)
import Html.Events exposing (onClick)
import Jwt exposing (..)
import Decoders exposing (..)
import Html.Attributes exposing (class, href, target, style)
import Navigation exposing (load, modifyUrl)
import QueryString exposing (QueryString, empty, add, render, parse, one, string, remove)
import Array exposing (get, fromList)
import Task exposing (perform, attempt)
import Http exposing (post, jsonBody)
import RemoteData exposing (sendRequest, WebData, RemoteData(..))
import Storage.Session exposing (set)
import Storage.Error exposing (Error)
import UTF8 exposing (toMultiByte)

{-| Program is initialized with flags from the javascript.
-}
main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = Login
    | Init
    | Logout
    | ProcessStorageVal (Result Error (Maybe String))
    | PerformStorageSet (Result Error ())
    | PerformSessionDeleteForLogout (Result Error ())
    | PerformSessionDelete (Result Error ())
    | ReceiveToken (WebData String)

------ MODEL ------


{-| Javascript flags structure.
-}
type alias Flags =
    { time : Float
    , appUrl : String
    , appId : String
    }

{-| time is used for checking the expiration of token.
    appUrl is required for redirects
    appId is required for redirects and for validating the token
    token contains the JWT token as String
    reservationId is used for obtaining the token from JWT
-}
type alias Model =
    { time : Float
    , appUrl : String
    , appId : String
    , token : WebData String
    , reservationId : Maybe String
    }


{-| Init function calls update with Init and initialized model.
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    update (Init) (initialModel flags)


------ UPDATE ------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            -- remove reservationId, try obtaining the token and update the URL
            let
                updated =
                    removeReservationAndSetTokenToLoading model
             in
             updated !
                [ obtainJwtTokenOrReadFromStorage updated.reservationId,
                  modifyUrl updated.appUrl
                ]

        ReceiveToken response ->
            -- save response, remove reservationId, save to storage
            let
                updated =
                    { model |
                    token = response
                    , reservationId = Nothing
                    }
             in
            ( updated, saveToStorage updated )

        Login ->
            -- Check if token is valid and if not, get new token from JWT
            getNewTokenIfTokenInvalid model

        ProcessStorageVal val ->
            -- updates model with the data from storage
            updateModelWithStorageVal val model

        PerformStorageSet val ->
            -- does nothing, the data in storage was set, we ignore errors
            ( model, Cmd.none )

        Logout ->
            -- deletes token from storage
            ( model, deleteFromStorage PerformSessionDeleteForLogout)

        PerformSessionDeleteForLogout val ->
            -- after session deletion it redirects to JWT server to invalidate its Session as well
            ( model, logout model )

        PerformSessionDelete val ->
            ( model, Cmd.none )



------ FUNCTIONS -------


{-| Initializes model with the data from JS flags. The token is obtained from the url.
-}
initialModel : Flags -> Model
initialModel flags =
    Model
        flags.time
        flags.appUrl
        flags.appId
        NotAsked
        (getReservationIdFromUrl flags.appUrl)

{-| Removes reservationId from the url in the model and sets token to Loading.
-}
removeReservationAndSetTokenToLoading : Model -> Model
removeReservationAndSetTokenToLoading model =
    let
        updatedUrl =
            removeReservationFromUrl model.appUrl

        updatedToken =
            Loading
    in
        { model
            | appUrl = updatedUrl
            , token = updatedToken
        }

{-| Performs either token request or tries to read the token from storage.
-}
obtainJwtTokenOrReadFromStorage : Maybe String -> Cmd Msg
obtainJwtTokenOrReadFromStorage reservationId =
    case reservationId of
        Nothing ->
            readTokenFromStorage

        Just reservation ->
            requestToken reservation



{-| Requests the token from JWT server using the reservationId.
-}
requestToken : String -> Cmd Msg
requestToken id =
    Http.post "http://jwt.domain.local/api/token" (jsonBody <| reservationIdEncoder id) tokenJsonDecoder
        |> RemoteData.sendRequest
        |> Cmd.map ReceiveToken


{-| Depending on the validity of the cookie it either saves it to session storage or tries reading from a cookie.
-}
saveToStorage : Model -> Cmd Msg
saveToStorage model =
    if isTokenOk model then
        storageSave model
    else
        Cmd.none


{-| Attempts to save the token to session storage
-}
storageSave : Model -> Cmd Msg
storageSave model =
    case RemoteData.toMaybe model.token of
        Nothing ->
            Cmd.none

        Just token ->
            let
                task =
                    Storage.Session.set "jwttoken" token
            in
                Task.attempt PerformStorageSet task


{-| Attemptes to delete the token from storage.
-}
deleteFromStorage : (Result Error () -> Msg) -> Cmd Msg
deleteFromStorage msg =
    let
        task =
            Storage.Session.remove "jwttoken"
    in
        Task.attempt msg task


{-| Gets reservationID as string from the url.
-}
getReservationIdFromUrl : String -> Maybe String
getReservationIdFromUrl url =
    parseReservationIdFromUrl url


{-| Attempts to read the JWT token from cookie.
-}
readTokenFromStorage : Cmd Msg
readTokenFromStorage =
    let
        task =
            Storage.Session.get "jwttoken"
    in
        Task.attempt ProcessStorageVal task


{-| If the token is invalid, redirects to JWT.
-}
getNewTokenIfTokenInvalid : Model -> ( Model, Cmd Msg )
getNewTokenIfTokenInvalid model =
    let
        isOk =
            isTokenOk model
    in
        if isOk then
            ( model, Cmd.none )
        else
            ( model, redirectToJwt model )


{-| Determines if the ID in JWT is same as our AppId.
-}
isClientValid : WebData String -> String -> Bool
isClientValid token id =
    let
        t =
            getDecodedToken token
    in
        case t of
            Err msg ->
                False

            Ok val ->
                if toString val.aud == toString id then
                    True
                else
                    False


{-| Gets expiration time from the JWT token.
-}
getTokenExpirationTime : WebData String -> Int
getTokenExpirationTime tokenString =
    let
        token =
            getDecodedToken tokenString
    in
        case token of
            Err msg ->
                0

            Ok val ->
                val.exp


{-| Gets the display name from the JWT token.
-}
getTokenDisplayName : WebData String -> String
getTokenDisplayName tokenString =
    case tokenString of
        Loading ->
            "Logging in..."

        Success val ->
            let
                token =
                    getDecodedToken tokenString
            in
                case token of
                    Err msg ->
                        ""

                    Ok val ->
                        "Logged in as "++ ( toMultiByte val.displayName )

        _ ->
            ""

getTokenLogin : WebData String -> Maybe String
getTokenLogin tokenString =
    case tokenString of
            Success val ->
                let
                    token =
                        getDecodedToken tokenString
                in
                    case token of
                        Err msg ->
                            Nothing

                        Ok val ->
                            Just ( toMultiByte val.login )

            _ ->
                Nothing

{-| Gets decoded token with tokenDecoder.
-}
getDecodedToken : WebData String -> Result JwtError JwtToken
getDecodedToken token =
    case RemoteData.toMaybe token of
        Nothing ->
            Err Unauthorized

        Just token ->
            decodeToken tokenDecoder token


{-| Redirects to JWT login.
-}
redirectToJwt : Model -> Cmd Msg
redirectToJwt model =
    load ("http://jwt.domain.local/login" ++ createQueryString model)


{-| Redirects to JWT logout.
-}
logout : Model -> Cmd Msg
logout model =
    load ("http://jwt.domain.local/logout" ++ createQueryString model)


{-| Creates Query string from the model (appId and current appUrl without token)
-}
createQueryString : Model -> String
createQueryString model =
    let
        url =
            removeReservationFromUrl model.appUrl

        queryString =
            empty
    in
        queryString
            |> add "backUrl" url
            |> add "clientId" model.appId
            |> render


{-| Parses token from the URL.
-}
parseReservationIdFromUrl : String -> Maybe String
parseReservationIdFromUrl url =
    let
        queryString =
            getQueryStringFromUrl url

        token =
            one string "reservationId" queryString
    in
        token


{-| Gets URL part from the url (before ?)
-}
getAddressFromUrl : String -> String
getAddressFromUrl url =
    let
        parametersList =
            String.split "?" url

        parameters =
            Array.get 0 (Array.fromList parametersList)
    in
        case parameters of
            Just val ->
                val

            Nothing ->
                ""


{-| Gets query part from the url (after ?)
-}
getQueryStringFromUrl : String -> QueryString
getQueryStringFromUrl url =
    let
        parametersList =
            String.split "?" url

        parameters =
            Array.get 1 (Array.fromList parametersList)
    in
        case parameters of
            Just val ->
                parse val

            Nothing ->
                empty


{-| Removes reservationID token parameter from the URL.
-}
removeReservationFromUrl : String -> String
removeReservationFromUrl url =
    let
        querySting =
            getQueryStringFromUrl url

        updated =
            (remove "reservationId" querySting) |> render

        finalUrlParams =
            if updated == "?" then
                ""
            else
                updated
    in
        getAddressFromUrl url ++ finalUrlParams


{-| Updates model from the data read from storage.
-}
updateModelWithStorageVal : Result Error (Maybe String) -> Model -> ( Model, Cmd Msg)
updateModelWithStorageVal storageVal model =
    let
        removeCmd = deleteFromStorage PerformSessionDelete
    in
    case storageVal of
        Err msg ->
            ( notAskedModel model, Cmd.none )

        Ok str ->
            case str of
                Nothing ->
                    ( notAskedModel model, removeCmd )

                Just str ->

                    {- rewrite model value only if it was not set from the POST request -}
                    case model.token of
                        Success val ->
                            ( model, Cmd.none )

                        _ ->
                            let
                                modelWithTokenFromStorage =
                                    { model | token = Success str }
                            in
                                if isTokenOk modelWithTokenFromStorage then
                                    ( modelWithTokenFromStorage, Cmd.none )
                                else
                                    ( notAskedModel model, removeCmd )


{-| Checks if the JWT token is OK.
-}
isTokenOk : Model -> Bool
isTokenOk model =
    let
        token =
            getDecodedToken model.token

        expired =
            case RemoteData.toMaybe model.token of
                Nothing ->
                    Ok True

                Just token ->
                    isExpired model.time token


        isValid =
            isClientValid model.token model.appId

        expiredValue =
            case expired of
                Err msg ->
                    True

                Ok val ->
                    val
    in
        isValid && not expiredValue

notAskedModel : Model -> Model
notAskedModel model =
    { model
    | token = NotAsked
    }

------ VIEW ------


{-| Two views, depending on if the user is logged in or not
-}
view : Model -> Html Msg
view model =
    let
        isOk =
            isTokenOk model

        name =
            getTokenDisplayName model.token

        elements =
            if isOk then
                [ p [ class "auth-component-logged-user" ] [ text <| name ]
                , button [ class "btn", class "btn-warning", class "auth-component-button", class "auth-component-button-logout", onClick Logout ] [ text "Logout" ]
                ]
            else
                [ button [ class "btn", class "btn-warning", class "auth-component-button", class "auth-component-button-login", onClick Login ]
                    [ text "Login" ]
                ]
    in
        div []
            elements



--debugModel model


{-| Method for viewing several information about the model.
-}
debugModel : Model -> Html Msg
debugModel model =
    let
        token =
            getDecodedToken model.token

        expired =
            case RemoteData.toMaybe model.token of
                Nothing ->
                    Ok True

                Just token ->
                    isExpired model.time token

        isValid =
            isClientValid model.token model.appId

        expiredValue =
            case expired of
                Err msg ->
                    "Error occured: " ++ toString msg

                Ok val ->
                    toString val

        tokenValue =
            case token of
                Err msg ->
                    "Error occured: " ++ toString msg

                Ok val ->
                    toString val
    in
        div []
            [ p [] [ text "Current token is: ", text <| toString model.token ]
            , p [] [ text "Decoded token is: ", text <| toString tokenValue ]
            , p [] [ text "Is token expired? ", text <| toString expiredValue ]
            , p [] [ text "Is token for right client? ", text <| toString isValid ]
            , p [] [ text "Is token OK? ", text <| toString (isTokenOk model) ]
            , button [ onClick Login ] [ text "Obtain new token from JWT" ]
            , p [] [ text "JWT url is : ", text <| "http://jwt.domain.local/login" ++ createQueryString model ]
            , p [] [ text "JWT logout is : ", text <| "http://jwt.domain.local/logout" ++ createQueryString model ]
            ]
