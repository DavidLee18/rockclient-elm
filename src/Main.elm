port module Main exposing (..)
import Dict exposing (Dict)
import Types exposing (Msg, Uid, AuthState(..), Msg(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Browser.Navigation
import Url
import Browser
import Types exposing (maybeRouter, Route(..))
import RockService exposing (getLeaders, getMyInfo)
import Types exposing (messages)
import Browser.Navigation exposing (pushUrl)
import Html exposing (div)

port logIn : Encode.Value -> Cmd msg
port logOut : () -> Cmd msg
port onAuthStateChanged : (Uid -> msg) -> Sub msg
port getMessages : (Decode.Value -> msg) -> Sub msg
port getNoties : (Decode.Value -> msg) -> Sub msg
port sendEmail : String -> Cmd msg
port sendMessage : Encode.Value -> Cmd msg

main : Program () Types.Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }

init : () -> Url.Url -> Browser.Navigation.Key -> (Types.Model, Cmd Msg)
init _ url key =
    let
        route = maybeRouter url
        msg = case route of
                Initial -> pushUrl key <| Url.toString { url | path = "/login" }
                Leaders -> getLeaders
                _ -> Cmd.none
    in  ({ auth = LoggedOut
         , info = Nothing
         , key = key
         , route = route
         }, msg)

subscriptions : Types.Model -> Sub Msg
subscriptions model =
    let
        subs model_ = case model_.route of
                        RetreatMessages -> [ getMessages <| GotMessages << Decode.decodeValue messages, getNoties <| GotNoties << Decode.decodeValue messages ]
                        _ -> []
    in
    Sub.batch <| (onAuthStateChanged <| Auth << LoggedIn) :: subs model


update : Msg -> Types.Model -> (Types.Model, Cmd Msg)
update m model =
    Debug.todo "update"


view : Types.Model -> Browser.Document Msg
view model =
    case model.route of
       Login -> let 
                    bodyLogin = div [] []
                in { title = "rock-client Elm", body = [bodyLogin] }
       _ -> { title = "rock-client Elm", body = [] }