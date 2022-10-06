module Login exposing (..)
import Types exposing (AuthState)

type alias Model =
    { auth: AuthState
    , email: String
    , password: String
    }