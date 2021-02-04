module Types exposing (..)
import Dict exposing (Dict)
import Http exposing (header, Expect, emptyBody, expectWhatever, Header)
import Json.Decode as Decode exposing (Decoder, succeed, fail, maybe)
import Json.Encode as Encode
import Maybe
import Url exposing (Protocol(..))
import Url exposing (Url)
import Browser exposing (UrlRequest)
import Browser.Navigation
import Url.Parser as Parser exposing (Parser)
import Url.Parser exposing (s, (<?>), (</>))
import Url.Parser.Query as Query
import Url.Parser exposing (parse)
import Url.Parser exposing (top)

type alias Uid = String

type alias Model =
    { auth : AuthState
    , info : Maybe Info
    , key : Browser.Navigation.Key
    , route : Route
    }

type Msg
    = Auth AuthState
    | CampusesEdited (HttpResult ())
    | GradeSet (HttpResult ())
    | GotMessages (Result Decode.Error (List Message))
    | GotNoties (Result Decode.Error (List Message))
    | GotLeaders (HttpResult (List Leader))
    | LeaderUnset (HttpResult ())
    | Members (HttpResult (List Member))
    | MongsanpoMembers (HttpResult (List MongsanpoResume))
    | MongsanpoRegistered (HttpResult ())
    | MyInfo (HttpResult Info)
    | RetreatEdited (HttpResult ())
    | RetreatRegistered (HttpResult ())
    | SemiSignedUp (HttpResult ())
    | SignedUp (HttpResult ())
    | UrlChanged Url
    | UrlRequested UrlRequest
    | YouthSignedUp (HttpResult ())

type Route
    = NotFound
    | SignUp
    | SemiSignUp
    | Leaders
    | YouthSignUp
    | Retreat
    | RetreatRegister
    | RetreatMessages

type alias HttpResult value = Result Http.Error value

type AuthState = LoggedIn Uid | LoggedOut

type AuthError
    = InvalidEmail
    | UserNotFound
    | UserDisabled
    | WrongPassword

type Campus
    = GangByeon --TODO: add remaining campuses

type Grade
    = MEMBER
    | LEADER
    | MISSION
    | ASSISTANT
    | ADMIN

type Department = CBA | BWM

type alias Info =
    { memId : Int
    , name : String
    , campus : String
    , dt_birth : String
    , mobile : String
    , uid : Uid
    , grade : Grade
    , department : Department
    , retreatInfo : Maybe RetreatInfo
    , gbsInfo : Maybe GbsInfo
    }

type alias GbsInfo =
    { gbs : String
    , position : String
    }

type alias RetreatInfo =
    { retreatId : Int
    , gbs : String
    , position : String
    , attendAll : Bool
    , dayTimeList : List String
    }

type alias Leader =
    { id : Int
    , name : String
    , mobile : String
    , memberId : Int
    , campuses : List String
    }

type alias Member =
    { id : Int
    , name : String
    , mobile : String
    , dt_birth : String
    , campus : String
    , active : Bool
    }

type alias Message =
    { author : String
    , isStaff : String
    , message : String
    , time : String
    , uid : Uid
    }

type alias MongsanpoResume =
    { name : String
    , mobile : String
    , belongTo : String
    , carNumber : String
    }

type alias RetreatResume =
    { memberUid : Uid
    , originalGbs : String
    , retreatGbs : String
    , position : String
    , lectureHope : Maybe String
    , dayTimeList : Maybe (List String)
    }

type alias UserResume =
    { email : String
    , password : String
    , name : String
    , mobile : String
    , birthDate : String
    , sex : Sex
    , campus : String
    , address : String
    , school : String
    , major : String
    , grade : String
    , guide : Maybe String
    }

type Sex
    = Male
    | Female

type alias SemiUserResume =
    { name : String
    , mobile : String
    , campus : String
    , adminUid : Uid
    }

type SignUpVariant
    = Semi
    | Youth

type Pasture
    = First
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth
    | Seventh
    | Eighth

type alias YouthUserResume =
    { email : String
    , password : String
    , name : String
    , mobile : String
    , birthDate : String
    , address : String
    , sex : Sex
    , team : Pasture
    }

authErrors : Dict String AuthError
authErrors = Dict.fromList [("auth/invalid-email", InvalidEmail), ("auth/user-disabled", UserDisabled), ("auth/user-not-found", UserNotFound), ("auth/wrong-password", WrongPassword)]

toAuthError : String -> Maybe AuthError
toAuthError errorString = Dict.get errorString authErrors

root : Url
root = { protocol = Https
       , host = "cba.sungrak.or.kr"
       , port_ = Just 9000
       , path = ""
       , query = Nothing
       , fragment = Nothing
       }

defaultHeaders : List Http.Header
defaultHeaders = [header "Authorization" "Basic YWRtaW46ZGh3bHJybGVoISEh"]

toDepartment : String -> Maybe Department
toDepartment s = case s of
                    "CBA" -> Just CBA
                    "BWM" -> Just BWM
                    _ -> Nothing

maybeDepartment : Maybe Department -> Decoder Department
maybeDepartment department = case department of
                                Just d -> Decode.succeed d
                                Nothing -> Decode.fail "failed to decode department"

fromGrade : Grade -> String
fromGrade grade = case grade of
                    MEMBER -> "MEMBER"
                    LEADER -> "LEADER"
                    MISSION -> "MISSION"
                    ASSISTANT -> "GANSA"
                    ADMIN -> "ADMIN"

fromPasture : Pasture -> Int
fromPasture pasture = case pasture of
                        First -> 1
                        Second -> 2
                        Third -> 3
                        Fourth -> 4
                        Fifth -> 5
                        Sixth -> 6
                        Seventh -> 7
                        Eighth -> 8

fromSex : Sex -> String
fromSex sex = case sex of
                Male -> "남자"
                Female -> "여자"

gbsInfo : Decoder GbsInfo
gbsInfo = Decode.map2 GbsInfo
            (Decode.field "gbs" Decode.string)
            (Decode.field "position" Decode.string)

toGrade : String -> Maybe Grade
toGrade string =  case string of
                            "MEMBER" -> Just MEMBER
                            "LEADER" -> Just LEADER
                            "MISSION" -> Just MISSION
                            "GANSA" -> Just ASSISTANT
                            "ADMIN" -> Just ADMIN
                            _ -> Nothing

maybeGrade : Maybe Grade -> Decoder Grade
maybeGrade grade = case grade of
                    Just g -> succeed g
                    Nothing -> fail "grade decode failed"

info : Decoder Info
info = Decode.map3 (<|) (Decode.map8 Info
        (Decode.field "memId" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "campus" Decode.string)
        (Decode.field "dt_birth" Decode.string)
        (Decode.field "mobile" Decode.string)
        (Decode.field "uid" Decode.string)
        (Decode.field "grade" <| Decode.andThen (maybeGrade << toGrade) Decode.string)
        (Decode.field "department" <| Decode.andThen (maybeDepartment << toDepartment) Decode.string))
        (maybe <| Decode.field "retreatInfo" retreatInfo)
        (maybe <| Decode.field "gbsInfo" gbsInfo)

leader : Decoder Leader
leader = Decode.map5 Leader
            (Decode.field "id" Decode.int)
            (Decode.field "name" Decode.string)
            (Decode.field "mobile" Decode.string)
            (Decode.field "memberId" Decode.int)
            (Decode.field "campuses" <| Decode.list Decode.string)

leaders : Decoder (List Leader)
leaders = Decode.field "data" <| Decode.list leader

logInForm : String -> String -> Encode.Value
logInForm email password = Encode.object [ ("email", Encode.string email)
                                         , ("password", Encode.string password)
                                         ]

member : Decoder Member
member = Decode.map6 Member
            (Decode.field "id" Decode.int)
            (Decode.field "name" Decode.string)
            (Decode.field "mobile" Decode.string)
            (Decode.field "dt_birth" Decode.string)
            (Decode.field "campus" Decode.string)
            (Decode.field "active" Decode.bool)

fromMessage : Message -> Encode.Value
fromMessage m = Encode.object [ ("author", Encode.string m.author)
                              , ("isStaff", Encode.string m.isStaff)
                              , ("message", Encode.string m.message)
                              , ("time", Encode.string m.time)
                              , ("uid", Encode.string m.uid)
                              ]

toMessage : Decoder Message
toMessage = Decode.map5 Message
                (Decode.field "author" Decode.string)
                (Decode.field "isStaff" Decode.string)
                (Decode.field "message" Decode.string)
                (Decode.field "time" Decode.string)
                (Decode.field "uid" Decode.string)

messages : Decoder (List Message)
messages = Decode.list toMessage

mongsanpoMembers : Decoder (List MongsanpoResume)
mongsanpoMembers = Decode.field "data" <| Decode.list
                    <| Decode.map4 MongsanpoResume
                        (Decode.field "name" Decode.string)
                        (Decode.field "mobile" Decode.string)
                        (Decode.field "belongTo" Decode.string)
                        (Decode.field "carNumber" Decode.string)

mongsanpoResume : MongsanpoResume -> Encode.Value
mongsanpoResume resume = Encode.object [ ("name", Encode.string resume.name)
                                       , ("mobile", Encode.string resume.mobile)
                                       , ("belongTo", Encode.string resume.belongTo)
                                       , ("carNumber", Encode.string resume.carNumber)
                                       ]

retreatInfo : Decoder RetreatInfo
retreatInfo = Decode.map5 RetreatInfo
                (Decode.field "retreatId" Decode.int)
                (Decode.field "gbs" Decode.string)
                (Decode.field "position" Decode.string)
                (Decode.field "attendAll" Decode.bool)
                (Decode.field "daytimeList" <| Decode.list Decode.string)

retreatRegistered : Info -> Bool
retreatRegistered info_ = case info_.retreatInfo of
                            Just _ -> True
                            Nothing -> False

retreatResume : RetreatResume -> Encode.Value
retreatResume resume = let
                            attendAll = List.isEmpty <| Maybe.withDefault [] resume.dayTimeList
                            resumeJson = [ ("memberUid", Encode.string resume.memberUid)
                                         , ("originalGbs", Encode.string resume.originalGbs)
                                         , ("retreatGbs", Encode.string resume.retreatGbs)
                                         , ("position", Encode.string resume.position)
                                         , ("attendType", Encode.string "GBS")
                                         , ("attendAll", Encode.bool attendAll)
                                        ] ++ (Maybe.withDefault [] <| Maybe.map (\list -> [("dayTimeList", Encode.list Encode.string list)]) resume.dayTimeList)
                                        ++ (Maybe.withDefault [] <| Maybe.map (\lecture -> [("lectureHope", Encode.string lecture)]) resume.lectureHope)
                        in Encode.object resumeJson

retreatEditResume : RetreatResume -> Encode.Value
retreatEditResume resume = Encode.object <| [ ("memberUid", Encode.string resume.memberUid)
                                            , ("retreatGbs", Encode.string resume.retreatGbs)
                                            , ("position", Encode.string resume.position)
                                            , ("attendAll", Encode.bool << List.isEmpty <| Maybe.withDefault [] resume.dayTimeList)
                                            ] ++ (Maybe.withDefault [] <| Maybe.map (\list -> [("dayTimeList", Encode.list Encode.string list)]) resume.dayTimeList)

router : Parser (Route -> a) a
router = Parser.oneOf [ Parser.map Retreat top
                      , Parser.map SignUp (s "sign-up")
                      , Parser.map SemiSignUp (s "semi-sign-up")
                      , Parser.map YouthSignUp (s "youth" </> s "sign-up")
                      , Parser.map Leaders (s "leaders")
                      , Parser.map Retreat (s "retreat")
                      , Parser.map RetreatRegister (s "retreat" </> s "register")
                      , Parser.map RetreatMessages (s "retreat" </> s "messages")
                      ]

maybeRouter : Url -> Route
maybeRouter url = Maybe.withDefault NotFound <| parse router url

skipNothing : List (Maybe a) -> List a
skipNothing = List.concatMap (\m -> case m of 
                                        Just j -> [j]
                                        Nothing -> [])

userResume : UserResume -> Encode.Value
userResume resume = Encode.object <| [ ("email", Encode.string resume.email)
                                     , ("password", Encode.string resume.password)
                                     , ("name", Encode.string resume.name)
                                     , ("mobile", Encode.string resume.mobile)
                                     , ("birthDate", Encode.string resume.birthDate)
                                     , ("sex", Encode.string <| fromSex resume.sex)
                                     , ("campus", Encode.string resume.campus)
                                     , ("address", Encode.string resume.address)
                                     , ("school", Encode.string resume.school)
                                     , ("major", Encode.string resume.major)
                                     , ("grade", Encode.string resume.grade)
                                     ] ++ (Maybe.withDefault [] <| Maybe.map (\guide -> [("guide", Encode.string guide)]) resume.guide)

semiUserResume : SemiUserResume -> Encode.Value
semiUserResume resume = Encode.object [ ("name", Encode.string resume.name)
                                      , ("mobile", Encode.string resume.mobile)
                                      , ("campus", Encode.string resume.campus)
                                      , ("adminUid", Encode.string resume.adminUid)
                                      ]

youthUserResume : YouthUserResume -> Encode.Value
youthUserResume resume = Encode.object [ ("email", Encode.string resume.email)
                                       , ("password", Encode.string resume.password)
                                       , ("name", Encode.string resume.name)
                                       , ("mobile", Encode.string resume.mobile)
                                       , ("birthDate", Encode.string resume.birthDate)
                                       , ("address", Encode.string resume.address)
                                       , ("sex", Encode.string <| fromSex resume.sex)
                                       , ("team", Encode.string <| (String.fromInt << fromPasture) resume.team ++ "목장")
                                       ]

get : { url : Url, expect : Expect Msg } -> Cmd Msg
get { url, expect } = Http.request { method = "GET"
                                   , headers = defaultHeaders
                                   , url = Url.toString url
                                   , body = emptyBody
                                   , expect = expect
                                   , timeout = Nothing
                                   , tracker = Nothing
                                   }

delete : Url -> Cmd Msg
delete url = Http.request { method = "DELETE"
                          , headers = defaultHeaders
                          , url = Url.toString url
                          , body = emptyBody
                          , expect = expectWhatever LeaderUnset
                          , timeout = Nothing
                          , tracker = Nothing
                          }

post : { url : Url, body : Http.Body, expect : Result Http.Error () -> Msg } -> Cmd Msg
post { url, body, expect } = Http.request { method = "POST"
                                          , headers = defaultHeaders
                                          , url = Url.toString url
                                          , body = body
                                          , expect = expectWhatever expect
                                          , timeout = Nothing
                                          , tracker = Nothing
                                          }

put : { url : Url, body : Http.Body, expect : Result Http.Error () -> Msg } -> Cmd Msg
put { url, body, expect } = Http.request { method = "PUT"
                                         , headers = defaultHeaders
                                         , url = Url.toString url
                                         , body = body
                                         , expect = expectWhatever expect
                                         , timeout = Nothing
                                         , tracker = Nothing
                                         }

compare : Grade -> Grade -> Order
compare g1 g2 = let
                    toInt g = case g of
                                MEMBER -> 1
                                LEADER -> 2
                                MISSION -> 3
                                ASSISTANT -> 4
                                ADMIN -> 5
                in Basics.compare (toInt g1) (toInt g2)