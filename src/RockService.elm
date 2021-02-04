module RockService exposing (..)
import Http exposing (expectJson)
import Types exposing (Msg, Msg(..), root, leaders, mongsanpoMembers, Uid, info)
import Json.Decode as Decode
import Json.Encode as Encode
import Types exposing (MongsanpoResume, mongsanpoResume, RetreatResume, retreatEditResume, retreatResume)
import Types exposing (Grade, Grade(..))
import Types exposing (fromGrade)
import Types exposing (UserResume, userResume)
import Types exposing (SemiUserResume, semiUserResume)
import Types exposing (YouthUserResume, youthUserResume)

editCampuses : Int -> List String -> Cmd Msg
editCampuses id campuses = Types.put { url = { root | path = "/leaders/" ++ String.fromInt id ++ "/edit" }
                                     , body = Http.jsonBody <| Encode.object [("names", Encode.list Encode.string campuses)]
                                     , expect = CampusesEdited
                                     }

editRetreat : RetreatResume -> Cmd Msg
editRetreat resume = Types.post { url = { root | path = "/retreat/edit" }
                                , body = Http.jsonBody <| retreatEditResume resume
                                , expect = RetreatEdited
                                }

getMyInfo : Uid -> Cmd Msg
getMyInfo uid = Types.get { url = { root | path = "/members/info", query = Just <| "uid=" ++ uid }
                          , expect = expectJson MyInfo info
                          }

getLeaders : Cmd Msg
getLeaders = Types.get { url = { root | path = "/leaders" }
                       , expect = expectJson GotLeaders leaders
                       }

getMembers : String -> Cmd Msg
getMembers member = Types.get { url = { root | path = "/members/search", query = Just <| "name=" ++ member }
                              , expect = expectJson Members <| Decode.list Types.member
                              }

getMongsanpoMembers : Cmd Msg
getMongsanpoMembers = Types.get { url = { root | path = "/mongsanpo/members" }
                                , expect = expectJson MongsanpoMembers mongsanpoMembers
                                }

registerMonsanpo : MongsanpoResume -> Cmd Msg
registerMonsanpo resume = Types.post { url = { root | path = "/mongsanpo/members" }
                                     , body = Http.jsonBody <| mongsanpoResume resume
                                     , expect = MongsanpoRegistered
                                     }

registerRetreat : RetreatResume -> Cmd Msg
registerRetreat resume = Types.post { url = { root | path = "/retreat/register" }
                                    , body = Http.jsonBody <| retreatResume resume
                                    , expect = RetreatRegistered
                                    }

setGrade : Int -> Maybe Grade -> Cmd Msg
setGrade memId mGrade = Types.post { url = { root | path = "/leaders/register" }
                                   , body = Http.stringBody "application/x-www-form-urlencoded" <| "id=" ++ String.fromInt memId ++ "&grade=" ++ (fromGrade << Maybe.withDefault LEADER) mGrade
                                   , expect = GradeSet
                                   }

semiSignUp : SemiUserResume -> Cmd Msg
semiSignUp resume = Types.post { url = { root | path = "/members/semi-join" }
                               , body = Http.jsonBody <| semiUserResume resume
                               , expect = SemiSignedUp
                               }

signUp : UserResume -> Cmd Msg
signUp resume = Types.post { url = { root | path = "/members/join" }
                           , body = Http.jsonBody <| userResume resume
                           , expect = SignedUp
                           }

youthSignUp : YouthUserResume -> Cmd Msg
youthSignUp resume = Types.post { url = { root | path = "members/bwm/join" }
                                , body = Http.jsonBody <| youthUserResume resume
                                , expect = YouthSignedUp
                                }

unsetLeader : Int -> Cmd Msg
unsetLeader id = Types.delete { root | path = "/leaders/" ++ String.fromInt id }