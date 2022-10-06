module Views exposing (..)

import Html exposing (Attribute, node, Html)
import Types exposing (Msg)
import Html exposing (span, text, input)
import Html.Attributes exposing (id, type_, attribute, class)
import Html.Events exposing (onInput)
import Html exposing (div)
import Html.Attributes
import Html.Events exposing (on)
import Json.Decode as Decode
import Html exposing (li)

accountListItem : String -> String -> Html msg
accountListItem name email = listItem [ attribute "twoline" "true"
                                      , attribute "graphic" "avatar"
                                      , noninteractive
                                      ] [ span [] [ text name ]
                                        , span [ attribute "slot" "secondary" ] [ text email ]
                                        , icon [ attribute "slot" "graphic" ] "account"
                                        ]

action : Attribute msg
action = attribute "slot" "action"

activatable : Attribute msg
activatable = attribute "activatable" "true"

activated : Attribute msg
activated = attribute "activated" "true"

appBarFixed : List (Attribute msg) -> List (Html msg) -> Html msg
appBarFixed = node "mwc-top-app-bar-fixed"

appBarTitle : String -> Html msg
appBarTitle title = div [ attribute "slot" "title" ] [ text title ]

actionItems : Attribute msg
actionItems = attribute "slot" "actionItems"

buffer : Float -> Attribute msg
buffer b = attribute "buffer" <| String.fromFloat b

materialButton : List (Attribute msg) -> List (Html msg) -> Html msg
materialButton = node "mwc-button"

checkBox : List (Attribute msg) -> List (Html msg) -> Html msg
checkBox = node "mwc-checkbox"

checked : Attribute msg
checked = Html.Attributes.checked True

checklistItem : List (Attribute msg) -> List (Html msg) -> Html msg
checklistItem = node "mwc-check-list-item"

circularProgress : List (Attribute msg) -> List (Html msg) -> Html msg
circularProgress = node "mwc-circular-progress"

dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog = node "mwc-dialog"

disabled : Attribute msg
disabled = Html.Attributes.disabled True

dismiss : Html msg
dismiss = iconButton [ attribute "slot" "dismiss"
                     , attribute "icon" "close"
                     ] []

divider : Html msg
divider = li [ attribute "divider" "true"
             , attribute "role" "separator"
             , attribute "padded" "true"
             ] []

drawer : List (Attribute msg) -> List (Html msg) -> Html msg
drawer = node "mwc-drawer"

extended : Attribute msg
extended = attribute "extended" "true"

formField : List (Attribute msg) -> List (Html msg) -> Html msg
formField = node "mwc-formfield"

helper : String -> Attribute msg
helper = attribute "helper"

icon : List (Attribute msg) -> String -> Html msg
icon attrs iconText = node "mwc-icon" attrs [ text iconText ]

icon_ : String -> Attribute msg
icon_ = attribute "icon"

iconButton : List (Attribute msg) -> List (Html msg) -> Html msg
iconButton = node "mwc-icon-button"

iconListItem : String -> String -> String -> Html msg
iconListItem size graphic item = listItem [ attribute "graphic" size ] [ span [] [ text item ]
                                                                       , icon [ attribute "slot" "graphic" ] graphic
                                                                       ]

indeterminate : Attribute msg
indeterminate = attribute "indeterminate" "true"

label : String -> Attribute msg
label = attribute "label"

labelText : String -> Attribute msg
labelText labeltext = attribute "labelText" labeltext

leading : Attribute msg
leading = attribute "leading" "true"

linearProgress : List (Attribute msg) -> List (Html msg) -> Html msg
linearProgress = node "mwc-linear-progress"

list : List (Attribute msg) -> List (Html msg) -> Html msg
list = node "mwc-list"

listItem : List (Attribute msg) -> List (Html msg) -> Html msg
listItem = node "mwc-list-item"

markers : Attribute msg
markers = attribute "markers" "true"

menu : List (Attribute msg) -> List (Html msg) -> Html msg
menu = node "mwc-menu"

metaListItem : String -> String -> Html msg
metaListItem meta item = listItem [ attribute "hasMeta" "true" ] [ span [] [ text item ]
                                                                 , icon [ attribute "slot" "meta" ] meta 
                                                                 ]

mini : Attribute msg
mini = attribute "mini" "true"

multi : Attribute msg
multi = attribute "multi" "true"

navigationIcon : Attribute msg
navigationIcon = attribute "slot" "navigationIcon"

noninteractive : Attribute msg
noninteractive = attribute "noninteractive" "true"

onAction : (Int -> msg) -> Attribute msg
onAction handler = on "action" (Decode.map handler <| Decode.field "index" Decode.int)

onActivated : (Int -> msg) -> Attribute msg
onActivated handler = on "MDCTabBar:activated" (Decode.map handler <| Decode.field "index" Decode.int)

onSelected : (Int -> msg) -> Attribute msg
onSelected s = on "selected" (Decode.map s <| Decode.field "index" Decode.int)

open : Attribute msg
open = attribute "open" "true"

pin : Attribute msg
pin = attribute "pin" "true"

progress : Float -> Attribute msg
progress p = attribute "progress" <| String.fromFloat p

radio : List (Attribute msg) -> List (Html msg) -> Html msg
radio = node "mwc-radio"

radioListItem : List (Attribute msg) -> List (Html msg) -> Html msg
radioListItem = node "mwc-radio-list-item"

required : Attribute msg
required = Html.Attributes.required True

select : List (Attribute msg) -> List (Html msg) -> Html msg
select = node "mwc-select"

selected : Attribute msg
selected = Html.Attributes.selected True

slider : List (Attribute msg) -> List (Html msg) -> Html msg
slider = node "mwc-slider"

snackbar : List (Attribute msg) -> List (Html msg) -> Html msg
snackbar = node "mwc-snackbar"

stacked : Attribute msg
stacked = attribute "stacked" "true"

switch : List (Attribute msg) -> List (Html msg) -> Html msg
switch = node "mwc-switch"

tab : List (Attribute msg) -> List (Html msg) -> Html msg
tab = node "mwc-tab"

tabBar : List (Attribute msg) -> List (Html msg) -> Html msg
tabBar = node "mwc-tab-bar"

textarea : List (Attribute msg) -> List (Html msg) -> Html msg
textarea = node "mwc-textarea"

textField : List (Attribute msg) -> List (Html msg) -> Html msg
textField = node "mwc-textfield"

twolineListItem : String -> String -> List (Attribute msg) -> Html msg
twolineListItem first second attrs = listItem (attribute "twoline" "true" :: attrs) [ span [] [ text first ]
                                                                                    , span [ attribute "slot" "secondary" ] [ text second ]
                                                                                    ]

validationMessage : String -> Attribute msg
validationMessage = attribute "validationMessage"