import Html exposing (Html, Attribute, text, toElement, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address, Message, (<~))
import StartApp as StartApp
import Time exposing (Time, every, second)
import String
import Date exposing (Date)
import Effects exposing (Effects)
import List
import Result exposing (Result, andThen)


main =
    .html <| StartApp.start { init = newModel, view = view, update = update, inputs = [Tick <~ every second]}


update : Action -> Model -> (Model, Effects Action)
update action model = (case action of
    Tick time -> model -- toString <| Date.second <| Date.fromTime <| time 
    ModStart (Dt str) -> str
    ModStart (Tm str) -> toString <| parseHourMinute str
    ModEnd (Dt str) -> str
    ModEnd (Tm str) -> str
    , Effects.none)

-- stub
updateRemain : Time -> Model -> Model
updateRemain time model = model ++ ""

parseHourMinute : String -> Result String Float
parseHourMinute  = resultSum << List.map2 (Result.map2 (*)) [Ok Time.hour, Ok Time.minute] << List.map String.toFloat << String.split ":"

resultSum : List (Result a Float) -> Result a Float 
resultSum = List.foldr (Result.map2 (+)) (Ok 0)

{-
piyo : List (Result String Float) -> Result String Float
piyo lst = List.map2 piyopiyo lst [Time.hour, Time.minute] `andThen` (List.map2 (*) [Time.hour, Time.minute]) `andThen` List.sum


piyopiyo : Result String Float -> Float -> Result String Float
piyopiyo a b = Result.map (*) a (Ok b) 
-}

type Action = Tick Time | ModStart RawStringDateTime | ModEnd RawStringDateTime 

type RawStringDateTime = Dt String | Tm String 


view : Address Action -> Model -> Html
view address model =
    div []
        [
            input
            [ placeholder "Start : date"
            , value model
            , type' "date"
            , on "input" targetValue (hoge address <| ModStart << Dt)
            , myStyle
            ]
            []
            ,input
            [ placeholder "Start : time"
            , value model
            , type' "time"
            , on "input" targetValue (hoge address <| ModStart << Tm)
            , myStyle
            ]
            []
            , input
            [ placeholder "End : date"
            , value model
            , type' "date"
            , on "input" targetValue (hoge address <| ModEnd << Dt)
            , myStyle
            ]
            []
            , input
            [ placeholder "End : time"
            , value model
            , type' "time"
            , on "input" targetValue (hoge address <| ModEnd << Tm)
            , myStyle
            ]
            []
            , div [ myStyle ] [ text (model) ]
        ]

hoge : Address Action -> (String -> Action) -> String -> Message
hoge addr const str = Signal.message addr (const str)

myStyle : Attribute
myStyle =
    style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
type alias Model = String

{-
type alias Model = 
    { start : Tm,
     end : Time}
                     -}

newModel = ("", Effects.none)
