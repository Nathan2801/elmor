module Main exposing (..)

import Browser
import Browser.Dom

import Task
import Process

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode exposing (..)

import Port

{- Main -}

main =
    Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

{- Model -}

type alias Model =
    { color : HSL
    , hovering : Bool
    , offset : MouseOffset
    , copied : Color
    }

setColorHue : Int -> Model -> Model
setColorHue v ({color} as m) =
    { m | color = { color | hue = v }}

setColorSaturation : Float -> Model -> Model
setColorSaturation v ({color} as m) =
    { m | color = { color | saturation = v }}

setColorLightness : Float -> Model -> Model
setColorLightness v ({color} as m) =
    { m | color = { color | lightness = v }}

init : () -> (Model, Cmd Msg)
init _ =
    ( {
        color = HSL 180 0.5 0.5,
        hovering = False,
        offset = MouseOffset 0 0,
        copied = ColorNone
      }
    , Cmd.none
    )

{- Update -}

type Msg
    = None
    | HueInputChange String
    | SaturationInputChange String
    | LightnessInputChange String
    | PickerMouseOffset MouseOffset
    | PickerMouseDown
    | PickerSize (Result Browser.Dom.Error Browser.Dom.Element)
    | CopyToClipboard Color String
    | ResetCopied

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = 
    case msg of
        None -> 
            ( m
            , Cmd.none
            )
        HueInputChange value ->
            ( m
            |> setColorHue (String.toInt value |> Maybe.withDefault 0)
            , Cmd.none
            )
        SaturationInputChange value ->
            ( m
            |> setColorSaturation ((String.toFloat value |> Maybe.withDefault 0.0) / 100)
            , Cmd.none
            )
        LightnessInputChange value ->
            ( m
            |> setColorLightness ((String.toFloat value |> Maybe.withDefault 0.0) / 100)
            , Cmd.none
            )
        PickerMouseOffset mouseOffset ->
            ( { m | offset = mouseOffset }
            , Browser.Dom.getElement "picker" |> Task.attempt (\a -> PickerSize a)
            )
        PickerMouseDown ->
            ( { m | hovering = not m.hovering }
            , Cmd.none
            )
        PickerSize result ->
            case result of
                Ok v ->
                    ( m
                    |> setColorSaturation (
                        if m.hovering then
                            (v.element.height - (toFloat m.offset.y)) / v.element.height
                        else
                            m.color.saturation
                        )
                    |> setColorLightness (
                        if m.hovering then
                            (toFloat m.offset.x) / v.element.width
                        else
                            m.color.lightness
                        ) 
                    , Cmd.none
                    )
                Err e ->
                    ( m
                    , Cmd.none
                    )
        CopyToClipboard color value ->
            ( { m | copied = color }
            , Cmd.batch [ Port.clipboard value 
                        , Task.perform (\_ -> ResetCopied) <| Process.sleep 1000
                        ]
            )
        ResetCopied ->
            ( { m | copied = ColorNone }
            , Cmd.none
            )

{- Subscriptions -}

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

{- General -}

type alias Vec3 a =
    { x : a
    , y : a
    , z : a
    }

hexDigits : String
hexDigits =
     "0123456789abcdef"

decToHex : Int -> String
decToHex n =
    String.dropLeft n hexDigits |> String.left 1

intToHex_ : Int -> Int -> String
intToHex_ n z =
    if n < 16 then
        String.repeat z "0" ++ (n |> decToHex)
    else
        (n // 16 |> decToHex) ++ (intToHex_ (n - n // 16 * 16) 0)

intToHex : Int -> String
intToHex n =
    intToHex_ n 1

floatMod : Float -> Float -> Float
floatMod a b =
    a - b * ((floor >> toFloat) (a / b))

{- Colors -}

type Color
    = ColorNone
    | ColorHSL
    | ColorRGB
    | ColorHex

type alias HSL =
    { hue : Int
    , saturation : Float
    , lightness : Float
    }

type alias RGB =
    { r : Int
    , g : Int
    , b : Int
    }

cssHSL : HSL -> String
cssHSL color =
    "hsl("
    ++ String.fromInt(color.hue)
    ++ ", "
    ++ (String.fromInt <| floor <| color.saturation * 100)
    ++ "%, "
    ++ (String.fromInt <| floor <| color.lightness * 100)
    ++ "%)"

cssRGB : RGB -> String
cssRGB color =
    "rgb("
    ++ String.fromInt color.r
    ++ ", "
    ++ String.fromInt color.g
    ++ ", "
    ++ String.fromInt color.b
    ++ ")"

cssHex : RGB -> String
cssHex color =
    "#"
    ++ intToHex color.r
    ++ intToHex color.g
    ++ intToHex color.b

rgbBase : Int -> Float -> Float -> Vec3 Float
rgbBase sector c x =
    case sector of
        0 -> Vec3 c x 0
        1 -> Vec3 x c 0
        2 -> Vec3 0 c x
        3 -> Vec3 0 x c
        4 -> Vec3 x 0 c
        5 -> Vec3 c 0 x
        _ -> Vec3 0 0 0

hsl2rgb : HSL -> RGB
hsl2rgb color =
    let
        h = color.hue
        s = color.saturation
        l = color.lightness
        c = (1 - abs(2 * l - 1)) * s
        x = c * (1 - abs ((floatMod (toFloat h / 60) 2) - 1))
        m = l - c / 2
        b = rgbBase (h // 60) c x
    in
    RGB (floor <| (b.x + m) * 255)
        (floor <| (b.y + m) * 255)
        (floor <| (b.z + m) * 255)

{- Custom Events -}

onMouseMove : Decoder a -> (a -> Msg) -> Attribute Msg
onMouseMove d f =
    on "mousemove" (Decode.map f d)

type alias MouseOffset =
    { x : Int
    , y : Int
    }

mouseOffsetDec : Decoder MouseOffset
mouseOffsetDec =
    map2 MouseOffset
    (field "offsetX" int)
    (field "offsetY" int)

{- View -}

colorPicker : Model -> Html Msg
colorPicker m =
    div [ class "picker" ]
    [ div [ class "picker-square margin-bottom"
          ]
          [ div [ class "border picker-component"
                , style "background" (cssHSL <| HSL m.color.hue 1.0 0.5)
                ]
                []
          , div [ class "border picker-component"
                , style "background" ("linear-gradient(transparent, " ++ (cssHSL <| HSL 0 (toFloat <| m.color.hue // 360) 0.5) ++ ")")
                ]
                []
          , div [ class "border picker-component"
                , style "background" "linear-gradient(90deg, black, transparent, white)"
                ]
                []
          , div [ id "picker"
                , class "border picker-component"
                , onMouseDown PickerMouseDown
                , onMouseMove mouseOffsetDec (\t -> PickerMouseOffset t)
                ]
                []
          ]
    , hueInput m
    , saturationInput m
    , lightnessInput m
    ]

hueBackgroundColor : Model -> Int -> String
hueBackgroundColor m i =
    cssHSL <| HSL (360 // 7 * i) m.color.saturation m.color.lightness

hueBackground : Model -> Attribute Msg
hueBackground m =
    "linear-gradient"
    ++ "("
    ++ "90deg"
    ++ ", "
    ++ (List.range 0 7
        |> List.map (\i -> hueBackgroundColor m i)
        |> String.join ", ")
    ++ ")"
    |> style "background"

hueInput : Model -> Html Msg
hueInput m =
    input [ type_ "range"
          , class "hsl-input border margin-bottom"
          , attribute "min" "0"
          , attribute "max" "360"
          , attribute "value" "180"
          , hueBackground m
          , onInput (\t -> HueInputChange t) ]
          []

saturationBackground : Model -> Attribute Msg
saturationBackground m =
    "linear-gradient("
    ++ "90deg"
    ++ ", "
    ++ (cssHSL <| HSL 0 0 m.color.lightness)
    ++ ", "
    ++ (cssHSL <| HSL m.color.hue 100 m.color.lightness)
    ++ ")"
    |> style "background"

saturationInput : Model -> Html Msg
saturationInput m =
    input [ type_ "range"
          , class "hsl-input border margin-bottom"
          , attribute "min" "0"
          , attribute "max" "100"
          , property "value" (Encode.int <| floor <| m.color.saturation * 100)
          , saturationBackground m
          , onInput (\t -> SaturationInputChange t) ]
          []

lightnessBackground : Model -> Attribute Msg
lightnessBackground m =
    "linear-gradient("
    ++ "90deg"
    ++ ", "
    ++ (cssHSL <| HSL 0 0.0 0.0)
    ++ ", "
    ++ (cssHSL <| HSL m.color.hue m.color.saturation 0.5)
    ++ ", "
    ++ (cssHSL <| HSL 0 0.0 1.0)
    ++ ")"
    |> style "background"

lightnessInput : Model -> Html Msg
lightnessInput m =
    input [ type_ "range"
          , class "hsl-input border margin-bottom"
          , attribute "min" "0"
          , attribute "max" "100"
          , property "value" (Encode.int <| floor <| m.color.lightness * 100)
          , lightnessBackground m
          , onInput (\t -> LightnessInputChange t) ]
          []

colorInfo : Model -> Html Msg
colorInfo m =
    let
        hslText = m.color |> cssHSL
        rgbText = m.color |> hsl2rgb |> cssRGB
        hexText = m.color |> hsl2rgb |> cssHex
    in
    div [ class "color-info" ]
    [ text "COLOR"
    , div [ class "color border margin-bottom"
          , style "background" (cssHSL <| HSL m.color.hue m.color.saturation m.color.lightness)
          ]
          [] 
    , div [ class <| if m.copied == ColorHSL then "copy-box" else ""
          ]
          [ text "HSL" ]
    , input [ readonly True
            , class "border margin-bottom"
            , property "value" (Encode.string hslText)
            , onClick <| CopyToClipboard ColorHSL hslText
            ]
            []
    , div [ class <| if m.copied == ColorRGB then "copy-box" else ""
          ]
          [ text "RGB"]
    , input [ readonly True
            , class "border margin-bottom"
            , property "value" (Encode.string rgbText)
            , onClick <| CopyToClipboard ColorRGB rgbText
            ]
            []
    , div [ class <| if m.copied == ColorHex then "copy-box" else ""
          ]
          [ text "HEX"]
    , input [ readonly True
            , class "border margin-bottom"
            , property "value" (Encode.string hexText)
            , onClick <| CopyToClipboard ColorHex hexText
            ]
            []
    ]

view : Model -> Html Msg
view m =
    div [ class "view"
        ]
    [ div [ class "header"
          ]
          [ h1 []
               [ text "Elmor" ]
          , p  [] [ text "Created by "
                  , a [ attribute "href" "https://github.com/Nathan2801"
                      ]
                      [ text "Johnathan" ]
                  ]
          ]
    , div [ class "app-body" ]
          [ colorPicker m 
          , colorInfo m
          ]
    ]
