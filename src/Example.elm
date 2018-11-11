module Example exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import ModularScale exposing (Interval(..), get)



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { base : String
    , interval : Interval
    , h1 : Int
    , h2 : Int
    , p : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { base = "1, 1.25"
      , interval = PerfectFourth
      , h1 = 5
      , h2 = 3
      , p = 1
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnBaseInput String
    | SelectInterval Interval
    | OnScaleIndexChange ScaleIndex


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnBaseInput input ->
            ( { model | base = input }, Cmd.none )

        SelectInterval interval ->
            ( { model | interval = interval }, Cmd.none )

        OnScaleIndexChange (H1 input) ->
            ( { model | h1 = input }, Cmd.none )

        OnScaleIndexChange (H2 input) ->
            ( { model | h2 = input }, Cmd.none )

        OnScaleIndexChange (P input) ->
            ( { model | p = input }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    let
        base =
            String.split "," model.base
                |> List.filterMap (String.toFloat << String.trim)

        config =
            ModularScale.config base model.interval
    in
    main_ []
        [ viewControls model
        , viewText model config
        ]


viewControls : Model -> Html Msg
viewControls model =
    div
        [ style "width" "100%"
        , style "max-width" "800px"
        , style "box-sizing" "border-box"
        , style "margin" "1rem auto"
        , style "padding" "1rem 0"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "font-family" "Helvetica, Arial, sans-serif"
        ]
        [ div []
            [ label
                [ style "display" "block"
                , style "margin-bottom" ".5rem"
                ]
                [ text "Base(s)" ]
            , input
                [ onInput OnBaseInput
                , value model.base
                , style "padding" ".25rem"
                , style "width" "5rem"
                , style "font-size" "0.875rem"
                , style "margin-right" "1rem"
                ]
                []
            ]
        , div []
            [ label
                [ style "display" "block"
                , style "margin-bottom" ".5rem"
                ]
                [ text "Interval" ]
            , select
                [ on "change" <|
                    Decode.map SelectInterval <|
                        Decode.map intervalFromValue targetValue
                ]
                (List.map (viewIntervalOption model.interval) intervalList)
            ]
        ]


viewIntervalOption : Interval -> Interval -> Html Msg
viewIntervalOption current interval =
    option
        [ value (intervalToString interval)
        , selected (current == interval)
        ]
        [ text (intervalToString interval) ]


viewText : Model -> ModularScale.Config -> Html Msg
viewText model config =
    div
        [ style "background-color" "#f0f0f0"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "width" "100%"
        , style "box-shadow" "8px 8px 0 0 #ccc"
        , style "max-width" "800px"
        , style "box-sizing" "border-box"
        , style "margin" (ms config 1 ++ " auto")
        , style "padding" "2rem 4rem 2rem 6rem"
        , style "line-height" (ms config 3)
        ]
        [ h1
            [ style "font-size" (ms config model.h1)
            , style "line-height" (ms config 0)
            , style "position" "relative"
            ]
            [ text title, viewScalePicker H1 model.h1 ]
        , h2
            [ style "font-size" (ms config model.h2)
            , style "line-height" (ms config 1)
            , style "position" "relative"
            ]
            [ text subtitle, viewScalePicker H2 model.h2 ]
        , p
            [ style "font-size" (ms config model.p)
            , style "line-height" (ms config 1)
            , style "position" "relative"
            ]
            [ text body, viewScalePicker P model.p ]
        , h3
            [ style "font-size" (ms config 2)
            , style "line-height" (ms config 1)
            ]
            [ text "Values being generated" ]
        , ul [ style "font-size" (ms config 1) ] <|
            List.map viewScaleValue <|
                viewScaleList config 20
        , a [ href "https://package.elm-lang.org/packages/rl-king/elm-modular-scale/latest" ]
            [ text "Package docs" ]
        , br [] []
        , a [ href "https://github.com/rl-king/elm-modular-scale" ]
            [ text "Package repo" ]
        ]


type ScaleIndex
    = H1 Int
    | H2 Int
    | P Int


viewScalePicker : (Int -> ScaleIndex) -> Int -> Html Msg
viewScalePicker scaleIndex index =
    input
        [ value (String.fromInt index)
        , type_ "number"
        , style "width" "2rem"
        , style "padding" ".25rem"
        , style "font-size" "0.875rem"
        , style "position" "absolute"
        , style "left" "-4rem"
        , style "top" "0"
        , on "change" <|
            Decode.andThen (scaleIndexFromValue scaleIndex) targetValue
        ]
        []


scaleIndexFromValue : (Int -> ScaleIndex) -> String -> Decode.Decoder Msg
scaleIndexFromValue scaleIndex value =
    case String.toInt value of
        Nothing ->
            Decode.fail "Not an int"

        Just int ->
            Decode.succeed (OnScaleIndexChange (scaleIndex int))


viewScaleList : ModularScale.Config -> Int -> List ( Int, String )
viewScaleList config length =
    List.map (\x -> ( x, ms config x )) (List.range -5 length)


viewScaleValue : ( Int, String ) -> Html msg
viewScaleValue ( index, size ) =
    li [] [ text ("Index " ++ String.fromInt index ++ " : " ++ size) ]


ms : ModularScale.Config -> Int -> String
ms config x =
    String.fromFloat (get config x) ++ "em"



-- TEXT


title : String
title =
    "Fusce rhoncus pharetra purus vel ornare"


subtitle : String
subtitle =
    "Nunc volutpat vitae eros id facilisis. Nam convallis quam sed elit facilisis congue."


body : String
body =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam nunc nisi, iaculis nec placerat sed, fermentum sit amet ante. Vivamus facilisis ultricies neque, eu imperdiet est fermentum at. Sed vitae enim eu ante dignissim tincidunt elementum vitae libero. Vestibulum feugiat dolor risus, at imperdiet eros dictum vitae. Pellentesque ut tellus sit amet mauris efficitur consequat. Integer vitae enim vel sem venenatis pretium et ut augue. Ut nec cursus libero, vel pellentesque nunc."



-- INTERVAL


intervalList : List Interval
intervalList =
    [ MinorSecond -- 1.067
    , MajorSecond -- 1.125
    , MinorThird -- 1.2
    , MajorThird -- 1.25
    , PerfectFourth -- 1.33
    , AugFourth -- 1.414
    , PerfectFifth -- 1.5
    , MinorSixth -- 1.6
    , GoldenSection -- 1.618
    , MajorSixth -- 1.667
    , MinorSeventh -- 1.778
    , MajorSeventh -- 1.875
    , Octave -- 2
    , MajorTenth -- 2.5
    , MajorEleventh -- 2.667
    , MajorTwelfth -- 3
    , DoubleOctave -- 4
    ]


intervalToString : Interval -> String
intervalToString interval =
    case interval of
        MinorSecond ->
            "MinorSecond"

        MajorSecond ->
            "MajorSecond"

        MinorThird ->
            "MinorThird"

        MajorThird ->
            "MajorThird"

        PerfectFourth ->
            "PerfectFourth"

        AugFourth ->
            "AugFourth"

        PerfectFifth ->
            "PerfectFifth"

        MinorSixth ->
            "MinorSixth"

        GoldenSection ->
            "GoldenSection"

        MajorSixth ->
            "MajorSixth"

        MinorSeventh ->
            "MinorSeventh"

        MajorSeventh ->
            "MajorSeventh"

        Octave ->
            "Octave"

        MajorTenth ->
            "MajorTenth"

        MajorEleventh ->
            "MajorEleventh"

        MajorTwelfth ->
            "MajorTwelfth"

        DoubleOctave ->
            "DoubleOctave"

        Ratio x ->
            "Ratio"


intervalFromValue : String -> Interval
intervalFromValue interval =
    case interval of
        "MinorSecond" ->
            MinorSecond

        "MajorSecond" ->
            MajorSecond

        "MinorThird" ->
            MinorThird

        "MajorThird" ->
            MajorThird

        "PerfectFourth" ->
            PerfectFourth

        "AugFourth" ->
            AugFourth

        "PerfectFifth" ->
            PerfectFifth

        "MinorSixth" ->
            MinorSixth

        "GoldenSection" ->
            GoldenSection

        "MajorSixth" ->
            MajorSixth

        "MinorSeventh" ->
            MinorSeventh

        "MajorSeventh" ->
            MajorSeventh

        "Octave" ->
            Octave

        "MajorTenth" ->
            MajorTenth

        "MajorEleventh" ->
            MajorEleventh

        "MajorTwelfth" ->
            MajorTwelfth

        "DoubleOctave" ->
            DoubleOctave

        _ ->
            Ratio 0
