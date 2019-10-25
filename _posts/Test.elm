module Test exposing (main)

import Html
import Json.Decode exposing (Decoder, Value)


main =
    Html.text <|
        Debug.toString <|
            boxCodec (Debug.todo "a") (Debug.todo "b")


type Box a b
    = Two a b
    | One a
    | Empty


type Codec a
    = Codec
        { encoder : a -> Value
        , decoder : Decoder a
        }


boxCodec aCodec bCodec =
    custom
        (\box ->
            case box of
                Two x y ->
                    Debug.todo "two"

                One x ->
                    Debug.todo "one"

                Empty ->
                    Debug.todo "zero"
        )
        |> variant2 Two aCodec bCodec
        |> variant1 One aCodec
        |> variant0 Empty


custom matcher =
    ""


variant0 ctor partial =
    ""


variant1 ctor arg0 partial =
    ""


variant2 : a -> b -> c -> d -> String
variant2 ctor arg0 arg1 partial =
    ""
