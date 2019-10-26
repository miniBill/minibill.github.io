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


boxCodec : Codec a -> Codec b -> Codec (Box a b)
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


variant0 : t -> something -> Codec t
variant0 ctor partial =
    Debug.todo "variant0"


variant1 :
    String
    -> (a -> v)
    -> Codec a
    -> CustomCodec ((a -> Value) -> b) v
    -> CustomCodec b v
variant1 name ctor m1 (CustomCodec am) =
    CustomCodec
        { match =
            am.match <|
                \v ->
                    JE.object
                        [ ( "tag", JE.string name )
                        , ( "args"
                          , JE.list identity
                                [ encoder m1 v
                                ]
                          )
                        ]
        , decoder =
            Dict.insert name
                (JD.map ctor
                    (JD.index 0 <| decoder m1)
                )
                am.decoder
        }


variant2 : a -> b -> c -> d -> Codec something
variant2 ctor arg0 arg1 partial =
    Debug.todo "variant2"
