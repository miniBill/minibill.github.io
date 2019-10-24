module Test exposing (Point)


type Box a b
    = Two a b
    | One a
    | Empty


boxCodec =
    {- something here -}
    variant Two
        (\box ->
            case box of
                Two a b ->
                    Just ( a, b )

                _ ->
                    Nothing
        )
