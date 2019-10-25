---
title: "Advanced Elm: Codecs"
tags: elm design phantom_types
---

## Intoduction ##
*Advanced Elm is a project to explore some of the more advanced corners of the Elm language.*

I have some objective for Advanced Elm:
1. showing that Elm, while simple and perfect for beginners, is powerful enough for a lot of advanced scenarios,
2. trying to unravel how to use advanced language features like phantom types and extensible records,
3. trying to shed some light on how I come up with the types, and the APIs.

In this post I'll explain how I write libraries that use difficult types, using [`elm-codec`](https://package.elm-lang.org/packages/miniBill/elm-codec/latest) as an example.

Rather than explaining how the current code works I'll reimplement it from scratch to emphasize the design process.

### `elm-codec` ###

`elm-codec` is a package for defining JSON encoder/decoder pairs.
It (almost, we'll see why) guarantees that the encoders and decoders are actually one the inverse of the other, reduces boilerplate and supports custom types.

## Antophilia ##
I'll usually start the design process from an API, and I'll try to create the best API that I think is feasible within the language.

We'll have a `Codec a` type for a pair of `a -> Value`, `Decoder a` values. When in doubt, start with an opaque type, there's always time to expose more in a point release (if you instead want to expose less, that's a breaking change).

Let's write the simplest thing that could possibly work:
```
type Codec a
    = Codec
        { encoder : a -> Value
        , decoder : Decoder a
        }
```

This is actually the definition of a `Codec a` in `elm-codec`!

### Records ###

Records are a bit more interesting.

Let's start thinking about what the API could be.

We will certainly need:
1. something to build a new record;
2. something to get the fields' value from an existing record;
3. the names of the fields (because we want to produce a nice JSON object);
4. codecs for the fields.

The first point is something we need once for each record, the last three are once per field. Every type alias (and records will often be type aliases) automatically creates a function to build a record of that type, so the alias name could be what we need for point 1.

To get fields' values we can use Elm's dot notation `.field`, and names can be simple strings.

In the spirit of [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest/) or [`elm-decode-pipeline`](https://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest/) (always look for inspiration in commonly used libraries, they've often put a lot of tought into API design) we'll try to create a pipeline API:

```Elm
type alias Point =
    { x : Float
    , y : Float
    }


pointCodec =
    object Point
        |> field "x" .x Codec.float
        |> field "y" .y Codec.float
```

Turns out we can *almost* implement this API. The only difference will be that after all the `field` lines, one needs a `buildObject` for technical reasons. We *could* implement this exact API but it would complicate the whole implementation for negligible gains.

If you want a challenge you can try do it. Hint: change the `encoder` inside the opaque type from `a -> Value` to `a -> (Value, List (String, Value))` and use the second item in the tuple to rebuild the current `Value` every time you pass through a `field`. This is inefficent, and horrible and means that every other Codec has to produce an useless empty list. Also, as we'll see later, the `Codec a` type might not be enough for a record decoder while we're building it.

### Custom types ###

This is the hardest part, and where `elm-codec` is novel compared to, for example [`jamesmacaulay/elm-json-bidirectional`](https://old.elm.dmy.fr/packages/jamesmacaulay/elm-json-bidirectional/1.1.0/) or [`prozacchiwawa/elm-json-codec`](https://package.elm-lang.org/packages/prozacchiwawa/elm-json-codec/latest/). This is also the reason I published `elm-codec` in the first place: the other two packages already existed, and are fine for basic types and records, but have low support for custom types.

We will need:
1. ways to build the variants;
2. the variants' names (because we want to produce a decent JSON object);
3. the arguments' codecs;
4. a way to extract the variant name and arguments from a value of the custom type.

The first three are trivial, the interesting point if the last one! We basically have two possible designes:

#### Partial pattern matching ####
This is probably the simplest idea, and possibly (haven't tried) the easiest implementation:

```Elm
type Box a b
    = Two a b
    | One a
    | Empty


boxCodec =
    {- something here -}
    variant2 Two
        (\box ->
            case box of
                Two a b ->
                    Just ( a, b )

                _ ->
                    Nothing
        )
        aCodec bCodec
```

This would work, but it doesn't use the compiler to help the user! There is no way to check that the user has written a matcher for every variant.

#### `case` ####
In general, the only way to extract arguments if you have a value of a custom type is to use pattern matching with `case`, and that's what we did in the previous subsection.
Once we've asked the library user to write `case` statements, why don't we go all the way and actually ask them to write the full `case` expression?
This immediately gives us the guarantee that there are no forgotten variants, so let's try and write the API:

```Elm
type Box a b
    = Two a b
    | One a
    | Empty


boxCodec =
    custom
        (\box ->
            case box of
                Two x y ->
                    {- something using x and y -}

                One x ->
                    {- something using x -}

                Empty ->
                    {- something -}
        )
        |> variant2 Two aCodec bCodec
        |> variant1 One aCodec
        |> variant0 Empty
```

As in the record case we'll actually need a final `buildCustom` function to close the pipeline. Again, if you find a clever way to avoid it, do let me know!

The most generic "something using `x` and `y`" that we can write is a function that uses them. Which function? We don't know yet! But it's something that will probably come from the `custom` function, and will be used in the `case` statement. It's something that we must give to the user and adding more arguments to the pattern match lambda besides `box` looks good, but we'll expand on this later.

### API ###

Why was the section introducing APIs called Antophilia you ask? Well, "api" in Italian means bees 😂.

## Implementation ##

### Basics ###
Nothing to see here (code has been compressed, the library is actually `elm-format`ted and has type annotations):

```Elm
build encoder_ decoder_ = Codec { encoder = encoder_, decoder = decoder_ }

string = build JE.string JD.string
bool = build JE.bool JD.bool
int = build JE.int JD.int
float = build JE.float JD.float
```

The only midly interesting one is for `char`:

```Elm
char =
    build
        (String.fromChar >> JE.string)
        (JD.string
            |> JD.andThen
                (\s ->
                    case String.uncons s of
                        Just ( h, "" ) ->
                            JD.succeed h

                        _ ->
                            JD.fail "Expected a single char"
                )
        )
```

We simply work around the fact that JSON has no `Char` type.

### Dict, Set, List, ... ###
Those are absolutely obvious too. You can have a look at `elm-codec`'s source if you're curious.

### Records ###
Let's remember the API we're aiming at:

```Elm
type alias Point =
    { x : Float
    , y : Float
    }


pointCodec =
    object Point
        |> field "x" .x Codec.float
        |> field "y" .y Codec.float
```

Again, we're missing a final `buildObject` in the pipeline, **but we don't know that yet**, so let's explore how we arrive there!

We start by considering the simplest case: a record with a single field.

```Elm
type alias Box =
    { x : Int
    }


boxCodec =
    object Box
        |> field "x" .x Codec.int
```

* `Box` has type `Int -> Box`, so `object Box` will probably have `Int -> Box` somewhere in its type
* `.x` has type `Box -> Int` and `Codec.int` has type `Int`
* `boxCodec` has type `Codec Box`

So `field "x" .x Codec.int` is associated with `Int` and should go from `Something (Int -> Box)` to `Codec Box`.
It looks like `field`'s type is `field : String -> (box -> field) -> Codec field -> Something (field -> Box) -> Codec Box`.
Seems promising.

Let's go back to `Point`.

If we're right `field "y" .y Codec.float` should go from `Something (Float -> Point)` to `Codec Point`, and `object Point` should be `Something (Float -> Float -> Box)` and `field "x" .x Codec.float` goes from `Something (Float -> Float -> Point)` to `Codec (Float -> Point)` so `Something` is `Codec`!
This will actually turn out to be false, but we're almost there.

Implementation time!

```Elm
field : String -> (obj -> field) -> Codec field -> Codec (field -> obj) -> Codec obj
field name fieldGetter (Codec fieldCodec) (Codec partialCodec) =
    build
        encoder
        decoder
```

Let's reason about types! Remember, highly generic code where the types are mostly type variables is like Sudoku, you can often understand what you need to write it just by looking at all the types.

The ingredients are:
* `name: String`
* `fieldGetter : obj -> field`
* `fieldCodec : { encoder : field -> Value, decoder : Decoder field }`
* `partialCodec : { encoder : (field -> obj) -> Value, decoder : Decoder (field -> obj) }`

`decoder`'s type is `Decoder obj`, and if we look at the ingredients this has the exact same type of `Json.Decode.map2 (\f v -> f v) partialCodec.decoder fieldCodec.decoder`! The actual correct implementation is `Json.Decode.map2 (\f v -> f v) partialCodec.decoder (Json.Decode.field name fieldCodec.decoder)` because we need to extract the value from the JSON object's field.

`encoder`'s type is `obj -> Value`, and we could write `fieldCodec.encoder << fieldGetter` but it's quite evident that we would be dropping all the information from our record except from that single field so it can't be right, we need to use `partialCodec.encoder`, but it has a strange type, how can it convert a function `field -> obj` into a `Value`? Something is fishy!

Let's pretend that we are implementing this in JS/another language and let's focus on the encoding part. We start with the record constructor, which we don't need for encoding, let's go on. When we encounter a `field`, we are given the field getter, name and `Codec`, so we can extract the value, name it and turn into a JSON value. If this were JS this is were we would add it to the object we are building, but in Elm there is no facility for adding fields to a `Value` (it could be something different from an object after all!). What do we have in Elm that supports adding? `Dict`s and `List`! For this usecase we never need to get individual items after we add them, so a `List` will do. If we start thinking in terms of adding `(String, Value)` couples to a list we quickly realize that we need to either:

* change the definition of `Codec` to keep this `List` around, but this is messy for every other type beside records or
* create a different type for record `Codec`s.

Let's go for the secound route (the first one is left as a challenge to the reader).

```Elm
type ObjectCodec a
    = ObjectCodec
        { encoder : a -> List ( String, Value )
        , decoder : Decoder a
        }

field : String -> (obj -> field) -> Codec field -> ObjectCodec (field -> obj) -> ObjectCodec obj
field name fieldGetter (ObjectCodec fieldCodec) (ObjectCodec partialCodec) =
    build
        encoder
        decoder
```

The ingredients are now:
* `name: String`
* `fieldGetter : obj -> field`
* `fieldCodec : { encoder : field -> Value, decoder : Decoder field }`
* `partialCodec : { encoder : (field -> obj) -> List (String, Value), decoder : Decoder (field -> obj) }`

Ah. Not much better! The problem is that `encoder` is `a -> ...` and `decoder` is `Decoder a`, but while we're building the `Codec`, `a` is a function type, which is not going to work.

Let's make ourselves a little more room to work:

```Elm
type ObjectCodec a b
    = ObjectCodec
        { encoder : a -> List ( String, Value )
        , decoder : Decoder b
        }

field : String -> (obj -> field) -> Codec field -> ObjectCodec something1 (field -> obj) -> ObjectCodec something2 obj
field name fieldGetter (ObjectCodec fieldCodec) (ObjectCodec partialCodec) =
    build
        encoder
        decoder
```

Now, if we write `ObjectCodec a a` we end up with the exact same thing, but we have more freedom if we need to.

The ingredients are at last:
* `name: String`
* `fieldGetter : obj -> field`
* `fieldCodec : { encoder : field -> Value, decoder : Decoder field }`
* `partialCodec : { encoder : something1 -> List (String, Value), decoder : Decoder (field -> obj) }`

And `encoder` is `something2 -> List (String, Value)`. We have `obj -> field` to combine with `field -> Value`, so `something2` should allow us to get an `obj` out somehow. We also want to use `partialCodec.encoder`, so `something1` should also allow us to get an `obj` out. What's the simplest thing? `something1 = something2 = obj`!

Let's review:
* `name: String`
* `fieldGetter : obj -> field`
* `fieldCodec : { encoder : field -> Value, decoder : Decoder field }`
* `partialCodec : { encoder : obj -> List (String, Value), decoder : Decoder (field -> obj) }`

And we want to create a `obj -> List (String Value)`. This is now trivial: `\v -> ( name, fieldCodec.encoder <| fieldGetter v ) :: partialCodec.encoder v`.

So what happens in the `Point` case? `object` must return `ObjectCodec Point (Float -> Float -> Point)`, so it's simply:

```Elm
object ctor =
    ObjectCodec
        { encoder = \_ -> []
        , decoder = Json.Decode.succeed ctor
        }
```

And the final value in the pipeline is an `ObjectCodec Point Point` which suggest that

```Elm
buildObject : ObjectCodec a a -> Codec a
buildObject (ObjectCodec om) =
    Codec
        { encoder = \v -> JE.object <| om.encoder v
        , decoder = om.decoder
        }
```

And we're done!

### Custom types ###
This is the really interesting part. Let's remember the target API:

```Elm
type Box a b
    = Two a b
    | One a
    | Empty


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
```

And let's write a simpler example:

```Elm
type Box a
    = One a


boxCodec aCodec =
    custom
        (\box ->
            case box of
                One x ->
                    Debug.todo "one"
        )
        |> variant1 One aCodec
```

The first argument to `custom` is (`Box -> a`), and `variant1` has type `(a -> Box) -> Codec a -> something -> Codec Box`

To be completed...
