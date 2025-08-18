port module Main exposing (main)

import Array
import Browser
import Html exposing (Html, br, button, div, input, p, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, Value, decodeValue)
import Matrix exposing (Matrix)


type Msg
    = MatrixReceived (Matrix Int)
    | ToggleMatrix
    | FirstWord String
    | SecondWord String
    | SubmitLCS


port getLCS : { first : String, second : String } -> Cmd msg


port receiveLCSMatrix : (Value -> msg) -> Sub msg


type alias Model =
    { lcs : Matrix Int
    , longestSubstring : String
    , firstWord : String
    , secondWord : String
    , displayMatrix : Bool
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { lcs = Matrix.empty
      , longestSubstring = ""
      , firstWord = ""
      , secondWord = ""
      , displayMatrix = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MatrixReceived mtx ->
            ( { model
                | lcs = mtx
                , longestSubstring =
                    getLongestCommonSubstring model.firstWord model.secondWord mtx
                        |> String.fromList
                        |> String.reverse
              }
            , Cmd.none
            )

        ToggleMatrix ->
            ( { model
                | displayMatrix =
                    if model.displayMatrix == True then
                        False

                    else
                        True
              }
            , Cmd.none
            )

        FirstWord word ->
            ( { model | firstWord = word }, Cmd.none )

        SecondWord word ->
            ( { model | secondWord = word }, Cmd.none )

        SubmitLCS ->
            ( model, getLCS { first = model.firstWord, second = model.secondWord } )


listOfListDecoder : Decoder (List (List Int))
listOfListDecoder =
    Json.Decode.list (Json.Decode.list Json.Decode.int)


matrixDecoder : Value -> Matrix Int
matrixDecoder =
    decodeValue
        (listOfListDecoder
            |> Json.Decode.map Matrix.fromLists
            |> Json.Decode.map (Maybe.withDefault Matrix.empty)
        )
        >> Result.withDefault Matrix.empty


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveLCSMatrix (matrixDecoder >> MatrixReceived)


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewPrettyMatrix : Matrix Int -> Html msg
viewPrettyMatrix matrix =
    div []
        (Matrix.pretty String.fromInt matrix
            |> String.split "\n"
            |> List.map text
            |> List.intersperse (br [] [])
        )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ viewInput "text" "First Word" model.firstWord FirstWord
            , viewInput "text" "Second Word" model.secondWord SecondWord
            , button [ onClick SubmitLCS ] [ text "Get Diff" ]
            ]
        , text ("Longest substring: \"" ++ model.longestSubstring ++ "\"")
        , div []
            [ button [ onClick ToggleMatrix ] [ text "View Matrix" ]
            , if model.displayMatrix then
                viewPrettyMatrix model.lcs

              else
                text ""
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias LCSAccumulator =
    { first : Int, second : Int, result : List Char, matrix : Matrix Int }


getCharacter : Int -> String -> Maybe Char
getCharacter idx val =
    String.toList val
        |> Array.fromList
        |> Array.get idx


getLongestCommonSubstring : String -> String -> Matrix Int -> List Char
getLongestCommonSubstring firstString secondString matrix =
    let
        initial : LCSAccumulator
        initial =
            { first = String.length firstString, second = String.length secondString, result = [], matrix = matrix }

        getNextValues : LCSAccumulator -> LCSAccumulator
        getNextValues acc =
            let
                firstChar : Maybe Char
                firstChar =
                    getCharacter (acc.first - 1) firstString

                secondChar : Maybe Char
                secondChar =
                    getCharacter (acc.second - 1) secondString
            in
            case ( firstChar, secondChar ) of
                ( Nothing, Nothing ) ->
                    acc

                ( Just a, Just b ) ->
                    if a == b then
                        { acc | first = acc.first - 1, second = acc.second - 1, result = List.append acc.result [ a ] }

                    else if
                        (Matrix.get (acc.first - 1) acc.second acc.matrix
                            |> Maybe.withDefault 0
                        )
                            <= (Matrix.get acc.first (acc.second - 1) acc.matrix |> Maybe.withDefault 0)
                    then
                        { acc | second = acc.second - 1 }

                    else
                        { acc | first = acc.first - 1 }

                ( _, _ ) ->
                    if
                        (Matrix.get (acc.first - 1) acc.second acc.matrix
                            |> Maybe.withDefault 0
                        )
                            <= (Matrix.get acc.first (acc.second - 1) acc.matrix |> Maybe.withDefault 0)
                    then
                        { acc | second = acc.second - 1 }

                    else
                        { acc | first = acc.first - 1 }

        lcsReducer values =
            if values.first == 0 || values.second == 0 then
                values.result

            else
                lcsReducer (getNextValues values)
    in
    lcsReducer initial
