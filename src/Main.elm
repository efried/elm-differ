module Main exposing (main)

import Browser
import Html exposing (Html, br, button, div, input, p, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import LCS exposing (lcs)


type Msg
    = FirstWord String
    | SecondWord String
    | SubmitLCS


type alias Model =
    { longestSubstring : Maybe String
    , firstWord : String
    , secondWord : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { longestSubstring = Nothing
      , firstWord = ""
      , secondWord = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FirstWord word ->
            ( { model | firstWord = word }, Cmd.none )

        SecondWord word ->
            ( { model | secondWord = word }, Cmd.none )

        SubmitLCS ->
            let
                mtx =
                    lcs model.firstWord model.secondWord
            in
            ( { model
                | longestSubstring =
                    Just (lcs model.firstWord model.secondWord)
              }
            , Cmd.none
            )


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ viewInput "text" "First Word" model.firstWord FirstWord
            , viewInput "text" "Second Word" model.secondWord SecondWord
            , button [ onClick SubmitLCS ] [ text "Get Diff" ]
            ]
        , case model.longestSubstring of
            Just substring ->
                if String.length substring > 0 then
                    text substring

                else
                    div [ style "color" "red" ] [ text "No Common Substring" ]

            Nothing ->
                text ""
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
