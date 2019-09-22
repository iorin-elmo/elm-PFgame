module Main exposing (main)

import Browser

import Html exposing (Html, button, div, text, br)
import Html.Events exposing (onClick)
import Array
import Random

type alias Model =
    { count : Int, number : Int, isOver : Bool, primes : List Int }


initialModel : Model
initialModel =
    { count = 1, number = 35, isOver = False, primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,57,59,61,67,71,73,79,83,89,97] }


type Msg
    = Pressed Int
    | Continue
    | Next Int
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pressed n ->
            if ((Basics.modBy n model.number)==0) then
              if (model.number//n==1) then
                ( { model | count = model.count+1 }
                , Random.generate Next (nextComposite (kindsOfButtons model.count - 1) (model.count//5+2))
                )
                
              else
                ( { model | number = model.number // n}
                , Cmd.none
                )
            else
                ( { model | isOver = True }
                , Cmd.none
                )
                
        Next q ->
            ( { model | number = q }
            , Cmd.none
            )
        
        Continue ->
            ( initialModel, Cmd.none )
                
        _ -> ( model, Cmd.none )

numSys : Int -> String
numSys n = 
    case (Basics.modBy 10 n) of
        1 -> "st"
        2 -> "nd"
        3 -> "rd"
        _ -> "th"

primeArray =
    [ 2
    , 3
    , 5
    , 7
    , 11
    , 13
    , 17
    ] |> Array.fromList

nextComposite lim n =
   Random.int 0 lim
        |> Random.list n
        |> Random.map
            (List.map (\i -> Array.get i primeArray |> Maybe.withDefault 1)
                >> List.foldl (*) 1)

makeButton : Int -> Html Msg
makeButton n =
    button[onClick <| Pressed n][text <| String.fromInt n]

kindsOfButtons n= 
    4*(n//100+1)

view : Model -> Html Msg
view model =
    if model.isOver then
      div []
        [ text "GameOver!!"
        , br[][]
        , text <| "Scores : " ++ (String.fromInt (model.count-1))
        , br[][]
        , button[onClick Continue][text "Continue"]
        ]
    else
      div []
        (List.append
            [ text <| String.append (String.fromInt model.count)
                   <| String.append (numSys model.count) 
                                    " problem"
            , br[][]
            , text <| String.fromInt model.number
            , br[][]
            ]
            (List.map makeButton (List.take (kindsOfButtons model.count) model.primes))
        )

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Random.generate Next (nextComposite 3 2) )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
        
        