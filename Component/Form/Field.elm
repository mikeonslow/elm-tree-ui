module Component.Form.Field (Model, Action, update, view, initialModel, mb, modelSignal) where

import String
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Address, mailbox)

template =
  div [ class "row" ] [
          div [class ("input-field col " ++ model.cols) ] [
            input [
              type' model.type'
              ,name model.name
              ,value model.value
              ,on "input" targetValue (\str -> Signal.message address (Validate str))
              ] [ ]
            ]
        ]

type alias Model = {
    name : String
    ,type' : String
    ,value : String
    ,required : Bool
    ,minLength : Int
    ,maxLength : Int
    ,errorMsg : String
    ,cols : String
}


initialModel : Model

initialModel = {
    name = ""
    ,type' = "text"
    ,value = "TEST"
    ,required = True
    ,minLength = 3
    ,maxLength = 5
    ,errorMsg = ""
    ,cols = "s12 m12 ll2"
    }


view : Signal.Address Action -> Model -> Html.Html
view address model =
  template


type Action
  = NoOp
  | Validate String


update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    Validate str ->
      let
        getValue str model = if model.minLength > (String.length str) then "ERROR" else str
        x = Debug.log str 1
      in
        { model | value = (getValue str model) }



mb : Signal.Mailbox Action
mb =
  Signal.mailbox NoOp


modelSignal : Signal.Signal Model
modelSignal =
  Signal.foldp update initialModel mb.signal

{--
main : Signal.Signal Html.Html
main =
  Signal.map (view mb.address) modelSignal
--}
