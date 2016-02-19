module Component.Form.Field (Model, Action, update, view, template, initialModel, mb, modelSignal) where

import String
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Address, mailbox)


template address model =
  div [ class "row" ] [
          div [class ("input-field col " ++ model.cols) ] [
            input [
              type' model.type'
              ,name model.name
              ,value model.value
              ,on "input" targetValue (\str -> Signal.message address (Validate str))
              ] [ ]
            ]
            , div [ ] [ text model.errorMessage ]

        ]


type alias Model = {
    name : String
    ,type' : String
    ,value : String
    ,required : Bool
    ,minLength : Int
    ,maxLength : Int
    ,errorMessage : String
    ,cols : String
}


initialModel : Model
initialModel = {
    name = ""
    ,type' = "text"
    ,value = ""
    ,required = True
    ,minLength = 3
    ,maxLength = 5
    ,errorMessage = "No error"
    ,cols = "s12 m12 ll2"
    }


view : Signal.Address Action -> Model -> Html.Html
view address model =
  template address model


type Action
  = NoOp
  | Validate String


update : Action -> Model -> Model
update action model =
  case action of

    NoOp -> model

    Validate str ->
      let
        checkValues str model =
          if model.required && 1 > (String.length str) then "Field is required and cannot be blank"
          else if model.minLength > (String.length str) then "Field requires at least "
            ++ (toString model.minLength) ++ " characters"
          else if model.maxLength < (String.length str) then "Field cannot be more than "
            ++ (toString model.maxLength) ++ " characters"
          else ""
      in
        { model | value = str, errorMessage = (checkValues str model)}



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
