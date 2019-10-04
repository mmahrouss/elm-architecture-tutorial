import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , validate : Bool
  }


init : Model
init =
  Model "" "" "" False

-- UPDATE


type Msg
  = Name String -- String -> Msg 
  | Password String -- //
  | PasswordAgain String -- // 
  | Validate
 

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name, validate = False }

    Password password ->
      { model | password = password, validate = False }

    PasswordAgain password ->
      { model | passwordAgain = password, validate = False }
      
    Validate -> 
      { model | validate = True }
      --model

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , button [ onClick Validate ] [ text "Submit" ] 
    , viewValidation model
    ]

-- takes 3 strings and a string to message function 
-- and returns an Html msg object
viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg = 
  input [type_ t, placeholder p, value v, onInput toMsg ] []
  
viewValidation : Model -> Html msg 
viewValidation model =
 if model.validate then
  if model.password == model.passwordAgain then
    validatePassword model.password 
  else
    if model.passwordAgain == "" then 
      div [ style "color" "gray" ] [ text "Please Re-enter password" ]
    else
      div [ style "color" "red" ] [ text "Passwords do not match!" ]
  else
  div [] []
accType : (Char -> Bool) -> Char -> Int -> Int
accType f ch sum = 
  sum + if f ch then 1 else 0


validatePassword : String -> Html msg
validatePassword p = 
  if not (String.length p > 8)
  then 
    div [ style "color" "red" ] 
    [ text "Passwords Must be longer than 8 characters" ]
  else
  if (String.foldl (accType Char.isUpper) 0 p ) == 0 
  || (String.foldl (accType Char.isLower) 0 p ) == 0
  || (String.foldl (accType Char.isDigit) 0 p ) == 0
  then 
  div [ style "color" "red" ] 
    [ text "Passwords Must contain one Lower case letter, Upper case letter and a digit" ]
  else
  div [ style "color" "green" ] [ text "OK" ]
  
            
