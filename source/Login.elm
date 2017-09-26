module Login exposing (Model, Msg, new, update, view, checkLoggedIn, isLoggedIn, isLoginPending)

import Html exposing (Html, div, p, text)
import Http
import Json.Decode exposing (..)
import Json.Encode
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Typography as Typo


-- MODEL --


type LoginState
    = Unknown
    | NotLoggedIn
    | LoggedIn


type alias Model =
    { state : LoginState
    , username : String
    , password : String
    , mdl : Material.Model
    }


new : Model
new =
    { state = Unknown
    , username = ""
    , password = ""
    , mdl = Material.model
    }


isLoggedIn : Model -> Bool
isLoggedIn model =
    model.state == LoggedIn


isLoginPending : Model -> Bool
isLoginPending model =
    model.state == Unknown



-- UPDATE --


type Msg
    = UsernameChanged String
    | PasswordChanged String
    | SubmitLogin
    | LoginChecked (Result Http.Error LoginState)
    | Mdl (Material.Msg Msg)


type alias LoginSuccessful =
    Bool


update : Msg -> Model -> ( Model, Cmd Msg, LoginSuccessful )
update msg model =
    case msg of
        UsernameChanged username ->
            ( { model | username = username }, Cmd.none, False )

        PasswordChanged password ->
            ( { model | password = password }, Cmd.none, False )

        SubmitLogin ->
            let
                { username, password } =
                    model
            in
                ( model, logIn username password, False )

        LoginChecked (Ok newLoginState) ->
            ( { model | state = newLoginState, username = "", password = "" }, Cmd.none, True )

        LoginChecked (Err error) ->
            ( { model | state = NotLoggedIn }, Cmd.none, False )

        Mdl msg_ ->
            let
                ( newModel, msg ) =
                    Material.update Mdl msg_ model
            in
                ( newModel, msg, False )


checkLoggedIn : Cmd Msg
checkLoggedIn =
    let
        url =
            "/my_zwave/login/show"

        request =
            Http.get url decodeLogin
    in
        Http.send LoginChecked request


logIn : String -> String -> Cmd Msg
logIn username password =
    let
        url =
            "/my_zwave/login/create"

        requestData =
            [ ( "username", Json.Encode.string username )
            , ( "password", Json.Encode.string password )
            ]
                |> Json.Encode.object
                |> Http.jsonBody

        request =
            Http.post url requestData decodeLogin
    in
        Http.send LoginChecked request


decodeLogin : Decoder LoginState
decodeLogin =
    let
        convert result =
            let
                resultingState =
                    if result then
                        LoggedIn
                    else
                        NotLoggedIn
            in
                Json.Decode.succeed resultingState
    in
        (field "loggedIn" bool)
            |> Json.Decode.andThen convert



-- VIEW --


view : Model -> Html Msg
view model =
    Card.view []
        [ Card.title []
            [ Options.styled p [ Typo.title ] [ text "Please login" ]
            ]
        , Card.text []
            [ div []
                [ Textfield.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Textfield.label "Username"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Options.onInput UsernameChanged
                    ]
                    []
                , Textfield.render Mdl
                    [ 1 ]
                    model.mdl
                    [ Textfield.label "Password"
                    , Textfield.floatingLabel
                    , Textfield.password
                    , Options.onInput PasswordChanged
                    ]
                    []
                , div []
                    [ Button.render Mdl
                        [ 2 ]
                        model.mdl
                        [ Button.raised
                        , Options.onClick SubmitLogin
                        ]
                        [ text "Log in" ]
                    ]
                ]
            ]
        ]
