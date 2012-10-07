{-# LANGUAGE OverloadedStrings #-}

module Views where

import Prelude hiding (head, id, div, span)
import qualified Prelude as P
import Data.Text (Text)
--import qualified Data.Text as T
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Internal (Attributable)
import qualified Text.Blaze.Html5.Attributes as A
--import Data.Monoid ((<>))
import Control.Monad

maybeWhen :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
maybeWhen maybeA f =
  case maybeA of
    Nothing -> return ()
    Just a' -> f a'

stylesheetLink :: Text -> Html
stylesheetLink href = link ! A.rel "stylesheet" ! (A.href $ toValue href)

empty :: Html
empty = return ()

(!.) :: Attributable h => h -> AttributeValue -> h
tag !. val = tag ! A.class_ val
(!#) :: Attributable h => h -> AttributeValue -> h
tag !# val = tag ! A.id val
(!+) :: Attributable h => h -> AttributeValue -> h
tag !+ val = tag ! A.type_ val
(!=) :: Attributable h => h -> AttributeValue -> h
tag != val = tag ! customAttribute "data-bind" val

baseTemplate :: Text -> Maybe Html -> Html -> Html
baseTemplate titleText maybeHeadHtml contentHtml = docTypeHtml $ do
  head $ do
    title $ toHtml titleText
    stylesheetLink "/static/css/bootstrap.min.css"
    stylesheetLink "/static/css/darkstrap.css"
    stylesheetLink "/static/font/fontface.css"
    stylesheetLink "/static/css/setladder.css"
    script ! A.src "/static/js/json2.js" $ empty
    script ! A.src "/static/js/jquery-1.8.1.min.js" $ empty
    script ! A.src "/static/js/bootstrap.min.js" $ empty
    script ! A.src "/static/js/knockout-2.1.0.js" $ empty
    maybeWhen maybeHeadHtml (>> return ())
  body $ contentHtml

pageTemplate :: Text -> Maybe Html -> Text -> Html -> Html
pageTemplate titleText maybeHeadHtml username contentHtml =
  baseTemplate titleText maybeHeadHtml $ div ! A.class_ "container" $ do
    div !. "row header" $ do
      div !. "span9" $ do
        div !. "headerLogo smallCorners dropShadow pull-left" $ empty
        h1 !. "logoText" $ "the Stanford Set Ladder"
      when (username /= "") $ do
        div !. "span3 alignRight userBox" $ do
          p $ do
            "Logged in as: "
            strong $ toHtml username
          div !. "btn-group" $ do
            a !. "btn btn-small" $ "Edit profile"
            a !. "btn btn-small" ! A.href "/logout" $ "Log out"
    div !. "row" $
      div !. "span12" $ contentHtml

index :: Text -> Html
index username = pageTemplate "Set Ladder" Nothing username $ do
  div !. "row" $
    div !. "span12" $ 
    p "Hello!"

closeButton :: Html
closeButton =
  button !. "close" !+ "button" ! customAttribute "data-dismiss" "alert" $
  preEscapedToHtml ("&times;" :: Text)

login :: Html
login = loginMessage ""

loginMessage :: Text -> Html
loginMessage m = baseTemplate "Login to Set Ladder" Nothing $ div !. "container" $ do
  div !. "row" $ div !. "span12" $ do
    div !. "jumboLogo largeCorners dropShadow" $ empty
    h1 !# "JumboHeader" !. "alignCenter" $ "the Stanford Set Ladder"
  div !. "row" $ div !. "span6 offset3" !# "LoginControls" $ do
    when (m == "loginfailed") $ do
      div !. "alert alert-error" $ do
        closeButton
        strong "Login failed! " >> "Incorrect username or password."
    when (m == "registersuccess") $ do
      div !. "alert alert-success" $ do
        closeButton
        strong "Registration successful! " >> "You should now be able to log in."
    form !. "form-horizontal" ! A.action "/auth" ! A.method "POST" $ do
      div !. "control-group" $ do
        label !. "control-label" ! A.for "InputUsername" $ "Username"
        div !. "controls" $ do
          input !+ "text" !# "InputUsername" ! A.name "username"
            ! A.placeholder "Username" ! A.autofocus "autofocus"
      div !. "control-group" $ do
        label !. "control-label" ! A.for "InputPassword" $ "Password"
        div !. "controls" $ do
          input !+ "password" !# "InputPassword" ! A.name "password"
            ! A.placeholder "Password"
      div !. "control-group" $ do
        div !. "controls" $ do
          button !+ "submit" !. "btn" $ "Sign in"
      div !. "control-group" $ do
        div !. "controls" $ do
          small $ "Don't have an account? "
          a !. "btn btn-small btn-inverse" ! A.href "/register" $ "Register"

register :: Html
register = registerMessage ""

registerMessage :: Text -> Html
registerMessage m = pageTemplate "Register for Set Ladder" scripts "" $ do
  div !. "row" $ do
    div !. "span12" $ do
      div !. "page-header" $ do
        h2 "Register for the Set Ladder"
  div !. "row" $ do
    div !. "span9" $ do
      when (m == "invaliddata") $ do
        div !. "alert alert-error" $ do
          closeButton
          strong "Registration failed! " >> "Some data was invalid."
      form !. "form-horizontal" ! A.action "/doregister"
        ! A.method "POST" $ do
        div !. "control-group" != "css: {error: usernameError()}" $ do
          label !. "control-label" ! A.for "InputUsername" $ "Username"
          div !. "controls" $ do
            input !+ "text" !# "InputUsername"
              != "value: enteredUsername"
              ! A.name "username" ! A.placeholder "Ex: lennartj"
            " "
            span !. "help-inline" != "text: displayUsernameState" $ empty
        div !. "control-group" $ do
          label !. "control-label" ! A.for "InputPassword1" $ "Password"
          div !. "controls" $ do
            input !+ "password" !# "InputPassword1"
              != "value: password1"
              ! A.name "password1" ! A.placeholder "Your password"
        div !. "control-group" != "css: {error: passwordsDontMatch()}" $ do
          label !. "control-label" ! A.for "InputPassword2" $
            "Password again"
          div !. "controls" $ do
            input !+ "password" !# "InputPassword2"
              != "value: password2"
              ! A.name "password2" ! A.placeholder "Your password again"
            span !. "help-inline" != "text: passwordsMatchText" $ empty
        div !. "control-group" != "css: {error: emailIsInvalid()}" $ do
          label !. "control-label" ! A.for "InputEmail" $ "Email"
          div !. "controls" $ do
            input !+ "text" !# "InputEmail"
              != "value: enteredEmail"
              ! A.name "email" ! A.placeholder "Ex: lennartj@stanford.edu"
            span !. "help-inline" != "text: displayEmailState" $ empty
            span !. "help-block" $
              "Please enter a valid email address for an account you have access to so we can send you a replacement if you forget your password. A stanford.edu domain email account is not required."
        div !. "control-group" $ do
          label !. "control-label" ! A.for "InputRealName" $ "Real name"
          div !. "controls" $ do
            input !+ "text" !# "InputRealName"
              ! A.name "realname" ! A.placeholder "Ex: Lennart Jansson"
            span !. "help-block" $ "Optional, you can change this later from your user profile."
        div !. "control-group" $ do
          label !. "control-label" ! A.for "InputLocation" $ "Location"
          div !. "controls" $ do
            input !+ "text" !# "InputLocation"
              ! A.name "location" ! A.placeholder "Ex: Stanford, CA"
            span !. "help-block" $ "Optional, you can change this later from your user profile."
        div !. "control-group" $ do
          div !. "controls" $ do
            input !+ "submit" !. "btn" != "enable: submitButtonState(), buttonEnable: submitButtonState()" ! A.value "Register"
    div !. "span3" $ do
      p !. "lead" $ "Registration is free and easy and we probably won't misuse your personal information!"
  where scripts = Just $ do
          script ! A.src "/static/js/register.js" $ empty