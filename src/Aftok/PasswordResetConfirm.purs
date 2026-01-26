module Aftok.PasswordResetConfirm where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.String (length) as String
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Web.Event.Event as WE
import Halogen.HTML.Properties as P
import CSS (backgroundImage, url)
import Landkit.Card as Card
import Aftok.Types (System)
import Aftok.Api.Account
  ( PasswordResetConfirmResponse(..)
  , confirmPasswordReset
  )

data ResetError
  = InvalidToken
  | PasswordMismatch
  | PasswordTooShort
  | ServiceError String

data ResetState
  = FormState
      { password :: String
      , confirmPassword :: String
      , error :: Maybe ResetError
      }
  | SuccessState

type State =
  { token :: String
  , resetState :: ResetState
  }

data Action
  = SetPassword String
  | SetConfirmPassword String
  | SubmitReset WE.Event

data Output = ResetComplete

type Slot id = forall query. H.Slot query Output id

type Capability m =
  { confirmPasswordReset :: String -> String -> m PasswordResetConfirmResponse
  }

type Input = { token :: String }

component
  :: forall query m
   . Monad m
  => System m
  -> Capability m
  -> H.Component query Input Output m
component system caps =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = eval }
    }
  where
  initialState :: Input -> State
  initialState input =
    { token: input.token
    , resetState: FormState { password: "", confirmPassword: "", error: Nothing }
    }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render st =
    Card.component
      $ HH.div
          [ P.classes (ClassName <$> [ "row", "no-gutters", "container" ]) ]
          [ HH.div
              [ P.classes (ClassName <$> [ "col-12", "col-md-6", "bg-cover", "card-img-left" ])
              , CSS.style $ backgroundImage (url "/assets/img/photos/latch.jpg")
              ]
              [ HH.div
                  [ P.classes (ClassName <$> [ "shape", "shape-right", "shape-fluid-y", "svg-shim", "text-white", "d-none", "d-md-block" ]) ]
                  [ HH.img [ P.src "/assets/img/shapes/curves/curve-4.svg" ] ]
              ]
          , HH.div
              [ P.classes (ClassName <$> [ "col-12", "col-md-6" ]) ]
              [ HH.div
                  [ P.classes (ClassName <$> [ "card-body" ]) ]
                  [ case st.resetState of
                      FormState formSt -> renderForm formSt
                      SuccessState -> renderSuccess
                  ]
              ]
          ]

  renderForm :: forall slots. { password :: String, confirmPassword :: String, error :: Maybe ResetError } -> H.ComponentHTML Action slots m
  renderForm formSt =
    HH.div_
      [ HH.h2
          [ P.classes (ClassName <$> [ "mb-0", "font-weight-bold", "text-center" ]) ]
          [ HH.text "Set New Password" ]
      , HH.p
          [ P.classes (ClassName <$> [ "text-center", "text-muted", "mt-3" ]) ]
          [ HH.text "Please enter your new password below." ]
      , HH.form
          [ P.classes (ClassName <$> [ "mb-6" ])
          , E.onSubmit SubmitReset
          ]
          [ HH.div
              [ P.classes (ClassName <$> [ "form-group" ]) ]
              [ HH.label
                  [ P.for "newPassword" ]
                  [ HH.text "New Password" ]
              , HH.input
                  [ P.type_ P.InputPassword
                  , P.classes (ClassName <$> [ "form-control" ])
                  , P.id "newPassword"
                  , P.placeholder "Enter new password"
                  , P.required true
                  , P.autofocus true
                  , P.value formSt.password
                  , E.onValueInput SetPassword
                  ]
              ]
          , HH.div
              [ P.classes (ClassName <$> [ "form-group" ]) ]
              [ HH.label
                  [ P.for "confirmPassword" ]
                  [ HH.text "Confirm Password" ]
              , HH.input
                  [ P.type_ P.InputPassword
                  , P.classes (ClassName <$> [ "form-control" ])
                  , P.id "confirmPassword"
                  , P.placeholder "Confirm new password"
                  , P.required true
                  , P.value formSt.confirmPassword
                  , E.onValueInput SetConfirmPassword
                  ]
              ]
          , case formSt.error of
              Nothing -> HH.div_ []
              Just err ->
                let
                  message = case err of
                    InvalidToken -> "This password reset link is invalid or has expired. Please request a new one."
                    PasswordMismatch -> "Passwords do not match."
                    PasswordTooShort -> "Password must be at least 8 characters long."
                    ServiceError msg -> "An error occurred: " <> msg
                in
                  HH.div
                    [ P.classes (ClassName <$> [ "alert", "alert-danger" ]) ]
                    [ HH.text message ]
          , HH.button
              [ P.classes (ClassName <$> [ "btn", "btn-block", "btn-primary" ]) ]
              [ HH.text "Reset Password" ]
          ]
      ]

  renderSuccess :: forall slots. H.ComponentHTML Action slots m
  renderSuccess =
    HH.div_
      [ HH.h2
          [ P.classes (ClassName <$> [ "mb-0", "font-weight-bold", "text-center" ]) ]
          [ HH.text "Password Reset Complete" ]
      , HH.div
          [ P.classes (ClassName <$> [ "text-center", "mt-4" ]) ]
          [ HH.p
              [ P.classes (ClassName <$> [ "text-muted" ]) ]
              [ HH.text "Your password has been successfully reset." ]
          , HH.a
              [ P.href "#login"
              , P.classes (ClassName <$> [ "btn", "btn-primary", "mt-3" ])
              ]
              [ HH.text "Sign In" ]
          ]
      ]

  eval :: Action -> H.HalogenM State Action () Output m Unit
  eval = case _ of
    SetPassword pwd ->
      H.modify_ \st -> case st.resetState of
        FormState formSt -> st { resetState = FormState (formSt { password = pwd }) }
        SuccessState -> st
    SetConfirmPassword pwd ->
      H.modify_ \st -> case st.resetState of
        FormState formSt -> st { resetState = FormState (formSt { confirmPassword = pwd }) }
        SuccessState -> st
    SubmitReset ev -> do
      lift $ system.preventDefault ev
      st <- H.get
      case st.resetState of
        FormState formSt -> do
          -- Validate passwords match
          if formSt.password /= formSt.confirmPassword then
            H.modify_ \s -> s { resetState = FormState (formSt { error = Just PasswordMismatch }) }
          else if String.length formSt.password < 8 then
            H.modify_ \s -> s { resetState = FormState (formSt { error = Just PasswordTooShort }) }
          else do
            response <- lift (caps.confirmPasswordReset st.token formSt.password)
            case response of
              ResetConfirmOK -> do
                H.modify_ \s -> s { resetState = SuccessState }
                H.raise ResetComplete
              ResetConfirmInvalidToken ->
                H.modify_ \s -> s { resetState = FormState (formSt { error = Just InvalidToken }) }
              ResetConfirmError { message } ->
                H.modify_ \s -> s { resetState = FormState (formSt { error = Just (ServiceError message) }) }
        SuccessState -> pure unit

apiCapability :: Capability Aff
apiCapability = { confirmPasswordReset }

mockCapability :: forall m. Applicative m => Capability m
mockCapability =
  { confirmPasswordReset: \_ _ -> pure ResetConfirmOK
  }
