module Aftok.PasswordReset where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
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
  ( PasswordResetRequestBody(..)
  , PasswordResetRequestResponse(..)
  , requestPasswordReset
  )

data ResetError
  = RequestError String

data ResetState
  = FormState { email :: String, error :: Maybe ResetError }
  | SuccessState

type State = ResetState

data Action
  = SetEmail String
  | SubmitRequest WE.Event

data Output = BackToLogin

type Slot id = forall query. H.Slot query Output id

type Capability m =
  { requestPasswordReset :: PasswordResetRequestBody -> m PasswordResetRequestResponse
  }

component
  :: forall query input m
   . Monad m
  => System m
  -> Capability m
  -> H.Component query input Output m
component system caps =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = eval }
    }
  where
  initialState :: input -> State
  initialState _ = FormState { email: "", error: Nothing }

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
                  [ case st of
                      FormState formSt -> renderForm formSt
                      SuccessState -> renderSuccess
                  ]
              , HH.p
                  [ P.classes (ClassName <$> [ "mb-0", "font-size-sm", "text-center", "text-muted" ]) ]
                  [ HH.text "Remember your password? "
                  , HH.a
                      [ P.href "#login" ]
                      [ HH.text "Sign in" ]
                  ]
              ]
          ]

  renderForm :: forall slots. { email :: String, error :: Maybe ResetError } -> H.ComponentHTML Action slots m
  renderForm formSt =
    HH.div_
      [ HH.h2
          [ P.classes (ClassName <$> [ "mb-0", "font-weight-bold", "text-center" ]) ]
          [ HH.text "Reset Password" ]
      , HH.p
          [ P.classes (ClassName <$> [ "text-center", "text-muted", "mt-3" ]) ]
          [ HH.text "Enter your email address and we'll send you a link to reset your password." ]
      , HH.form
          [ P.classes (ClassName <$> [ "mb-6" ])
          , E.onSubmit SubmitRequest
          ]
          [ HH.div
              [ P.classes (ClassName <$> [ "form-group" ]) ]
              [ HH.label
                  [ P.classes (ClassName <$> [ "sr-only" ])
                  , P.for "passwordResetEmail"
                  ]
                  [ HH.text "Email" ]
              , HH.input
                  [ P.type_ P.InputEmail
                  , P.classes (ClassName <$> [ "form-control" ])
                  , P.id "passwordResetEmail"
                  , P.placeholder "Email address"
                  , P.required true
                  , P.autofocus true
                  , P.value formSt.email
                  , E.onValueInput SetEmail
                  ]
              ]
          , case formSt.error of
              Nothing -> HH.div_ []
              Just (RequestError msg) ->
                HH.div
                  [ P.classes (ClassName <$> [ "alert", "alert-danger" ]) ]
                  [ HH.text msg ]
          , HH.button
              [ P.classes (ClassName <$> [ "btn", "btn-block", "btn-primary" ]) ]
              [ HH.text "Send Reset Link" ]
          ]
      ]

  renderSuccess :: forall slots. H.ComponentHTML Action slots m
  renderSuccess =
    HH.div_
      [ HH.h2
          [ P.classes (ClassName <$> [ "mb-0", "font-weight-bold", "text-center" ]) ]
          [ HH.text "Check Your Email" ]
      , HH.div
          [ P.classes (ClassName <$> [ "text-center", "mt-4" ]) ]
          [ HH.p
              [ P.classes (ClassName <$> [ "text-muted" ]) ]
              [ HH.text "If an account with that email exists, we've sent you a password reset link." ]
          , HH.p
              [ P.classes (ClassName <$> [ "text-muted" ]) ]
              [ HH.text "Please check your inbox and follow the instructions to reset your password." ]
          , HH.p
              [ P.classes (ClassName <$> [ "text-muted", "small", "mt-4" ]) ]
              [ HH.text "Didn't receive the email? Check your spam folder or try again." ]
          ]
      ]

  eval :: Action -> H.HalogenM State Action () Output m Unit
  eval = case _ of
    SetEmail email ->
      H.modify_ \st -> case st of
        FormState formSt -> FormState (formSt { email = email })
        SuccessState -> st
    SubmitRequest ev -> do
      lift $ system.preventDefault ev
      st <- H.get
      case st of
        FormState formSt -> do
          response <- lift (caps.requestPasswordReset (ResetByEmail formSt.email))
          case response of
            ResetRequestSent -> H.put SuccessState
            ResetRequestError { message } ->
              H.put $ FormState (formSt { error = Just (RequestError message) })
        SuccessState -> pure unit

apiCapability :: Capability Aff
apiCapability = { requestPasswordReset }

mockCapability :: forall m. Applicative m => Capability m
mockCapability =
  { requestPasswordReset: \_ -> pure ResetRequestSent
  }
