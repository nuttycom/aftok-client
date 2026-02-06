module Aftok.GitHub.Settings where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Aftok.Api.Types (APIError)
import Aftok.Api.GitHub as GitHub
import Aftok.HTML.Classes as C
import Aftok.Types (System)

type CState =
  { username :: Maybe String
  , inputUsername :: String
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | SetInputUsername String
  | LinkUsername
  | UnlinkUsername

type Slot id = forall query. H.Slot query Void id

type Capability (m :: Type -> Type) =
  { getGitHubUsername :: m (Either APIError GitHub.GitHubUsername)
  , linkGitHubUsername :: String -> m (Either APIError Unit)
  , unlinkGitHubUsername :: m (Either APIError Unit)
  }

component
  :: forall query input m
   . Monad m
  => System m
  -> Capability m
  -> H.Component query input Void m
component _system caps =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }
  where
  initialState :: CState
  initialState =
    { username: Nothing
    , inputUsername: ""
    , loading: false
    , error: Nothing
    }

  render :: forall slots. CState -> H.ComponentHTML Action slots m
  render st =
    HH.div
      [ P.classes (ClassName <$> [ "card", "mb-4" ]) ]
      [ HH.div
          [ P.classes (ClassName <$> [ "card-header" ]) ]
          [ HH.h5
              [ P.classes (ClassName <$> [ "mb-0" ]) ]
              [ HH.text "GitHub Integration" ]
          ]
      , HH.div
          [ P.classes (ClassName <$> [ "card-body" ]) ]
          [ case st.error of
              Just err ->
                HH.div
                  [ P.classes (ClassName <$> [ "alert", "alert-danger" ]) ]
                  [ HH.text err ]
              Nothing -> HH.text ""
          , case st.username of
              Just uname ->
                linkedView uname st.loading
              Nothing ->
                unlinkeedView st.inputUsername st.loading
          ]
      ]

  linkedView :: forall slots. String -> Boolean -> H.ComponentHTML Action slots m
  linkedView uname loading =
    HH.div_
      [ HH.p_
          [ HH.text "Your account is linked to GitHub username: "
          , HH.strong_ [ HH.text uname ]
          ]
      , HH.p
          [ P.classes (ClassName <$> [ "text-muted", "small" ]) ]
          [ HH.text "This enables automatic time tracking when you open or update pull requests on linked repositories." ]
      , HH.button
          [ P.classes [ C.btn, C.btnSecondary, C.btnSmall ]
          , P.type_ ButtonButton
          , P.disabled loading
          , E.onClick \_ -> UnlinkUsername
          ]
          [ HH.text (if loading then "Unlinking..." else "Unlink GitHub") ]
      ]

  unlinkeedView :: forall slots. String -> Boolean -> H.ComponentHTML Action slots m
  unlinkeedView inputValue loading =
    HH.div_
      [ HH.p_
          [ HH.text "Link your GitHub username to enable automatic time tracking from pull requests." ]
      , HH.div
          [ P.classes (ClassName <$> [ "form-row", "align-items-center" ]) ]
          [ HH.div
              [ P.classes (ClassName <$> [ "col-auto" ]) ]
              [ HH.input
                  [ P.type_ P.InputText
                  , P.classes [ C.formControl, C.formControlSm ]
                  , P.placeholder "GitHub username"
                  , P.value inputValue
                  , P.disabled loading
                  , E.onValueInput SetInputUsername
                  ]
              ]
          , HH.div
              [ P.classes (ClassName <$> [ "col-auto" ]) ]
              [ HH.button
                  [ P.classes [ C.btn, C.btnPrimary, C.btnSmall ]
                  , P.type_ ButtonButton
                  , P.disabled (loading || inputValue == "")
                  , E.onClick \_ -> LinkUsername
                  ]
                  [ HH.text (if loading then "Linking..." else "Link GitHub") ]
              ]
          ]
      ]

  handleAction :: forall slots. Action -> H.HalogenM CState Action slots Void m Unit
  handleAction = case _ of
    Initialize -> do
      H.modify_ (_ { loading = true, error = Nothing })
      result <- lift $ caps.getGitHubUsername
      case result of
        Left err -> do
          H.modify_ (_ { loading = false, error = Just (show err) })
        Right { username } -> do
          H.modify_ (_ { loading = false, username = username })
    SetInputUsername uname ->
      H.modify_ (_ { inputUsername = uname })
    LinkUsername -> do
      inputUname <- H.gets _.inputUsername
      H.modify_ (_ { loading = true, error = Nothing })
      result <- lift $ caps.linkGitHubUsername inputUname
      case result of
        Left err -> do
          H.modify_ (_ { loading = false, error = Just (show err) })
        Right _ -> do
          H.modify_ (_ { loading = false, username = Just inputUname, inputUsername = "" })
    UnlinkUsername -> do
      H.modify_ (_ { loading = true, error = Nothing })
      result <- lift $ caps.unlinkGitHubUsername
      case result of
        Left err -> do
          H.modify_ (_ { loading = false, error = Just (show err) })
        Right _ -> do
          H.modify_ (_ { loading = false, username = Nothing })

apiCapability :: Capability Aff
apiCapability =
  { getGitHubUsername: GitHub.getGitHubUsername
  , linkGitHubUsername: GitHub.linkGitHubUsername
  , unlinkGitHubUsername: GitHub.unlinkGitHubUsername
  }
