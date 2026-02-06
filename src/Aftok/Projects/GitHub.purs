module Aftok.Projects.GitHub where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties.ARIA as ARIA
import Aftok.Api.Types (APIError)
import Aftok.Api.GitHub as GitHub
import Aftok.HTML.Classes as C
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.Types (System, ProjectId)

data Query a = OpenModal ProjectId a

data Output
  = RepoLinked
  | RepoUnlinked

type CState =
  { projectId :: Maybe ProjectId
  , repos :: Array GitHub.RepoLink
  , newOwner :: String
  , newRepo :: String
  , webhookSecret :: Maybe WebhookSetup
  , loading :: Boolean
  , error :: Maybe String
  }

-- | Information shown after successfully linking a repo
type WebhookSetup =
  { linkId :: String
  , owner :: String
  , repo :: String
  , secret :: String
  }

data Action
  = SetNewOwner String
  | SetNewRepo String
  | LinkRepo
  | UnlinkRepo String
  | Close
  | DismissSecret

type Slot id = forall output. H.Slot Query output id

type Capability (m :: Type -> Type) =
  { getProjectRepoLinks :: ProjectId -> m (Either APIError (Array GitHub.RepoLink))
  , linkProjectRepo :: ProjectId -> String -> String -> m (Either APIError GitHub.LinkRepoResponse)
  , unlinkProjectRepo :: ProjectId -> String -> m (Either APIError Unit)
  }

modalId :: String
modalId = "gitHubReposModal"

component
  :: forall input output m
   . Monad m
  => System m
  -> Capability m
  -> H.Component Query input output m
component system caps =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              }
    }
  where
  initialState :: CState
  initialState =
    { projectId: Nothing
    , repos: []
    , newOwner: ""
    , newRepo: ""
    , webhookSecret: Nothing
    , loading: false
    , error: Nothing
    }

  render :: CState -> H.ComponentHTML Action (()) m
  render st =
    HH.div
      [ P.classes [ C.modal ]
      , P.id modalId
      , P.tabIndex (negate 1)
      , ARIA.role "dialog"
      , ARIA.labelledBy (modalId <> "Title")
      , ARIA.hidden "true"
      ]
      [ HH.div
          [ P.classes [ C.modalDialog, ClassName "modal-lg" ], ARIA.role "document" ]
          [ HH.div
              [ P.classes [ C.modalContent ] ]
              [ HH.div
                  [ P.classes [ C.modalHeader ] ]
                  [ HH.h5 [ P.classes [ C.modalTitle ], P.id (modalId <> "Title") ] [ HH.text "GitHub Integration" ]
                  , HH.button
                      [ P.classes [ C.close ]
                      , ARIA.label "Close"
                      , P.type_ ButtonButton
                      , E.onClick \_ -> Close
                      ]
                      [ HH.span [ ARIA.hidden "true" ] [ HH.text "\x00D7" ] ]
                  ]
              , HH.div
                  [ P.classes [ C.modalBody ] ]
                  [ case st.error of
                      Just err ->
                        HH.div
                          [ P.classes (ClassName <$> [ "alert", "alert-danger" ]) ]
                          [ HH.text err ]
                      Nothing -> HH.text ""
                  , case st.webhookSecret of
                      Just setup -> webhookSetupPanel setup
                      Nothing -> repoManagementPanel st
                  ]
              , HH.div
                  [ P.classes [ C.modalFooter ] ]
                  [ HH.button
                      [ P.type_ ButtonButton
                      , P.classes [ C.btn, C.btnSecondary ]
                      , E.onClick \_ -> Close
                      ]
                      [ HH.text "Close" ]
                  ]
              ]
          ]
      ]

  repoManagementPanel :: CState -> H.ComponentHTML Action (()) m
  repoManagementPanel st =
    HH.div_
      [ HH.h6_ [ HH.text "Linked Repositories" ]
      , if st.repos == [] then
          HH.p
            [ P.classes (ClassName <$> [ "text-muted" ]) ]
            [ HH.text "No repositories linked yet." ]
        else
          HH.ul
            [ P.classes (ClassName <$> [ "list-group", "mb-3" ]) ]
            (repoListItem <$> st.repos)
      , HH.hr_
      , HH.h6_ [ HH.text "Link a New Repository" ]
      , HH.p
          [ P.classes (ClassName <$> [ "text-muted", "small" ]) ]
          [ HH.text "Link a GitHub repository to enable automatic time tracking from pull requests." ]
      , linkRepoForm st.newOwner st.newRepo st.loading
      ]

  repoListItem :: GitHub.RepoLink -> H.ComponentHTML Action (()) m
  repoListItem repo =
    HH.li
      [ P.classes (ClassName <$> [ "list-group-item", "d-flex", "justify-content-between", "align-items-center" ]) ]
      [ HH.span_
          [ HH.text (repo.owner <> "/" <> repo.repo) ]
      , HH.button
          [ P.classes [ C.btn, C.btnSecondary, C.btnSmall ]
          , P.type_ ButtonButton
          , E.onClick \_ -> UnlinkRepo repo.linkId
          ]
          [ HH.text "Unlink" ]
      ]

  linkRepoForm :: String -> String -> Boolean -> H.ComponentHTML Action (()) m
  linkRepoForm owner repo loading =
    HH.div
      [ P.classes (ClassName <$> [ "form-row", "align-items-end" ]) ]
      [ HH.div
          [ P.classes (ClassName <$> [ "col-md-4" ]) ]
          [ HH.label [ P.for "repoOwner" ] [ HH.text "Owner" ]
          , HH.input
              [ P.type_ P.InputText
              , P.classes [ C.formControl, C.formControlSm ]
              , P.id "repoOwner"
              , P.placeholder "owner or organization"
              , P.value owner
              , P.disabled loading
              , E.onValueInput SetNewOwner
              ]
          ]
      , HH.div
          [ P.classes (ClassName <$> [ "col-md-4" ]) ]
          [ HH.label [ P.for "repoName" ] [ HH.text "Repository" ]
          , HH.input
              [ P.type_ P.InputText
              , P.classes [ C.formControl, C.formControlSm ]
              , P.id "repoName"
              , P.placeholder "repository name"
              , P.value repo
              , P.disabled loading
              , E.onValueInput SetNewRepo
              ]
          ]
      , HH.div
          [ P.classes (ClassName <$> [ "col-md-4" ]) ]
          [ HH.button
              [ P.classes [ C.btn, C.btnPrimary, C.btnSmall ]
              , P.type_ ButtonButton
              , P.disabled (loading || owner == "" || repo == "")
              , E.onClick \_ -> LinkRepo
              ]
              [ HH.text (if loading then "Linking..." else "Link Repository") ]
          ]
      ]

  webhookSetupPanel :: WebhookSetup -> H.ComponentHTML Action (()) m
  webhookSetupPanel setup =
    HH.div_
      [ HH.div
          [ P.classes (ClassName <$> [ "alert", "alert-success" ]) ]
          [ HH.text ("Repository " <> setup.owner <> "/" <> setup.repo <> " has been linked!") ]
      , HH.h6_ [ HH.text "Webhook Setup Instructions" ]
      , HH.p_
          [ HH.text "To complete the setup, configure a webhook in your GitHub repository:" ]
      , HH.ol_
          [ HH.li_
              [ HH.text "Go to your repository Settings "
              , HH.span [ P.classes (ClassName <$> [ "text-muted" ]) ] [ HH.text "(Settings > Webhooks > Add webhook)" ]
              ]
          , HH.li_
              [ HH.text "Set "
              , HH.strong_ [ HH.text "Payload URL" ]
              , HH.text " to:"
              , HH.br_
              , HH.code
                  [ P.classes (ClassName <$> [ "d-block", "bg-light", "p-2", "mt-1" ]) ]
                  [ HH.text "https://aftok.com/api/webhooks/github" ]
              ]
          , HH.li_
              [ HH.text "Set "
              , HH.strong_ [ HH.text "Content type" ]
              , HH.text " to: "
              , HH.code_ [ HH.text "application/json" ]
              ]
          , HH.li_
              [ HH.text "Set "
              , HH.strong_ [ HH.text "Secret" ]
              , HH.text " to:"
              , HH.br_
              , HH.code
                  [ P.classes (ClassName <$> [ "d-block", "bg-light", "p-2", "mt-1", "user-select-all" ]) ]
                  [ HH.text setup.secret ]
              , HH.small
                  [ P.classes (ClassName <$> [ "text-muted", "d-block", "mt-1" ]) ]
                  [ HH.text "Copy this secret now - it won't be shown again!" ]
              ]
          , HH.li_
              [ HH.text "Under "
              , HH.strong_ [ HH.text "Which events..." ]
              , HH.text ", select "
              , HH.strong_ [ HH.text "Let me select individual events" ]
              , HH.text " and check only "
              , HH.strong_ [ HH.text "Pull requests" ]
              ]
          , HH.li_
              [ HH.text "Click "
              , HH.strong_ [ HH.text "Add webhook" ]
              ]
          ]
      , HH.div
          [ P.classes (ClassName <$> [ "mt-3" ]) ]
          [ HH.button
              [ P.classes [ C.btn, C.btnPrimary ]
              , P.type_ ButtonButton
              , E.onClick \_ -> DismissSecret
              ]
              [ HH.text "I've saved the secret" ]
          ]
      ]

  handleQuery :: forall a. Query a -> H.HalogenM CState Action (()) output m (Maybe a)
  handleQuery = case _ of
    OpenModal pid a -> do
      H.modify_ (\_ -> initialState { projectId = Just pid, loading = true })
      lift $ system.toggleModal modalId ModalFFI.ShowModal
      -- Load existing repo links
      result <- lift $ caps.getProjectRepoLinks pid
      case result of
        Left err -> do
          H.modify_ (_ { loading = false, error = Just (show err) })
        Right repos -> do
          H.modify_ (_ { loading = false, repos = repos })
      pure (Just a)

  handleAction :: Action -> H.HalogenM CState Action (()) output m Unit
  handleAction = case _ of
    SetNewOwner owner ->
      H.modify_ (_ { newOwner = owner })
    SetNewRepo repo ->
      H.modify_ (_ { newRepo = repo })
    LinkRepo -> do
      st <- H.get
      case st.projectId of
        Nothing -> pure unit
        Just pid -> do
          H.modify_ (_ { loading = true, error = Nothing })
          result <- lift $ caps.linkProjectRepo pid st.newOwner st.newRepo
          case result of
            Left err -> do
              H.modify_ (_ { loading = false, error = Just (show err) })
            Right resp -> do
              let
                newLink =
                  { linkId: resp.linkId
                  , owner: st.newOwner
                  , repo: st.newRepo
                  , createdAt: ""
                  }
                setup =
                  { linkId: resp.linkId
                  , owner: st.newOwner
                  , repo: st.newRepo
                  , secret: resp.webhookSecret
                  }
              H.modify_
                ( _
                    { loading = false
                    , repos = st.repos <> [ newLink ]
                    , newOwner = ""
                    , newRepo = ""
                    , webhookSecret = Just setup
                    }
                )
    UnlinkRepo linkId -> do
      st <- H.get
      case st.projectId of
        Nothing -> pure unit
        Just pid -> do
          H.modify_ (_ { loading = true, error = Nothing })
          result <- lift $ caps.unlinkProjectRepo pid linkId
          case result of
            Left err -> do
              H.modify_ (_ { loading = false, error = Just (show err) })
            Right _ -> do
              H.modify_
                ( \s -> s
                    { loading = false
                    , repos = filter (\r -> r.linkId /= linkId) s.repos
                    }
                )
    DismissSecret ->
      H.modify_ (_ { webhookSecret = Nothing })
    Close -> do
      H.modify_ (const initialState)
      lift $ system.toggleModal modalId ModalFFI.HideModal

apiCapability :: Capability Aff
apiCapability =
  { getProjectRepoLinks: GitHub.getProjectRepoLinks
  , linkProjectRepo: GitHub.linkProjectRepo
  , unlinkProjectRepo: GitHub.unlinkProjectRepo
  }
