module Aftok.Settings where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as P
import Aftok.GitHub.Settings as GitHubSettings
import Aftok.Types (System)

type SettingsState = {}

data SettingsAction = Initialize

type Slot id = forall query output. H.Slot query output id

type Slots =
  ( gitHubSettings :: GitHubSettings.Slot Unit
  )

_gitHubSettings = Proxy :: Proxy "gitHubSettings"

type Capability (m :: Type -> Type) =
  { gitHubSettingsCaps :: GitHubSettings.Capability m
  }

component
  :: forall query input output m
   . Monad m
  => System m
  -> Capability m
  -> H.Component query input output m
component system caps =
  H.mkComponent
    { initialState: const {}
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { initialize = Just Initialize
              }
    }
  where
  render :: SettingsState -> H.ComponentHTML SettingsAction Slots m
  render _ =
    HH.section
      [ P.classes (ClassName <$> [ "section-border", "border-primary" ]) ]
      [ HH.div
          [ P.classes (ClassName <$> [ "container", "pt-6" ]) ]
          [ HH.h1
              [ P.classes (ClassName <$> [ "mb-0", "font-weight-bold", "text-center" ]) ]
              [ HH.text "Account Settings" ]
          , HH.p
              [ P.classes (ClassName <$> [ "text-muted", "text-center", "mx-auto", "mb-4" ]) ]
              [ HH.text "Manage your account preferences and integrations" ]
          , HH.div
              [ P.classes (ClassName <$> [ "row", "justify-content-center" ]) ]
              [ HH.div
                  [ P.classes (ClassName <$> [ "col-md-8" ]) ]
                  [ HH.slot _gitHubSettings unit (GitHubSettings.component system caps.gitHubSettingsCaps) unit absurd
                  ]
              ]
          ]
      ]

apiCapability :: Capability Aff
apiCapability =
  { gitHubSettingsCaps: GitHubSettings.apiCapability
  }
