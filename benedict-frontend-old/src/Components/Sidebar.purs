module Components.Sidebar where

import Prelude (class Functor, ($), map, (<<<), (<>), pure, bind)
import Data.String as Str

import Halogen (ComponentDSL, Natural, ComponentHTML, Component, component, modify)
import Halogen.HTML.Core (className)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E

data SidebarState
    = Workspace
    | Profile

initState :: SidebarState
initState = Workspace

data SidebarQuery a = Select SidebarState a

sidebar :: forall g. (Functor g) => Component SidebarState SidebarQuery g
sidebar = component render eval
  where
    render :: SidebarState -> ComponentHTML SidebarQuery
    render state =
        let
          workspaceActive = case state of
              Workspace -> true
              _ -> false
          profileActive = case state of
              Profile -> true
              _ -> false
        in
          H.div
              [ P.classes $ sidebarClasses "ui top demo horizontal inverted sidebar labeled icon menu visible" ]
              [ H.a
                  [ P.classes $ itemClasses workspaceActive
                  , E.onClick $ E.input_ $ Select Workspace
                  ]
                  [ H.text "Workspace" ]
              , H.a
                  [ P.classes $ itemClasses profileActive
                  , E.onClick $ E.input_ $ Select Profile
                  ]
                  [ H.text "Profile" ]
              ]

    eval :: Natural SidebarQuery (ComponentDSL SidebarState SidebarQuery g)
    eval (Select newState next) = do
        modify (\state -> newState)
        pure next

    sidebarClasses = map className <<< Str.split " "

    itemClasses active =
        let
          finalClass = "item" <> if active then " active" else ""
        in
          map className <<< Str.split " " $ finalClass
