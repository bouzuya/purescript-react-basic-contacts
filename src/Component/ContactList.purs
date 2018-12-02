module Component.ContactList
  ( contactList
  ) where

import Prelude

import React.Basic (Component, JSX, StateUpdate(..), createComponent, make)
import React.Basic.DOM as H

type Contact =
  { name :: String
  , address :: String
  , tel :: String
  }

type Props =
  {}

data Action
  = Noop

component :: Component Props
component = createComponent "ContactList"

contactList :: JSX
contactList = make component { initialState, render, update } {}
  where
    initialState =
      { contactList:
        [ { name: "JR 三ノ宮駅"
          , address: "神戸市中央区布引町四丁目1-1"
          , tel: "999-999-9999"
          }
        , { name: "阪急 神戸三宮駅"
          , address: "神戸市中央区加納町四丁目2番1号"
          , tel: "999-999-9999"
          }
        ]
      }

    render self@{ state: { contactList: list }} =
      H.div_
      [ H.h1_
        [ H.text "Contacts" ]
      , H.ul_
        ( list <#>
          \contact ->
            H.li_
              [ H.div_
                [ H.div_ [ H.text contact.name ]
                , H.div_ [ H.text contact.address ]
                , H.div_ [ H.text contact.tel ]
                ]
              ]
        )
      ]

    update { state } = case _ of
      Noop -> Update state
