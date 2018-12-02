module Component.ContactList
  ( contactList
  ) where

import Data.Maybe (fromMaybe)
import Prelude ((<#>), (<>))
import React.Basic (Component, JSX, StateUpdate(..), capture, createComponent, make)
import React.Basic.DOM as H
import React.Basic.DOM.Events (preventDefault, targetValue)

type Contact =
  { name :: String
  , address :: String
  , tel :: String
  }

type Props =
  {}

data Action
  = UpdateName String
  | UpdateAddress String
  | UpdateTel String
  | AddContact

component :: Component Props
component = createComponent "ContactList"

contactList :: JSX
contactList = make component { initialState, render, update } {}
  where
    initialContact =
      { name: ""
      , address: ""
      , tel: ""
      }
    initialState =
      { contactForm: initialContact
      , contactList:
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

    render self@{ state: { contactForm: form, contactList: list } } =
      H.div_
      [ H.h1_
        [ H.text "Contacts" ]
      , H.div_
        [ H.label_
          [ H.span_ [ H.text "name" ]
          , H.input
            { onChange:
                capture self targetValue (\v -> UpdateName (fromMaybe "" v))
            , value: form.name
            }
          ]
        , H.br {}
        , H.label_
          [ H.span_ [ H.text "address" ]
          , H.input
            { onChange:
                capture self targetValue (\v -> UpdateAddress (fromMaybe "" v))
            , value: form.address
            }
          ]
        , H.br {}
        , H.label_
          [ H.span_ [ H.text "tel" ]
          , H.input
            { onChange:
                capture self targetValue (\v -> UpdateTel (fromMaybe "" v))
            , value: form.tel
            }
          ]
        , H.br {}
        , H.button
          { onClick:
              capture
                self
                preventDefault
                (\_ -> AddContact)
          , children: [ H.text "Add" ]
          }
        ]
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
      UpdateName v ->
        Update state { contactForm = state.contactForm { name = v } }
      UpdateAddress v ->
        Update state { contactForm = state.contactForm { address = v } }
      UpdateTel v ->
        Update state { contactForm = state.contactForm { tel = v } }
      AddContact ->
        Update
          state
            { contactForm = initialContact
            , contactList = state.contactList <> [state.contactForm]
            }
