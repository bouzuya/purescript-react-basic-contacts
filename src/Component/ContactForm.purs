module Component.ContactForm
  ( contactForm
  ) where

import Data.Maybe (fromMaybe)
import Effect (Effect)
import Prelude (Unit, const)
import React.Basic (Component, JSX, StateUpdate(..), capture, createComponent, make)
import React.Basic.DOM as H
import React.Basic.DOM.Events (preventDefault, targetValue)

type Contact =
  { name :: String
  , address :: String
  , tel :: String
  }

type Props =
  { onAdd :: Contact -> Effect Unit }

data Action
  = UpdateName String
  | UpdateAddress String
  | UpdateTel String
  | AddContact

component :: Component Props
component = createComponent "ContactList"

contactForm :: Props -> JSX
contactForm = make component { initialState, render, update }
  where
    initialContact =
      { name: ""
      , address: ""
      , tel: ""
      }
    initialState =
      { contactForm: initialContact
      }

    render self@{ props: { onAdd }, state: { contactForm: form } } =
      H.div_
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

    update { props: { onAdd }, state } = case _ of
      UpdateName v ->
        Update state { contactForm = state.contactForm { name = v } }
      UpdateAddress v ->
        Update state { contactForm = state.contactForm { address = v } }
      UpdateTel v ->
        Update state { contactForm = state.contactForm { tel = v } }
      AddContact ->
        SideEffects (const (onAdd state.contactForm))
