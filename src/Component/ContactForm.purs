module Component.ContactForm
  ( contactForm
  ) where

import Data.Maybe (fromMaybe)
import Effect (Effect)
import Prelude (Unit)
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM as H
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler)

type Contact =
  { name :: String
  , address :: String
  , tel :: String
  }

type Props =
  { contact :: Contact
  , onSubmit :: Contact -> Effect Unit
  , onUpdate :: Contact -> Effect Unit
  }

component :: Component Props
component = createComponent "ContactForm"

contactForm :: Props -> JSX
contactForm =
  makeStateless
    component
    \{ contact, onSubmit, onUpdate } ->
      H.div_
      [ H.label_
        [ H.span_ [ H.text "name" ]
        , H.input
          { onChange:
              handler
                targetValue
                (\v -> onUpdate contact { name = (fromMaybe "" v) })
          , value: contact.name
          }
        ]
      , H.br {}
      , H.label_
        [ H.span_ [ H.text "address" ]
        , H.input
          { onChange:
              handler
                targetValue
                (\v -> onUpdate contact { address = (fromMaybe "" v) })
          , value: contact.address
          }
        ]
      , H.br {}
      , H.label_
        [ H.span_ [ H.text "tel" ]
        , H.input
          { onChange:
              handler
                targetValue
                (\v -> onUpdate contact { tel = (fromMaybe "" v) })
          , value: contact.tel
          }
        ]
      , H.br {}
      , H.button
        { onClick:
            handler
              preventDefault
              (\_ -> onSubmit contact)
        , children: [ H.text "OK" ]
        }
      ]
