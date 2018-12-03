module Component.ContactList
  ( contactList
  ) where

import Component.ContactForm as ContactForm
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude (const, join, map, (<#>), (<>))
import React.Basic (Component, JSX, StateUpdate(..), createComponent, make, send)
import React.Basic.DOM as H

type Contact =
  { name :: String
  , address :: String
  , tel :: String
  }

type Props =
  {}

data Action
  = UpdateContact Contact

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
      , edit: Nothing
      }

    render self@{ state: { contactForm: form, contactList: list, edit } } =
      H.div_
      [ H.h1_
        [ H.text "Contacts" ]
      , ContactForm.contactForm
        { edit: join (map (Array.index list) edit)
        , onSubmit: \c -> send self (UpdateContact c)
        }
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
      UpdateContact contact ->
        Update
          state
            { contactList = case state.edit of
                Nothing -> state.contactList <> [contact]
                Just index ->
                  fromMaybe
                    state.contactList -- TODO: error
                    (Array.modifyAt index (const contact) state.contactList)
            }
