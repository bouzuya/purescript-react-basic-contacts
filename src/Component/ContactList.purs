module Component.ContactList
  ( contactList
  ) where

import Component.ContactForm as ContactForm
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude (const, (<>))
import React.Basic (Component, JSX, Self, StateUpdate(..), createComponent, make, send)
import React.Basic.DOM as H
import React.Basic.Events (handler_)

type Contact =
  { name :: String
  , address :: String
  , tel :: String
  }

type Props =
  {}

type State =
  { contactForm :: Contact
  , contactList :: Array Contact
  , selected :: Maybe Int
  }

data Action
  = EditContact Contact
  | UpdateContact Contact
  | SelectContact Int

component :: Component Props
component = createComponent "ContactList"

contactList :: JSX
contactList = make component { initialState, render, update } {}

initialContact :: Contact
initialContact =
  { name: ""
  , address: ""
  , tel: ""
  }

initialState :: State
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
  , selected: Nothing
  }

render :: Self Props State Action -> JSX
render self@{ state: { contactForm: form, contactList: list, selected } } =
  H.div_
  [ H.h1_
    [ H.text "Contacts" ]
  , ContactForm.contactForm
    { contact: form
    , onSubmit: (\c -> send self (UpdateContact c))
    , onUpdate: (\c -> send self (EditContact c))
    }
  , H.ul_
    ( Array.mapWithIndex
        (let
          f :: Int -> Contact -> JSX
          f =
            \index contact -> (
              H.li_
                [ H.div_
                  [ H.div_ [ H.text contact.name ]
                  , H.div_ [ H.text contact.address ]
                  , H.div_ [ H.text contact.tel ]
                  , H.button
                    { onClick: (handler_ (send self (SelectContact index)))
                    , children: [ H.text "Edit" ]
                    }
                  ]
                ]
            )
        in f)
        list
    )
  ]

update :: Self Props State Action -> Action -> StateUpdate Props State Action
update { state } = case _ of
  EditContact contact ->
    Update state { contactForm = contact }
  SelectContact index ->
    Update
      state
      { contactForm = fromMaybe initialContact (Array.index state.contactList index)
      , selected = Just index }
  UpdateContact contact ->
    Update
      state
        { contactForm = initialContact
        , contactList = case state.selected of
            Nothing -> state.contactList <> [contact]
            Just index ->
              fromMaybe
                state.contactList
                (Array.modifyAt index (const contact) state.contactList)
        , selected = Nothing
        }

