module Component.ContactList
  ( contactList
  ) where

import React.Basic (Component, JSX, StateUpdate(..), createComponent, make)
import React.Basic.DOM as H

type Props =
  {}

data Action
  = Noop

component :: Component Props
component = createComponent "ContactList"

contactList :: JSX
contactList = make component { initialState, render, update } {}
  where
    initialState = {}

    render self =
      H.h1_
      [ H.text "Contacts" ]

    update { state } = case _ of
      Noop -> Update state
