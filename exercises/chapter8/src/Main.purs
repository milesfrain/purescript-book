module Main where

import Prelude
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), examplePerson)
import Data.AddressBook.Validation (Errors, validatePerson')
import Data.Array ((..), length, modifyAt, zipWith)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (over)
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, component, element, useState, (/\))
import React.Basic.Hooks as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

formField :: String -> String -> String -> (String -> Effect Unit) -> R.JSX
formField name hint value updateFunc =
  D.div
    { className: "form-group row"
    , children:
        [ D.label
            { className: "col-sm col-form-label"
            , children: [ D.text name ]
            }
        , D.div
            { className: "col-sm"
            , children:
                [ D.input
                    { className: "form-control"
                    , placeholder: hint
                    , value
                    , onChange: handler targetValue $ traverse_ updateFunc
                    }
                ]
            }
        ]
    }

renderValidationError :: String -> R.JSX
renderValidationError err = D.li_ [ D.text err ]

renderValidationErrors :: Errors -> Array R.JSX
renderValidationErrors [] = []

renderValidationErrors xs =
  [ D.div
      { className: "alert alert-danger"
      , children: [ D.ul_ (map renderValidationError xs) ]
      }
  ]

mkAddressBookApp :: Effect (ReactComponent {})
mkAddressBookApp = do
  component "AddressBookApp" \props -> R.do
    pp@(Person p@{ homeAddress: Address a }) /\ setPerson <- useState examplePerson
    --person /\ setPerson <- useState examplePerson
    let
      x = 44
    {-
      errors = case validatePerson' pp of
        Left e -> e
        Right _ -> []

      updatePhoneNumber s (PhoneNumber o) = PhoneNumber o { number = s }

      renderPhoneNumber :: PhoneNumber -> Int -> R.JSX
      renderPhoneNumber (PhoneNumber phone) index =
        formField (show phone."type") "XXX-XXX-XXXX" phone.number
          $ \s -> setPerson \_ -> over person $ _ { phones = fromMaybe p.phones $ modifyAt index (updatePhoneNumber s) p.phones }
      -}
    pure
      $ D.div
          { className: "container"
          , children:
              [ D.div
                  { className: "row"
                  , children: renderValidationErrors [] --errors
                  }
              , D.div
                  { className: "row"
                  , children:
                      [ D.form
                          { children:
                              [ D.h3_ [ D.text "Basic Information" ]
                              , formField "Street" "Street" a.street
                                  $ \s -> setPerson (over Person (over Address (_ { street = s })))
                              ]
                          {-
                              , formField "First Name" "First Name" p.firstName
                                  $ \s -> setPerson $ over pp $ _ { firstName = s }
                              ]
                              , formField "Last Name" "Last Name" p.lastName
                                  $ \s -> setPerson \_ -> Person p { lastName = s }
                              , D.h3_ [ D.text "Address" ]
                              , formField "Street" "Street" a.street
                                  $ \s -> setPerson \_ -> Person p { homeAddress = Address a { street = s } }
                                  setPerson (over Person (over Address (_ { street = s})))

                              , formField "City" "City" a.city
                                  $ \s -> setPerson \_ -> Person p { homeAddress = Address a { city = s } }
                              , formField "State" "State" a.state
                                  $ \s -> setPerson \_ -> Person p { homeAddress = Address a { state = s } }
                              , D.h3_ [ D.text "Contact Information" ]
                              ]
                                <> zipWith renderPhoneNumber p.phones (0 .. length p.phones)
                                  -}
                          }
                      ]
                  }
              ]
          }

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Container element not found."
    Just c -> do
      addressBookApp <- mkAddressBookApp
      let
        app = element addressBookApp {}
      render app c
