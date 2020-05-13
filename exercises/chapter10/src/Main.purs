module Main where

import Prelude
import Data.AddressBook (PhoneNumber, Person, examplePerson)
import Data.AddressBook.Validation (Errors, validatePerson')
import Data.Argonaut (decodeJson, encodeJson, jsonParser, stringify)
import Data.Array (length, mapWithIndex, updateAt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Storage (getItem, setItem)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (ReactComponent, component, element, useState)
import React.Basic.Hooks as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- Note that there's a Purty formmating bug that
-- adds an unwanted blank line
-- https://gitlab.com/joneshf/purty/issues/77
renderValidationErrors :: Errors -> Array R.JSX
renderValidationErrors [] = []

renderValidationErrors xs =
  let
    rendererror :: String -> R.JSX
    rendererror err = D.li_ [ D.text err ]
  in
    [ D.div
        { className: "alert alert-danger row"
        , children: [ D.ul_ (map rendererror xs) ]
        }
    ]

-- Helper function to render a single form field with an
-- event handler to update
formField :: String -> String -> String -> (String -> Effect Unit) -> R.JSX
formField name placeholder value setValue =
  D.label
    { className: "form-group row"
    , children:
        [ D.div
            { className: "col-sm col-form-label"
            , children: [ D.text name ]
            }
        , D.div
            { className: "col-sm"
            , children:
                [ D.input
                    { className: "form-control"
                    , placeholder
                    , value
                    , onChange:
                        let
                          handleValue :: Maybe String -> Effect Unit
                          handleValue (Just v) = setValue v

                          handleValue Nothing = pure unit
                        in
                          handler targetValue handleValue
                    }
                ]
            }
        ]
    }

mkAddressBookApp :: Effect (ReactComponent { initialPerson :: Person })
mkAddressBookApp =
  -- incoming \props are unused
  component "AddressBookApp" \props -> R.do
    -- `useState` takes a default initial value and returns the
    -- current value and a way to update the value.
    -- Consult react-hooks docs for a more detailed explanation of `useState`.
    Tuple person setPerson <- useState props.initialPerson
    let
      errors = case validatePerson' person of
        Left e -> e
        Right _ -> []

      -- helper-function to return array unchanged instead of Nothing if index is out of bounds
      updateAt' :: forall a. Int -> a -> Array a -> Array a
      updateAt' i x xs = fromMaybe xs (updateAt i x xs)

      -- helper-function to render a single phone number at a given index
      renderPhoneNumber :: Int -> PhoneNumber -> R.JSX
      renderPhoneNumber index phone =
        formField
          (show phone."type")
          "XXX-XXX-XXXX"
          phone.number
          (\s -> setPerson _ { phones = updateAt' index phone { number = s } person.phones })

      -- helper-function to render all phone numbers
      renderPhoneNumbers :: Array R.JSX
      renderPhoneNumbers = mapWithIndex renderPhoneNumber person.phones

      validateAndSave :: Effect Unit
      validateAndSave = do
        log "Running validators"
        case validatePerson' person of
          Left errs -> log $ "There are " <> show (length errs) <> " validation errors."
          Right validPerson -> do
            setItem "person" $ stringify $ encodeJson validPerson
            log "Saved"

      -- helper-function to render saveButton
      saveButton :: R.JSX
      saveButton =
        D.label
          { className: "form-group row col-form-label"
          , children:
              [ D.button
                  { className: "btn-primary btn"
                  , onClick: handler_ validateAndSave
                  , children: [ D.text "Save" ]
                  }
              ]
          }
    pure
      $ D.div
          { className: "container"
          , children:
              renderValidationErrors errors
                <> [ D.div
                      { className: "row"
                      , children:
                          [ D.form_
                              $ [ D.h3_ [ D.text "Basic Information" ]
                                , formField "First Name" "First Name" person.firstName \s ->
                                    setPerson _ { firstName = s }
                                , formField "Last Name" "Last Name" person.lastName \s ->
                                    setPerson _ { lastName = s }
                                , D.h3_ [ D.text "Address" ]
                                , formField "Street" "Street" person.homeAddress.street \s ->
                                    setPerson _ { homeAddress { street = s } }
                                , formField "City" "City" person.homeAddress.city \s ->
                                    setPerson _ { homeAddress { city = s } }
                                , formField "State" "State" person.homeAddress.state \s ->
                                    setPerson _ { homeAddress { state = s } }
                                , D.h3_ [ D.text "Contact Information" ]
                                ]
                              <> renderPhoneNumbers
                          ]
                      }
                  ]
                <> [ saveButton ]
          }

main :: Effect Unit
main = do
  log "Rendering address book component"
  --log $ stringify $ encodeJson examplePerson
  -- Get window object
  w <- window
  -- Get window's HTML document
  doc <- document w
  -- Get "container" element in HTML
  ctr <- getElementById "container" $ toNonElementParentNode doc
  case ctr of
    Nothing -> throw "Container element not found."
    Just c -> do
      -- Create AddressBook react component
      addressBookApp <- mkAddressBookApp
      -- Retrieve person from local storage
      item <- getItem "person"
      initialPerson <- case decodeJson item of
        Left e1 -> do
          log $ "No string decoded: " <> e1
          log "Returning examplePerson"
          pure examplePerson
        Right (jsonString :: String) -> do
          log $ "Got json string: " <> show jsonString
          case jsonParser jsonString of
            Left e2 -> do
              log $ "Parsing error " <> e2
              log "Returning examplePerson"
              pure examplePerson
            Right j -> case decodeJson j of
              Left e3 -> do
                log $ "Decoding error " <> e3
                log "Returning examplePerson"
                pure examplePerson
              Right (p :: Person) -> do
                log $ "Got person " <> show p
                pure p
      let
        -- Create JSX node from react component. Pass-in empty props
        app = element addressBookApp { initialPerson }
      -- Render AddressBook JSX node in DOM "container" element
      D.render app c
