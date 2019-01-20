module Component.App
  ( app
  ) where

import Data.Array as Array
import Data.Foldable (fold)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Effect.Random as Math
import Partial.Unsafe (unsafePartial)
import Prelude (bind, pure, show, (-), (<>))
import React.Basic (Component, JSX, Self, StateUpdate(..), capture, capture_, createComponent, make, send)
import React.Basic.DOM (css)
import React.Basic.DOM as H
import React.Basic.DOM.Events (targetValue)

type Props =
  {}

type State =
  { characters :: NonEmptyString
  , generated :: Maybe String
  , length :: Int
  }

data Action
  = EditCharacters String
  | EditLength String
  | Generate
  | Generated String

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialState :: State
initialState =
  let
    s :: String
    s = fold
          [ "!\"#$%&'()*+,-./"
          , "0123456789:;<=>?"
          , "@ABCDEFGHIJKLMNO"
          , "PQRSTUVWXYZ[\\]^_"
          , "`abcdefghijklmno"
          , "pqrstuvwxyz{|}~"
          ]
  in
    { characters: unsafePartial (NonEmptyString.unsafeFromString s)
    , generated: Nothing
    , length: 16
    }

render :: Self Props State Action -> JSX
render self =
  H.div
  { className: "app"
  , children:
    [ -- CSS (1): <style> ... </style>
      H.style_
      [ H.text "html, body { margin: 0; }"
      , H.text ".body > .ok-button:hover { background-color: red !important; }"
      ]
    , H.div
      { className: "header"
      , children:
        [ H.h1_
          [ H.text "Password Generator" ]
        ]
      }
    , H.div
      { className: "body"
      , children:
        [ H.label_
          [ H.span_ [ H.text "length" ]
          , H.input
            { onChange:
                capture
                  self
                  targetValue
                  (\v -> EditLength (fromMaybe "" v))
            , value: show self.state.length
            }
          ]
        , H.br {}
        , H.label_
          [ H.span_ [ H.text "characters" ]
          , H.input
            { onChange:
                capture
                  self
                  targetValue
                  (\v -> EditCharacters (fromMaybe "" v))
            , value: NonEmptyString.toString self.state.characters
            }
          ]
        , H.br {}
        , H.button
          { className: "ok-button"
          , onClick: capture_ self Generate
          -- CSS (2): style=" ... "
          , style:
              css
                { width: "200px"
                , height: "24px"
                , backgroundColor: "#ccccff"
                , border: "0"
                , boxShadow: "2px 2px"
                , cursor: "pointer"
                }
          , children:
            [ H.text "OK"
            ]
          }
        , H.br {}
        ] <>
        case self.state.generated of
          Nothing -> []
          Just generated -> [H.span_ [ H.text generated ]]
      }
    , H.div
      { className: "footer" }
    ]
  }

update :: Self Props State Action -> Action -> StateUpdate Props State Action
update self (EditCharacters s) =
  case NonEmptyString.fromString s of
    Nothing -> NoUpdate
    Just s' -> Update self.state { characters = s' }
update self (EditLength s) =
  case Int.fromString s of
    Nothing -> NoUpdate
    Just i -> Update self.state { length = i }
update self Generate =
  SideEffects
    \self' -> do
      let
        cs =
          String.split
            (Pattern "")
            (NonEmptyString.toString self.state.characters)
      generated <-
        Array.foldM
          (\a _ -> do
            n <- Math.randomInt 0 ((Array.length cs) - 1)
            pure (a <> fromMaybe "" (Array.index cs n)))
          ""
          (Array.range 1 self.state.length)
      send self' (Generated generated)
update self (Generated generated) =
  Update self.state { generated = Just generated }
