module ContentEditable.SVGComponent where

import Prelude

import ContentEditable.Component as ContentEditable
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Svg.Attributes as SA
import Svg.Elements as SE
import Web.Event.Event as WE

type Shape = { width :: Number, height :: Number }

type State = { shape :: Shape
             , initialText :: String
             , maxShape :: Shape
             , fitContentDynamic :: Boolean
             }

data Query a
  = PreventDefault WE.Event (Query a)
  | StopPropagation WE.Event (Query a)
  | Init a
  | TextInput ContentEditable.Message a
  | UpdateForeignObjectShape a
  | SetText String a

data Message = TextUpdate String

type Input = State

data Slot = InnerContentEditable
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

svgContenteditable :: H.Component HH.HTML Query Input Message Aff
svgContenteditable =
  H.lifecycleParentComponent
    { initialState : initialState
    , render : render
    , eval : eval
    , receiver : const Nothing
    , initializer : Just $ H.action Init
    , finalizer : Nothing
    }
  where

  initialState :: Input -> State
  initialState = identity

  render :: State -> H.ParentHTML Query ContentEditable.Query Slot Aff
  render state =
      SE.foreignObject
      [ SA.height state.shape.height
      , SA.width state.shape.width
      ]
      [ HH.slot
        InnerContentEditable
        ContentEditable.contenteditable
        state.initialText
        (HE.input TextInput)
      ]

  eval :: Query ~> H.ParentDSL State Query ContentEditable.Query Slot Message Aff
  eval = case _ of
    PreventDefault e q -> do
      H.liftEffect $ WE.preventDefault e
      eval q

    StopPropagation e q -> do
      H.liftEffect $ WE.stopPropagation e
      eval q

    Init next -> next <$ do
      state <- H.get
      eval $ H.action UpdateForeignObjectShape

    TextInput (ContentEditable.TextUpdate text) next -> next <$ do
      H.raise $ TextUpdate text
      eval $ H.action UpdateForeignObjectShape

    UpdateForeignObjectShape next -> next <$ do
      state <- H.get
      if not state.fitContentDynamic
        then pure unit
        else do
          -- Update foreignObject wrapper shape to fit content.
          -- The actual text box is dynamically sized, but the foreighObject wrapper
          -- can't be set to fit the text, so we update it manually.
          maybeMaybeTextFieldScrollShape <- H.query InnerContentEditable  $ H.request ContentEditable.GetScrollShape
          let scrollShape = clippedScrollShape state.maxShape
                            $ fromMaybe state.shape
                            $ join maybeMaybeTextFieldScrollShape
          H.modify_ _{ shape = scrollShape}

    SetText text next -> next <$ do
      _ <- H.query InnerContentEditable $ H.action $ ContentEditable.SetText text
      eval $ H.action UpdateForeignObjectShape

clippedScrollShape :: Shape -> Shape -> Shape
clippedScrollShape maxShape textFieldScrollShape =
   { width : min textFieldScrollShape.width
                  maxShape.width
   , height : min textFieldScrollShape.height
                  maxShape.height
   }
