module ContentEditable.SVGComponent where

import Prelude

import ContentEditable.Component as ContentEditable
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Svg.Attributes as SA
import Svg.Elements as SE

type Shape = { width :: Number, height :: Number }

type State = { shape :: Shape
             , initialText :: String
             , maxShape :: Shape
             , fitContentDynamic :: Boolean
             }

data Action
  = Init
  | TextInput String
  | UpdateForeignObjectShape
  | OnFocus String
  | OnBlur String

data Query a
  = SetText String a
  | Focus a
  | Blur a

data Message
  = TextUpdate String
  | Focused String
  | Blurred String

type Slot = H.Slot Query Message

type Input = State

type Slots = ( innerContentEditable :: ContentEditable.Slot Unit )

_innerContentEditable :: SProxy "innerContentEditable"
_innerContentEditable = SProxy

svgContenteditable :: H.Component HH.HTML Query Input Message Aff
svgContenteditable =
  H.mkComponent
    { initialState : identity
    , render : render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction
                                    , handleQuery = handleQuery
                                    , initialize   = Just Init
                                    })
    }
  where

  render :: State -> H.ComponentHTML Action Slots Aff
  render state =
      SE.foreignObject
      [ SA.height $ show state.shape.height
      , SA.width $ show state.shape.width
      ]
      [ HH.slot
        (SProxy :: SProxy "innerContentEditable")
        unit
        ContentEditable.contenteditable
        state.initialText
        case _ of
          ContentEditable.TextUpdate text -> Just $ TextInput text
          ContentEditable.Focused text -> Just $ OnFocus text
          ContentEditable.Blurred text -> Just $ OnBlur text
      ]

  handleAction :: Action -> H.HalogenM State Action Slots Message Aff Unit
  handleAction = case _ of
    Init -> do
      state <- H.get
      handleAction UpdateForeignObjectShape

    TextInput text -> do
      H.raise $ TextUpdate text
      handleAction UpdateForeignObjectShape

    UpdateForeignObjectShape -> do
      state <- H.get
      if not state.fitContentDynamic
        then pure unit
        else do
          -- Update foreignObject wrapper shape to fit content.
          -- The actual text box is dynamically sized, but the foreighObject wrapper
          -- can't be set to fit the text, so we update it manually.
          maybeMaybeTextFieldScrollShape <- H.query _innerContentEditable unit $ H.request ContentEditable.GetScrollShape
          let scrollShape = clippedScrollShape state.maxShape
                            $ fromMaybe state.shape
                            $ join maybeMaybeTextFieldScrollShape
          H.modify_ _{ shape = scrollShape}

    OnFocus text -> H.raise $ Focused text

    OnBlur text -> H.raise $ Blurred text

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Message Aff (Maybe a)
  handleQuery = case _ of
    SetText text a -> pure a <$ do
      _ <- H.query _innerContentEditable unit $ H.tell $ ContentEditable.SetText text
      handleAction UpdateForeignObjectShape

    Focus a -> pure a <$ do
      H.query _innerContentEditable unit $ H.tell $ ContentEditable.Focus

    Blur a -> pure a <$ do
      H.query _innerContentEditable unit $ H.tell $ ContentEditable.Blur

clippedScrollShape :: Shape -> Shape -> Shape
clippedScrollShape maxShape textFieldScrollShape =
   { width : min textFieldScrollShape.width
                  maxShape.width
   , height : min textFieldScrollShape.height
                  maxShape.height
   }
