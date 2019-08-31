module ContentEditable.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse,  for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Web.DOM.Element as DE
import Web.DOM.Node as DN
import Web.DOM.NodeList as NL
import Web.DOM.Document (createElement)
import Web.HTML as WH
import Web.HTML.HTMLElement (toNode, fromElement, toElement)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.Event.Event as WE
import Web.UIEvent.KeyboardEvent as KE

type State = String

data Action
  = Init
  | TextInput KE.KeyboardEvent

data Query a
  = GetScrollShape (Maybe { width :: Number, height :: Number } -> a)
  | SetText String a

newtype Message = TextUpdate String

type Slot = H.Slot Query Message

type Input = String

type Slots = ()

contenteditable :: H.Component HH.HTML Query Input Message Aff
contenteditable =
  H.mkComponent
    { initialState : identity
    , render : render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction
                                    , handleQuery  = handleQuery
                                    , initialize   = Just Init
                                    })
    }
  where

  editorRef :: H.RefLabel
  editorRef = H.RefLabel "editor"

  render :: State -> H.ComponentHTML Action Slots Aff
  render _ =
    HH.div
    -- use xmlns for using inside a SVG foreignObject
    [ HH.attr (HH.AttrName "xmlns") "http://www.w3.org/1999/xhtml"
    , HP.class_ $ HH.ClassName "text-field"
    , HH.attr (HH.AttrName "contenteditable") "true"
    , HE.onKeyUp $ \e -> Just $ TextInput e
    , HP.ref editorRef
    ]
    []

  handleAction :: Action -> H.HalogenM State Action Slots Message Aff Unit
  handleAction = case _ of
    -- | Update DOM directly to initalise the text of the contenteditable component.
    -- | We can't render the text normally as the contenteditable component keeps its
    -- | own state and updates its text outside of Halogen's control.
    Init -> do
      state <- H.get
      _ <- handleQuery $ SetText state (\_ -> unit)
      pure unit

    TextInput keyboardEvent -> do
      let event = KE.toEvent keyboardEvent
      let maybeNode = WE.target event >>= DN.fromEventTarget
      case maybeNode of
        Nothing -> pure unit
        Just node -> do
          nodeText <- H.liftEffect $ getContentEditableText node
          H.put nodeText
          H.raise $ TextUpdate nodeText

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Message Aff (Maybe a)
  handleQuery = case _ of
    SetText text a -> pure a <$ do
      maybeRef <- H.getHTMLElementRef editorRef
      case maybeRef of
        Nothing -> pure unit
        Just element -> do
          let node = toNode element
          H.liftEffect $ setText text node

    -- | When using inside an SVG foreignObject, the scrollShape needs to be
    -- | known to manually fit the foreignObject to the content
    GetScrollShape reply -> do
      maybeRef <- H.getHTMLElementRef editorRef
      case maybeRef of
        Nothing -> pure $ Just $ reply Nothing
        Just element -> do
          scrollWidth <- H.liftEffect $ DE.scrollWidth $ toElement element
          scrollHeight <- H.liftEffect $ DE.scrollHeight $ toElement element
          pure $ Just $ reply $ Just { width : scrollWidth + 10.0, height : scrollHeight + 10.0 }

setText :: String -> DN.Node -> Effect Unit
setText text editorNode =
  let
    lines = String.split (String.Pattern "\n") text
  in do
    -- Clear existing text
    children <- DN.childNodes editorNode >>= NL.toArray
    for_ children \child -> DN.removeChild child editorNode
    -- Insert lines as individual divs
    window <- WH.window
    documentHTML <- document window
    let document = toDocument documentHTML
    for_ lines \line -> do
      newDiv <- createElement "div" document
      case fromElement newDiv of
        Nothing -> pure unit
        Just newDivHTMLElement ->
          let newNode = toNode newDivHTMLElement in do
            DN.setTextContent line newNode
            _ <- DN.appendChild newNode editorNode
            pure unit

getContentEditableText :: DN.Node -> Effect String
getContentEditableText node = do
  childrenNodeList <- DN.childNodes node
  children <- NL.toArray childrenNodeList
  nodeText <- traverse DN.textContent children
  pure $ String.joinWith "\n" nodeText
