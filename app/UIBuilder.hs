{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase #-}

module UIBuilder where

import           Control.Lens.Operators            ((^.))
import           Data.Foldable                     (fold)
import           Data.Function                     ((&))
import           Data.String                       (fromString)
import           EventHandler                      (DirectoryButtonEvents (..),
                                                    DirectoryEvents (DirectoryRequest),
                                                    Events (..),
                                                    LeftButtonEvents (..),
                                                    RightScreenEvents (..))
import           Model                             (ParticularScreen (..),
                                                    PoniesModel (..),
                                                    ScreenState (..),
                                                    freezeScreen,
                                                    workingDirectory,
                                                    workingDirectoryField, CabalState (Loaded))
import           Monomer                           (alignCenter, alignMiddle,
                                                    box, hsplit_, label_,
                                                    maxWidth, multiline,
                                                    spacer_,
                                                    textCenter, textMiddle,
                                                    textSize, vsplit_,
                                                    width, alignBottom, filler, hstack_, textLeft, textRight)
import           Monomer.Core.Combinators          (alignRight, maxHeight,
                                                    padding, styleBasic)
import           Monomer.Widgets                   (WidgetEnv, WidgetNode)
import           Monomer.Widgets.Containers.Box    (box_)
import           Monomer.Widgets.Containers.Stack  (hstack, vstack)
import           Monomer.Widgets.Containers.ZStack (zstack)
import           Monomer.Widgets.Singles.Button    (button, mainButton)
import           Monomer.Widgets.Singles.Image     (image_)
import           Monomer.Widgets.Singles.Label     (label)
import           Monomer.Widgets.Singles.Spacer    (filler_, spacer)
import           Monomer.Widgets.Singles.TextField (textField)
import Monomer (height)
import Parsers.CabalFile (CFTopLevelData (..), CabalFile (MkCF), CFTopLevel (MkCFTopLevel))
import Monomer (alignLeft)
import Data.List (intersperse)
import Data.ByteString qualified as BS

type PoniesNode = WidgetNode PoniesModel Events

uiBuilder :: WidgetEnv PoniesModel Events -> PoniesModel -> PoniesNode
uiBuilder widEnv model = addPadding . zstack $  [ case model._screenState of
      MissingWorkingDirectory -> directoryBar
      NoView                  -> leftBar
      WithRightScreen _       -> fullViewScreen ]
    <> [ spacer | model ^. freezeScreen ]
  where
    addPadding :: PoniesNode -> PoniesNode
    addPadding     = (`styleBasic` [padding 15])
    leftBar        = vstack
      [ filler_ [width 25]
      , directoryBar
      , spacer_ [width 250]
      , commandButtons
      , filler_ [width 25]
      ] `styleBasic` [maxHeight 768, maxWidth 300]
      & box_ [alignCenter] & flip styleBasic [maxWidth 300]
    fullViewScreen = box_ [alignCenter, alignMiddle] $ hstack [ leftBar , viewScreen model `styleBasic` [height 720, width 1000]]

directoryBar :: PoniesNode
directoryBar =
  vstack
    [ image_ "https://cabal.readthedocs.io/en/stable/_static/Cabal-dark.png"
        [alignCenter] `styleBasic` [maxHeight 103]
    , spacer
    , hstack [ label "Current Directory:"
             , spacer
             , box_ [alignRight]
               $ "Pick Directory"
               `button` DirectoryButtons SelectFolderDialog
               `styleBasic` [maxWidth 300]
             ]
    , spacer
    , hstack
        [ textField workingDirectoryField,
          spacer,
          mainButton "Select" (UpdateDirectory DirectoryRequest)
        ]
    ]

commandButtons :: PoniesNode
commandButtons =
  vstack
    [
      button "View Project" do LeftButtonPresses ViewProject,
      button "Build Project" do LeftButtonPresses BuildProject,
      button "Run Project" do LeftButtonPresses RunProject,
      button "Run Tests" do LeftButtonPresses RunTests,
      button "Repl" do LeftButtonPresses Repl,
      button "More" do LeftButtonPresses ButtonsMore
    ]

viewScreen :: PoniesModel -> PoniesNode
viewScreen model = case _screenState model of
    WithRightScreen InitScreen -> initScreen $ model ^. workingDirectory
    WithRightScreen MultipleCabalFiles -> multiScreen $ model ^. workingDirectory
    WithRightScreen ShowStanzas -> showStanzas model
    _ -> error "viewScreen should not have been called without WithRightScreen in the model."

initScreen :: String -> PoniesNode
initScreen directory = vstack
  [ label_ (fold [ "The directory ", fromString directory, " does not contain a .cabal file. "
                 , "Please be aware that initializing this directory as a Cabal package "
                 , "may override existing files. Would you like to initialize the directory?"])
                 [multiline] `styleBasic` [textSize 36, textCenter, textMiddle]
  , spacer_ [width 50]
  , box (mainButton "Initialize Project" (RightScreenEvents InitDirectory) `styleBasic` [width 400])
  ] `styleBasic` [maxWidth 950] & box & flip styleBasic [maxWidth 1060, textCenter, textMiddle]

multiScreen :: String -> PoniesNode
multiScreen directory =
  label_ (fold [ "The directory ", fromString directory, " contains multiple .cabal files. "
               , "Ponies cannot work with such a directory. Consider "
               , "manually reducing the directory to a single .cabal file, whether "
               , "by removal or by changing the extension. Alternately, consider "
               , "changing the folder."])
                 [multiline] `styleBasic` [textSize 36, textCenter, textMiddle]
  `styleBasic` [maxWidth 950] & box & flip styleBasic [maxWidth 1060, textCenter, textMiddle]

showStanzas :: PoniesModel -> PoniesNode
showStanzas model = vstack
    [ box_ [alignCenter, alignMiddle] $ hstack  [stanzas, topLevels] `styleBasic` [maxHeight 1100]
    , box_ [alignBottom, alignMiddle] $ toolbar `styleBasic` [height 200]]
  where
    toolbar :: PoniesNode
    toolbar = box_ [alignCenter, alignMiddle] (label "placeholder")

    stanzas :: PoniesNode
    stanzas = box_ [alignCenter, alignMiddle] (label "placeholder")

    topLevels :: PoniesNode
    topLevels = box_ [alignRight, alignMiddle] $ makeTopLevelsWindow $ case model._cabalState of
      Loaded (MkCF (MkCFTopLevel a) _) -> a
      _ -> error "topLevels invoked without loaded cabal file."

makeTopLevelsWindow :: [CFTopLevelData] -> PoniesNode
makeTopLevelsWindow fields = vstack $ makeGrid =<< fields
  where
    makeGrid :: CFTopLevelData -> [PoniesNode]
    makeGrid = \case
      CFTLCabalVersion vnum -> pure $ hstack
        [ [alignLeft] `box_` label "cabal-version:"
        , filler
        , [alignRight] `box_` label (fromString $ intersperse '.' (show =<< vnum))
        ]
      CFTLName term -> pure $ hstack
        [ [alignLeft] `box_` label "name:"
        , filler
        , [alignRight] `box_` label (fromString $ show term)]
      CFTLComment comm -> [] -- Unimplemented.
      CFTLRaw _ -> []