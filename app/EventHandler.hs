{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module EventHandler where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (writeTVar)
import           Control.Lens.Operators      ((.~), (^.))
import           Data.Bool                   (bool)
import qualified Data.ByteString             as BS
import           Data.Function               ((&))
import           Data.List                   (isSuffixOf)
import           Data.Text                   (Text, pack, unpack)
import           Graphics.UI.TinyFileDialogs (IconType (Error), OK (OK),
                                              messageBox, selectFolderDialog)
import           Model                       (CabalState (..),
                                              ParticularScreen (..),
                                              PoniesModel (..),
                                              ScreenState (..), StoredData (..),
                                              _screenState,
                                              _workingDirectoryField,
                                              cabalState, freezeScreen,
                                              screenState, workingDirectory,
                                              workingDirectoryField)
import           Monomer                     (EventResponse (..),
                                              WidgetRequest (..))
import           Monomer.Main.Core           (AppEventHandler, AppEventResponse)
import           Parsers.CabalFile           (parseCabalFile)
import           System.Directory            (doesDirectoryExist,
                                              getDirectoryContents,
                                              setCurrentDirectory)
import           System.Process              (callCommand)
import Data.Functor ((<&>))

data Events
  = NoEvent
  | SystemAction SystemActions
  | DirectoryButtons DirectoryButtonEvents
  | UpdateDirectory DirectoryEvents
  | LeftButtonPresses LeftButtonEvents
  | RightScreenEvents RightScreenEvents

data SystemActions
  = UnfreezeScreen
  | ReloadScreen
  | UpdateModelReload PoniesModel
  | UpdatePoniesModel PoniesModel

data DirectoryButtonEvents
  = SelectFolderDialog
  | ReceiveFilePath FilePath
  | SelectFolderDialogCancel

data DirectoryEvents
  = DirectoryRequest
  | DirectoryError
  | DirectorySuccess FilePath

data LeftButtonEvents
  = ViewProject
  | BuildProject
  | RunProject
  | RunTests
  | Repl
  | ButtonsMore

data RightScreenEvents
  = InitDirectory

type Responses = [AppEventResponse PoniesModel Events]

eventHandler :: AppEventHandler PoniesModel Events
eventHandler widEnv widNode model = \case
  NoEvent -> []
  SystemAction act -> systemActionEventHandler act
  UpdateDirectory event -> directoryEventHandler event
  DirectoryButtons event -> directoryButtonEventHandler event
  LeftButtonPresses event -> leftButtonPressHandler event
  RightScreenEvents event -> rightScreenEventHandler event
  where
    systemActionEventHandler :: SystemActions -> Responses
    systemActionEventHandler = \case
      UnfreezeScreen -> [Model $ model & freezeScreen .~ False]
      ReloadScreen -> [Task $ updateStoredData model, Request $ ExitApplication True]
      UpdateModelReload newModel ->
          Event . SystemAction <$> [UpdatePoniesModel newModel, ReloadScreen]
      UpdatePoniesModel newModel -> [Model newModel]

    directoryEventHandler :: DirectoryEvents -> Responses
    directoryEventHandler = \case
      DirectoryRequest ->
        [ Task $ attemptDirectoryUpdate (_workingDirectoryField model)]
      DirectoryError ->
        [ Task $ SystemAction UnfreezeScreen <$
          messageBox "A Message from Ponies Cabal GUI Wrapper"
          "The file path is not a valid directory.\
          \ Please enter a valid file path."
          Error OK
        ]
      DirectorySuccess fp ->
        [ Model $ model & workingDirectory .~ fp & screenState .~ NoView
        , Event $ SystemAction ReloadScreen
        ]

    directoryButtonEventHandler :: DirectoryButtonEvents -> Responses
    directoryButtonEventHandler = \case
      SelectFolderDialog -> [Model $ model & freezeScreen .~ True, Task getDirectory]
      ReceiveFilePath filePath ->
        [ Model $ model
          & workingDirectoryField .~ pack filePath
        , Event $ SystemAction UnfreezeScreen ]
      SelectFolderDialogCancel -> [Event $ SystemAction UnfreezeScreen]

    leftButtonPressHandler ::  LeftButtonEvents -> Responses
    leftButtonPressHandler = \case
        ViewProject -> case _screenState model of
            NoView -> [Task $ checkDirectory model]
            _      -> []
        ioCommand -> [Task $ doCommand model ioCommand]

    rightScreenEventHandler :: RightScreenEvents -> Responses
    rightScreenEventHandler = \case
        InitDirectory -> [Task do
            setCurrentDirectory $ model ^. workingDirectory
            NoEvent <$ callCommand "cabal init"]

getDirectory :: IO Events
getDirectory = selectFolderDialog "Select Project Directory" ""
  <&> DirectoryButtons . \case
    Nothing       -> SelectFolderDialogCancel
    Just filePath -> ReceiveFilePath . unpack $ filePath

updateStoredData :: PoniesModel -> IO Events
updateStoredData MkPoniesModel
    {_workingDirectory, _workingDirectoryField, _cabalState, _screenState, _TVar}
    = (NoEvent <$) . atomically . writeTVar _TVar
      $ MkStoredData _workingDirectory _workingDirectoryField
          _cabalState _screenState False

attemptDirectoryUpdate :: Text -> IO Events
attemptDirectoryUpdate newDirectory =
  bool
    (UpdateDirectory DirectoryError)
    (UpdateDirectory $ DirectorySuccess unpackedDirectory)
  <$> doesDirectoryExist unpackedDirectory
  where
    unpackedDirectory = unpack newDirectory

doCommand :: PoniesModel -> LeftButtonEvents -> IO Events
doCommand model command = do
    setCurrentDirectory $ model ^. workingDirectory
    NoEvent <$ case command of
        BuildProject -> callCommand "cabal build"
        RunProject   -> callCommand "cabal run"
        Repl         -> callCommand "cabal repl"
        RunTests     -> callCommand "cabal test"
        _            -> pure ()

checkDirectory :: PoniesModel -> IO Events
checkDirectory model =
    (\(screen, cState) -> SystemAction . UpdateModelReload $
      model
        & screenState .~ WithRightScreen screen
        & cabalState .~ cState)
      <$> do
    cabalFiles <- filter (isSuffixOf ".cabal")
        <$> getDirectoryContents (model ^. workingDirectory)
    case length cabalFiles of
        0 -> pure (InitScreen, NullCabalState)
        1 -> do
          cabalFile <- parseCabalFile <$> BS.readFile
              ((model ^. workingDirectory) <> "/" <> cabalFiles !! 0)
          pure (ShowStanzas, Loaded cabalFile)
        _ -> pure (MultipleCabalFiles, NullCabalState)