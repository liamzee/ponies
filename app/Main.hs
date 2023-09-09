{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Assets                      (proximaNova)
import           Control.Concurrent.STM      (modifyTVar')
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO)
import           Control.Monad.STM           (atomically)
import           Data.Text                   (pack)
import           EventHandler                (eventHandler)
import           Model                       (PoniesModel (..),
                                              ScreenState (..), StoredData (..),
                                              initModel, initStoredData)
import           Monomer                     (MainWindowState (MainWindowNormal),
                                              appDisableAutoScale,
                                              appFontDefMem, appTheme,
                                              appWindowResizable,
                                              appWindowState, appWindowTitle,
                                              darkTheme, startApp)
import           UIBuilder                   (uiBuilder)

{- | In main, we create a TVar to store data between application runs,
  then start an apploop that manages monomer states, as monomer
  lacks support for window resizing. -}

main :: IO ()
main = newTVarIO initStoredData >>= startAppLoop

{- | The main loop handles the window resizing by loading the
  TVar, specifying updated initModel for the app, then checking
  if the exit condition is set, and exiting if it is.

  Next, it sets the exit condition, relying on app restart
  to unset it, runs the app on variable settings, then continues
  the loop. -}

startAppLoop :: TVar StoredData -> IO ()
startAppLoop storedData = do
    MkStoredData
      { _storedWorkingDirectory
      , _storedCabalState
      , _storedScreenState
      , _exit} <- readTVarIO storedData

    let updatedData = (initModel storedData)
            { _workingDirectory  = _storedWorkingDirectory
            , _workingDirectoryField = pack _storedWorkingDirectory
            , _cabalState        = _storedCabalState
            , _screenState       = _storedScreenState }

    if _exit
        then pure ()
        else do
        atomically $ modifyTVar' storedData
            do \(!storedState) -> storedState { _exit = True }
        startApp updatedData eventHandler uiBuilder
            . ( : settings ). appWindowState $ MainWindowNormal
            case _storedScreenState of
                MissingWorkingDirectory -> (300 , 210)
                NoView                  -> (300 , 768)
                WithRightScreen _       -> (1360, 768)
        startAppLoop storedData
  where
    settings
      = [ appFontDefMem "Regular" proximaNova
        , appTheme darkTheme
        , appDisableAutoScale True
        , appWindowResizable False
        , appWindowTitle "Ponies Cabal Wrapper"
        ]
