{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DerivingStrategies #-}

module Model where

import           Control.Concurrent.STM.TVar (TVar)
import           Control.Lens.TH             (makeLenses)
import           Data.Text                   (Text)
import           Parsers.CabalFile           (CabalFile (..))
import Data.Kind (Type)

{- | The state of Ponies. I have some flags, some hierarchy, etc,
  to simplify data access. A TVar is included to allow
  the system to preserve information between restarts. -}

type PoniesModel :: Type
data PoniesModel = MkPoniesModel
  { _screenState           :: ScreenState,
    _workingDirectory      :: FilePath,
    _workingDirectoryField :: Text,
    _cabalState            :: CabalState,
    _freezeScreen          :: Bool,
    _TVar                  :: TVar StoredData
  }
  deriving stock Eq

type ScreenState :: Type
data ScreenState
  = MissingWorkingDirectory
  | NoView
  | WithRightScreen ParticularScreen
  deriving stock Eq

type StoredData :: Type
data StoredData = MkStoredData
  { _storedWorkingDirectory      :: !FilePath,
    _storedWorkingDirectoryField :: !Text,
    _storedCabalState            :: !CabalState,
    _storedScreenState           :: !ScreenState,
    _exit                        :: !Bool
  }
  deriving stock Eq

type ParticularScreen :: Type
data ParticularScreen
  = InitScreen
  | MultipleCabalFiles
  | ShowStanzas
  deriving stock Eq

type CabalState :: Type
data CabalState
  = NullCabalState
  | Loaded CabalFile
  deriving stock Eq 

makeLenses ''PoniesModel

makeLenses ''ScreenState

makeLenses ''StoredData

makeLenses ''ParticularScreen

makeLenses ''CabalState

initStoredData :: StoredData
initStoredData = MkStoredData "" "" NullCabalState MissingWorkingDirectory False

initModel :: TVar StoredData -> PoniesModel
initModel = MkPoniesModel MissingWorkingDirectory "" "" NullCabalState False
