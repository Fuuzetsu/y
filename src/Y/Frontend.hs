{-# LANGUAGE TemplateHaskell #-}

module Y.Frontend where

import Control.Lens.TH
import qualified FRP.Sodium as Sodium

import Y.String

data InputOccurence = KChar Char | KEsc
    deriving (Show)

data ViewModel = ViewModel YiString
    deriving Show

data Frontend = Frontend
    { _feInputEvent :: IO (Sodium.Event InputOccurence)
    , _feRender :: ViewModel -> IO ()
    }

makeLenses ''Frontend
