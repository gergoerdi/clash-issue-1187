{-# LANGUAGE RecordWildCards #-}
module Calculator.State where

import Clash.Prelude
import Calculator.ALU
import Data.Maybe

data Op
    = Add
    | Subtract
    deriving (Show, Generic, NFDataX)

data St n = MkSt
    { value :: BCD n
    , opBuf :: Op
    , inputBuf :: Maybe (BCD n)
    }
    deriving (Show, Generic, NFDataX)

initSt :: (KnownNat n) => St n
initSt = MkSt{ value = repeat 0, opBuf = Add, inputBuf = Nothing }

data Cmd
    = Digit Digit
    | Op Op
    | Backspace
    | Clear
    | Equals
    deriving (Show, Generic, NFDataX)

update :: (KnownNat n) => Cmd -> St n -> St n
update _ = id

displayedDigits :: (KnownNat n) => St n -> Vec n (Maybe Digit)
displayedDigits MkSt{..} = map Just $ fromMaybe value inputBuf
