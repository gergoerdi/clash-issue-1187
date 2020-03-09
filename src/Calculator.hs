{-# LANGUAGE PartialTypeSignatures, NumericUnderscores, ApplicativeDo, RecordWildCards #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

{-# LANGUAGE GADTs #-}
module Calculator where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Keypad
import RetroClash.Clock
import RetroClash.SerialRx
import RetroClash.SerialTx
import Control.Monad
import Data.Maybe
import Data.Char
import Control.Monad.State

import Calculator.ALU
import Calculator.State

{-# ANN topEntity
  (Synthesize
    { t_name   = "Calculator"
    , t_inputs =
          [ PortName "CLK"
          , PortName "RX"
          , PortName "ROWS"
          ]
    , t_output = PortProduct ""
          [ PortName "TX"
          , PortName "COLS"
          ]
    }) #-}
topEntity
    :: Clock System
    -> Signal System Bit
    -> Signal System (Vec 4 (Active Low))
    -> ( Signal System Bit
      , Signal System (Vec 4 (Active Low))
      )
topEntity = withResetEnableGen board
  where
    board rx rows =
        ( tx
        , reverse <$> cols
        )
      where
        digits = logic @4 cmd

        (tx, ack) = serialTx (SNat @9600) (fmap bitCoerce <$> serialDisplay ack digits)
        cmd = (byteToCmd . bitCoerce =<<) <$> (serialRx (SNat @9600) rx)

        input = inputKeypad keymap
        (cols, key) = input rows

pattern ByteChar c <- (chr . fromIntegral -> c) where
  ByteChar = fromIntegral . ord

byteToCmd :: Unsigned 8 -> Maybe Cmd
byteToCmd _ = Nothing

serialDisplay
    :: forall n dom. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Vec n (Maybe Digit))
    -> Signal dom (Maybe (Unsigned 8))
serialDisplay ack digits = mealyStateB step (Nothing @(Index (7 + n)), repeat 0) (ack, digits)
  where
    step (next, digits) = do
        (i, bs) <- get
        case i of
            Nothing -> do
                let bs' = clear ++ map fromDigit digits
                when (bs /= bs') $ put (Just 0, bs')
                return Nothing
            Just i -> do
                when next $ put (succIdx i, rotateLeftS bs (SNat @1))
                return . Just $ head bs

    fromDigit :: Maybe Digit -> Unsigned 8
    fromDigit = maybe (ByteChar ' ') $ \n -> ByteChar '0' + fromIntegral n

    clear :: Vec 7 (Unsigned 8)
    clear =
        0x1b :> ByteChar '[' :> ByteChar '2' :> ByteChar 'J' :> -- clear screen
        0x1b :> ByteChar '[' :> ByteChar 'H' :>                 -- cursor to home position
        Nil

logic
    :: forall n dom. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom (Maybe Cmd)
    -> Signal dom (Vec n (Maybe Digit))
logic = moore (flip $ maybe id update) displayedDigits initSt

type Hex = Unsigned 4

keymap :: Matrix 4 4 Hex
keymap =
    (0x1 :> 0x2 :> 0x3 :> 0xa :> Nil) :>
    (0x4 :> 0x5 :> 0x6 :> 0xb :> Nil) :>
    (0x7 :> 0x8 :> 0x9 :> 0xc :> Nil) :>
    (0x0 :> 0xf :> 0xe :> 0xd :> Nil) :>
    Nil
