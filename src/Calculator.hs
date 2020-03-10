module Calculator where

import Clash.Prelude
import RetroClash.Utils (withResetEnableGen)
import RetroClash.Keypad (Matrix, inputKeypad)
import RetroClash.Clock
import RetroClash.SerialRx
import RetroClash.SerialTx
import Control.Monad
import Data.Maybe
import Data.Char
import Control.Monad.State

type Digit = Index 10

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
    -> Signal System (Vec 4 Bool)
    -> ( Signal System Bit
      , Signal System (Vec 4 Bool)
      )
topEntity = withResetEnableGen board
  where
    board rx rows =
        ( tx
        , reverse <$> cols
        )
      where
        digits = logic @4 cmd

        (tx, ack) = serialTx @8 (SNat @9600) (fmap bitCoerce <$> serialDisplay ack digits)
        cmd = (const Nothing =<<) <$> (serialRx @8 (SNat @9600) rx)

        (cols, _) = inputKeypad (repeat $ repeat 0)  rows

serialDisplay
    :: forall n dom. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Vec n (Maybe Digit))
    -> Signal dom (Maybe (Unsigned 8))
serialDisplay ack digits = pure Nothing

logic
    :: forall n dom. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom (Maybe ())
    -> Signal dom (Vec n (Maybe Digit))
logic = const $ pure $ repeat Nothing
