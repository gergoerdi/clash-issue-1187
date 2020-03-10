{-# LANGUAGE NumericUnderscores #-}
module RetroClash.Clock
    ( HzToPeriod

    , Seconds
    , Milliseconds
    , Microseconds
    , Nanoseconds

    , ClockDivider
    ) where

import Clash.Prelude
import GHC.TypeNats

type HzToPeriod (rate :: Nat) = (Seconds 1 + rate - 1) `Div` rate

type Seconds (s :: Nat) = Milliseconds (1_000 * s)
type Milliseconds (ms :: Nat) = Microseconds (1_000 * ms)
type Microseconds (us :: Nat) = Nanoseconds (1_000 * us)
type Nanoseconds (ns :: Nat) = 1_000 * ns

type ClockDivider dom ps = ps `Div` DomainPeriod dom
