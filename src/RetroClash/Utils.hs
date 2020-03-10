{-# LANGUAGE ScopedTypeVariables #-}
module RetroClash.Utils
    ( (.==)

    , unchanged
    , debounce

    , oneHot
    , roundRobin

    , succIdx, moreIdx

    , mealyStateB
    ) where

import Clash.Prelude
import Data.Maybe (fromMaybe)
import Control.Monad.State
import RetroClash.Clock

oneHot :: forall n. (KnownNat n) => Index n -> Vec n Bool
oneHot = bitCoerce . bit @(Unsigned n) . fromIntegral

unchanged :: (HiddenClockResetEnable dom, Eq a, NFDataX a) => a -> Signal dom a -> Signal dom Bool
unchanged x0 x = x .==. register x0 x

debounce
    :: forall ps a dom. (Eq a, NFDataX a, HiddenClockResetEnable dom, KnownNat (ClockDivider dom ps))
    => SNat ps -> a -> Signal dom a -> Signal dom a
debounce _ init this = regEn init stable this
  where
    counter = register (0 :: Index (ClockDivider dom ps)) counter'
    counter' = mux (unchanged init this) (moreIdx <$> counter) 0
    stable = counter' .==. pure maxBound

roundRobin
    :: forall n dom a. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> (Signal dom (Vec n Bool), Signal dom (Index n))
roundRobin next = (selector, i)
  where
    i = regEn (0 :: Index n) next $ nextIdx <$> i
    selector = bitCoerce . oneHot <$> i

infix 4 .==
(.==) :: (Eq a, Functor f) => f a -> a -> f Bool
fx .== y = (== y) <$> fx

nextIdx :: (Eq a, Enum a, Bounded a) => a -> a
nextIdx = fromMaybe minBound . succIdx

moreIdx :: (Eq a, Enum a, Bounded a) => a -> a
moreIdx = fromMaybe maxBound . succIdx

succIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
succIdx x | x == maxBound = Nothing
          | otherwise = Just $ succ x

mealyState
   :: (HiddenClockResetEnable dom, NFDataX s)
   => (i -> State s o) -> s -> (Signal dom i -> Signal dom o)
mealyState f = mealy step
  where
    step s x = let (y, s') = runState (f x) s in (s', y)

mealyStateB
    :: (HiddenClockResetEnable dom, NFDataX s, Bundle i, Bundle o)
    => (i -> State s o) -> s -> (Unbundled dom i -> Unbundled dom o)
mealyStateB f s0 = unbundle . mealyState f s0 . bundle
