module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Component.WorkoutList (ui)

launchHalogen :: Eff (HA.HalogenEffects ()) Unit
launchHalogen = HA.runHalogenAff $ do
    body <- HA.awaitBody
    runUI ui unit body

main :: Unit
main = unsafePerformEff $ do
    launchHalogen
