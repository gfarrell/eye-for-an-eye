module WorldSpec (spec) where

import World (
  EventGenerator,
  genEvent,
  deterministicGenEvent
  )

import Test.Hspec

spec :: Spec
spec = do
  describe "deterministicGenEvent" $
    it "always returns the specified event value" $ do
      rs <- mapM deterministicGenEvent [0.42, 0.2712, 0.314159]
      shouldBe rs [0.42, 0.2712, 0.314159]

  describe "genEvent" $
    it "generates a double between 0 and 1" $ do
      r <- genEvent
      shouldBe True (r >= 0 && r <= 1)
