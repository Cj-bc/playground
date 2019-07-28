import Test.Hspec
import BlackJack (Card(..), Phase(..), Player(..), Game(..), Action(..), doPhase, doAction)

doPhaseSpec :: Spec
doPhaseSpec =
    describe "continue Game based on phase" $ do
      context "when DealCard phase, deal cards from deck" $ do
        doPhase (Game [] [] [A,Two,Three,Four,Five] DealCard)
          `shouldReturn` (Game [A,Two] [Three,Four] [Five] PlayerTurn)

      -- I have no idea how to test PlayerTurn as it require user input


      context "when dealer's point is less than 17, dealer hit" $ do
        (doPhase (Game [A,Two] [Three,Four] [Five,Six] DealerTurn))
          `shouldReturn` (Game [A,Two] [Three,Four,Five] [Six] DealerTurn)

      context "when dealer's point is more than 17, dealer stand" $ do
        (doPhase (Game [A,Two] [Seven,Ten] [Five,Six] DealerTurn))
          `shouldReturn` (Game [A,Two] [Seven,Ten] [Five,Six] ComparePoints)

      context "when player's point is heigher than dealer, player win it" $ do
        (doPhase (Game [Ten,J] [Two,Three] [] ComparePoints))
          `shouldReturn` (Game [Ten,J] [Two,Three] [] (GameEnd Player))

      context "when dealer's point is heigher than player, dealer win it" $ do
        (doPhase (Game [Two,Three] [Ten,J] [] ComparePoints))
          `shouldReturn`(Game [Two,Three] [Ten,J] [] (GameEnd Dealer))

      context "when player bust, dealer win" $ do
        (doPhase (Game [J,Q,K] [A,Two] [] ComparePoints))
          `shouldReturn` (Game [J,Q,K] [A,Two] [] (GameEnd Dealer))

      context "when dealer bust, player win" $ do
        (doPhase (Game [A,Two] [Seven,Nine,Six] [] ComparePoints))
          `shouldReturn` (Game [A,Two] [Seven,Nine,Six] [] (GameEnd Player))


main :: IO ()
main = hspec doPhaseSpec

--      it "end game with dealer's win, when player bust" $ do
--        doPhase (Game [J,Q,K] [A,Two] [] BustCheck)
--          `shouldReturn` (Game [J,Q,K] [A,Two] [] (GameEnd Dealer))
--
--      it "end game with player's win, when dealer bust" $ do
--        doPhase (Game [A,Two] [J,Q,K] [] BustCheck)
--          `shouldReturn` (Game [A,Two] [J,Q,K] [] (GameEnd Player))


