module SimpleResourceAllocationVotingContractLearningProject where

import qualified Language.PlutusTx            as PlutusTx
import qualified Language.PlutusTx.Prelude    as P
import qualified Language.PlutusTx.Builtins   as Builtins
import qualified Data.ByteString.Lazy.Char8   as BSLC
import           Ledger
import           Ledger.Validation
import           Wallet
import           Playground.Contract


votingValidator :: ValidatorScript
votingValidator = ValidatorScript $ Ledger.fromCompiledCode $$(PlutusTx.compile
  [||
  \(projectCandidate :: ???) (projectVote :: int) (p :: PendingTx') ->
  --first variable needs to be redeemerScript. second should be datascript.
  --how to make that work with the voting???
  --probably need to wait for a tutorial on multiple input handling???
    let
    
      voteTally :: int -> bool
      voteTally n = = $$(P.foldr) (\i acc -> Builtins.greaterThanInteger (i+acc) 0) 0 projectVote
     
      --This is not an excellent voting mechanism (i.e. everyone votes 1 for winner and 0 for losers) since multiple candidates
      --could end up with vote tallies greater than zero.
      --The following are my probably incorrect assumptions about how this line above works:
      --”$$(P.” calls Prelude functions (e.g. in this case “foldr”),
      --this is different than "$" which just denotes a parenthetical,
      --”\” denotes a lambda (i.e. just a function without a name we’ll only use once),
      --”i” is just the argument we’re passing to the nameless function,
      --”greatherThanInteger i 0” just checks if “i” is greater than “0”,
      --”acc” is an accumulator (explained in “Folds and Horses” chapter in Learn you a Haskell), and
      --the 0 after the parenthetical is the starting point for the accumulator.

      voteCarries :: Bool
      voteCarries = $$(P.foldr) (\i acc -> $$(P.and) acc (voteTally i)) True projectVote

    in

      if voteCarries
      then ()
      else $$(P.error) ($$(P.traceH) "Sorry. You did not win the vote." ())
      --"traceH" is the Prelude function to include an error message in the log.
   ||])

  scAddress :: Address'
  scAddress = Ledger.scriptAddress votingValidator
  
  watchSCAddress :: MockWallet ()
  watchSCAddress = startWatching scAddress

  voteCheck :: Int -> MockWallet ()
  voteCheck num = if num /= 1 or (-1) then throwOtherError "You may only vote 1 for the winner or -1 for losers."
   else pure ()

  fundProject :: [char] -> value -> MockWallet ()
  fundProject [char] prize = do
   let hashedChar = plcSHA2_256 $ BSLC.pack $ show char
   in payToScript_ scAddress prize $ DataScript $ Ledger.lifted hashedCharProject
   register closeProjectTrigger (closeProjectHandler hashedChar)
   --Does this need to be added as a second input to the onchain code (as per @bobert tutorial "writing your first datascript") 
   --and then dealt with there somehow?
   
  postCandidate :: [char] -> MockWallet ()
  postCandidate char = do
   let hashedChar = plcSHA2_256 $ BSLC.pack $ show char
   in collectFromScript votingValidator $ RedeemerScript $ Ledger.lifted hashedCharProspect
   --Does this need to be added as a first input to the onchain code (as per @bobert tutorial "writing your first redeemerscript") 
   --and then dealt with there somehow?
   
  projectVote :: Int -> MockWallet ()
  projectVote numVote = do
   voteCheck num
   let hashedChar = plcSHA2_256 $ BSLC.pack $ show num
   collectFromScript votingValidator $ RedeemerScript $ Ledger.lifted numVote
   --should we be using collectFromScript here where the voter is not getting a payout.
   --should this just be a third input in the onchain code?

  closeContractTrigger :: EventTrigger
  closeContractTrigger = andT
   (fundsAtAddressT scAddress $ GEQ 1)
   (blockHeightT (Interval (Height 100) (Height 101)))

  closeContractHandler :: ByteString -> EventHandler MockWallet
  closeContractHandler hashedChar = EventHandler (\_ -> do
   logMsg "No candidate won."
   logMsg "Ending project and withdrawing money from SC."
   collectFromScript votingValidator $ RedeemerScript $ Ledger.lifted hashedChar)

  $(mkFunction 'voteCheck)
  $(mkFunction 'fundProject)
  $(mkFunction 'postCandidate)
  $(mkFunction 'projectVote)
  $(mkFunction 'watchSCAddress)
  $(mkFunction 'projectVote)
  


