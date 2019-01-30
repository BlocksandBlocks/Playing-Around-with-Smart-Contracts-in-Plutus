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
  \(projectCandidate :: [char]) (projectVote :: int) (p :: PendingTx') ->
  --first variable is the redeemerScript. second is datascript.  
  --third variable is PendingTX' and is info about the current transaction provided by the slot leader.
  --how to make that work with the voting???
  --probably need to wait for a tutorial on multiple input handling???
  --what if postCandidate in the offchain code involved the [char] and a project number and then projectVote in the offchain code
  --involved voting by using the project number.
  --Would we still need the accumulator style vote counting in that case...does that even work anyway????
    let
    
      voteTally :: int -> bool
      voteTally n = = $$(P.foldr) (\i acc -> Builtins.greaterThanInteger (i+acc) 0) false projectVote
      --First problem is that this probably doesn't actually accumulate all the votes of various voters.  
      --This is not an excellent voting mechanism (i.e. everyone votes 1 for winner and 0 for losers) since multiple candidates
      --could end up with vote tallies greater than zero.
      --The following are my probably incorrect assumptions about how this line above works:
      --”$$(P.” calls Prelude functions (e.g. in this case “foldr”),
      --this is different than "$" which just denotes a parenthetical,
      --”\” denotes a lambda (i.e. just a function without a name we’ll only use once),
      --”i” is just the argument we’re passing to the nameless function,
      --”greatherThanInteger (i+acc) 0” just checks if “(i+acc)” is greater than “0”,
      --”acc” is an accumulator (explained in “Folds and Horses” chapter in Learn you a Haskell), and
      --the "false" after the parenthetical is the starting point for the accumulator.

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

  fundProject :: value -> MockWallet ()
  fundProject prize = payToScript_ scAddress prize 
   --This used to look like projectVote below, but we couldn't have two dataScripts.
   --Made this a payToScript but not a DataScript to make room for projectVote as the DataScript?
   --We don't need the char since we can just tell parties (exogenous to the SC) that this is the SC where they submit projects.
   
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
   in votingValidator $ DataScript $ Ledger.lifted numVote

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
  


