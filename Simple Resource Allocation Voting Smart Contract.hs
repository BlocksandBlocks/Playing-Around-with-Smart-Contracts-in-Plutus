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
  \(projectVote :: int) (fundProject :: [char]) (p :: PendingTx') ->
  --first variable is the redeemerScript. second is datascript.  
  --third variable is PendingTX' and is info about the current transaction provided by the slot leader.
  --how to make that work with the voting???
  --probably need to wait for a tutorial on multiple input handling???
  --what if postCandidate in the offchain code involved the [char] and a project number and then projectVote in the offchain code
  --involved voting by using the project number.
  --Would we still need the accumulator style vote counting in that case...does that even work anyway????
    let
    
      voteTally :: int -> bool
      voteTally n = $$(P.foldr) (\i acc -> Builtins.greaterThanInteger (i+acc) 0) false projectVote
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
      --"traceH" is a Prelude function .
   ||])

scAddress :: Address'
scAddress = Ledger.scriptAddress votingValidator
  
watchSCAddress :: MockWallet ()
watchSCAddress = startWatching scAddress

voteCheck :: Int -> MockWallet ()
  voteCheck num = if num /= 1 or (-1) then throwOtherError "You may only vote 1 for the winner or -1 for losers."
  else pure ()

fundProject :: [char] -> value -> MockWallet ()
fundProject char prize = do
  payToScript_ scAddress prize $ DataScript $ Ledger.lifted char
  register closeContractTrigger (closeContractHandler char)
   {-It's possible we don't need the char since we can just tell parties (exogenous to the SC) that this is 
   the SC where they submit projects.-}
   --The Jelly Bean Game example has multiple inputs like this.
   
postCandidateAndVote :: [char] -> int -> MockWallet ()
postCandidate char numVote = do
  voteCheck num
  collectFromScript votingValidator $ RedeemerScript $ Ledger.lifted char
   --Here the candidate providers would provide a candidate and vote (presumably for their own candidate).
   --Addresses just voting would provide a blank char list and then just vote in the int spot.
   --But there's no way to lift both the char and the int.   Damn!

closeContractTrigger :: EventTrigger
closeContractTrigger = andT
  (fundsAtAddressT scAddress $ GEQ 1)
  (blockHeightT (Interval (Height 100) (Height 101)))

closeContractHandler :: ByteString -> EventHandler MockWallet
closeContractHandler hashedChar = EventHandler (\_ -> do
    logMsg "No candidate won."
    logMsg "Ending project and withdrawing money from SC."
    collectFromScript votingValidator $ RedeemerScript $ Ledger.lifted hashedChar)

  
  $(mkFunction 'fundProject)
  $(mkFunction 'postCandidateAndVote)
  $(mkFunction 'watchSCAddress)
