{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module DecentralSeed where

import Plutarch
import Plutarch.Prelude
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
 )
import Plutarch.Api.V2 
import Plutarch.DataRepr
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
  ptryFromC
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx qualified
import Utils

-- from CheckSignatories
import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Interval


--data OurDatum = OurDatum {password :: BuiltinByteString}

-- data POurParam (s :: S) = POurParam (Term s (PDataRecord '[ "password" ':= PInteger ]))
--   deriving stock (Generic)
--   deriving anyclass (PlutusType, PIsData, PDataFields)

-- instance DerivePlutusType POurParam where 
--     type DPTStrat _ = PlutusTypeData 

-- instance PTryFrom PData POurParam 
-- instance PTryFrom PData (PAsData POurParam)


data SeedPhraseParam (s :: S) = SeedPhraseParam (Term s (PDataRecord 
                              '[ "pInfoHash" ':= PByteString ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType SeedPhraseParam where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData SeedPhraseParam 
instance PTryFrom PData (PAsData SeedPhraseParam)

-- from Plutus side
-- data SeedPhraseParam = SeedPhraseParam {
--     pInfoHash    :: BuiltinByteString

--   } deriving Haskell.Show

-- data POurDatum (s :: S) = POurDatum (Term s (PDataRecord '[ "password" ':= PInteger ]))
--   deriving stock (Generic)
--   deriving anyclass (PlutusType, PIsData, PDataFields)

-- instance DerivePlutusType POurDatum where 
--     type DPTStrat _ = PlutusTypeData 

-- instance PTryFrom PData POurDatum 


data SeedPhraseDatum (s :: S) = SeedPhraseDatum (Term s (PDataRecord 
                  '[ 
                    "encryptedWordsWithIndex" ':= PByteString,
                     "ownerPKH" ':= PPubKeyHash ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType SeedPhraseDatum where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData SeedPhraseDatum 
instance PTryFrom PData (PAsData SeedPhraseDatum)

-- from Plutus side
-- data SeedPhraseDatum = SeedPhraseDatum {
--     encryptedWordsWithIndex    ::  BuiltinByteString,     
--     ownerPKH                   ::  PubKeyHash 
--   }




-- data POurRedeemer (s :: S) = 
--     Mint (Term s (PDataRecord '[ ])) 
--     | Burn (Term s (PDataRecord '[ ]) )
--   deriving stock (Generic)
--   deriving anyclass (PlutusType, PIsData, PShow)

-- instance DerivePlutusType POurRedeemer where 
--     type DPTStrat _ = PlutusTypeData

-- instance PTryFrom PData POurRedeemer  







-- validateSmallChecks :: OurDatum -> OurRedeemer -> ScriptContext -> () 
pvalidateDecentralSeed :: Term s (SeedPhraseParam :--> SeedPhraseDatum :--> PUnit :--> PScriptContext :--> PUnit)
pvalidateDecentralSeed = phoistAcyclic $ plam $ \param datum _ _ctx -> unTermCont $ do 
    ctxF <- pletFieldsC @'["txInfo"] _ctx 
    -- infoF <- pletFieldsC @'["signatories"] ctxF.txInfo 
    let signatories = pfield @"signatories" # ctxF.txInfo
    datumF <- pletFieldsC @'["ownerPKH"] datum 
    paramF <- pletFieldsC @'["pInfoHash"] param
    -- redeemF <- pletFieldsC @'["password"] redeemer 
    pure $ 
      pif 
      -- ((paramF.pInfoHash) #== paramF.pInfoHash )   
      (pelem # pdata datumF.ownerPKH # pfromData signatories) 
      (pconstant ())
      (ptraceError "Owner pub key hash has to sign ")
      -- perror

-- pvalidateDecentralSeedW :: Term s (POurParam :--> PValidator )
pvalidateDecentralSeedW :: ClosedTerm (SeedPhraseParam :--> PValidator )
pvalidateDecentralSeedW =  plam $ \param datum redeemer ctx -> unTermCont $ do 
  -- (ourDatum, _) <- ptryFromC @POurDatum datum 
  -- (ourRedeemer, _) <- ptryFromC @POurRedeemer redeemer 
  -- (ourParam, _) <- ptryFromC @POurParam param
  -- pure $ popaque $ pvalidateDecentralSeed # ourParam # ourDatum # ourRedeemer # ctx 
    let ourDatum :: Term _ SeedPhraseDatum 
        ourDatum = punsafeCoerce datum 
        ourRedeemer :: Term _ PUnit 
        ourRedeemer = punsafeCoerce redeemer
        ourParam :: Term _ SeedPhraseParam
        ourParam = punsafeCoerce param
    --  in pure $ popaque $ pvalidateDecentralSeed # ourParam # ourDatum # ourRedeemer # ctx 
     in pure $ popaque $ pvalidateDecentralSeed # ourParam # ourDatum # ourRedeemer # ctx


-- emurgoDAOValidatorW :: ClosedTerm (PCurrencySymbol :--> PValidator)
-- emurgoDAOValidatorW = plam $ \cs datum redeemer ctx -> unTermCont $ do 
--   (dat, _) <- ptryFromC @PDaoDatum datum 
--   (redmr, _) <- ptryFromC @PDaoAction redeemer 
--   pure $ popaque $ emurgoValidator # cs # dat # redmr # ctx


unit1 :: Term s PUnit
unit1 = pconstant ()

int1 :: Term s PInteger
int1 = pconstant 24
int2 :: Term s PInteger
int2 = pconstant 10
fields :: Term _ (PDataRecord '[ "_0" ':= PInteger ])
fields = pdcons # (pdata int1) # pdnil



-- datum1 = POurDatum (pdcons # (pdata int1) # pdnil)
-- datum2 = POurRedeemer (pdcons # (pdata int1) # pdnil)
-- datum2 = Mint


hashStr :: PubKeyHash
hashStr = "0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"

mockCtx2 :: ScriptContext
mockCtx2 =
  ScriptContext
    (TxInfo
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      (interval (POSIXTime 1) (POSIXTime 2))
    --   [fromString hashStr, "f013", "ab45"]
    --   ["abce0f123e", "f013", "ab45"]
    --   ["0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a", "f013", "ab45"]
      [hashStr, "f013", "ab45"]    --- this works too. So did not need `fromString`
      mempty
      ""
    )
    (Minting (CurrencySymbol "abc"))

-- result1 = pvalidateSmallChecksW # datum1 # datum2 # 


-- createDatum :: Terms s (PInteger :--> POurDatum)
-- createDatum phoistAcyclic $ plam $ \i -> unTermCont $ do 
--   ctxF <- pletFieldsC @'["txInfo", "purpose"] context 

-- dataRec1 = pdcons # int1 # pdnil

-- fields = pdcons # currSymDat # pdnil
-- evalWithArgsT (checkSignatory # pubKeyHash) [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData mockCtx]



