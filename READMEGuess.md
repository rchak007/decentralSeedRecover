# guessNumberLucid
Off-Chain code with Lucid #plutarch #cardano





Employ Lucid to interact with off-chain code.

Employ blockfrost to connect to Cardano blockchain.





## Plutarch On Chain code

### Overview

Plutarch is an eDSL in Haskell for writing on-chain scripts for Cardano. With some caveats, Plutarch is a [simply-typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) (or STLC). Writing a script in Plutarch allows us to leverage the language features provided by Haskell while retaining the ability to compile to compact Untyped Plutus Core (or UPLC, which is an untyped lambda calculus).

When we talk about “Plutarch scripts,” we are referring to values of type `Term (s :: S) (a :: PType)`. `Term` is a `newtype` wrapper around a more complex type, the details of which Plutarch end-users can ignore. A `Term` is a typed lambda term; it can be thought of as representing a computation that, if successfully evaluated, will return a value of type `a`.



### Why Plutarch?

Plutarch written validators are often significantly more efficient than Plutus Tx written validators. With Plutarch, you have much more fine gained control of the Plutus Core you generate, without giving up any type information.

To put things into perspective, one validator script from a large production contract was rewritten in Plutarch, changed from Plutus Tx. Here's the comparison between the Plutarch script's execution cost compared to the Plutus Tx script's execution cost. These numbers were gathered by simulating the whole contract flow on a testnet:

| Version            | CPU         | Memory  | Script Size |
| ------------------ | ----------- | ------- | ----------- |
| PlutusTx (current) | 198,505,651 | 465,358 | 2013        |
| Plutarch           | 51,475,605  | 99,992  | 489         |



[Reference]: https://github.com/Plutonomicon/plutarch-plutus#why-plutarch	"Why Plutarch"







### guessNumber



Here we start with Number guess game and the Cardano plutus onChain script is written in Plutarch. 

Below is snippet of onCHain code for :

#### Datum and Redeemer

```haskell
data POurDatum (s :: S) = POurDatum (Term s (PDataRecord '[ "password" ':= PInteger ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POurDatum where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData POurDatum 

data POurRedeemer (s :: S) = POurRedeemer (Term s (PDataRecord '[ "password" ':= PInteger ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POurRedeemer where 
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POurRedeemer  
```



#### Plutarch validator

This is a simple validator where it matches the existing Datum number with Redeemer number, and if its equal then it unlocks the funds. Again this is a trivial example as we inline the Datum so its actually visible but ideally it should be hashed.

```haskell
pvalidateSmallChecks :: Term s (POurDatum :--> POurRedeemer :--> PScriptContext :--> PUnit)
pvalidateSmallChecks = phoistAcyclic $ plam $ \datum redeemer _ctx -> unTermCont $ do 
    -- ctxF <- pletFieldsC @'["txInfo"] ctx 
    -- infoF <- pletFieldsC @'["signatories"] ctxF.txInfo 
    datumF <- pletFieldsC @'["password"] datum 
    redeemF <- pletFieldsC @'["password"] redeemer 
    pure $
      pif ( (redeemF.password) #== datumF.password )
      (pconstant ())
      perror

pvalidateSmallChecksW :: Term s PValidator 
pvalidateSmallChecksW = phoistAcyclic $ plam $ \datum redeemer ctx ->
    let ourDatum :: Term _ POurDatum 
        ourDatum = punsafeCoerce datum 
        ourRedeemer :: Term _ POurRedeemer 
        ourRedeemer = punsafeCoerce redeemer
     in popaque $ pvalidateSmallChecks # ourDatum # ourRedeemer # ctx 
```





