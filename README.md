# Decentralized Seed Phrase Recovery

![image-20231222153734139](Images/image-20231222153734139.png)

## Problem

Storing HD wallet seed phrases is stressful. Current methods, including paper and offline storage, have risks of misplacement and hacking, leading to significant cryptocurrency losses.

## Solution

Our solution is a user-friendly decentralized seed phrase manager for Cardano. It encrypts seed phrases using On-Chain Encrypted Storage, enhancing security and convenience. By distributing seed phrases on the blockchain, the solution mitigates the risks associated with centralized storage, making it challenging for a single entity to compromise multiple seed phrases.

![image-20230805080433577](Images/DecentralizedSeedManagementV2.drawio.png)

## How It Works

### Securing User Seed Phrase

To secure the user's Seed Phrase,

1. The user interface would allow the user to either generate a new seed phrase or put in an existing seed phrase they want secure.
2. The user submits 23/24 seed words, keeping 1 hidden for extra security.
3. Index of the hidden word and passphrase are provided.
4. Personal info (hashed) is added to the script to produce parameterized contracts for UTxO identification.
5. The Dapp encrypts 23 words and their respective indexes using AES encryption using the  user passphrase + seed + index as the encryption key, and then stores the encrypted result int the datum of a UTxO on-chain.

This solution will build a parameterized Smart contract on the Cardano blockchain that will use the hashed personal information as a parameter to personalize the script in which the UTxO securing the Seed Phrase in it's Datum.

#### Implementation

* Parameterized smart contracts on Cardano Blockchain written in Plutus/Plutarch are used to secure the UTxO.
* The UTxO in parameterized script is used mainly to secure the UTxO, with an option to withdraw the min ADA if needed.
* The Off-chain code is written using Lucid for AES encryption and UTxO locking.

### Seed Phrase Recovery

#### Recovery Info

The following information would be required for the Seed Phrase recovery:

* Passphrase/password.
* 1 hidden word from the Seed Phrase + it's index.
* Personal Information used to find the Script holding the UTxO where the Seed Phrase is secure.

#### Recovery Process

To ensur a safe recovery mechanism, the users would recover their Seed Phrases by providing the key i.e their passphrase + 1 seed word + word index and personal information in the exact order to find and to decrypt the rest of the 23 words and their respective indexes.
At the point of recovery, the off-chain component uses this provided information to find the parameterized script in which the encrypted seed phrase is secured on the Cardano blockchain using the hash of the user's personal information and seed phrase is then decrypted using the AES encryption algorithm.

## The Importance Of This Solution

* It offers an easy-to-use, and secure recovery mechanism for lost seed phrases.
* It Simplifies information security and storage for recovery, encouraging users to define convenient and secure passphrases and personal information rather than storing their seed phases directly on their computer, on a piece of paper, using some other insecure/expensive storage mechanism or depending on expensive centralized solutions.
* Enhances security by allowing multiple storage locations without compromising funds.

## AES Encryption Implementation

This solution will use the password + 1 word left out with it's index as the encryption key for the encryption process to secure the rest of the 23 words using the AES ([Advanced Encryption Standard](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard)) Encryption Algorithm.

### Seed Phrase Encryption

Illustrated below is an example. In this example our encrypted string is `U2FsdGVkX1+tz5nkrdI4eX34/tBFPy+MeSM2AHTTU2A+CEyqORTdZXqPF5TVXBfn`

![image-20230819075607363](Images/image-20230819075607363.png)

The User would keep the recovery info of user Password, 1 word and index and the personal information provided safely in any normal cloud storage or other methods, where its easily recoverable with 2FA authentication knowing that this piece of information isn't used for wallet recovery on its own and pose no meaning to anyone who finds it apart from the user.

### Seed Phrase Decryption

![image-20230819080601903](Images/image-20230819080601903.png)

## Recursive Encryption

To increase the computational cost against Brute Force attacks, we will be implementing a 3-stage recursive encryption process.

This solution will use 100 times + index of word times recursive encryption. The idea is to increase the decryption computational time cost to make Brute Force algorithms ridiculously expensive to attempt.

### Illustration

#### Step 1: First stage encryption process

`U2FsdGVkX1+tz5nkrdI4eX34/tBFPy+MeSM2AHTTU2A+CEyqORTdZXqPF5TVXBfn`

![image-20230819091501304](Images/image-20230819091501304.png)

#### Step 2: USing the output of the 1st encryption process

`U2FsdGVkX1+9Y/FrplmURq+6DxLddfn9lZx9zWka6yLydnXhqNBdX3DcNOGmleaq45kP3XuW/Oi2syaONiioXyE0O1te/9tC1NH4ITp2iAzbFsWzWFkaLtSKA19R80dL`

![image-20230819091529558](Images/image-20230819091529558.png)

Step 3: USing the output of the 2nd encryption process

`U2FsdGVkX19B1Qg9uG4LBlXTJ0I695Hj/ys9kRgUukgH7PTtESyIpEYFZRASyIt+JhKBBxsg/d7punQLGEGXcgOuTHPhgiJwgV4tANa7scKtRp2FJoJkjEDLhs3YG8K3DDUZ93Rs5t2Mozu1tK261Mp0Q2J4tvyir9a5agn6796EiIHmdakzfK1yPMrEqo3XC+KWhjZqoa4Lkusc3dzIQ==`

![image-20230819091559387](Images/image-20230819091559387.png)

### Reverse

#### Step 1

![image-20230819091713086](Images/image-20230819091713086.png)

#### Step 2

![image-20230819091738107](Images/image-20230819091738107.png)

#### Step 3

To get back the original 23 words.

![image-20230819091758045](Images/image-20230819091758045.png)

## Pilot Test with Cardano PreProd

### OnChain Code

https://github.com/rchak007/plutusAppsJambhala/blob/main/src/Contracts/SeedPhraseManager.hs

#### Datum

```haskell
data SeedPhraseDatum = SeedPhraseDatum
  { encryptedWordsWithIndex :: BuiltinByteString,
    ownerPKH :: PubKeyHash
  }

unstableMakeIsData ''SeedPhraseDatum
```

#### Parameterized

```haskell
data SeedPhraseParam = SeedPhraseParam
  { pInfoHash :: BuiltinByteString
  }
  deriving (Haskell.Show)
```

#### Redeemer

```haskell
data SeedPhraseRedeem = Unit ()
```

#### Validator

```haskell
mkRequestValidator :: SeedPhraseParam -> SeedPhraseDatum -> () -> ScriptContext -> Bool
mkRequestValidator sParam dat _ ctx =
  traceIfFalse "signedByOwner: Not signed by ownerPKH" signedByOwner
  where
    txinfo :: TxInfo
    txinfo = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner = txSignedBy txinfo $ ownerPKH dat
{-# INLINEABLE mkRequestValidator #-}
```

#### Reference Serialize

```haskell
-- referenceInstance :: Scripts.Validator
referenceInstance :: Validator
-- referenceInstance = Api.Validator $ Api.fromCompiledCode $$(PlutusTx.compile [||wrap||])
referenceInstance = Validator $ fromCompiledCode $$(PlutusTx.compile [||wrap||])
  where
    -- wrap l1 = Scripts.mkUntypedValidator $ mkRequestValidator (PlutusTx.unsafeFromBuiltinData l1)
    wrap l1 = mkUntypedValidator $ mkRequestValidator (PlutusTx.unsafeFromBuiltinData l1)
    
    
    

referenceSerialized :: Haskell.String
referenceSerialized =
  C.unpack $
    B16.encode $
      serialiseToCBOR
        -- ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unValidatorScript referenceInstance) :: PlutusScript PlutusScriptV2)
        ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ unValidatorScript referenceInstance) :: PlutusScript PlutusScriptV2)
```

#### Deploy

To get the .plutus script

https://github.com/rchak007/plutusAppsJambhala/blob/main/src/Deploy.hs

```haskell
scripts :: Scripts
scripts = Scripts {reference = Contracts.SeedPhraseManager.referenceSerialized}

main :: IO ()
main = do
  -- writeInitDatum
  -- writeContractDatum

  -- _ <- writeValidatorScript
  -- _ <- writeLucidValidatorScript
  I.writeFile "scripts.json" (encodeToLazyText scripts)
  putStrLn "Scripts compiled"

  return ()
```

### OffChain Encryption Lucid Code

https://github.com/rchak007/decentralSeedRecover/blob/main/pages/offChainV2.tsx

#### Parameter, Datum and Redeemer

```typescript
  const ParamsSchema = Data.Tuple([
      Data.Object({
        pInfoHash: Data.Bytes()
      })
    ]);
  type Params = Data.Static<typeof ParamsSchema>;
  const Params = ParamsSchema as unknown as Params;

  const MyDatumSchema = 
      Data.Object({
        encryptedWordsWithIndex: Data.Bytes(),
        ownerPKH: Data.Bytes()
      })

  type MyDatum = Data.Static<typeof MyDatumSchema>;
  const MyDatum = MyDatumSchema as unknown as MyDatum;

  const Redeemer = () => Data.void();
```

```typescript
// Function that will Lock the UTXO with this Datum 
const sLockEncryptedSeedPhrase = async () => {
    ....
    ....
```

#### Parameterize the script

Gets the personal info from UI and applies the hash to Plutus script.

```typescript
const decentralSeedPlutus = "59087f5908...."

      const paramInit :  Params = [{
        pInfoHash: fromText(hashedDataInString)
      }];
      
      
      const sValidator : SpendingValidator = {
        type: "PlutusV2",
        script: applyParamsToScript<Params>(
          decentralSeedPlutus,
          paramInit,
          Params,
        ),
      }

```

#### Encrypt SeedPhrase into Datum

```typescript
    seed23Words = seed23InputValue + ' ' + indexInputValue;
      passPhrase = passPhraseinputValue;

      const encryptedS = encryptD( seed23Words, passPhrase)   
      // const encryptedS = encryptSeedPassInfo( seed23Words, passPhrase);
      console.log("encryptedS = ", encryptedS)
      // const encryptedString = String(encryptedS)
      const encryptedString = "testSample1"

      const datumInit : MyDatum = 
          {
            encryptedWordsWithIndex: fromText(encryptedS) ,  
            ownerPKH: paymentCredential?.hash!    // pubkey hash
          };

...
  // Encrypt function
  function encryptD(text: string, key: string): string {
    const encrypted = CryptoJS.AES.encrypt(text, key);
    return encrypted.toString(); // Convert to Base64 string
  }
```

#### Create UTXO with datum

```typescript

...
....

		const tx = await lucid.newTx()
        .payToContract(sValAddress, {inline: Data.to( datumInit, MyDatum)}, {lovelace: BigInt(2000000)})
        .complete();
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log("Lock Test TxHash: " + txHash)
        settxHash(txHash);
        return txHash;
```

### OffChain Decryption Lucid Code

Function `  const sDecentralSeedRedeem = async () => {` decrypts the encrypted seed phrase.

```typescript
  const sDecentralSeedRedeem = async () => {
```

#### Get script address

Gets the personal info from UI and applies the hash to Plutus script to get the script address where UTXO is stored

```typescript
const decentralSeedPlutus = "59087f5908...."

      const paramInit :  Params = [{
        pInfoHash: fromText(hashedDataInString)
      }];
      
      
      const sValidator : SpendingValidator = {
        type: "PlutusV2",
        script: applyParamsToScript<Params>(
          decentralSeedPlutus,
          paramInit,
          Params,
        ),
      }
      
      // this gets our address
      const sValAddress = lucid.utils.validatorToAddress(sValidator)

```

#### Get Datum that has encrypted Seed phrase

```typescript
....
// we get the UTXO's at this address
const valUtxos = await lucid.utxosAt(sValAddress)
....
        for ( let i=0; i<valUtxos.length; i++ ) {
          console.log("I = ", i)
          const curr = valUtxos[i]
          console.log("Curr on i = ", curr)
          console.log("Curr datum on i = ", curr)
            if (!curr.datum) {
              console.log ("came here after the 1st IF")
              if (!curr.datumHash) {
                console.log ("came here after the 1st IF")
                continue;
              }
            }
```

#### Decrypt Seed phrase

```typescript
const encryptedWordsWithIndexFound = utxoInDatum.encryptedWordsWithIndex
  const ownerPubKeyHashFound = utxoInDatum.ownerPKH
  console.log("Encrypted words = ", encryptedWordsWithIndexFound)
  console.log("Owner pubkeyHash = ", ownerPubKeyHashFound)

  // Now Decrypt from the Encrypted word on Datum
  // const decryptedS = decryptD( toText(utxoInDatum.encryptedWordsWithIndex), passPhrase)
const decryptedS = decryptD( toText(encryptedWordsWithIndexFound), passPhrase)
  console.log("Decrupted 23 words with Index = ", decryptedS)
  varDecryptWord = decryptedS;
  const decryptWord = varDecryptWord;
  // console.log("decryptWord output = ", decryptWord)
  setdecryptWord(varDecryptWord);   // set the output retrieved words


  // Decrypt function
  function decryptD(encryptedText: string, key: string): string {
    // const keyWordArray = CryptoJS.enc.Utf8.parse(key);
    const decrypted = CryptoJS.AES.decrypt(encryptedText, key);
    // const decrypted = CryptoJS.PBKDF2(encryptedText, key);
    return decrypted.toString(CryptoJS.enc.Utf8);
  }
```

### UI for the Dapp

![image-20231222153440403](Images/image-20231222153440403.png)

![image-20231222153453821](Images/image-20231222153453821.png)

### Lock and Encrypt Seed Phrase

After providing the 23 / 24 words of seed phrase, index of left out word, Pass phrase for recovery and personal info (for Unique script address) the Dapp will put the encrypted seed phrase with index onChain as Datum.

https://preprod.cardanoscan.io/transaction/d28013ffef2018523b125e2bb81a33eb45422f04877480a662bdcdd1ba45478a

![image-20231201174329324](Images/image-20231201174329324.png)

![image-20231201174356755](Images/image-20231201174356755.png)

### Datum with Encrypted Seed Phrase 23 words + index of word left out

https://preprod.cardanoscan.io/datumInspector?datum=d8799f5f5840553246736447566b58313830734f713177526b594b745655486674437a4d6d534f7a2f632b6e6f4969754869792f7653683178686b5646626974794e796d2f39584056635975577a57593763527573756544637942546b6c596a485a4a6a6539445363694734417a4f386d70372b365936336571645161345030454155516f4e69555840716b4b33436d7246584c68504b7555757a6c44665033506369743150504d39306c666263382f6f7956322b5177532f4f45632f5a413841783134577a47683553582c6b4b6768384634764e534c7634586d59556444682f2b526a43514b495552695472335745783834754f57343dff581ce9efb9bb50fc3531da4955a7f4d06b22951cbcb373368f978640c3f4ff

![image-20231201174445388](Images/image-20231201174445388.png)

#### Console log

![image-20231201174148542](Images/image-20231201174148542.png)

### Decrypt Seed Words and Redeem

User will provide the Personal info to locate the script address, the passphrase to decrypt the encrypted words.

https://preprod.cardanoscan.io/transaction/b633d6f4a4472f872b5d37f3f46fffdb3fb8841e024f2e58f17ce09c3a31861a

![image-20231201174913684](Images/image-20231201174913684.png)

This will decrypt the seed phrase and also redeem the min Ada stored at the script.

This way the user has now recovered their lost Seed phrase.

![image-20231201175006897](Images/image-20231201175006897.png)
