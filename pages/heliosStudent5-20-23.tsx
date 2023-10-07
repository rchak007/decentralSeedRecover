const sDeposit = async () => {
    if (lucid) {

      const sValCbor = "589b5899010000222323232325333573466e1d200200215333573466ebcd5d0991aab9e375400200e6ae84c8d55cf1baa00100614985854cd5ce2481445061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f536d616c6c56616c696461746f722e68733a35333a352d3432001635573c0046aae74004dd51aba135744002646aae78dd50008009"
      const sValidator : SpendingValidator = 
        {type:"PlutusV2", script: sValCbor}
      
      const sValAddress = lucid.utils.validatorToAddress(sValidator)

      const datum = Data.to(
        new Constr(0, [BigInt(24)])
      )

      const utxos = await lucid.wallet.getUtxos();
      
      const tx = await lucid
        .newTx()
        .payToContract(sValAddress, {inline: datum}, {lovelace: BigInt(100_000_000)})
        .complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log("Lock Test TxHash: " + txHash)
        return txHash;

    }

    // deposit

  }

  // spend

  const sSpend = async () => {
    if (lucid) {

      const sValCbor = "589b5899010000222323232325333573466e1d200200215333573466ebcd5d0991aab9e375400200e6ae84c8d55cf1baa00100614985854cd5ce2481445061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f536d616c6c56616c696461746f722e68733a35333a352d3432001635573c0046aae74004dd51aba135744002646aae78dd50008009"
      const sValidator : SpendingValidator = 
        {type:"PlutusV2", script: sValCbor}
      
      const sValAddress = lucid.utils.validatorToAddress(sValidator)

      const valUtxos = await lucid.utxosAt(sValAddress)

      const redeemer = Data.to(
        new Constr(0, [BigInt(24)])
      )

      let found = undefined

      for ( let i=0; i<valUtxos.length; i++ ) {
        const curr = valUtxos[i]
          if (!curr.datum) {
            if (!curr.datumHash) {
              continue;
            }
            curr.datum = await lucid.datumOf(curr)
          }
        const pDatum : Constr <Data> = Data.from(curr.datum!)

        if (pDatum.fields[0] === BigInt(24)) {
          found = curr
        } 
      }

      if (!found) throw new Error("Naughty Datum")

      const utxos = await lucid.wallet.getUtxos();
      
      const tx = await lucid
        .newTx()
        .attachSpendingValidator(sValidator)
        .collectFrom([found], redeemer)
        .complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log("Collect Test TxHash: " + txHash)
        return txHash;

    }

  }

    // spend & deposit
    // schema Datum
    const PasswordDatum = Data.Object({
      password: Data.Integer()
    })
    type PasswordDatum = Data.Static<typeof PasswordDatum>

    const sSpendAndDeposit = async () => {
      if (lucid) {
  
        const sValCbor = "589b5899010000222323232325333573466e1d200200215333573466ebcd5d0991aab9e375400200e6ae84c8d55cf1baa00100614985854cd5ce2481445061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f536d616c6c56616c696461746f722e68733a35333a352d3432001635573c0046aae74004dd51aba135744002646aae78dd50008009"
        const sValidator : SpendingValidator = 
          {type:"PlutusV2", script: sValCbor}
        
        const sValAddress = lucid.utils.validatorToAddress(sValidator)
  
        const valUtxos = await lucid.utxosAt(sValAddress)
  
        const datum = Data.to<PasswordDatum>({password: BigInt(24)}, PasswordDatum)

        const redeemer = Data.to<PasswordDatum>({password: BigInt(25)}, PasswordDatum)
  
        let found = undefined
  
        for ( let i=0; i<valUtxos.length; i++ ) {
          const curr = valUtxos[i]

          if (!curr.datum) {
            if (!curr.datumHash) {
              continue;
            }
            curr.datum = await lucid.datumOf(curr)
          }
                                          
          const pDatum = Data.from<PasswordDatum>(curr.datum!, PasswordDatum)
  
          if (pDatum.password == BigInt(25)) {
            found = curr
          } 
        }
  
        if (!found) throw new Error("Naughty Datum")
  
        const utxos = await lucid.wallet.getUtxos();
        
        const tx = await lucid
          .newTx()
          .attachSpendingValidator(sValidator)
          .collectFrom([found], redeemer)
          .payToContract(sValAddress, {inline: datum}, {lovelace: BigInt(100_000_000)})
          .complete();
  
          const signedTx = await tx.sign().complete();
          const txHash = await signedTx.submit();
          console.log("Collect Test TxHash: " + txHash)
          return txHash;
  
      }
  
    }