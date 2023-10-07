import type { NextPage } from 'next'
import Head from 'next/head'
import WalletConnect from '../components/WalletConnect'
import { useStoreActions, useStoreState } from "../utils/store"
import Link from 'next/link'
import { useState, useEffect } from 'react'
import { getAssets } from "../utils/cardano";
import NftGrid from "../components/NftGrid";
import initLucid from '../utils/lucid'
import { Lucid, Credential, TxHash, Lovelace, Constr, SpendingValidator, Data, fromText, Unit, MintingPolicy, PolicyId, Address, UTxO, applyParamsToScript, Assets, ScriptHash, Redeemer, paymentCredentialOf, KeyHash, generatePrivateKey, getAddressDetails, toUnit } from 'lucid-cardano'
import * as helios from '@hyperionbt/helios'
import {fromAssets, toAssets, union, Value} from "../utils/valueUtils"
import { fromAddress, OfferDatum, OfferInfo, toAddress } from '../utils/offerUtils'
import { kMaxLength } from 'buffer'


// const cborHex = "590a1901000032323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323222253335734646464646a666ae68cdc39aab9d375400c9000092999ab9a337106eb4c038dd60041bad300a37580102660822660822660822660766eaccccc0b0014d5d09bac0080014a06eaccc0e8dd59981598069bac0084c0101010037566605660126eb0020c038dd600409981d9bab333302c0050020014a06eacc0a9301051a004c4b4000133029375a0086eb4c030dd60040998130029aba137580102660822660822660822660822660822660766eaccccc0b0014d5d09bac0080014a06eaccc0acc024dd60041ba8337026eb4c038dd60041bad301037580102660766eaccccc0b0014c03cdd6004000a50375666056601a6eb0021300101010013303b3756666605800a004002940dd598152601051a007270e000133029375a0086eb4c030dd60040998128998130029aba1375801026604c00a601e6eb00204cc0944cdd798081bac0084c0101000013303b3756666605800a60226eb0020005281bab3302b3009375801060206eb0020494ccd5cd19b88375a60680026eb4c028dd60040a5015333573466e24dd6981a0009bad300e3758010294054ccd5cd19b88375a660486eacc0dcc0dcc08c019300106d8799f4040ff00483030e4c1c5280992999ab9a3302a375a601e6eb00252000133042133042133042133042133042133042133023375a6604a6eacc0e0c0e0c09001d30106d8799f4040ff00482036b10244cc0f0dd59999810003002000a513756660766eaccc0ecdd59981618071bac0094c0101010037566605860146eb0024c0d4008dd59815a601051a009896800013303c3756666605a00c60206eb0024009281bab3303b37566605860146eb0024c03cdd60049bab302b4c1051a002625a0001337106eb4014dd698069bac00913302a375a00a6eb4c030dd6004899b89337026eb4c07cc078018dd6980e980f00324101756e0626604e00c60660042660842660842660842660842660842660466eb4cc094dd5981c181c1812003a60106d8799f4040ff00483030e4c1c4cc0f0dd59999810003002000a513756660766eaccc0ecdd59981618071bac0094c0101010037566605860146eb0024c0d4008dd59815a601051a00989680001337106eb4014dd698069bac00913302a375a00a6eb4c030dd6004899b89337026eb4c07cc078018dd6980e980f00324101756e0626604e00c60660046e9ccd5d01aba1375801066ae80d5d09aba2375801066ae80c024dd600419aba0300a375801066ae80d5d0980c9bac00833574060166eb0020cd5d018061bac008335740601a6eb0020cd5d0181a00099aba03032001335740606e00266ae80d5d0981400099aba03011375801066ae80d5d09aba23012375801097ae00064c011e581c89e0e1706f1b825460bd49954cc04dad5159e522e649a1ad17f3988600302f301e302f302f3034302000330183019001302d0011498588d5d0980980091aba13011001235742601c00246ae84c0300048d5d0980500091aba13008001235742600c00246ae84c0100048d5d0980100091aba230020012357446ae88c0080048d5d1180100091aba23002001235744600400246ae88c0080048d5d1180100091aba23002001235744600400246ae88d5d10009180b980b980b80091aba1300b001230153015301a0012222301b3758a666ae680044dd39980c00211981389980300080209980a80080189ba733018004233027133006001004133014001375260260064464a666ae68cdc39aab9d37540029001099baf0023015300400114a060286028004466022002900111180f19b88002001232333001001375860226022004466ebcc048004c048c05c00c8894ccd5cd1aba30021615333573460026ae840084d5d080109998018019aba2002001223233300100122533357346ae8c0045854ccd5cd19baf35573a6ae84004c0600104d55cf1aba100113300200235744002006444a666ae68d5d18008b0a999ab9a3375e6aae74d5d080098098020998010011bab35573c6ae840044ccc00c00c008d5d1000912999ab9a500214a200244660386eb0d5d09aba2300300223375e00200446ae88d5d11aba235744600400246ae88c03800488c05ccdc4801000919801260106d8799f4040ff00001225333573466e1d2000375a0022980101a0001374c66ae80cdd818048011ba633574066ec0c0380080052f5bded8c097adef6c602222300e3758a666ae680044dd39980580211980d09980480080209980400080189ba73300b00423301a1330090010041330070013752600c00646e50dd98009119baf325333573466e1cd55ce800a400426ae84d55cf0008a601014000375460100040024464a666ae68cdc39aab9d001480104cdd78011aba135573c0022940dd5180380111192999ab9a3370e6aae74dd5000a4000266ebc008c014cc010005200014a06008600800444a666ae68cdc39aab9d375400400220042c46ae84c02000488c8ccc004004dd618020018011112999ab9a35746004297ae015333573460026ae840084cd5d01aba10023330030033574400400226660060066ae880080048d5d0980100091aba230040012323333001001002223300837560046eacc014005300101a0002222533357346ae8c00c40044cccc010010d5d1001801198010009aba1003235742600400246ae88c0080048d55cf1baa0012232374c6660020026601000600497adef6c60222533357346ae8c00840044cc88c94ccd5cd1aba30011002133574066ec000cdd30008011991191998008009980780180125eb7bdb1808894ccd5cd1aba3002100113322325333573466e1c00520001002133574066ec000cdd400080119b803301100700233011006002357420046660060066ae88008004cc02401c008cc024018008d5d08011998018019aba20020012232330010013300700300222533357346ae8c0045288992999ab9a3008332232330010013300c00300222533357346ae8c0045288992999ab9a300d300d337106601800a0026601800800229404cc00c00cd5d10011aba1001330060050013300600400114a02660060066ae88008d5d0800911919800800801912999ab9a35746002297adef6c6015333573466ebcd55ce9aba1001003137566aae78d5d08008998010011aba20012232333001001003002222533357346ae8c0085200015333573466ebcd55ce9aba10020011375a6aae78d5d080109998018019aba20020012333573400294128911919191998008009998010010018020019112999ab9a357460022004266ae80d5d08009998018018011aba2001222533357346ae8c00452f5c0264a666ae68cc0200148cdd780080109998020020019aba200213357400026660080080066ae88008d55ce9aba1001323300100100322533357346ae8c00452f5c0266ae80d55ce9aba100133002002357440024464666002002006004444a666ae68d5d18010a5015333573460026ae8400852889998018019aba20020012253335734a0040022941"


// console.log("Logging deserialization of UPLC");
// console.log(helios.deserializeUplc(`{"type": "PlutusScriptV1", "cborHex": "${cborHex}"}`))

const Helios: NextPage = () => {
  const walletStore = useStoreState((state: any) => state.wallet)
  const [nftList, setNftList] = useState([])
  const [lucid, setLucid] = useState<Lucid>()
  const [script, setScript] = useState<SpendingValidator>()
  const [scriptAddress, setScriptAddress] = useState("")
  const [ariadyTxHash, setAriadyTxHash] = useState("")
  const [efrainTxHash, setEfrainTxHash] = useState("")


  useEffect(() => {
    if (lucid) {
      ;
    } else {
      initLucid(walletStore.name).then((Lucid: Lucid) => { setLucid(Lucid) })
    }
  }, [lucid])

  // data ToyRedeemer = ToyRedeemer{key :: Integer}

  const sDeposit = async () => {
    if (lucid) {
      // const sValCbor = "589b5899010000222323232325333573466e1d200200215333573466ebcd5d0991aab9e375400200e6ae84c8d55cf1baa00100614985854cd5ce2481445061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f536d616c6c56616c696461746f722e68733a35333a352d3432001635573c0046aae74004dd51aba135744002646aae78dd50008009"
      const sValCbor = "582858260100003232322225333573466ebcc010c014008c010c01400c526165742460046ea800555cf1"
      
      const sValidator : SpendingValidator = {
        type: "PlutusV2",
        script: sValCbor
      }
    
      const sValAddress = lucid.utils.validatorToAddress(sValidator)
      
      const datum = Data.to(
        new Constr(0, [BigInt(24)])
      )
      console.log("PayToScript Address: ", sValAddress )
      
      const utxos = await lucid.wallet.getUtxos()
      const tx = await lucid.newTx()
        // .payToContract(sValAddress, {inline: datum}, {lovelace: BigInt(100_000_000)})
        .payToContract(sValAddress, {inline: datum}, {lovelace: BigInt(100000)})
        .complete();

        console.log("Went into sDeposit module:After payto ")
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
      const sValCbor = "582858260100003232322225333573466ebcc010c014008c010c01400c526165742460046ea800555cf1"
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
          const sValCbor = "582858260100003232322225333573466ebcc010c014008c010c01400c526165742460046ea800555cf1"
          const sValidator : SpendingValidator = 
            {type:"PlutusV2", script: sValCbor}
          const sValAddress = lucid.utils.validatorToAddress(sValidator)
          const valUtxos = await lucid.utxosAt(sValAddress)
          const datum = Data.to<PasswordDatum>({password: BigInt(24)}, PasswordDatum)
          // const redeemer = Data.to<PasswordDatum>({password: BigInt(25)}, PasswordDatum)
          const redeemer = Data.to<PasswordDatum>({password: BigInt(24)}, PasswordDatum)
    
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
    
            // if (pDatum.password == BigInt(25)) {
            if (pDatum.password == BigInt(24)) {
              found = curr
            } 
          }
    
          if (!found) throw new Error("Naughty Datum")
    
          const utxos = await lucid.wallet.getUtxos();
          
          const tx = await lucid
            .newTx()
            .attachSpendingValidator(sValidator)
            .collectFrom([found], redeemer)
            .payToContract(sValAddress, {inline: datum}, {lovelace: BigInt(1000000)})
            .complete();
    
            const signedTx = await tx.sign().complete();
            const txHash = await signedTx.submit();
            console.log("Collect Test TxHash: " + txHash)
            return txHash;
    
        }
      }

  return (
    <div className="px-10">
      <div className="navbar bg-base-100">
        <div className="flex-1">
          <Link href="/" className="btn btn-ghost normal-case text-xl">Cardano</Link>
        </div>
        <div className="flex-none">
          <WalletConnect />
        </div>
      </div>
      <div>Address: {walletStore.address}</div>
      <div className='m-10'>
        <p> 
          Emurgo example
        </p>
      </div>
      <div className="mx-60 my-10">
        <button className="btn btn-primary m-5" onClick={() => { sDeposit() }} >sDeposit</button>
        <button className="btn btn-secondary m-5" onClick={() => { sSpend() }}>sSpend</button>
        <button className="btn btn-secondary m-5" onClick={() => { sSpendAndDeposit() }}> sSpendAndDeposit</button>
        {/* <button className="btn btn-secondary m-5" onClick={() => { unlockGuess() }}>Unlock Guess</button>
        <button className="btn btn-secondary m-5" onClick={() => { deploySequentialMint("Boo") }}>Deploy Sequential Mint</button>
        <button className="btn btn-secondary m-5" onClick={() => { getOffers(paymentCredentialOf(walletStore.address)) }}>Unlock Guess</button> */}
      </div>
    </div>
  )
}

export default Helios
