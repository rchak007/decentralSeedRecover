import type { NextPage } from 'next'
import Head from 'next/head'
import WalletConnect from '../components/WalletConnect'
import { useStoreActions, useStoreState } from "../utils/store"
import Link from 'next/link'
import { useState, useEffect } from 'react'
import { getAssets } from "../utils/cardano";
import NftGrid from "../components/NftGrid";
import initLucid from '../utils/lucid'
import {  Lucid, 
          Credential, 
          TxHash, 
          Lovelace, 
          Constr, 
          SpendingValidator, 
          Data, 
          fromText, 
          Unit, 
          MintingPolicy, 
          PolicyId, 
          Address, 
          UTxO, 
          applyParamsToScript, 
          Assets, 
          ScriptHash, 
          Redeemer, 
          paymentCredentialOf, 
          KeyHash, 
          generatePrivateKey, 
          getAddressDetails, 
          toUnit, 
          datumJsonToCbor,
        } from 'lucid-cardano'
import * as helios from '@hyperionbt/helios'
import {fromAssets, toAssets, union, Value} from "../utils/valueUtils"
import { fromAddress, OfferDatum, OfferInfo, toAddress } from '../utils/offerUtils'
import { kMaxLength } from 'buffer'

// import { readFirstLineFromFile } from '../utils/fileUtils';

import * as CryptoJS from 'crypto-js';


// https://github.com/HarunJr/decentralSeedRecover/blob/main/code/SeedPhraseManager/assets/plutus-scripts/seed-phrase.plutus
// const decentralSeedPlutus = "5908b55908b201000033232323322323232323232323232332232332232323232323222323222323253353232325335323235002222222222222533533355301a12001321233001225335002210031001002502325335333573466e3c0380040c00bc4d409400454090010840c040b8d401088004d40048800840844cd5ce249257369676e656442794f776e65723a204e6f74207369676e6564206279206f776e6572504b48000203333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd406c070d5d0a80619a80d80e1aba1500b33501b01d35742a014666aa03eeb94078d5d0a804999aa80fbae501e35742a01066a0360506ae85401cccd5407c0a5d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40cdd69aba150023034357426ae8940088c98c80d8cd5ce01c81c01a09aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a819bad35742a00460686ae84d5d1280111931901b19ab9c039038034135573ca00226ea8004d5d09aba2500223263203233573806a06806026aae7940044dd50009aba1500533501b75c6ae854010ccd5407c0948004d5d0a801999aa80fbae200135742a004604e6ae84d5d1280111931901719ab9c03103002c135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a008602e6ae84d5d1280211931901019ab9c02302201e3333573466e1cd55ce9baa0054800080848c98c807ccd5ce01101080e9999ab9a3370e6aae7540192000233221233001003002375c6ae854018dd71aba135744a00c464c6403c66ae70084080070407c4c98c8074cd5ce249035054350001f135573ca00226ea80044d55cf280089baa0013200135501822112225335001135003220012213335005220023004002333553007120010050040011232230023758002640026aa030446666aae7c004940288cd4024c010d5d080118019aba2002018232323333573466e1cd55cea80124000466442466002006004601c6ae854008c014d5d09aba2500223263201633573803203002826aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea80124000466442466002006004602e6ae854008cd403c058d5d09aba2500223263201b33573803c03a03226aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931900e99ab9c02001f01b01a019135573aa00226ea8004d5d0a80119a805bae357426ae8940088c98c805ccd5ce00d00c80a89aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5405488c8cccd55cf80112804119a8039991091980080180118031aab9d5002300535573ca00460086ae8800c0584d5d080088910010910911980080200189119191999ab9a3370ea002900011a80398029aba135573ca00646666ae68cdc3a801240044a00e464c6402466ae7005405004003c4d55cea80089baa0011212230020031122001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402066ae7004c04803803403002c4d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6401866ae7003c0380284d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200a33573801a01801026ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201333573802c02a02202001e01c01a01801626aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900619ab9c00f00e00a009135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8024cd5ce00600580380309aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900519ab9c00d00c008007006135573aa00226ea80048c8cccd5cd19b8750014800880188cccd5cd19b8750024800080188c98c8018cd5ce00480400200189aab9d37540029309100109100089000a4810350543100112323001001223300330020020013351223002489383536333730653461363633396639323163336265616664313932643665313638616565653862636636326233616638363839643831396430002123001002200101"

// for now use Plutarch mine
const decentralSeedPlutus = "58a558a3010000222253335734664646460044660040040024600446600400400244a666aae7c0045280a999ab9a3375e6ae8400400c528898011aba200137526eb8d5d09aba23235573c6ea800400cdd6191aba132357446ae88d5d11aba2357446ae88d5d11aba20013235573c6ea8004004d5d0991aab9e37540020022930a99ab9c4911e646174756d20616e6420706172616d2061726520646966666572656e7420001601"


const minAda = 2000000;
const minAdaBigInt = 2000000n;

const offChain: NextPage = () => {
  const walletStore = useStoreState((state: any) => state.wallet)
  const [nftList, setNftList] = useState([])
  const [lucid, setLucid] = useState<Lucid>()
  const [script, setScript] = useState<SpendingValidator>()
  const [scriptAddress, setScriptAddress] = useState("")
  const [ariadyTxHash, setAriadyTxHash] = useState("")
  const [efrainTxHash, setEfrainTxHash] = useState("")
  const [inputValue, setInputValue] = useState('');
  const [inputPrivateKey, setInputValuePrivateKey] = useState('');
  const [privAddress,setPrivAddress] = useState<string>('');
  const [txHash,settxHash] = useState<string>('');
  const [firstLine, setFirstLine] = useState<string>('');
  const [readSecretData, setReadSecretData] = useState<any>(null);

  useEffect(() => {
    if (lucid) {
      ;
    } else {
      initLucid(walletStore.name).then((Lucid: Lucid) => { setLucid(Lucid) })
    }
  }, [lucid])


  const gpInfoHash = fromText("MyInfo1-10-26-23-2106")

  // Type definition could be auto generated from on-chain script
  const ParamsSchema = Data.Tuple([
                        Data.Object({ 
                          pInfoHash: Data.Bytes()
                        })
                      ])
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

  const sLockEncryptedSeedPhrase = async () => {
    if (lucid) {
      const sValCbor = decentralSeedPlutus;

      const { paymentCredential } = lucid.utils.getAddressDetails(
        await lucid.wallet.address(),
      );      

      console.log("Payment pub key hash = ", paymentCredential?.hash!)     

      const paramInit :  Params = [{
        pInfoHash: gpInfoHash
      }];
      
      
      const sValidator : SpendingValidator = {
        type: "PlutusV2",
        script: applyParamsToScript<Params>(
          decentralSeedPlutus,
          paramInit,
          Params,
        ),
        
      }

      const sValAddress = lucid.utils.validatorToAddress(sValidator)
      console.log("PayToScript Address of Decentralized seed recovery Script: ", sValAddress )

      // Now do encryption
      const seed23Words = 'custom help female park blush clutch fancy lion fence innocent rebuild amount tip custom help female park blush clutch fancy lion fence innocent ';   // for now 13 words
      let passPhrase = 'myOwnPassword$123';
      encryptSeedPassInfo( seed23Words, passPhrase).then(encryptedString => {
        console.log(encryptedString);
      });

      const deadlineCalc = (lucid.utils.unixTimeToSlot(Date.now() + 1000000))

      console.log("deadline = ", deadlineCalc)

      const timestampInSeconds = Math.floor(Date.now() / 1000);
      console.log("time stamp = ", timestampInSeconds)
      const dateTimeString = fromText(timestampInSeconds.toLocaleString());
      console.log("dateTime in string = ", dateTimeString)

//    for now for testing including contributions map filled in.
      const datumInit : MyDatum = 
          {
            // encryptedWordsWithIndex: dateTimeString,
            encryptedWordsWithIndex: paymentCredential?.hash!,
            ownerPKH: paymentCredential?.hash!    // pubkey hash
          };



      const datumData = Data.to(datumInit, MyDatum)
      // const datumData = Data.to(datumInit, MyDatum)
      // const datumData = Data.to<MyDatumSchema>( datumInit, MyDatumSchema)
      console.log("Datum as Data = ", datumData)
      
      // lovelace: Lovelace;
      const utxos = await lucid.wallet.getUtxos()
      // lovelace: Lovelace;

      const tx = await lucid.newTx()
        // .payToContract(sValAddress, {inline: Data.to( datumInit1, MyDatum)}, {lovelace: BigInt(4000000), [currency1 + fromText("MyCrowdFund")]: 1n })
        .payToContract(sValAddress, {inline: Data.to( datumInit, MyDatum)}, {lovelace: BigInt(2000000)})
        .complete();

        console.log("Went into sDeposit module:After payto ") 
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log("Lock Test TxHash: " + txHash)
        settxHash(txHash);
        return txHash;
    
    }   // end if Lucid
  }   // End sdeposit





  // const sContributeCrowdFund = async () => {
  const sDecentralSeedRedeem = async () => {
      if (lucid) {
        const sValCbor = decentralSeedPlutus;

        const { paymentCredential } = lucid.utils.getAddressDetails(
          await lucid.wallet.address(),
        );      
  
        console.log("Payment pub key hash = ", paymentCredential?.hash!)     
  
        const paramInit :  Params = [{
          pInfoHash: gpInfoHash
        }];
        
        
        // "590a5f590a5c01000032323232323232323232323232323232323232323232323232323232323232323232323232323232323222232323232323232323232323253330343370e90010010991919299981b99b87480080084c8c8c8c8c8c8c8c8c8c8c8c8c8c94ccc114cdc3a4004004264646464646464646464646464a6660a466e1d20000021613232323232323232323232533305d3370e90000010a99982ea9982099b8733303d375660c002a6eb8c180048dd71830008a40042a6608266e1cc100dd5983000a9bad306000f153304133043375660c002a6088666090e00ccc13c138138dd698300079998279bae3060012375c60c002290010998219bab3060015304433304870066609e09c09c6eb4c18003cccc13cdd718300091bae30600114800854ccc1754cc104cdc399981e9bab306000a375c60c000e6eb8c180019200213304333304f04e04e3039375860c060be0086088666090e00ccc13c138138c0e4dd61830182f807999827827027181d01b8a99982e99821982219982438033304f04e04e375a60c001e66609e09c09c607406e66609e09c09c6eb4c18001054ccc1754cc104cdd79830004983000a0a9982099baf3060008306001315330413375e60c000a60c00202a6608266ebcc18001cc1800484cdd7983000318300088a99982e99821982219982438033304f04e04e303a037375660c002a6eacc18002854ccc174cdd7983000b18300058a99982e9981e191919299983019b87480000084c8c198c0fc004c18c00453011e581c0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a0030630023063001375406e02e2a6660ba66e25200433303d375660c001409c09c2930b0b0b0b0b0b0b0b0a99982e9981e183001300b8a99982ea9982099b89375a60c00446eb4c18008454cc104cdc49bad3060010375a60c001e266e24dd698300079bad306000f1533305d5330413370e66607a6eacc180054dd718300091bae30600114800854cc104cdc398201bab3060015375a60c001e2a66082660866eacc180054c110ccc121c01998278270271bad306000f33304f375c60c00246eb8c1800452002133043375660c002a6088666090e00ccc13c138138dd698300079998279bae3060012375c60c002290010a99982e99b88375a60c00266eb4c1800645261616161630600023060001375406860b600260b400260b200260b000260ae002609a6096609860ae60ac00260aa002609664a6660a80022c264a6660aa002260ae0042c60aa00266460a044a6660ac00220a6264a6660ac60080022660aa002600660b00042600660b000460b0002644646464a6660ae66e1d200000214a0266e3cdd7182d000802982d001182d0009baa323058304e0013057304d001375c60aa0026eb0c154050c154008c154004dd5191829982480098290041828000982780098270009826800982600098211820182098261825800982500098201991198231129998260008b09929998261919baf374e608c00a6e9cc118004c13c0044c13cc1380044c00cc138008c110c138004008dd61825005182500a1bac30493048007303e304800616304800230480013754608a0026074608800260726086002646084608460846084002608200260806080002606c608001a607c002607a00260780026076002607400260600242c607400460740026ea8c0dcc0d801058c0dc008c0dc004dd5181a18198009814804a99981799b87480000084c8c8c8c8c8c8c9265333036001149858c0d80194ccc0cccdc3a400000426464a66606a66e1cdc6800a40702646464646493299981d0008a4c2c60740066eb4004c0e4004c0dc00c58dd7000981b0008b181b001181b0009baa00130320011533302f3370e900100109924ca6660600022930b0b181900118190009baa006533302b3370e900000109919299981699b87371a002901c0991919299981819b89480000044c8c8c8c94ccc0d14cc0b8cdc3800a4000266e1c005203813232325333037337126e340052040132323232323232323232324994ccc1080045261630420033303b23232323200553330433370e900000109919299982299b87371a002901c0991919191924ca6660940022930b18250019bad0013049001304700316375c002608c0022c608c004608c0026ea8004dd60009820800981f8019bad001303e001303c003375a002607600260720062c6eb8004c0e0004c0d801058dc68009bae0013034001303200316375a0026062002605e0062c6eb8004c0b800458c0b8008c0b8004dd500198109129998138008a4000266e00dd6981518021814800980118140009191919299981399b874800800852000132375a6058600c0026054002605400460540026ea80048c8cdd818138009813981300098139baa0012301f22533302500114a02a66604866ebcc09c00400c5288980118130009111999802001240004666600a00490003ad3756002006460046ea40048888cc07c894ccc094004401454ccc090cdd79814981380080309802181418138008980118130008009299980f8008a4000266603c66ebcc08cc084004dd48079bad3022302137566044604200290001119980f0010008018a50223375e6e98008dd3000919801119299980e180280089128008911801001998029299980e19baf00137509000091280089118010018008009180191998011bab001232223002003374c002244a002ae8c88cc054894ccc06c00440404c8ccc014c07cc0780088cc06ccdd81810980f80180080108009801180e0008009111998021119980380280100080100091801911ba63300337560046eac0048c00888dd4198019bad002375a0024446666008006440040040024601e6004002446464466002006004444a66602e00226602a0060042646464a66603266ebc0080044cc060cdd800119804980e803180e80199980411001002980d8020a99980c99b90375c0046eb80044cc060018cccc0208800400cc06c0100144cc06000ccccc02088004018014c06c010c074008c070010c064004894ccc05400840044cccc00c88004c05c008c0580080052210022253330113370e002900008038998020019980280100091198021ba9002374c00244660066ea4008dd4000911980619bb00020010034bd6f7b630119191919002a99980699b87480000084c8c94ccc03ccdc39b8d001480e04c8c8c94ccc048cdc4a4000002264646464a66602ca6602066e1c005200013370e002901c0991919299980c99b89371a00290200991919191919191919191924ca6660480022930b18120019980e919191919002a99981299b87480000084c8c94ccc09ccdc39b8d001480e04c8c8c8c8c926533302c001149858c0b000cdd6800981580098148018b1bae0013028001163028002302800137540026eb0004c08c004c08400cdd68009810000980f0019bad001301d001301b00316375c002603400260300082c6e34004dd7000980b000980a0018b1bad0013013001301100316375c00260200022c602000460200026ea80048c8c8c94ccc030cdc3a40080042601e0022c601e004601e0026ea80048c030dd50009198038008010a512300222533300800110051330063003300a001300230090012323002233002002001230022330020020014bd702ba05734aae7d5d12ba15573caae741"  // Crowd Fund
        const sValidator : SpendingValidator = {
          type: "PlutusV2",
          script: applyParamsToScript<Params>(
            // sValCbor,
            "5908b55908b201000033232323322323232323232323232332232332232323232323222323222323253353232325335323235002222222222222533533355301a12001321233001225335002210031001002502325335333573466e3c0380040c00bc4d409400454090010840c040b8d401088004d40048800840844cd5ce249257369676e656442794f776e65723a204e6f74207369676e6564206279206f776e6572504b48000203333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd406c070d5d0a80619a80d80e1aba1500b33501b01d35742a014666aa03eeb94078d5d0a804999aa80fbae501e35742a01066a0360506ae85401cccd5407c0a5d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40cdd69aba150023034357426ae8940088c98c80d8cd5ce01c81c01a09aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a819bad35742a00460686ae84d5d1280111931901b19ab9c039038034135573ca00226ea8004d5d09aba2500223263203233573806a06806026aae7940044dd50009aba1500533501b75c6ae854010ccd5407c0948004d5d0a801999aa80fbae200135742a004604e6ae84d5d1280111931901719ab9c03103002c135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a008602e6ae84d5d1280211931901019ab9c02302201e3333573466e1cd55ce9baa0054800080848c98c807ccd5ce01101080e9999ab9a3370e6aae7540192000233221233001003002375c6ae854018dd71aba135744a00c464c6403c66ae70084080070407c4c98c8074cd5ce249035054350001f135573ca00226ea80044d55cf280089baa0013200135501822112225335001135003220012213335005220023004002333553007120010050040011232230023758002640026aa030446666aae7c004940288cd4024c010d5d080118019aba2002018232323333573466e1cd55cea80124000466442466002006004601c6ae854008c014d5d09aba2500223263201633573803203002826aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea80124000466442466002006004602e6ae854008cd403c058d5d09aba2500223263201b33573803c03a03226aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931900e99ab9c02001f01b01a019135573aa00226ea8004d5d0a80119a805bae357426ae8940088c98c805ccd5ce00d00c80a89aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5405488c8cccd55cf80112804119a8039991091980080180118031aab9d5002300535573ca00460086ae8800c0584d5d080088910010910911980080200189119191999ab9a3370ea002900011a80398029aba135573ca00646666ae68cdc3a801240044a00e464c6402466ae7005405004003c4d55cea80089baa0011212230020031122001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402066ae7004c04803803403002c4d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6401866ae7003c0380284d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200a33573801a01801026ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201333573802c02a02202001e01c01a01801626aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900619ab9c00f00e00a009135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8024cd5ce00600580380309aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900519ab9c00d00c008007006135573aa00226ea80048c8cccd5cd19b8750014800880188cccd5cd19b8750024800080188c98c8018cd5ce00480400200189aab9d37540029309100109100089000a4810350543100112323001001223300330020020013351223002489383536333730653461363633396639323163336265616664313932643665313638616565653862636636326233616638363839643831396430002123001002200101",
            paramInit,
            Params,
          ),
          
        }

        // const sValidator : SpendingValidator = 
        //   {type:"PlutusV2", script: sValCborCrowdFund}
        const sValAddress = lucid.utils.validatorToAddress(sValidator)
        console.log("Validator script address = ", sValAddress)
        const valUtxos = await lucid.utxosAt(sValAddress)
        const redeemer = Data.to(
          new Constr(0, [])
        )

        let found = undefined;
        
        console.log("UTXOs length = ", valUtxos.length)
        for ( let i=0; i<valUtxos.length; i++ ) {
          console.log("I = ", i)
          const curr = valUtxos[i]
          console.log("Curr on i = ", curr)
          console.log("Curr datum on i = ", curr)
            if (!curr.datum) {
              if (!curr.datumHash) {
                continue;
              }

            }
            curr.datum = await lucid.datumOf(curr)
            console.log("Datum on UTXO = ", curr.datum)
          // const pDatum : Constr <Data> = Data.from(curr.datum!)
  
          try {
            console.log(" About to convert datum to our schema type")

            const utxoInDatum = Data.from(curr.datum!, MyDatum)
            
            const encryptedWordsWithIndexFound = utxoInDatum.encryptedWordsWithIndex
            const ownerPubKeyHashFound = utxoInDatum.ownerPKH
            console.log("Encrypted words = ", encryptedWordsWithIndexFound)
            console.log("Owner pubkeyHash = ", ownerPubKeyHashFound)


          }   // End - try to read Datum  
          catch (error) {
            // Handle the error here -- we will just skip for now
            console.log('An error occurred in reading datum', error);
          }


          // if (pDatum.fields[0] === BigInt(24)) {
          found = curr
        } 
        
        console.log("Found on UTXO = ", found)
  
        // if (!found) throw new Error("Naughty Datum")
       
        const utxos = await lucid.wallet.getUtxos();
        
        const tx = await lucid
          .newTx()
          .attachSpendingValidator(sValidator)
          .collectFrom([found], Data.to(Redeemer()))
          .complete();
  
          const signedTx = await tx.sign().complete();
          const txHash = await signedTx.submit();
          console.log("Collect Test TxHash: " + txHash)
          return txHash;
      }   // End - if lucid

  

  }      // End - Function sContributeCrowdFund
 


  let iterationNumberAsset = 0;
  // Recursive function to traverse the object and its nested properties
  function traverseAsset(obj: any, policyId: string) {

    for (const key in obj) {
      iterationNumberAsset++;
      // console.log("T - iter# = ", iterationNumber)
    
      if (typeof obj[key] === 'object') {
          traverseAsset(obj[key], policyId);
      } else {
          if (key === policyId) {
            console.log("policy found value = ", key, obj[key])
            return obj[key]
          } else if (key === "lovelace") {
            console.log("lovelace = ", key, obj[key])
          }
          if (key != fullPolicy && key != "lovelace") {
            // this is error because we found some other Asset other than our Unique NFT and lovelace
            throw new Error("we found some other Asset other than our Unique NFT and lovelace")
          }

      }
    }
  }   // End traverseAsset


  const sSpendAndDeposit = async () => {
        if (lucid) {
          // const sValCbor = "582858260100003232322225333573466ebcc010c014008c010c01400c526165742460046ea800555cf1"
          // const sValCbor = "590a5f590a5c01000032323232323232323232323232323232323232323232323232323232323232323232323232323232323222232323232323232323232323253330343370e90010010991919299981b99b87480080084c8c8c8c8c8c8c8c8c8c8c8c8c8c94ccc114cdc3a4004004264646464646464646464646464a6660a466e1d20000021613232323232323232323232533305d3370e90000010a99982ea9982099b8733303d375660c002a6eb8c180048dd71830008a40042a6608266e1cc100dd5983000a9bad306000f153304133043375660c002a6088666090e00ccc13c138138dd698300079998279bae3060012375c60c002290010998219bab3060015304433304870066609e09c09c6eb4c18003cccc13cdd718300091bae30600114800854ccc1754cc104cdc399981e9bab306000a375c60c000e6eb8c180019200213304333304f04e04e3039375860c060be0086088666090e00ccc13c138138c0e4dd61830182f807999827827027181d01b8a99982e99821982219982438033304f04e04e375a60c001e66609e09c09c607406e66609e09c09c6eb4c18001054ccc1754cc104cdd79830004983000a0a9982099baf3060008306001315330413375e60c000a60c00202a6608266ebcc18001cc1800484cdd7983000318300088a99982e99821982219982438033304f04e04e303a037375660c002a6eacc18002854ccc174cdd7983000b18300058a99982e9981e191919299983019b87480000084c8c198c0fc004c18c00453011e581c0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a0030630023063001375406e02e2a6660ba66e25200433303d375660c001409c09c2930b0b0b0b0b0b0b0b0a99982e9981e183001300b8a99982ea9982099b89375a60c00446eb4c18008454cc104cdc49bad3060010375a60c001e266e24dd698300079bad306000f1533305d5330413370e66607a6eacc180054dd718300091bae30600114800854cc104cdc398201bab3060015375a60c001e2a66082660866eacc180054c110ccc121c01998278270271bad306000f33304f375c60c00246eb8c1800452002133043375660c002a6088666090e00ccc13c138138dd698300079998279bae3060012375c60c002290010a99982e99b88375a60c00266eb4c1800645261616161630600023060001375406860b600260b400260b200260b000260ae002609a6096609860ae60ac00260aa002609664a6660a80022c264a6660aa002260ae0042c60aa00266460a044a6660ac00220a6264a6660ac60080022660aa002600660b00042600660b000460b0002644646464a6660ae66e1d200000214a0266e3cdd7182d000802982d001182d0009baa323058304e0013057304d001375c60aa0026eb0c154050c154008c154004dd5191829982480098290041828000982780098270009826800982600098211820182098261825800982500098201991198231129998260008b09929998261919baf374e608c00a6e9cc118004c13c0044c13cc1380044c00cc138008c110c138004008dd61825005182500a1bac30493048007303e304800616304800230480013754608a0026074608800260726086002646084608460846084002608200260806080002606c608001a607c002607a00260780026076002607400260600242c607400460740026ea8c0dcc0d801058c0dc008c0dc004dd5181a18198009814804a99981799b87480000084c8c8c8c8c8c8c9265333036001149858c0d80194ccc0cccdc3a400000426464a66606a66e1cdc6800a40702646464646493299981d0008a4c2c60740066eb4004c0e4004c0dc00c58dd7000981b0008b181b001181b0009baa00130320011533302f3370e900100109924ca6660600022930b0b181900118190009baa006533302b3370e900000109919299981699b87371a002901c0991919299981819b89480000044c8c8c8c94ccc0d14cc0b8cdc3800a4000266e1c005203813232325333037337126e340052040132323232323232323232324994ccc1080045261630420033303b23232323200553330433370e900000109919299982299b87371a002901c0991919191924ca6660940022930b18250019bad0013049001304700316375c002608c0022c608c004608c0026ea8004dd60009820800981f8019bad001303e001303c003375a002607600260720062c6eb8004c0e0004c0d801058dc68009bae0013034001303200316375a0026062002605e0062c6eb8004c0b800458c0b8008c0b8004dd500198109129998138008a4000266e00dd6981518021814800980118140009191919299981399b874800800852000132375a6058600c0026054002605400460540026ea80048c8cdd818138009813981300098139baa0012301f22533302500114a02a66604866ebcc09c00400c5288980118130009111999802001240004666600a00490003ad3756002006460046ea40048888cc07c894ccc094004401454ccc090cdd79814981380080309802181418138008980118130008009299980f8008a4000266603c66ebcc08cc084004dd48079bad3022302137566044604200290001119980f0010008018a50223375e6e98008dd3000919801119299980e180280089128008911801001998029299980e19baf00137509000091280089118010018008009180191998011bab001232223002003374c002244a002ae8c88cc054894ccc06c00440404c8ccc014c07cc0780088cc06ccdd81810980f80180080108009801180e0008009111998021119980380280100080100091801911ba63300337560046eac0048c00888dd4198019bad002375a0024446666008006440040040024601e6004002446464466002006004444a66602e00226602a0060042646464a66603266ebc0080044cc060cdd800119804980e803180e80199980411001002980d8020a99980c99b90375c0046eb80044cc060018cccc0208800400cc06c0100144cc06000ccccc02088004018014c06c010c074008c070010c064004894ccc05400840044cccc00c88004c05c008c0580080052210022253330113370e002900008038998020019980280100091198021ba9002374c00244660066ea4008dd4000911980619bb00020010034bd6f7b630119191919002a99980699b87480000084c8c94ccc03ccdc39b8d001480e04c8c8c94ccc048cdc4a4000002264646464a66602ca6602066e1c005200013370e002901c0991919299980c99b89371a00290200991919191919191919191924ca6660480022930b18120019980e919191919002a99981299b87480000084c8c94ccc09ccdc39b8d001480e04c8c8c8c8c926533302c001149858c0b000cdd6800981580098148018b1bae0013028001163028002302800137540026eb0004c08c004c08400cdd68009810000980f0019bad001301d001301b00316375c002603400260300082c6e34004dd7000980b000980a0018b1bad0013013001301100316375c00260200022c602000460200026ea80048c8c8c94ccc030cdc3a40080042601e0022c601e004601e0026ea80048c030dd50009198038008010a512300222533300800110051330063003300a001300230090012323002233002002001230022330020020014bd702ba05734aae7d5d12ba15573caae741"  // Crowd Fund
          const sValidator : SpendingValidator = 
            {type:"PlutusV2", script: sValCborCrowdFund}
          const sValAddress = lucid.utils.validatorToAddress(sValidator)
          const valUtxos = await lucid.utxosAt(sValAddress)

          // const currency1 = "6fcf234bee205dc56ad308ea5882eb59045136056de323daf221f9de";
          // const currency1 = "45cd7f7a6cd2ada53dbbc4ca4b1342392c99f3276cb8f19574c11a99";
          // const currency1 = "2e7b94d9808e80348737b7d204a755f20b842541c5d2a7ef31a419ef";

          // use Contributor wallet to contribute 
          const contributor1Seed = await fetchSeed();
          // const seed = "....."
          lucid.selectWalletFromSeed(contributor1Seed);

          const { paymentCredential } = lucid.utils.getAddressDetails(
            await lucid.wallet.address(),
          );  


//        we need to first use Policy ID which is the key to find the UTXO
//        1) we need to make sure UTXO value and datum also have this Policy ID
//        2) we then construct the new Datum - we only need to change values on 2 fields
//                          

//        this is what is already at the script
          const datumInit : MyDatum = 
          // { pDat:
            {
              // beneficiary: fromText("0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"),    // pubkey hash
              beneficiary: paymentCredential?.hash!,    // should be pubkey hash from NAMI
              deadline: 35859429n,
              // deadline: 35693597n, 
              // aCurrency: fromText("45cd7f7a6cd2ada53dbbc4ca4b1342392c99f3276cb8f19574c11a99"),
              aCurrency: currency1,
              // aCurrency: "6fcf234bee205dc56ad308ea5882eb59045136056de323daf221f9de",
              // aToken: fromText("CrowdFundingToken"),
              aToken: "MyCrowdFund",
              targetAmount: 30000000n,                // 30 Ada Target
              actualtargetAmountsoFar: 2000000n,      // Min 2 Ada when we first deposit
              // commented temporarily
              contributorsMap: []            // Initial map is blank
                   // contributorsMap: [[paymentCredential?.hash!,2000000n]]    
            }
          // };
          const datum1 : MyDatum = 
            // { pDat: 
              {
                // beneficiary: fromText("0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"),    // pubkey hash
                beneficiary: paymentCredential?.hash!,    // pubkey hash
                deadline: 35859429n,                      // BigInt(deadlineCalc),
                // deadline: 5555n, 
                // aCurrency: fromText("45cd7f7a6cd2ada53dbbc4ca4b1342392c99f3276cb8f19574c11a99"),
                aCurrency: currency1,
                // aToken: fromText("CrowdFundingToken"),
                aToken: fromText("MyCrowdFund"),
                targetAmount: 30000000n,                // 30 Ada Target
                actualtargetAmountsoFar: 2000000n,      // Min 2 Ada when we first deposit
                contributorsMap: []             // Initial map is blank
              }
            // }
          // Datum we want to deposit
          const datum2 : MyDatum = 
            // { pDat: 
              {
                // beneficiary: fromText("0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"),    // pubkey hash
                beneficiary: fromText("e9efb9bb50fc3531da4955a7f4d06b22951cbcb373368f978640c3f4"),    // pubkey hash
                deadline: BigInt(35231522),     // what's already on the UTXO - needs to be same.
                // deadline: 5555n, 
                // aCurrency: fromText("45cd7f7a6cd2ada53dbbc4ca4b1342392c99f3276cb8f19574c11a99"),
                // aCurrency: fromText("e0d3120855499e55ef6994a68b2180d265724d62341e55be11826bc4"),
                aCurrency: currency1,
                // aToken: fromText("CrowdFundingToken"),
                aToken: fromText("MyCrowdFund"),
                targetAmount: 30000000n,                // 30 Ada Target
                actualtargetAmountsoFar: 22000000n,      // Min 2 Ada when we first deposit + this contribution 20 Ada
                contributorsMap: [[paymentCredential?.hash!, 20000000n]]
                // contributorsMap: new Map([])             // Initial map is blank
              }          
            // };
          let found = undefined
    
          for ( let i=0; i<valUtxos.length; i++ ) {
            const curr = valUtxos[i]
            console.log("************************* ");
            console.log("current Utxo at scrips = ", curr);
            console.log("current Datum at Utxo at scrips = ", curr.datum);
            console.log("current Address Utxo at scrips = ", curr.address);
            console.log("current TxHash Utxo at scrips = ", curr.txHash);
            const currValue = fromAssets(curr.assets);
            console.log("current Assets Utxo at scrips = ", curr.assets);
            console.log("current Value Utxo at scrips = ", currValue);
            type ValueType = typeof currValue;
            // const extractedValue = currValue.get('');

            // Find a specific key and its value using forEach
            let foundKey: Data | undefined;
            let foundValue: Data | undefined;

            currValue.forEach((value, key) => {
              // if (/* your condition to find the key */) {
                foundKey = key;
                foundValue = value;
                console.log("key = ", key);
                console.log("value = ", value);
                // Check if the value is a Map
                if (value instanceof Map) {
                  console.log('The value is a Map.');
                } else {
                  console.log('The value is not a Map.');
                }


              // }
            });

            // currValTo = Data.from(currValue); 
            // console.log(" Extracted = ", extractedValue);


            if (!curr.datum) {
              if (!curr.datumHash) {

                continue;
              }
              curr.datum = await lucid.datumOf(curr);
            }
          
            const datum2 = await lucid.datumOf(curr)
            console.log("2nd time current Datum at Utxo at scrips = ", datum2);
            console.log("2nd time current Datum Beneficiary = ", datum2.toString);

            // Destructure the nested structure to extract values
            try {
              // Start traversal with the given data
              iterationNumber = 0;
              contribIterNo = 0;
              // traverse(curr.datum, currency1);
              traverseDatum(datum2, currency1);


            } catch (error) {
              // Handle the error here -- we will just skip
              console.log('An error occurred: for traverse', error);
            }

            // // Destructure the nested structure to extract values
            // try {
            //   const [[{ fields: [value1, value2, value3, value4] }]] = curr.datum;

            //   // Print the extracted values
            //   console.log("Value 1:", value1);
            //   console.log("Value 2:", value2);
            //   console.log("Value 3:", value3);
            //   console.log("Value 4:", value4);
            // } catch (error) {
            //   // Handle the error here -- we will just skip
            //   console.log('An error occurred: for values', error);
            // }

            const MyDatumSchemaTest = Data.Bytes();
            type MyDatumTest = Data.Static<typeof MyDatumSchemaTest>;
            const MyDatumTest = MyDatumSchemaTest as unknown as MyDatumTest;
          
            const datum: MyDatumTest = "31313131"; //hex
            const datumTo = Data.to(datum, MyDatumTest);
            const newDatum = Data.from(
              Data.to(datum, MyDatumTest),
              MyDatumTest,
            );

            console.log("---------------------------- datum = ", datum);
            console.log("---------------------------- typeof datum = ", (typeof datum));
            console.log("----------------------------New datum = ", newDatum);
            console.log("----------------------------Type of New datum = ", (typeof newDatum));
            console.log("----------------------------DatumTo = ", datumTo);
            console.log("----------------------------type of DatumTo = ", (typeof datumTo));

            // const cborDatum = datumJsonToCbor(curr.datum!);

            console.log("---------------------------- Typeof Curr.datum = ", (typeof curr.datum!));
            if (curr.txHash === "bd8cdefdf99cce340b562be158615224f9986762ce07ddb46875dd791baf74df") { 
              // console.log("---------------------------- Data.from<MyDatumSchema2>(curr.datum!, MyDatumSchema2) = ", Data.toJson(curr.datum!));
              // console.log("---------------------------- Data.from<MyDatumSchema2>(curr.datum!, MyDatumSchema2) = ", Data.toJson(datum2));
            }
            // console.log("---------------------------- Data.from<MyDatumSchema2>(curr.datum!, MyDatumSchema2) = ", Data.from(curr.datum!));
            
            // console.log("curr.datum!  data.to = ")
            
            // const utxoInDatumAny = Data.from<MyDatumSchema2>(curr.datum!, MyDatumSchema2)
            // const pDatum = Data.from<MyDatumSchema2>({
            //   beneficiary: curr.datum!.beneficiary,
            //   deadline: curr.datum!.deadline,
            //   aCurrency: curr.datum!.aCurrency,
            //   aToken: curr.datum!.aToken,
            //   targetAmount: curr.datum!.targetAmount,
            //   actualtargetAmountsoFar: curr.datum!.actualtargetAmountsoFar,
            // });
                        

            try {
              console.log(" About to convert datum to our schema type")
              const utxoInDatumAny = Data.from<MyDatumSchema>(curr.datum!, MyDatumSchema);

              // const newDatum = Data.from(
              //   Data.to(datum, MyDatumSchema2),
              //   MyDatumSchema2,
              // );

              // Continue with your code using utxoInDatumAny
              if (utxoInDatumAny.pDat.aCurrency === "2e7b94d9808e80348737b7d204a755f20b842541c5d2a7ef31a419ef") {
                console.log(" Successful compare")
              }
              console.log("utxoInDatumAny beneficiary = ", utxoInDatumAny.pDat.beneficiary)
              console.log("utxoInDatumAny Deadline = ", utxoInDatumAny.pDat.deadline)
              console.log("utxoInDatumAny Currency = ", utxoInDatumAny.pDat.aCurrency)
              console.log("utxoInDatumAny Token = ", utxoInDatumAny.pDat.aToken)
              console.log("utxoInDatumAny Target amount = ", utxoInDatumAny.pDat.targetAmount)
              console.log("utxoInDatumAny target so far = ", utxoInDatumAny.pDat.actualtargetAmountsoFar)
              console.log("utxoInDatumAny Contributors map = ", utxoInDatumAny.pDat.contributorsMap)
              const contrMap = utxoInDatumAny.pDat.contributorsMap
              let contrCount = 1;
              let contrMapContr: bigint = 0n;
              contrMap.forEach(item => {
                console.log("count = ", contrCount, " item = ", item);
                console.log("item 0 PubKeyHash = ",  item[0]);
                console.log("item 1 Contribution made = ",  item[1]);
                console.log("type of contrAmt = ", typeof item[1]);
                contrMapContr = contrMapContr + item[1]; 
                contrCount = contrCount + 1;
              });
              console.log(" Total contribution in datum contr map = ", contrMapContr)

              // Test adding a new element to contributors map
              const newElement = [paymentCredential?.hash!, 12000000n];
              // Create a new array by spreading the existing contrMap and adding the newElement
              const updatedContrMap = [...contrMap, newElement];
              console.log("Updated Contributors map = ", updatedContrMap);
              let updContrCount = 1;
              let updContrMapContr: bigint = 0n;
              
              updatedContrMap.forEach(item => {
                  console.log("count = ", contrCount, " item = ", item);
                  console.log("item 0 PubKeyHash = ", item[0]);
                  console.log("item 1 Contribution made = ", item[1]);
                  console.log("type of contrAmt = ", typeof item[1]);
                  updContrMapContr = updContrMapContr + item[1]; 
                  updContrCount = updContrCount + 1;
              });              
              console.log(" Updated Total contribution in datum contr map = ", updContrMapContr)



              // contributorsMap: Data.Array(Data.Tuple([Data.Bytes(),Data.Integer()]))  
              console.log("datum = ", await lucid.datumOf(curr))
            } catch (error) {
              // Handle the error here -- we will just skip
              console.log('An error occurred: will skip ', error);
            }
            

          // const pDatum = Data.from<PasswordDatum>(curr.datum!, PasswordDatum)
    
          const utxos = await lucid.wallet.getUtxos();
          
          // const tx = await lucid
          //   .newTx()
          //   .attachSpendingValidator(sValidator)
          //   .collectFrom([found], redeemer)
          //   .payToContract(sValAddress, {inline: datum}, {lovelace: BigInt(1000000)})
          //   .complete();
    
          //   const signedTx = await tx.sign().complete();
          //   const txHash = await signedTx.submit();
          //   console.log("Collect Test TxHash: " + txHash)
          //   return txHash;
    
        }
      }
  }    // End sSpendAndDeposit
  let iterationNumber = 0;
  let contribIterNo = 0;
  





    // Recursive function to traverse the object and its nested properties
    function traverseDatum(obj: any, policyId: string) {

      for (const key in obj) {
        iterationNumber++;
        // console.log("T - iter# = ", iterationNumber)
      
        if (typeof obj[key] === 'object') {
          contribIterNo = 0;    // when you hit object we can reset contrib Iter
          // Recurse into nested objects and arrays
          if (iterationNumber == 1) {
            // console.log(" T - If stmt iter # = ", iterationNumber);
            traverseDatum(obj[key], policyId);
          } else  { 
            // console.log(" T - else stmt iter # = ", iterationNumber);
            traverseDatum(obj[key], policyId);
          }
        } else {
          // console.log(" type of key = ", typeof key)
          // Print or process the leaf values
          // console.log(`${key}: ${obj[key]}`);
          // let stringValue: string = key;
          // try {
          //   let integerValue: number = Number(stringValue);
          //   console.log("Converted value: ", integerValue); // Output: 123
          // } catch (error) {
          //   // Handle the error here -- we will just skip
          //   console.log('An error occurred converting string to integer ', error);
          // }
          if (iterationNumber === 3) {
            console.log(" this is Beneficiary = ", `${obj[key]}`)
          } else if (iterationNumber === 4) {
            console.log(" this is Deadline = ", `${obj[key]}`)
          } else if (iterationNumber === 5) {
            console.log(" this is Policy = ", `${obj[key]}`)
            console.log("T - iter# = ", iterationNumber)
          } else if (iterationNumber === 6) {
            console.log(" this is Token Name = ", `${obj[key]}`)
          } else if (iterationNumber === 7) {
            console.log(" this is Target Amount = ", `${obj[key]}`)
            console.log("T - iter# = ", iterationNumber)
          } else if (iterationNumber === 8) {
            console.log(" this is Target so Far = ", `${obj[key]}`)
          } else if (iterationNumber > 13) {
            // console.log("inside > 13 - iter# = ", iterationNumber)
            // console.log("inside > 13 - contribiter# = ", contribIterNo)
              if (contribIterNo === 0) {
                console.log(" this is Contribution pubKeyHash  = ", `${obj[key]}`)
                contribIterNo++;
              } else if (contribIterNo === 1) {
                console.log(" this is Contribution Amount  = ", `${obj[key]}`)
                contribIterNo++;
              } else if (contribIterNo > 1) {
                contribIterNo++;
              }
  
  
          }
        }
      }
    }   // End traverseDatum

  // Recursive function to traverse the object and its nested properties
  function traverse(obj: any, policyId: string) {

    for (const key in obj) {
      iterationNumber++;
      // console.log("T - iter# = ", iterationNumber)
    
      if (typeof obj[key] === 'object') {
        contribIterNo = 0;    // when you hit object we can reset contrib Iter
        // Recurse into nested objects and arrays
        if (iterationNumber == 1) {
          // console.log(" T - If stmt iter # = ", iterationNumber);
          traverse(obj[key], policyId);
        } else  { 
          // console.log(" T - else stmt iter # = ", iterationNumber);
          traverse(obj[key], policyId);
        }
      } else {
        // console.log(" type of key = ", typeof key)
        // Print or process the leaf values
        // console.log(`${key}: ${obj[key]}`);
        // let stringValue: string = key;
        // try {
        //   let integerValue: number = Number(stringValue);
        //   console.log("Converted value: ", integerValue); // Output: 123
        // } catch (error) {
        //   // Handle the error here -- we will just skip
        //   console.log('An error occurred converting string to integer ', error);
        // }
        if (iterationNumber === 6) {
          console.log(" this is Beneficiary = ", `${obj[key]}`)
        } else if (iterationNumber === 7) {
          console.log(" this is Deadline = ", `${obj[key]}`)
        } else if (iterationNumber === 8) {
          console.log(" this is Policy = ", `${obj[key]}`)
        } else if (iterationNumber === 9) {
          console.log(" this is Token Name = ", `${obj[key]}`)
        } else if (iterationNumber === 10) {
          console.log(" this is Target Amount = ", `${obj[key]}`)
        } else if (iterationNumber === 11) {
          console.log(" this is Target so Far = ", `${obj[key]}`)
        } else if (iterationNumber > 13) {
          // console.log("inside > 13 - iter# = ", iterationNumber)
          // console.log("inside > 13 - contribiter# = ", contribIterNo)
            if (contribIterNo === 0) {
              console.log(" this is Contribution pubKeyHash  = ", `${obj[key]}`)
              contribIterNo++;
            } else if (contribIterNo === 1) {
              console.log(" this is Contribution Amount  = ", `${obj[key]}`)
              contribIterNo++;
            } else if (contribIterNo > 1) {
              contribIterNo++;
            }


        }
      }
    }
  }  // end traverse




// this function is only a test to show script address calculated thats all;
  const sPrintScriptAddress = async () => { 
        if (lucid) {
          // const sValCbor = "589b5899010000222323232325333573466e1d200200215333573466ebcd5d0991aab9e375400200e6ae84c8d55cf1baa00100614985854cd5ce2481445061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f536d616c6c56616c696461746f722e68733a35333a352d3432001635573c0046aae74004dd51aba135744002646aae78dd50008009"
          // const sValCborSmallValidator = "582858260100003232322225333573466ebcc010c014008c010c01400c526165742460046ea800555cf1"  // small validator
          // const sValCborCrowdFund = "590a5f590a5c01000032323232323232323232323232323232323232323232323232323232323232323232323232323232323222232323232323232323232323253330343370e90010010991919299981b99b87480080084c8c8c8c8c8c8c8c8c8c8c8c8c8c94ccc114cdc3a4004004264646464646464646464646464a6660a466e1d20000021613232323232323232323232533305d3370e90000010a99982ea9982099b8733303d375660c002a6eb8c180048dd71830008a40042a6608266e1cc100dd5983000a9bad306000f153304133043375660c002a6088666090e00ccc13c138138dd698300079998279bae3060012375c60c002290010998219bab3060015304433304870066609e09c09c6eb4c18003cccc13cdd718300091bae30600114800854ccc1754cc104cdc399981e9bab306000a375c60c000e6eb8c180019200213304333304f04e04e3039375860c060be0086088666090e00ccc13c138138c0e4dd61830182f807999827827027181d01b8a99982e99821982219982438033304f04e04e375a60c001e66609e09c09c607406e66609e09c09c6eb4c18001054ccc1754cc104cdd79830004983000a0a9982099baf3060008306001315330413375e60c000a60c00202a6608266ebcc18001cc1800484cdd7983000318300088a99982e99821982219982438033304f04e04e303a037375660c002a6eacc18002854ccc174cdd7983000b18300058a99982e9981e191919299983019b87480000084c8c198c0fc004c18c00453011e581c0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a0030630023063001375406e02e2a6660ba66e25200433303d375660c001409c09c2930b0b0b0b0b0b0b0b0a99982e9981e183001300b8a99982ea9982099b89375a60c00446eb4c18008454cc104cdc49bad3060010375a60c001e266e24dd698300079bad306000f1533305d5330413370e66607a6eacc180054dd718300091bae30600114800854cc104cdc398201bab3060015375a60c001e2a66082660866eacc180054c110ccc121c01998278270271bad306000f33304f375c60c00246eb8c1800452002133043375660c002a6088666090e00ccc13c138138dd698300079998279bae3060012375c60c002290010a99982e99b88375a60c00266eb4c1800645261616161630600023060001375406860b600260b400260b200260b000260ae002609a6096609860ae60ac00260aa002609664a6660a80022c264a6660aa002260ae0042c60aa00266460a044a6660ac00220a6264a6660ac60080022660aa002600660b00042600660b000460b0002644646464a6660ae66e1d200000214a0266e3cdd7182d000802982d001182d0009baa323058304e0013057304d001375c60aa0026eb0c154050c154008c154004dd5191829982480098290041828000982780098270009826800982600098211820182098261825800982500098201991198231129998260008b09929998261919baf374e608c00a6e9cc118004c13c0044c13cc1380044c00cc138008c110c138004008dd61825005182500a1bac30493048007303e304800616304800230480013754608a0026074608800260726086002646084608460846084002608200260806080002606c608001a607c002607a00260780026076002607400260600242c607400460740026ea8c0dcc0d801058c0dc008c0dc004dd5181a18198009814804a99981799b87480000084c8c8c8c8c8c8c9265333036001149858c0d80194ccc0cccdc3a400000426464a66606a66e1cdc6800a40702646464646493299981d0008a4c2c60740066eb4004c0e4004c0dc00c58dd7000981b0008b181b001181b0009baa00130320011533302f3370e900100109924ca6660600022930b0b181900118190009baa006533302b3370e900000109919299981699b87371a002901c0991919299981819b89480000044c8c8c8c94ccc0d14cc0b8cdc3800a4000266e1c005203813232325333037337126e340052040132323232323232323232324994ccc1080045261630420033303b23232323200553330433370e900000109919299982299b87371a002901c0991919191924ca6660940022930b18250019bad0013049001304700316375c002608c0022c608c004608c0026ea8004dd60009820800981f8019bad001303e001303c003375a002607600260720062c6eb8004c0e0004c0d801058dc68009bae0013034001303200316375a0026062002605e0062c6eb8004c0b800458c0b8008c0b8004dd500198109129998138008a4000266e00dd6981518021814800980118140009191919299981399b874800800852000132375a6058600c0026054002605400460540026ea80048c8cdd818138009813981300098139baa0012301f22533302500114a02a66604866ebcc09c00400c5288980118130009111999802001240004666600a00490003ad3756002006460046ea40048888cc07c894ccc094004401454ccc090cdd79814981380080309802181418138008980118130008009299980f8008a4000266603c66ebcc08cc084004dd48079bad3022302137566044604200290001119980f0010008018a50223375e6e98008dd3000919801119299980e180280089128008911801001998029299980e19baf00137509000091280089118010018008009180191998011bab001232223002003374c002244a002ae8c88cc054894ccc06c00440404c8ccc014c07cc0780088cc06ccdd81810980f80180080108009801180e0008009111998021119980380280100080100091801911ba63300337560046eac0048c00888dd4198019bad002375a0024446666008006440040040024601e6004002446464466002006004444a66602e00226602a0060042646464a66603266ebc0080044cc060cdd800119804980e803180e80199980411001002980d8020a99980c99b90375c0046eb80044cc060018cccc0208800400cc06c0100144cc06000ccccc02088004018014c06c010c074008c070010c064004894ccc05400840044cccc00c88004c05c008c0580080052210022253330113370e002900008038998020019980280100091198021ba9002374c00244660066ea4008dd4000911980619bb00020010034bd6f7b630119191919002a99980699b87480000084c8c94ccc03ccdc39b8d001480e04c8c8c94ccc048cdc4a4000002264646464a66602ca6602066e1c005200013370e002901c0991919299980c99b89371a00290200991919191919191919191924ca6660480022930b18120019980e919191919002a99981299b87480000084c8c94ccc09ccdc39b8d001480e04c8c8c8c8c926533302c001149858c0b000cdd6800981580098148018b1bae0013028001163028002302800137540026eb0004c08c004c08400cdd68009810000980f0019bad001301d001301b00316375c002603400260300082c6e34004dd7000980b000980a0018b1bad0013013001301100316375c00260200022c602000460200026ea80048c8c8c94ccc030cdc3a40080042601e0022c601e004601e0026ea80048c030dd50009198038008010a512300222533300800110051330063003300a001300230090012323002233002002001230022330020020014bd702ba05734aae7d5d12ba15573caae741"  // Crowd Fund
          
          const sValidator : SpendingValidator = {
            type: "PlutusV2",
            script: sValCborCrowdFund
          }
        
          // to print at terminal too
          // const fs = require('fs');
          // const path = require('path');
          // const logFile = path.join(process.cwd(), 'npm_run_dev.log');
          // const writeLog = (message) => {
          //   fs.writeFileSync(logFile, message + '\n', { encoding: 'utf-8' });
          // };
          // writeLog('This message was printed to the npm run dev window.');
    
    
          const sValAddress = lucid.utils.validatorToAddress(sValidator)
          
          // const datum = Data.to(
          //   new Constr(0, [BigInt(24)])
          // )
          console.log("PayToScript Address from New show address button: ", sValAddress )

        
        }
  }    // End sPrintScriptAddress


///////////////////////////////////////////////////////////////////////
/////////////            MINT NFT 



// const sPrintScriptAddress = async () => { 
  const sMintCrowdFundToken = async () => { 
    if (lucid) {

        const { paymentCredential } = lucid.utils.getAddressDetails(
            await lucid.wallet.address(),
        );
        // First we need to create a minting policy for the assets we want to mint. 
        //    we utilize a native script time-locking policy with our wallet as required signer:    

        const mintingPolicy = lucid.utils.nativeScriptFromJson(
          {
            type: "all",
            scripts: [
              { type: "sig", keyHash: paymentCredential.hash },
              {
                type: "before",
                slot: lucid.utils.unixTimeToSlot(Date.now() + 1000000),   // for testing minting 10
              },
            ],
          },
        );    // End minting policy 
            
      

        // Next we derive the policy id from the minting policy script:
        const policyId = lucid.utils.mintingPolicyToId(mintingPolicy);

        // const unit = policyId + fromText("CrowdFundingToken");
        const unit = policyId + fromText("MyCrowdFund");
        console.log("Policy ID for our CrowdFunding Token: ", policyId )
        const tx = await lucid.newTx()
          // .mintAssets({ [unit]: 10n })    // Mint 10 for testing
          .mintAssets({ [unit]: 1n })    // Mint 10 for testing
          .validTo(Date.now() + 200000)
          .attachMintingPolicy(mintingPolicy)
          .complete();

        const signedTx = await tx.sign().complete();

        const txHash = await signedTx.submit();
        console.log("Collect Test TxHash: " + txHash)
        return txHash;
    }
  }  // End sMintCrowdFundToken

     
  const createSeedPhrase = async () => { 
    if (lucid) {
      const seed = lucid.utils.generateSeedPhrase();
      console.log("Seed phrase created: ", seed)
      lucid.selectWalletFromSeed(seed);
    }
  }   // End createSeedPhrase


  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setInputValue(event.target.value);
  };


  const handleButtonSDeposit = async () => {
    try {
      await sDeposit(); 
    } catch (error) {
      // Handle any errors that might occur during the asynchronous operation
      console.error('Error in SDeposit CrowdFund:', error);
    }
  };  

  const handleButtonClickSeedInfo = async () => {
    try {
      await infoFromSeed(inputValue); // Use 'await' to wait for the asynchronous function to complete
    } catch (error) {
      // Handle any errors that might occur during the asynchronous operation
      console.error('Error in infoFromSeed:', error);
    }
  };  

  
  const infoFromSeed = async (seed: string) => { 
    // This test routine i am trying to get address and credentials from Seed from a secret file.
    // Also getting from Nami connected wallet too.

    // const seed = lucid.utils.generateSeedPhrase();
    // console.log("Seed phrase created: ", seed)
    if (lucid) {

      // First lets bring Nami and get its PaymentCredential Hash
      const { paymentCredential: namiPaymentCredential } = lucid.utils.getAddressDetails(
        await lucid.wallet.address(),
      );  
      const namiCredHash = namiPaymentCredential?.hash!
      console.log("Nami Cred Hash = ", namiCredHash)


      // Now we connect from Seed
      const options = {
        accountIndex: 0, // Replace 0 with the desired account index value
        // You can also provide other optional properties like addressType and password if needed
        // addressType: "Base",
        // password: "your_password_here",
      };
      lucid.selectWalletFromSeed(seed, options);
      const address = await lucid.wallet.address();
      const { paymentCredential: seedCredential } = lucid.utils.getAddressDetails(
        await lucid.wallet.address(),
      );       
      const seedCredHash = seedCredential?.hash!
      console.log("Seed Cred Hash = ", seedCredHash)

      setPrivAddress(address);
      console.log("address 0 for seed phrase ", address)

      // You can Payment PubkeyHash directly from an address
      const { paymentCredential: testCred } = getAddressDetails("addr_test1vr4cmkp58duj520sszr3hkfkjpzc04e29nega3acqeadgycxkd60u");
      // addr_test1qppr9pp9grd329vcz89xz9g0sg36kff09lm22p57qm2zm64yt7acf0k6xwhzk8jawtc7s96m2x5s2af3v5vjsg9tng0skvvfa0
      // 
      const testCredHash = testCred?.hash!
      console.log("Test Cred Hash = ", testCredHash)
    }
  }   // End infoFromSeed


  const createPrivateKey = async (key: string) => { 
    // const seed = lucid.utils.generateSeedPhrase();
    // console.log("Seed phrase created: ", seed)
    if (lucid) {
      // const seed1 = "year gold install glide mirror useful assault very bid leader shuffle soon hamster betray raven entry very brick leave federal aim dress suit suit"
      // const options = {
      //   accountIndex: 1, // Replace 0 with the desired account index value
      //   // You can also provide other optional properties like addressType and password if needed
      //   // addressType: "Base",
      //   // password: "your_password_here",
      // };
      const privateKey = lucid.utils.generatePrivateKey(); // Bech32 encoded private key
      lucid.selectWalletFromPrivateKey(privateKey);
      console.log("Private key ", privateKey)
      // cboe Hex - 5820fed6c111612c424b6bbe28b42e388faa3ec2102b7cd56fb249dc326a4f808498
      //Private key ed25519_sk1rkm9k26wpt9524q6pzmf94ltw9z5z2qe0zzhvau0kam46xq8m6yqw3lpcw
      //crowdfund.tsx:374 address 1 for seed phrase  addr_test1vr4cmkp58duj520sszr3hkfkjpzc04e29nega3acqeadgycxkd60u

      // lucid.selectWalletFromSeed(seed, options);
      const address = await lucid.wallet.address();
      // addr_test1vq8f02sr8nhwwckz22zumny59pch3uqmgkjctlgdfk5rs7sx52ldh   - beneficiary from CDP
      console.log("address 1 for seed phrase ", address)
    }
  }   // End infoFromSeed



  async function writeToServerFile() {
    try {
      const response = await fetch('/api/writeToFile', {
        method: 'POST',
      });
  
      if (response.ok) {
        console.log('From CrowdFund - Data written to the file successfully!');
      } else {
        console.error('Failed to write data to the file.');
      }
    } catch (error) {
      console.error('Error:', error);
    }
  }

 

  const handleChangePrivateKey = (event: React.ChangeEvent<HTMLInputElement>) => {
    setInputValuePrivateKey(event.target.value);
  };
  const handleButtonClickPriv = async () => {
    try {
      await showAddress4PrivateKey(inputPrivateKey); // Use 'await' to wait for the asynchronous function to complete
    } catch (error) {
      // Handle any errors that might occur during the asynchronous operation
      console.error('Error in showAddress4PrivateKey:', error);
    }
  };  

  const showAddress4PrivateKey = async (key: string) => { 
    // Input private will show Address
    if (lucid) {
      lucid.selectWalletFromPrivateKey(key);
      const address = await lucid.wallet.address();
      // addr_test1vq8f02sr8nhwwckz22zumny59pch3uqmgkjctlgdfk5rs7sx52ldh   - beneficiary from CDP
      setPrivAddress(address); 
      console.log("address 1 for seed phrase ", address)
    }
  }   // End showAddress4PrivateKey


  const fetchSeed = async () => {
    try {
      const response = await fetch('/api/getData');
      const data = await response.json();
      // setReadSecretData(data);
      if (data.seed[0]) {
        // contributorSeed = data.seed[0].description;
        return data.seed[0].description;
      }      
    } catch (error) {
      console.error('Error fetching secret data:', error);
      throw error;
    }
  };  


  const fetchSecretData = async () => {
    try {
      const response = await fetch('/api/getData');
      const data = await response.json();
      setReadSecretData(data);
      if (data.items[0]) {
        // contributorSeed = data.seed[0].description;
        return data.seed[0].description;
      }      
    } catch (error) {
      console.error('Error fetching secret data:', error);
      throw error;
    }
  };  


  const testCode = async () => { 
    if (sValCborCrowdFund == sValCborCrowdFundold) {
      console.log("old and new CBOR are same match ")
    } else {
      console.log("old and new CBOR dont match ")
    }
  }

  const encryptSeedPassInfo = async (seedU: string, passwordU: string) => { 
    // Input private will show Address
    if (lucid) {
      const date1 = Date.now();
      const date2 = date1 + 10000000000;
      // 1690862203312
      // 1690947216000
      // 1691033616 - 1690862311
      const slot1 = lucid.utils.unixTimeToSlot(date2)
      const slotTime1 = lucid.utils.slotToUnixTime(slot1)
      console.log("date 1 = ", date1)
      console.log("date 2 = ", date2)
      console.log("slot 1 = ", slot1)
      console.log("slotTime 1 = ", slotTime1)

      // Usage
      const seed23Words = 'custom help female park blush clutch fancy lion fence innocent rebuild amount tip custom help female park blush clutch fancy lion fence innocent ';   // for now 13 words
      let encryptionPhrase = 'myOwnPassword$123';

      const encryptIterations = 1;

      // Get the current date and time before starting the encryption process
      const startTime = new Date().getTime();
      console.log("Start time before encryption: ", startTime)

      // const encryptedText = encrypt(seed23Words, encryptionPhrase);
      // console.log('Encrypted:', encryptedText);

      let encyptedTextRecursive = seed23Words;          // begin with words to encrypt 
      console.log('encyptedTextRecursive Seed Phrase:', encyptedTextRecursive);
      const salt = CryptoJS.lib.WordArray.random(128/8);  // Random salt
      const encryptedPBKDF2 = CryptoJS.PBKDF2(encryptionPhrase, salt, { keySize: 128/32, iterations: 2 });
      console.log("encryptedPBKDF2 as string = ", encryptedPBKDF2.toString())
      
      const test = CryptoJS.PBKDF2('password', 'ATHENA.MIT.EDUraeburn', { keySize: 128/32 }).toString();
      console.log("Test PBKDF2 = ", test)

      for (let i = 0; i < encryptIterations; i++) {
        // encyptedTextRecursive = encrypt(encyptedTextRecursive, encryptionPhrase);
        encyptedTextRecursive = encrypt(encyptedTextRecursive, encryptedPBKDF2.toString());
        console.log('Iter#, encyptedTextRecursive:', i, encyptedTextRecursive);
      }
      const endTime = new Date().getTime();
      const elapsedTime = endTime - startTime ;
      console.log("Elapsed time in sec = " , elapsedTime / 1000)

      console.log("End time after 100 recursive encryptions: ", endTime)
      console.log("Final encyptedTextRecursive after ", encryptIterations, " iterations:", encyptedTextRecursive);

      
      // const decryptedText = decrypt(encryptedText, encryptionPhrase);
      // console.log('Decrypted:', decryptedText);      

      let decryptedTextRecursive = encyptedTextRecursive;          // begin with decrypted final word
      console.log('decryptedTextRecursive:', decryptedTextRecursive);
      for (let i = 0; i < encryptIterations; i++) {
        // decryptedTextRecursive = decrypt(decryptedTextRecursive, encryptionPhrase);
        decryptedTextRecursive = decrypt(decryptedTextRecursive, encryptedPBKDF2.toString());
        console.log('decryptedTextRecursive:', decryptedTextRecursive);
      }
      console.log('Final decryptedTextRecursive:', decryptedTextRecursive);


      return encyptedTextRecursive;
      //infoFromSeed

      // fetchFirstLine(); // Call the function when the component mounts
    }   // if Lucid
  }   // End function `testCodeEncryption`

  // https://github.com/brix/crypto-js/blob/develop/test/aes-test.js

// Encrypt function
  function encrypt(text: string, key: string): string {
    const encrypted = CryptoJS.AES.encrypt(text, key);
    return encrypted.toString();
  }

  // Decrypt function
  function decrypt(encryptedText: string, key: string): string {
    const decrypted = CryptoJS.AES.decrypt(encryptedText, key);
    // const decrypted = CryptoJS.PBKDF2(encryptedText, key);
    return decrypted.toString(CryptoJS.enc.Utf8);
  }


  async function fetchFirstLine() {
    console.log("Starting fetch first line")
    try {
      const response = await fetch('/api/read-file', {
        method: 'GET',
      })
      console.log("Starting fetch second line - response.ok = ", response.ok)
      if (response.ok) {
        const data = await response.json();
        console.log('CrowdFund-FetchFirstLine - Data read from file successfully! JSON = ', data);     
        const firstLine =   data.firstLine
        setFirstLine(firstLine);
      } else {
        console.error('Failed to read data to the file.' , response.status);
      }
    } catch (error) {
      console.error('Error:', error);
    }
  }


  // const fetchFirstLine = async () => {
  //   try {
  //     console.log("in fetchFirstLine 1st line ")
  //     const response = await fetch('/api/read-file');
  //     console.log("in fetchFirstLine 2nd line - response.ok is ", response.ok)
  //     if (response.ok) {
  //       const data = await response.json();
  //       setFirstLine(data.firstLine);
  //     } else {
  //       console.error('Error fetching data:', response.statusText);
  //     }
  //   } catch (error) {
  //     console.error('Error fetching data:', error);
  //   }
  // };


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
        {/* <p> 
          Emurgo example
        </p> */}
      </div>
      <div className="mx-60 my-10">
        <button className="btn btn-primary m-5" onClick={() => { sMintCrowdFundToken() }} >sMintCrowdFundToken</button>
        <button className="btn btn-primary m-5" onClick={() => { sPrintScriptAddress() }} >sPrintScriptAddress</button>
        <button className="btn btn-primary m-5" onClick={() => { sLockEncryptedSeedPhrase() }} >sLockEncryptedSeedPhrase</button>
        <button className="btn btn-secondary m-5" onClick={() => { sSpend() }}>sSpendCrowd</button>
        <button className="btn btn-secondary m-5" onClick={() => { sSpendAndDeposit() }}> sSpendAndDeposit</button>
        <button className="btn btn-secondary m-5" onClick={() => { sDecentralSeedRedeem() }}> sDecentralSeedRedeem</button>
        <button className="btn btn-secondary m-5" onClick={() => { writeToServerFile() }}> writeToServerFile</button>
        <button className="btn btn-secondary m-5" onClick={() => { fetchFirstLine() }}> fetchFirstLine</button>
      </div>

      <div className="mx-60 my-10">
        {/* <button className="btn btn-primary m-5" onClick={() => { sDeposit() }} >sDepositCrowd</button> */}
        <button className="btn btn-secondary m-5" onClick={handleButtonSDeposit}> sDepositCrowd</button>
        {txHash !== null && <p>Tx Hash: {txHash}</p>}
      </div>

      <div className="mx-60 my-10">
        <button className="btn btn-secondary m-5" onClick={() => { createSeedPhrase() }}> createSeedPhrase</button>
        <button className="btn btn-secondary m-5" onClick={() => { createPrivateKey() }}> createPrivateKey</button>
        {privAddress !== null && <p>Result: {privAddress}</p>}
      </div>

      {/* <div className="mx-60 my-10" style={{ padding: '120px' }}> */}
      <div className="mx-60 my-10" style={{ padding: '40px'}}>
          <input
          type="text"
          value={inputValue}
          onChange={handleChange}
          placeholder="Enter your Seed Phrase"
          style={{ width: '1200px' }} // Set the width to your desired value
          />
          {/* <button className="btn btn-secondary m-5" onClick={() => { infoFromSeed(inputValue) }}> infoFromSeed</button> */}
          <button className="btn btn-secondary m-5" onClick={handleButtonClickSeedInfo}> info From Seed</button>
          {privAddress !== null && <p>Address: {privAddress}</p>}
      </div>
      <div className="mx-60 my-10" style={{ padding: '40px'}}>
          <input
          type="text"
          value={inputPrivateKey}
          onChange={handleChangePrivateKey}
          placeholder="Enter your Private key"
          style={{ width: '800px' }} // Set the width to your desired value
          />
          <button className="btn btn-secondary m-5" onClick={handleButtonClickPriv}> Show Address for Private Key</button>
          {privAddress !== null && <p>Address: {privAddress}</p>}
      </div>
        {/* <button className="btn btn-secondary m-5" onClick={() => { unlockGuess() }}>Unlock Guess</button>
        <button className="btn btn-secondary m-5" onClick={() => { deploySequentialMint("Boo") }}>Deploy Sequential Mint</button>
        <button className="btn btn-secondary m-5" onClick={() => { getOffers(paymentCredentialOf(walletStore.address)) }}>Unlock Guess</button> */}
      <div className="mx-60 my-10">
        {/* <button className="btn btn-secondary m-5" onClick={() => { testCodeEncryption() }}> testCodeEncryption</button> */}
        <h1>First Line from File:</h1>
        <p>{firstLine}</p>
      </div>
      <div className="mx-60 my-10">
        <button className="btn btn-secondary m-5" onClick={() => { testCode() }}> testCode</button>
      </div>      
      <div>
        <button className="btn btn-secondary m-5" onClick={fetchSecretData}>Fetch Secret Data</button>
        {/* <button className="btn btn-secondary m-5" onClick={() => { testCode() }}> testCode</button> */}
        {readSecretData && (
          <div>
            <pre>{JSON.stringify(readSecretData, null, 2)}</pre>   // this gives the whole JSON file  
            <h2> {readSecretData.seed[0].name} : {readSecretData.seed[0].description}</h2>
          </div>
        )}
      </div>      
    </div>
    
  )   // End return
}

export default offChain
