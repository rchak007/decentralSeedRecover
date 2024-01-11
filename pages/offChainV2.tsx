
// trying UI with ChatGpt suggestion

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
          toText,
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
          C
        } from 'lucid-cardano'
import * as helios from '@hyperionbt/helios'
import {fromAssets, toAssets, union, Value} from "../utils/valueUtils"
import { fromAddress, OfferDatum, OfferInfo, toAddress } from '../utils/offerUtils'
import { kMaxLength } from 'buffer'

// import { readFirstLineFromFile } from '../utils/fileUtils';

import * as CryptoJS from 'crypto-js';
// import * as Aes from 

// from my Plutarch decentralSeed
// const decentralSeedPlutus = "58a758a501000022223253335734664646460044660040040024600446600400400244a666aae7c0045280a999ab9a3375e6ae8400400c528898011aba200137526eb8d5d09aba20013758646ae84c8d5d11aba2357446ae88d5d11aba2357446ae88004c8d55cf1baa00100135742646aae78dd50008010a4c2a66ae712411e646174756d20616e6420706172616d2061726520646966666572656e742000163235573c6ea800400d"

// https://github.com/HarunJr/decentralSeedRecover/blob/main/code/SeedPhraseManager/assets/plutus-scripts/lucid-seed-phrase.plutus
// const decentralSeedPlutus = "59087a590877010000323322323233223232323232323232323233223232323232322322323222323253353330083333573466e1cd55cea803240004664424660020060046eb8d5d0a8031bae357426ae8940188c98c8070cd5ce00f80e00d1999ab9a3370e6aae74dd5002240004038464c6403866ae7007c070068cccd5cd19b8735573aa0049000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233501901a35742a01866a0320346ae85402ccd406406cd5d0a805199aa80ebae501c35742a012666aa03aeb94070d5d0a80419a80c8121aba150073335501d02575a6ae854018c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233502f75a6ae854008c0c0d5d09aba2500223263203233573806a06406026aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a05eeb4d5d0a80118181aba135744a004464c6406466ae700d40c80c04d55cf280089baa001357426ae8940088c98c80b8cd5ce01881701609aab9e5001137540026ae854014cd4065d71aba150043335501d021200135742a006666aa03aeb88004d5d0a80118119aba135744a004464c6405466ae700b40a80a04d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a80118099aba135744a004464c6403866ae7007c070068406c4c98c806ccd5ce2481035054350001b135573ca00226ea80044d55cf280089baa001322225335323235002222222222222533533355302912001321233001225335002210031001002501a25335333573466e3c0380040a009c4d40700045406c010840a04098d401088004d40048800840644cd5ce249257369676e656442794f776e65723a204e6f74207369676e6564206279206f776e6572504b480001832123001002375c0022464460046eb0004c8004d5406488cccd55cf8009280d919a80d18021aba1002300335744004028464646666ae68cdc39aab9d5002480008cc8848cc00400c008c030d5d0a80118029aba135744a004464c6402866ae7005c0500484d55cf280089baa0012323232323333573466e1cd55cea8022400046666444424666600200a0080060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008c054d5d0a80119a80680a1aba135744a004464c6403266ae7007006405c4d55cf280089baa00135742a008666aa010eb9401cd5d0a8019919191999ab9a3370ea0029002119091118010021aba135573ca00646666ae68cdc3a80124004464244460020086eb8d5d09aab9e500423333573466e1d400d20002122200323263201b33573803c03603203002e26aae7540044dd50009aba1500233500975c6ae84d5d1280111931900a99ab9c018015013135744a00226ae8940044d55cf280089baa0011335500175ceb44488c88c008dd5800990009aa80b11191999aab9f00225019233501833221233001003002300635573aa004600a6aae794008c010d5d100180909aba100112232323333573466e1d400520002350073005357426aae79400c8cccd5cd19b875002480089401c8c98c8048cd5ce00a80900800789aab9d5001137540022424460040062244002464646666ae68cdc3a800a400c46424444600800a600e6ae84d55cf280191999ab9a3370ea004900211909111180100298049aba135573ca00846666ae68cdc3a801a400446424444600200a600e6ae84d55cf280291999ab9a3370ea00890001190911118018029bae357426aae7940188c98c8040cd5ce00980800700680600589aab9d500113754002464646666ae68cdc39aab9d5002480008cc8848cc00400c008c014d5d0a8011bad357426ae8940088c98c8030cd5ce00780600509aab9e5001137540024646666ae68cdc39aab9d5001480008dd71aba135573ca004464c6401466ae700340280204dd5000919191919191999ab9a3370ea002900610911111100191999ab9a3370ea004900510911111100211999ab9a3370ea00690041199109111111198008048041bae35742a00a6eb4d5d09aba2500523333573466e1d40112006233221222222233002009008375c6ae85401cdd71aba135744a00e46666ae68cdc3a802a400846644244444446600c01201060186ae854024dd71aba135744a01246666ae68cdc3a8032400446424444444600e010601a6ae84d55cf280591999ab9a3370ea00e900011909111111180280418071aba135573ca018464c6402666ae7005804c04404003c03803403002c4d55cea80209aab9e5003135573ca00426aae7940044dd50009191919191999ab9a3370ea002900111999110911998008028020019bad35742a0086eb4d5d0a8019bad357426ae89400c8cccd5cd19b875002480008c8488c00800cc020d5d09aab9e500623263200c33573801e01801401226aae75400c4d5d1280089aab9e500113754002464646666ae68cdc3a800a400446424460020066eb8d5d09aab9e500323333573466e1d400920002321223002003375c6ae84d55cf280211931900499ab9c00c009007006135573aa00226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263200a33573801a01401000e00c26aae7540044dd50009191999ab9a3370ea0029001100391999ab9a3370ea0049000100391931900319ab9c009006004003135573a6ea80052612001122002122001490103505431003200135500322112225335001135003220012213335005220023004002333553007120010050040011122002122122330010040031123230010012233003300200200101"
// const decentralSeedPlutus = "5908b55908b201000033232323322323232323232323232332232332232323232323222323222323253353232325335323235002222222222222533533355301a12001321233001225335002210031001002502325335333573466e3c0380040c00bc4d409400454090010840c040b8d401088004d40048800840844cd5ce249257369676e656442794f776e65723a204e6f74207369676e6564206279206f776e6572504b48000203333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd406c070d5d0a80619a80d80e1aba1500b33501b01d35742a014666aa03eeb94078d5d0a804999aa80fbae501e35742a01066a0360506ae85401cccd5407c0a5d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40cdd69aba150023034357426ae8940088c98c80d8cd5ce01c81c01a09aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a819bad35742a00460686ae84d5d1280111931901b19ab9c039038034135573ca00226ea8004d5d09aba2500223263203233573806a06806026aae7940044dd50009aba1500533501b75c6ae854010ccd5407c0948004d5d0a801999aa80fbae200135742a004604e6ae84d5d1280111931901719ab9c03103002c135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a008602e6ae84d5d1280211931901019ab9c02302201e3333573466e1cd55ce9baa0054800080848c98c807ccd5ce01101080e9999ab9a3370e6aae7540192000233221233001003002375c6ae854018dd71aba135744a00c464c6403c66ae70084080070407c4c98c8074cd5ce249035054350001f135573ca00226ea80044d55cf280089baa0013200135501822112225335001135003220012213335005220023004002333553007120010050040011232230023758002640026aa030446666aae7c004940288cd4024c010d5d080118019aba2002018232323333573466e1cd55cea80124000466442466002006004601c6ae854008c014d5d09aba2500223263201633573803203002826aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea80124000466442466002006004602e6ae854008cd403c058d5d09aba2500223263201b33573803c03a03226aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931900e99ab9c02001f01b01a019135573aa00226ea8004d5d0a80119a805bae357426ae8940088c98c805ccd5ce00d00c80a89aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5405488c8cccd55cf80112804119a8039991091980080180118031aab9d5002300535573ca00460086ae8800c0584d5d080088910010910911980080200189119191999ab9a3370ea002900011a80398029aba135573ca00646666ae68cdc3a801240044a00e464c6402466ae7005405004003c4d55cea80089baa0011212230020031122001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402066ae7004c04803803403002c4d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6401866ae7003c0380284d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200a33573801a01801026ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201333573802c02a02202001e01c01a01801626aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900619ab9c00f00e00a009135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8024cd5ce00600580380309aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900519ab9c00d00c008007006135573aa00226ea80048c8cccd5cd19b8750014800880188cccd5cd19b8750024800080188c98c8018cd5ce00480400200189aab9d37540029309100109100089000a4810350543100112323001001223300330020020013351223002489383536333730653461363633396639323163336265616664313932643665313638616565653862636636326233616638363839643831396430002123001002200101"

// from Haruns but compiled in Jambhala
// const decentralSeedPlutus = "59087a590877010000323322323233223232323232323232323233223232323232322322323222323253353330083333573466e1cd55cea803240004664424660020060046eb8d5d0a8031bae357426ae8940188c98c8070cd5ce00f80e00d1999ab9a3370e6aae74dd5002240004038464c6403866ae7007c070068cccd5cd19b8735573aa0049000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233501901a35742a01866a0320346ae85402ccd406406cd5d0a805199aa80ebae501c35742a012666aa03aeb94070d5d0a80419a80c8121aba150073335501d02575a6ae854018c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233502f75a6ae854008c0c0d5d09aba2500223263203233573806a06406026aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a05eeb4d5d0a80118181aba135744a004464c6406466ae700d40c80c04d55cf280089baa001357426ae8940088c98c80b8cd5ce01881701609aab9e5001137540026ae854014cd4065d71aba150043335501d021200135742a006666aa03aeb88004d5d0a80118119aba135744a004464c6405466ae700b40a80a04d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a80118099aba135744a004464c6403866ae7007c070068406c4c98c806ccd5ce2481035054350001b135573ca00226ea80044d55cf280089baa001322225335323235002222222222222533533355302912001321233001225335002210031001002501a25335333573466e3c0380040a009c4d40700045406c010840a04098d401088004d40048800840644cd5ce249257369676e656442794f776e65723a204e6f74207369676e6564206279206f776e6572504b480001832123001002375c0022464460046eb0004c8004d5406488cccd55cf8009280d919a80d18021aba1002300335744004028464646666ae68cdc39aab9d5002480008cc8848cc00400c008c030d5d0a80118029aba135744a004464c6402866ae7005c0500484d55cf280089baa0012323232323333573466e1cd55cea8022400046666444424666600200a0080060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008c054d5d0a80119a80680a1aba135744a004464c6403266ae7007006405c4d55cf280089baa00135742a008666aa010eb9401cd5d0a8019919191999ab9a3370ea0029002119091118010021aba135573ca00646666ae68cdc3a80124004464244460020086eb8d5d09aab9e500423333573466e1d400d20002122200323263201b33573803c03603203002e26aae7540044dd50009aba1500233500975c6ae84d5d1280111931900a99ab9c018015013135744a00226ae8940044d55cf280089baa0011335500175ceb44488c88c008dd5800990009aa80b11191999aab9f00225019233501833221233001003002300635573aa004600a6aae794008c010d5d100180909aba100112232323333573466e1d400520002350073005357426aae79400c8cccd5cd19b875002480089401c8c98c8048cd5ce00a80900800789aab9d5001137540022424460040062244002464646666ae68cdc3a800a400c46424444600800a600e6ae84d55cf280191999ab9a3370ea004900211909111180100298049aba135573ca00846666ae68cdc3a801a400446424444600200a600e6ae84d55cf280291999ab9a3370ea00890001190911118018029bae357426aae7940188c98c8040cd5ce00980800700680600589aab9d500113754002464646666ae68cdc39aab9d5002480008cc8848cc00400c008c014d5d0a8011bad357426ae8940088c98c8030cd5ce00780600509aab9e5001137540024646666ae68cdc39aab9d5001480008dd71aba135573ca004464c6401466ae700340280204dd5000919191919191999ab9a3370ea002900610911111100191999ab9a3370ea004900510911111100211999ab9a3370ea00690041199109111111198008048041bae35742a00a6eb4d5d09aba2500523333573466e1d40112006233221222222233002009008375c6ae85401cdd71aba135744a00e46666ae68cdc3a802a400846644244444446600c01201060186ae854024dd71aba135744a01246666ae68cdc3a8032400446424444444600e010601a6ae84d55cf280591999ab9a3370ea00e900011909111111180280418071aba135573ca018464c6402666ae7005804c04404003c03803403002c4d55cea80209aab9e5003135573ca00426aae7940044dd50009191919191999ab9a3370ea002900111999110911998008028020019bad35742a0086eb4d5d0a8019bad357426ae89400c8cccd5cd19b875002480008c8488c00800cc020d5d09aab9e500623263200c33573801e01801401226aae75400c4d5d1280089aab9e500113754002464646666ae68cdc3a800a400446424460020066eb8d5d09aab9e500323333573466e1d400920002321223002003375c6ae84d55cf280211931900499ab9c00c009007006135573aa00226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263200a33573801a01401000e00c26aae7540044dd50009191999ab9a3370ea0029001100391999ab9a3370ea0049000100391931900319ab9c009006004003135573a6ea80052612001122002122001490103505431003200135500322112225335001135003220012213335005220023004002333553007120010050040011122002122122330010040031123230010012233003300200200101"

// compiled from Jambhla but using spacebudz wormhole methods
// https://github.com/spacebudz/wormhole/blob/af6828ad27e0ae801cb8331f389563c79634c8cd/src/onchain.hs   --- referenceInstance and referenceSerialized
// https://github.com/spacebudz/wormhole/blob/af6828ad27e0ae801cb8331f389563c79634c8cd/src/ghc/app/Main.hs#L13
const decentralSeedPlutus = "59087f59087c01000032323232332232323232323232323233223233223232323232322335501a22323222323253353330083333573466e1cd55cea803240004664424660020060046eb8d5d0a8031bae357426ae8940188c98c807ccd5ce01101080e9999ab9a3370e6aae74dd5002240004042464c6403e66ae70088084074cccd5cd19b8735573aa0049000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233501a01b35742a01866a0340366ae85402ccd4068070d5d0a805199aa80f3ae501d35742a012666aa03ceb94074d5d0a80419a80d0139aba150073335501e02875a6ae854018c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233503275a6ae854008c0ccd5d09aba2500223263203533573807006e06626aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a064eb4d5d0a80118199aba135744a004464c6406a66ae700e00dc0cc4d55cf280089baa001357426ae8940088c98c80c4cd5ce01a01981789aab9e5001137540026ae854014cd4069d71aba150043335501e024200135742a006666aa03ceb88004d5d0a80118131aba135744a004464c6405a66ae700c00bc0ac4d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a801180b1aba135744a004464c6403e66ae7008808407440804c98c8078cd5ce24810350543500020135573ca00226ea80044d55cf280089baa001222325335001101b1335738921257369676e656442794f776e65723a204e6f74207369676e6564206279206f776e6572504b480001a335501d32235002222222222222533533355301312001321233001225335002210031001002501c25335333573466e3c0380040a40a04d407800454074010840a4409cd400488008d400c88004c8004d5406488448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000448c88c008dd6000990009aa80c911999aab9f0012500a233500930043574200460066ae880080608c8c8cccd5cd19b8735573aa004900011991091980080180118071aba150023005357426ae8940088c98c8058cd5ce00c80c00a09aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180b9aba1500233500f016357426ae8940088c98c806ccd5ce00f00e80c89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403a66ae7008007c06c0680644d55cea80089baa00135742a00466a016eb8d5d09aba2500223263201733573803403202a26ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355016223233335573e0044a010466a00e66442466002006004600c6aae754008c014d55cf280118021aba200301613574200222440042442446600200800624464646666ae68cdc3a800a400046a00e600a6ae84d55cf280191999ab9a3370ea00490011280391931900919ab9c01501401000f135573aa00226ea800448488c00800c44880048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900819ab9c01301200e00d00c00b135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900619ab9c00f00e00a135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028cd5ce00680600409baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c804ccd5ce00b00a80880800780700680600589aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae7003c0380280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200933573801801600e00c26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401466ae7003403002001c0184d55cea80089baa0012323333573466e1d40052002200623333573466e1d40092000200623263200633573801201000800626aae74dd5000a4c244004244002240029210350543100112230020011123230010012233003300200200101"


const minAda = 2000000;
const minAdaBigInt = 2000000n;


// const infoToHash = "Passport# 123456 address 1111 Los angeles 10-21-23-750AM";    // adding date/time just for testing purpose for unique info
const infoToHash = "Passport# 123456 address 1111 Los angeles 11-03-23-318PM";    // adding date/time just for testing purpose for unique info
let gPersonalInfo = "Passport# 123456 address 1111 Los angeles 11-18-23-0045AM"
let seed23Words : string = 'custom help female park blush clutch fancy lion fence innocent rebuild amount tip custom help female park blush clutch fancy lion fence innocent';   // for now 13 words
let indexWord : number = 10;
let passPhrase : string = 'myOwnPassword$123';
let varDecryptWord = "";

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
  const [decryptWord,setdecryptWord] = useState<string>('');
  const [firstLine, setFirstLine] = useState<string>('');
  const [readSecretData, setReadSecretData] = useState<any>(null);

// for decentralized Seed phrase mgr
  // for seed phrase 23 words from User
  const [seed23InputValue, setseed23InputValue] = useState('');
  // Index of hidden word
  const [indexInputValue, setIndexInputValue] = useState('');
  // for Pass Phrase from User
  const [passPhraseinputValue, setPassPhraseinputValue] = useState('');
  // for Personal Info
  const [personalInfoinputValue, setPersonalInfoinputValue] = useState('');


  useEffect(() => {
    if (lucid) {
      ;
    } else {
      initLucid(walletStore.name).then((Lucid: Lucid) => { setLucid(Lucid) })
    }
  }, [lucid])



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

  const sLockEncryptedSeedPhrase = async () => {
    if (lucid) {
      gPersonalInfo = personalInfoinputValue;
      const hashedDataInString = hashInfo(gPersonalInfo);
      // const hashedDataInString = hashInfo(personalInfoinputValue);
      

      const sValCbor = decentralSeedPlutus;

      const { paymentCredential } = lucid.utils.getAddressDetails(
        await lucid.wallet.address(),
      );      

      console.log("Payment pub key hash = ", paymentCredential?.hash!)     

      // Customer Personal info hashed to form the script address - used as parameter


      console.log("info to Hash = ", gPersonalInfo)
      console.log("hashed info string = ", hashedDataInString)
      console.log("hashed info from text = ", fromText(hashedDataInString))


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

      const sValAddress = lucid.utils.validatorToAddress(sValidator)
      console.log("PayToScript Address of Decentralized seed recovery Script: ", sValAddress )

      // Now do encryption
      // let encryptedString : string = '';
      // 23 words - 1 word left out. plus index at the end
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
      console.log("Datum = ", datumInit)

      
      const datumData = Data.to(datumInit, MyDatum)
      console.log("Datum as Data = ", datumData)
      
      const utxos = await lucid.wallet.getUtxos()
    
      // C.min_fee
      const tx = await lucid.newTx()
        .payToContract(sValAddress, {inline: Data.to( datumInit, MyDatum)}, {lovelace: BigInt(2000000)})
        .complete();
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
        gPersonalInfo = personalInfoinputValue;
        const sValCbor = decentralSeedPlutus;

        const { paymentCredential } = lucid.utils.getAddressDetails(
          await lucid.wallet.address(),
        );      
  
        console.log("Payment pub key hash = ", paymentCredential?.hash!)     
  
        // Customer Personal info hashed to form the script address - used as parameter
        const hashedDataInString = hashInfo(gPersonalInfo);
        console.log("info to Hash = ", gPersonalInfo)
        console.log("hashed info string = ", hashedDataInString)
        console.log("hashed info From Text = ", fromText(hashedDataInString))
  
  
        const paramInit :  Params = [{
          pInfoHash: fromText(hashedDataInString)
          // pInfoHash: fromText(infoToHash)
        }];
        
        
        const sValidator : SpendingValidator = {
          type: "PlutusV2",
          script: applyParamsToScript<Params>(
            decentralSeedPlutus,
            paramInit,
            Params,
          ),
        }

        // const sValidator : SpendingValidator = 
        //   {type:"PlutusV2", script: sValCborCrowdFund}
        const sValAddress = lucid.utils.validatorToAddress(sValidator)
        console.log("Validator decentralSeedPlutus script address = ", sValAddress)
        const valUtxos = await lucid.utxosAt(sValAddress)
        console.log(" type of valUtxos = ", typeof(valUtxos))

        const redeemer = Data.to(
          new Constr(0, [])
        )
        let found: UTxO | undefined = undefined;

        console.log("UTXOs length = ", valUtxos.length)
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
            console.log ("came here after the IFs")
            found = curr 
        }
        if (!found) throw new Error("Naughty Datum")

          // curr.datum = await lucid.datumOf(curr)
          // console.log("Datum on UTXO = ", curr.datum)

          // console.log("Curr = ", curr);

          try {
            console.log(" About to convert datum to our schema type")

            const utxoInDatum = Data.from(found.datum!, MyDatum)
            
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

          }   // End - try to read Datum  
          catch (error) {
            // Handle the error here -- we will just skip for now
            console.log('An error occurred in reading datum', error);
          }

          
        
        
        console.log("Found on UTXO = ", found)
        // if (!found) throw new Error("Naughty Datum")
       
        const utxos = await lucid.wallet.getUtxos();
        
        const tx = await lucid
          // .newTx()
          .newTx().addSigner(await lucid.wallet.address())
          .attachSpendingValidator(sValidator)
          .collectFrom([found], Data.void())
          .complete();
  
          const signedTx = await tx.sign().complete();
          const txHash = await signedTx.submit();
          console.log("Collect Test TxHash: " + txHash)
          const decryptWord = varDecryptWord;
          setdecryptWord(varDecryptWord);
          // setdecryptWord(decryptWord);   // set the output retrieved words
          return txHash;
      }   // End - if lucid

  

  }      // End - Function sContributeCrowdFund
 

  // function hexToBytes(hex) {
  //   const bytes = new Uint8Array(hex.length / 2);
  //   for (let i = 0; i < bytes.length; i++) {
  //     bytes[i] = parseInt(hex.substr(i * 2, 2), 16);
  //   }
  //   return bytes;
  // }
  
  // const hexString = '5a7c202f0172c3b810c2a44e1d36c29cc39a62c29dc2acc286c29ec3b8c3b9c381c3aec387516f36286fc280c3bdc283';
  // const byteArray = hexToBytes(hexString);
  // console.log(byteArray);

  // const text = new TextDecoder('utf-8').decode(byteArray);
  // console.log(text);
  

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

          const sValCbor = decentralSeedPlutus;

          const { paymentCredential } = lucid.utils.getAddressDetails(
            await lucid.wallet.address(),
          );      
    
          console.log("Payment pub key hash = ", paymentCredential?.hash!)     
    
          // const infoToHash = "Passport# 123456 address 1111 Los angeles 10-21-23-730AM";    // adding date/time just for testing purpose for unique info
          // const hashedData = hashInfo(infoToHash);
          const hashedDataInString = hashInfo(gPersonalInfo);
          console.log("info to Hash = ", infoToHash)
          console.log("hashed info = ", hashedDataInString)
          console.log("hashed info from text = ", fromText(hashedDataInString))
    
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

          const sValAddress = lucid.utils.validatorToAddress(sValidator)
          
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


  const handleChangeSeed = (event: React.ChangeEvent<HTMLInputElement>) => {
    setseed23InputValue(event.target.value);
  };
  const handleChangeIndex = (event: React.ChangeEvent<HTMLInputElement>) => {
    setIndexInputValue(event.target.value);
  };
  const handleChangePassword = (event: React.ChangeEvent<HTMLInputElement>) => {
    setPassPhraseinputValue(event.target.value);
  };
  const handleChangePersonalInfo = (event: React.ChangeEvent<HTMLInputElement>) => {
    setPersonalInfoinputValue(event.target.value);
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



  // const hashInfo = (data: string): string => {
  //   const hash = CryptoJS.SHA256(data);
  //   return hash.toString(CryptoJS.enc.Hex);  // Convert the WordArray to a hex string for easier display/usage
  // }  


  const hashInfo = (data: string): string => {
    
    const hash = CryptoJS.SHA256(data);
    const hexString = hash.toString(CryptoJS.enc.Hex); 
    const stringFromHex = hexToString(hexString);
    return stringFromHex
    // return  // Convert the WordArray to a hex string for easier display/usage
  }  

  const hexToString = (hex: string) => {
    let str = '';
    for (let i = 0; i < hex.length; i += 2) {
      str += String.fromCharCode(parseInt(hex.substr(i, 2), 16));
    }
    return str;
  };


  const encryptSeedPassInfo =  (seedU: string, passwordU: string) => { 
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
      const seed23Words = seedU;   
      // let encryptionPhrase: CryptoJS.lib.WordArray = CryptoJS.enc.Utf8.parse(passwordU);
      let encryptionPhrase = (passwordU);

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
        encyptedTextRecursive = encryptD(encyptedTextRecursive, encryptionPhrase);
        // encyptedTextRecursive = encrypt(encyptedTextRecursive, encryptedPBKDF2.toString());
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
        decryptedTextRecursive = decryptD(decryptedTextRecursive, encryptionPhrase);
        // decryptedTextRecursive = decrypt(decryptedTextRecursive, encryptedPBKDF2.toString());
        console.log('decryptedTextRecursive:', decryptedTextRecursive);
      }
      console.log('Final decryptedTextRecursive:', decryptedTextRecursive);


      return encyptedTextRecursive;
      //infoFromSeed

      // fetchFirstLine(); // Call the function when the component mounts
    }   // if Lucid
  }   // End function `testCodeEncryption`

  // https://github.com/brix/crypto-js/blob/develop/test/aes-test.js


    // const keyWordArray = CryptoJS.enc.Utf8.parse(key);
    // console.log("Key type = ", typeof(key))
    // const encrypted = CryptoJS.AES.encrypt(text, keyWordArray);
  // Encrypt function
  function encryptD(text: string, key: string): string {
    const encrypted = CryptoJS.AES.encrypt(text, key);
    return encrypted.toString(); // Convert to Base64 string
  }

  // Decrypt function
  function decryptD(encryptedText: string, key: string): string {
    // const keyWordArray = CryptoJS.enc.Utf8.parse(key);
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


  function ReusableInput({ type = "text", id, value, onChange, placeholder, style }) {
    return (
      <input
        type={type}
        id={id}
        value={value}
        onChange={onChange}
        placeholder={placeholder}
        style={style}
      />
    );
  }
  
  const compPassword = (
      <ReusableInput
        id="password"
        value={passPhraseinputValue}
        onChange={handleChangePassword}
        placeholder="Enter Password"
        style={{ width: '920px' }}
      />
  );
  
  const compIndex = (
      <ReusableInput
          type="number"
          id="indexWord"
          value={indexInputValue}
          onChange={handleChangeIndex}
          placeholder="Enter index of hidden word"
          style={{ width: '240px' }} // Set the width to your desired value          
      />
  );

  const compPersonalInfo = (  
      <ReusableInput
          type="text"
          id="personalInfo"
          value={personalInfoinputValue}
          onChange={handleChangePersonalInfo}
          placeholder="Enter Personal Info"
          style={{ width: '1200px' }} // Set the width to your desired value          
      /> 
  );



  const compPersonalInfo2 = (
    <input
      type="text"
      id="personalInfo"
      value={personalInfoinputValue}
      onChange={handleChangePersonalInfo}
      placeholder="Enter Personal Info"
      style={{ width: '1200px' }}
    />
  );
  




  return (

<div className="px-10 font-montserrat text-gray-300 bg-gray-800">
  
  <div className="navbar bg-gray-900"> 
  {/* <div className="mt-10 p-5 bg-gray-700 rounded-lg shadow-xl mx-60"> */}
    <div className="flex-1">
      <Link href="/" className="btn text-xl text-white">Cardano</Link>
    </div>
    <div className="flex-none">
      <WalletConnect />
    </div>
  </div>

  {/* <div className="text-center my-5 text-lg"> */}
  <div className="mt-10 p-5 bg-gray-700 rounded-lg shadow-xl mx-60">
    <span className="font-semibold">My Wallet Address:</span> {walletStore.address}
  </div>

  {/* <div className="grid grid-cols-1 md:grid-cols-2 gap-5 mx-10 my-10"> */}
  <div className="mt-10 p-5 bg-gray-700 rounded-lg shadow-xl mx-60">
    <button className="btn bg-gradient-to-r from-blue-500 to-teal-400 hover:scale-105 text-white rounded-lg shadow-lg" onClick={() => { sPrintScriptAddress() }}>Get Script Address</button>
    {/* Other buttons can be added here in similar style */}
    {/* Uncomment and style other buttons as needed */}
  </div>

  <div className="mt-10 p-5 bg-gray-700 rounded-lg shadow-xl mx-60">
    <input
      type="text"
      id="seed"
      value={seed23InputValue}
      onChange={handleChangeSeed}
      placeholder="Enter 23 words Seed Phrase hiding 1 word"
      className="w-full px-4 py-2 bg-gray-600 border border-gray-500 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
    />
    <br />
    <br />
      {/* we will re-use this in Redeem so make it a variable */}
    {compIndex}            
    <br />
    <br />
    {compPassword}        {/* we will re-use this in Redeem so make it a variable */}
    <br />
    <br />
    <div>
    {compPersonalInfo2}      {/* we will re-use this in Redeem so make it a variable */}
    </div>
    <br />
    <br />
    <button className="btn bg-gradient-to-r from-green-500 to-blue-400 hover:scale-105 text-white rounded-lg shadow-lg mt-5" onClick={() => { sLockEncryptedSeedPhrase() }}>Lock & Encrypt SeedPhrase</button>
    {/* Include other components or buttons here as needed */}
  </div>

  <div className="mx-10 my-10">
    {txHash !== null && <p>Tx Hash: {txHash}</p>}
    {/* Include other transaction-related components here */}
  </div>

  <div className="redeem mt-10 p-5 bg-gray-700 rounded-lg shadow-xl mx-60">
      <br />
      <br />
      {compPassword}       {/*   we will re-use this in Redeem so make it a variable */}
      <br />
      <br />
      {compPersonalInfo}    {/*   we will re-use this in Redeem so make it a variable */}
      <br />
      <br />
    <button className="btn bg-gradient-to-r from-purple-500 to-pink-400 hover:scale-105 text-white rounded-lg shadow-lg" onClick={() => { sDecentralSeedRedeem() }}>Decrypt Seed Words and Redeem</button>
    {/* Include other components or buttons here as needed */}
  </div>

  <div className="mx-10 my-10">
    {decryptWord !== null && <p>Decrypted Seed Phrase and Index : {decryptWord}</p>}
    {/* Include other display components or messages here */}
  </div>
</div>

    
  )   // End return
}

export default offChain
