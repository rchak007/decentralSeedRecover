import { Blockfrost, Lucid } from 'lucid-cardano';
import { contr1Seed } from "./seed.tsx"

const initLucid = async (wallet: string) => {
//     const api = (await window.cardano[
//         wallet.toLowerCase()
//     ].enable())

    const nami = await window.cardano.nami.enable();
    // const eternl = window.cardano.eternl.enable();
    // const gero = await window.cardano.gero.enable();
    
    const lucid = await Lucid.new(
        // new Blockfrost('https://cardano-preprod.blockfrost.io/api/v0', "preprodLT9nUTb22P26FiVH42jSFJU2IZaNxZVz") //process.env.NEXT_PUBLIC_BLOCKFROST as string),
        // ,'Preprod');
        new Blockfrost('https://cardano-preprod.blockfrost.io/api/v0', "preprod232JoxaAx9xRCqCdMJlweO5paor8jPfL") //process.env.NEXT_PUBLIC_BLOCKFROST as string),
        ,'Preprod');
    // const lucid = await Lucid.new(
            // new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", "previewY7wWn4mtcYHascUO7PyxeCXadkAkBVz2"),
            // "Preview",
          // );
    // const lucid = await Lucid.new(
    //         new Blockfrost('https://cardano-mainnet.blockfrost.io/api/v0', "mainneto2wd71NAi5sZMWDHUTXxgvMTEC6ciS2I") //process.env.NEXT_PUBLIC_BLOCKFROST as string),
    //         ,'Mainnet')
    //lucid.selectWallet(api)
    lucid.selectWallet(nami)
    // lucid.selectWalletFromSeed(contr1Seed)    // to use from seed
    //setLucid(lucid)
    return lucid;
}


export default initLucid;