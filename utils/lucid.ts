import { Blockfrost, Lucid } from 'lucid-cardano';
import { contr1Seed } from "./seed.tsx"

const initLucid = async (wallet: string) => {
//     const api = (await window.cardano[
//         wallet.toLowerCase()
//     ].enable())

    const nami = await window.cardano.nami.enable();
    // const eternl = window.cardano.eternl.enable();
    // const gero = await window.cardano.gero.enable();

    const apiValue = await fetchBlockFrostApi();  
    
    const lucid = await Lucid.new(
        new Blockfrost('https://cardano-preprod.blockfrost.io/api/v0', apiValue.toString()) //process.env.NEXT_PUBLIC_BLOCKFROST as string),
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

export const fetchBlockFrostApi = async () => {
    try {
      const response = await fetch('/api/getData');
      const data = await response.json();
      if (data.seed[0]) {
        const blockfrostApi = data.seed[2].description;
        console.log("api = ", blockfrostApi)
        return blockfrostApi; // Return the value
      }      
    } catch (error) {
      console.error('Error fetching secret data:', error);
    }
    return null; // Return null if there was an error or no data was found
  }; 

export default initLucid;