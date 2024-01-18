## Here are some key things to know about the code in this project:

pages/api/* - These files define API routes for the Next.js backend API. For example pages/api/getData.ts serves some mock secret data.
utils/* - Common utility functions like interacting with the blockchain, wallet connections etc.
components/* - React components for the UI like wallet connect, NFT grid etc.
store.ts - Sets up global app state management with Easy Peasy. Exposes hooks to access state.
lucid.ts - Helper to initialize the Lucid Cardano library and connect to a wallet.
cardano.ts - Functions to get wallet balances and NFTs using the Blockfrost API.
valueUtils.ts - Utilities for converting between token value formats.
offerUtils.ts - Utilities for constructing on-chain offer data.
pages/index.tsx - Main app page, fetches data and sets up state.
pages/offChainV2.tsx - Off-chain workflow page for managing seed phrases.

### So in summary:

API routes, utility functions, React components and state management setup.
Main pages index.tsx and offChainV2.tsx.
Integrations for wallet, blockchain interaction.


#### The pages/index.tsx file is the main page component rendered at the root route '/' of the Next.js application.

## Some key things:

It imports React hooks like useState, useEffect to manage state and effects.
It uses useStoreState and useStoreActions from the Easy Peasy store to access global state.
It fetches wallet address and NFT data for the connected wallet using getAssets and displays it.
It fetches some sample secret data from /api/getData endpoint on mount and wallet change.
It has sample code to fetch data from /secret.json public data file.
It redirects to /offChainV2 route on mount using Next.js router.
The main UI with wallet connect, navbar, NFT grid etc is conditionally rendered but returned null for now.

So in summary, it sets up data fetching, state management for the home page but just redirects for now. The main UI will likely be built out later.


## Here are some key things about pages/offChainV2.tsx:

It implements an off-chain workflow for managing secret seed phrases.
The seed phrase is stored in component state, not persisted anywhere.
There are functions to generate a random phrase, set the phrase, and clear the phrase.
The phrase is revealed or hidden based on a showSecret state.
It has a simulated "submit" action that logs the phrase to the console.
In a real app, this would submit the phrase to a secure server endpoint.
The page never directly exposes the raw phrase in the client.
All logic is done off-chain, no on-chain transactions.
This avoids exposing secret data on the blockchain.
The page demonstrates one pattern for managing sensitive data with blockchain apps.
The same logic could be adapted for private keys, API keys etc.
No real security, just illustrating the concept.

So in summary, it shows an off-chain workflow for handling secret data in a blockchain app context. The key is avoiding on-chain exposure.

 
