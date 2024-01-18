// pages/api/getData.ts
import { NextApiRequest, NextApiResponse } from 'next';

// Sample secret data (replace with your actual data)
// ONLY BLOCKFROST APi is needed
const secretData = {
    
        "seed": [
          {
            "id": 1,
            "name": "Contributor Seed",
            "description": "word 1 word2 "
          },
          {
            "id": 2,
            "name": "Beneficiary Private key",
            "description": "ed25519_sk1rkm9k26wpt9......"
          },
          {
            "id": 3,
            "name": "BlockFrost",
            "description": "preprod2...."
          },
          {
          "id": 4,
          "name": "FirstNami",
          "description": "word 1 word 2 word 3....."
          },   
          {
            "id": 5,
            "name": "EumrgoCourseNami",
            "description": "word 1 word 1"
            },          
        ]
    };

export default (req: NextApiRequest, res: NextApiResponse) => {
  // Return the secret data as a JSON response
  res.status(200).json(secretData);
};
