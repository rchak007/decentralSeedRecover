import { NextApiRequest, NextApiResponse } from 'next';
import { readFirstLineFromFile } from '../../utils/fileUtils';

import fs from 'fs';
import path from 'path';

export default function handler(req: NextApiRequest, res: NextApiResponse) {
    console.log("from read-file")
    if (req.method === 'GET') {
        console.log("from read-file handler 1st line ")
        const filePath = path.join(process.cwd(), '/Wallets/Contributor/contributor.seed');
        // const filePath = '/Wallets/Contributor/contributor.seed'; // Replace this with your file's path
        try {
            console.log("From read-file.ts -file name is ", filePath)
            const firstLine = readFirstLineFromFile(filePath);
            console.log("firstLine is = ", firstLine)
            res.status(200).json({ firstLine });
        } catch (error) {
            res.status(500).json({ error: 'Error reading file from read-file.' });
        }
    } else {
        res.status(405).json({ error: 'Method not allowed from read-file.' });
      }
}


// import fs from 'fs';
// import path from 'path';

// export default function handler(req, res) {
//   if (req.method === 'POST') {
//     const filePath = path.join(process.cwd(), 'testfile.txt');
//     const dataToWrite = 'This is the data that will be written to the file.\n';

//     fs.writeFile(filePath, dataToWrite, (error) => {
//       if (error) {
//         console.error('Error writing to the file:', error);
//         res.status(500).json({ error: 'Error writing to the file.' });
//       } else {
//         console.log('Data has been written to the file successfully!');
//         res.status(200).json({ message: 'Data written to the file successfully!' });
//       }
//     });
//   } else {
//     res.status(405).json({ error: 'Method not allowed.' });
//   }
// }