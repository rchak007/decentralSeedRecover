import fs from 'fs';
import path from 'path';

export default function handler(req, res) {
  console.log("from writeToFile")
  if (req.method === 'POST') {
    const filePath = path.join(process.cwd(), 'testfile.txt');
    const dataToWrite = 'This is the data that will be written to the file.\n';
    console.log("from write-toFile= ", filePath)
    fs.writeFile(filePath, dataToWrite, (error) => {
      if (error) {
        console.error('Error writing to the file:', error);
        res.status(500).json({ error: 'Error writing to the file.' });
      } else {
        console.log('From WriteToFile--- Data has been written to the file successfully!');
        res.status(200).json({ message: 'Data written to the file successfully!' });
      }
    });
  } else {
    res.status(405).json({ error: 'Method not allowed.' });
  }
}
