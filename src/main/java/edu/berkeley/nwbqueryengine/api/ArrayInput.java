package edu.berkeley.nwbqueryengine.api;

import edu.berkeley.nwbqueryengine.data.NwbResult;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * Created by petr-jezek on 21.7.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class ArrayInput implements Input<byte[], String> {

    protected Input input = new FileInput();

//    The way to read nbw file from python, convert it to byteArray and send it to the server

    //    with open('/tmp/test.nwb', mode='rb') as file: # b is important -> binary
//            fileContent = file.read()
//
//    b = bytearray(fileContent)
//    res = gateway.executeQuery(b, "query")
//
//
    @Override
    public List<NwbResult> executeQuery(byte[] storage, String query) throws InputException {
        String fileName = "/tmp/test_" + System.currentTimeMillis() + ".nwb";
        List<NwbResult> res;
        File file = new File(fileName);
        try {
            FileUtils.writeByteArrayToFile(file, storage);
            res = input.executeQuery(fileName, query);
        } catch (IOException e) {
            throw new InputException(e);
        } finally {
            FileUtils.deleteQuietly(file);
        }

        return res;
    }
}
