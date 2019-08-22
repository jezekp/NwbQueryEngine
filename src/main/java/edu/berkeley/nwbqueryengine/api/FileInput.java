package edu.berkeley.nwbqueryengine.api;

import edu.berkeley.nwbqueryengine.NwbProcessor;
import edu.berkeley.nwbqueryengine.connectors.HDF5Connector;
import edu.berkeley.nwbqueryengine.data.NwbResult;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.query.parser.QueryParser;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.File;
import java.io.FileFilter;
import java.util.LinkedList;
import java.util.List;

/**
 * Created by petr-jezek on 10.7.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class FileInput implements Input<String, String> {

    private Log logger = LogFactory.getLog(getClass());

    public List<NwbResult> executeQuery(String file, String expression) throws InputException {
        List<NwbResult> completeRes = new LinkedList<>();
        try {
            QueryParser p = new QueryParser();
            logger.debug("Expression: " + expression);
            Query query = p.parse(expression);

            File obj = new File(file);
            if (obj.isDirectory()) {
                goRecursively(obj, completeRes, query);
            } else {
                logger.info("Processing file: " + obj);
                completeRes = processFile(obj, query);
            }
            logger.info("I have complete: " + completeRes.size());
        } catch (Exception e) {
            e.printStackTrace();
            logger.error(e);
            throw new InputException(e);
        }
        return completeRes;

    }

    private synchronized List<NwbResult> processFile(File obj, Query query) throws Exception {
        HDF5Connector connector = new HDF5Connector(obj);
        NwbProcessor processor = new NwbProcessor(connector);

        List<NwbResult> res = processor.evaluate(query);

        res.forEach(name -> {
            logger.debug("I have item: " + name);
        });
        logger.info("I have: " + res.size());
        return res;
    }

    private void goRecursively(File obj, List<NwbResult> completeRes, Query query) throws Exception {
        logger.debug("Go through the directory: " + obj);
        for (File item : obj.listFiles()) {
            if(item.isDirectory()) {
                goRecursively(item, completeRes, query);
            }
            else if(item.getAbsolutePath().toLowerCase().endsWith(".nwb")) {
                logger.info("Processing file: " + item + " in directory: " + obj);
                completeRes.addAll(processFile(item, query));
            }
        }
    }
}
