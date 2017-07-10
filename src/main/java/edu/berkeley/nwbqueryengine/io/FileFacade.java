package edu.berkeley.nwbqueryengine.io;

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
public class FileFacade implements Facade<String, String>{

    private Log logger = LogFactory.getLog(getClass());

    public void executeQuery(String file, String expression) {
        try {
            QueryParser p = new QueryParser();

            logger.debug("Expression: " + expression);
            Query query = p.parse(expression);


            List<NwbResult> completeRes = new LinkedList<>();
            File obj = new File(file);
            if (obj.isDirectory()) {
                for (File item : obj.listFiles(new FileFilter() {
                    @Override
                    public boolean accept(File pathname) {
                        return pathname.getName().toLowerCase().endsWith(".nwb");
                    }
                })) {
                    completeRes.addAll(processFile(item, query));
                }

            } else {
                completeRes = processFile(obj, query);
            }
            logger.info("I have complete: " + completeRes.size());
        } catch (Exception e) {
            logger.error(e);
        }

    }

    public List<NwbResult> processFile(File obj, Query query) throws Exception {
        HDF5Connector connector = new HDF5Connector(obj);
        NwbProcessor processor = new NwbProcessor(connector);

        List<NwbResult> res = processor.evaluate(query);

        res.forEach(name -> {
            logger.debug("Have res: " + name);
            //            System.out.println(name);
        });
        logger.info("I have: " + res.size());
        return res;
    }
}
