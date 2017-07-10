package edu.berkeley.nwbqueryengine;

import edu.berkeley.nwbqueryengine.connectors.HDF5Connector;
import edu.berkeley.nwbqueryengine.io.Facade;
import edu.berkeley.nwbqueryengine.io.FileFacade;
import edu.berkeley.nwbqueryengine.python.PyServer;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.query.parser.QueryParser;
import edu.berkeley.nwbqueryengine.data.NwbResult;
import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import py4j.GatewayServer;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileFilter;
import java.util.LinkedList;
import java.util.List;


/**
 * Created by petr-jezek on 6.4.17*
 * <p>
 * queries examples:
 * </p>
 * <p>
 *
    "analysis=(description LIKE whisker)"<br><br/>
 "processing=(electrode_idx>30)"<br><br/>
 "epochs=(start_time>200 & stop_time<400 | stop_time>1600)"<br><br/>
 </p>
 * <p>
 * jezekp@kiv.zcu.cz
 * <p/>
 */
public class Main {
    private static String path = "/home/petr-jezek/Data/nwb_datasets/nwbMatlab_DG";
    //private static String path = "/tmp/datasets";
    private static String file = "ANM186997_20130317.nwb";
    private static String fname = path + "/" + file;

    private static Log logger = LogFactory.getLog(Main.class);


    public static void main(String[] args) {
        BufferedReader br = null;
        try {
            System.loadLibrary("HDFql");
            if (args.length > 0) {
                String arg1 = args[0];
                if(arg1.equals("pyserver")) {
                    PyServer server = new PyServer();
                    server.start();
                } else {
                    if (args.length > 1) {
                        String file = args[0];
                        String expression = args[1];
                        logger.debug("Expression: " + expression);
                        Facade f = new FileFacade();
                        f.executeQuery(file, expression);
                    }
                }

            } else {
                String message = "A file/dir has not been given...";
                logger.error(message);
                System.out.println(message);

            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            IOUtils.closeQuietly(br);
        }


    }

}
