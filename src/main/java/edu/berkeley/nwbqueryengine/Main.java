package edu.berkeley.nwbqueryengine;

import edu.berkeley.nwbqueryengine.api.ArrayInput;
import edu.berkeley.nwbqueryengine.api.FileInput;
import edu.berkeley.nwbqueryengine.api.Input;
import edu.berkeley.nwbqueryengine.python.PyServer;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


/**
 * Created by petr-jezek on 6.4.17*
 * <p>
 * queries examples:
 * </p>
 * <p>
 * <p>
 * "analysis=(description LIKE whisker)"<br><br/>
 * "processing=(electrode_idx>30)"<br><br/>
 * "epochs=(start_time>200 & stop_time<400 | stop_time>1600)"<br><br/>
 * </p>
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
        try {
            System.loadLibrary("HDFql");
            if (args.length > 0) {
                String arg1 = args[0];
                if (arg1.equals("pyserver")) {
                    PyServer server = new PyServer();
                    //server.start(new FileInput());
                    server.start(new ArrayInput());
                } else {
                    if (args.length > 1) {
                        String expression = args[1];
                        logger.debug("Expression: " + expression);
                        Input f = new FileInput();
                        f.executeQuery(arg1, expression);
                    }
                }

            } else {
                String message = "A file/dir has not been given...";
                logger.error(message);
                System.out.println(message);

            }
        } catch (Exception e) {
            logger.error(e);
        }


    }

}
