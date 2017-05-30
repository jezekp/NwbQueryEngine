package edu.berkeley.nwbqueryengine;

import edu.berkeley.nwbqueryengine.connectors.HDF5Connector;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.query.parser.QueryParser;
import edu.berkeley.nwbqueryengine.query.result.NwbResult;
import edu.berkeley.nwbqueryengine.util.BTreePrinter;
import edu.berkeley.nwbqueryengine.util.MathEval;
import org.apache.commons.io.IOUtils;
import org.apache.commons.jexl3.JexlEngine;
import org.apache.commons.jexl3.JexlExpression;
import org.apache.commons.jexl3.JxltEngine;
import org.apache.commons.jexl3.MapContext;
import org.apache.commons.jexl3.internal.Engine;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.util.List;


/**
 * Created by petr-jezek on 6.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class Main {

    private static String path = "/home/petr-jezek/Data/nwb_datasets/nwbMatlab_DG";
    //private static String path = "/tmp/datasets";
    private static String file = "ANM186997_20130321.nwb";
    private static String fname = path + "/" + file;

    private static Log logger = LogFactory.getLog(Main.class);


    public static void main(String[] args) {
        BufferedReader br = null;
        try {
            System.loadLibrary("HDFql");
            QueryParser p = new QueryParser();
            Query query;
            if (args.length > 0) {
                String file = args[0];
                if (args.length > 1) {
                    String expression = args[1];
                    logger.debug("Expression: " + expression);
                    query = p.parse(expression);
                } else {
                    // Query query = p.parse("epochs=('start_time'>'200' & stop_time<400 | 'stop_time'>'1600')");
                    //Query query = p.parse("epochs=('start_time'<'200' | 'stop_time'>'1600')");
                    //Query query = p.parse("processing=(electrode_idx>30)");
                    query = p.parse("epochs=(start_time>200 & stop_time<400 | stop_time>1600)");
                }
                HDF5Connector connector = new HDF5Connector();
                for (int i = 0; i < 10; i++) {
                    long start = System.currentTimeMillis();
                    List<NwbResult> res = connector.executeQuery(query, new File(file));
                    long diff = System.currentTimeMillis() - start;
                    res.forEach(name -> {
                        logger.debug("Have res: " + name);
                        //            System.out.println(name);
                    });
                    logger.debug(i + " I have: " + res.size());
                    logger.debug("Done in: " + diff / 1000 + " seconds");
                    System.out.println(i + " I have: " + res.size());

                }
            } else {
                String message = "A file/dir has not been given...";
                logger.error(message);
                System.out.println(message);

                //query = p.parse("epochs=('start_time'<'200' | 'stop_time'>'1600')");
            }
            //printer.printNode(expression);
            //query.leftSideOfExpressions(expression);


//for(int i = 0; i < 100; i++) {
//    connector.test(fname);
//}

//            String[] array = new String[] {"ANM184389_20130207.nwb",  "ANM184389_20130213.nwb",  "ANM199549_20130530.nwb",  "ANM199551_20130626.nwb",  "ANM199552_20130603.nwb",  "ANM199552_20130608.nwb",  "ANM184389_20130211.nwb",  "ANM186997_20130317.nwb",  "ANM199549_20130604.nwb",  "ANM199552_20130601.nwb",  "ANM199552_20130604.nwb",  "ANM203464_20130702.nwb",
//                    "ANM184389_20130212.nwb",  "ANM186997_20130321.nwb", "ANM199549_20130605.nwb",  "ANM199552_20130602.nwb",  "ANM199552_20130606.nwb",  "ANM203464_20130705.nwb"};
//
//            for(String item : array) {
//                List<NwbResult> res = connector.executeQuery(query, path + "/" + item);
//                logger.debug(item + " resSize: "+ res.size());
//            }


//            br = new BufferedReader(new InputStreamReader(System.in));
//
//            while (true) {
//
//                System.out.print(">");
//                String input = br.readLine();
//
//                if ("q".equals(input)) {
//                    System.out.println("Exit!");
//                    System.exit(0);
//                }
//
//                QueryParser p = new QueryParser();
//                Query query = p.parse(input);
//                HDF5Connector connector = new HDF5Connector();
//                List<NwbResult> res = connector.executeQuery(query, fname);
//                res.forEach(name -> System.out.println(name));
//                System.out.println("Done.... ");
//
//            }

        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            IOUtils.closeQuietly(br);
        }

        //HDFqlExample.test();
/*        try {
            HDF5Connector.x();
        } catch (Exception e) {
            e.printStackTrace();
        }*/

    }


}
