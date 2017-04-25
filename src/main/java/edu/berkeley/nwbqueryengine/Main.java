package edu.berkeley.nwbqueryengine;

import com.hdfql.HDFql;
import com.hdfql.HDFqlExample;
import edu.berkeley.nwbqueryengine.connectors.HDF5Connector;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.query.parser.ExpressionParser;
import edu.berkeley.nwbqueryengine.query.result.NwbResult;
import edu.berkeley.nwbqueryengine.util.BTreePrinter;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import java.util.List;


/**
 * Created by petr-jezek on 6.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class Main {

    private static String path = "/home/petr-jezek/Data/nwb_datasets/nwbMatlab_DG";
    private static String file = "ANM184389_20130207.nwb";
    private static String fname = path + "/" + file;

    private static Log logger = LogFactory.getLog(Main.class);


    public static void main(String[] args) {
        try {


            System.loadLibrary("HDFql");
            ExpressionParser p = new ExpressionParser();
            //Query query = p.parse("epochs=('start_time'>'200' & stop_time<400 | 'stop_time'>'1600')");
            Query query = p.parse("epochs=('start_time'<'200' | 'stop_time'>'1600')");

            BTreePrinter printer = new BTreePrinter();
            //printer.printNode(expression);
            //query.leftSideOfExpressions(expression);
            HDF5Connector connector = new HDF5Connector();
            List<NwbResult> res =  connector.executeQuery(query, fname);
            res.forEach(name -> logger.debug("Have res: " + name));
            System.out.println("Done.... ");

        } catch (Exception e) {
            e.printStackTrace();
        }

        //HDFqlExample.test();
/*        try {
            HDF5Connector.x();
        } catch (Exception e) {
            e.printStackTrace();
        }*/

    }


}
