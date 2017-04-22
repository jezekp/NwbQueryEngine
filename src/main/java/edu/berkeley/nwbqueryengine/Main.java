package edu.berkeley.nwbqueryengine;

import com.hdfql.HDFql;
import com.hdfql.HDFqlExample;
import edu.berkeley.nwbqueryengine.connectors.HDF5Connector;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.query.parser.ExpressionParser;
import edu.berkeley.nwbqueryengine.util.BTreePrinter;

import java.util.List;


/**
 * Created by petr-jezek on 6.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class Main {


    public static void main(String[] args) {
        System.loadLibrary("HDFql");
        ExpressionParser p = new ExpressionParser();
        Query query = p.parse("epochs=('start_time'>'5'&'stop_time'<'10')");


        BTreePrinter printer = new BTreePrinter();
        //printer.printNode(expression);
        //query.leftSideOfExpressions(expression);
        HDF5Connector connector = new HDF5Connector();
        connector.executeQuery(query);

        //HDFqlExample.test();
/*        try {
            HDF5Connector.x();
        } catch (Exception e) {
            e.printStackTrace();
        }*/

System.out.println("Success: " + HDFql.SUCCESS);

    }


}
