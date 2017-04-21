package edu.berkeley.nwbqueryengine;

import com.hdfql.HDFqlExample;
import edu.berkeley.nwbqueryengine.connectors.HDF5Connector;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.query.parser.ExpressionParser;


/**
 * Created by petr-jezek on 6.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class Main {


    public static void main(String[] args) {
        ExpressionParser p = new ExpressionParser();
        Query root = p.parse("CellInfo=('area'='c1'|'area'='c2'&'h'='c3'|h3=c8)");

        //HDFqlExample.test();
        try {
            HDF5Connector.x();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }


}
