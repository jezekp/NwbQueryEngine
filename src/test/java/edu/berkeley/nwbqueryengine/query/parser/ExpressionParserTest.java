package edu.berkeley.nwbqueryengine.query.parser;

import edu.berkeley.nwbqueryengine.connectors.HDF5Connector;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.query.result.NwbResult;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Created by petr-jezek on 19.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
class ExpressionParserTest {

    private static String path = "/home/petr-jezek/Data/nwb_datasets/nwbMatlab_DG";
    private static String file = "ANM184389_20130207.nwb";
    private static String fname = path + "/" + file;

    @Test
    void parse() {
        System.loadLibrary("HDFql");
        QueryParser p = new QueryParser();
        Query root = p.parse("CellInfo=('area'='c1'|'area'='c2'&'h'='c3'|h3=c8)");

        Expression leftSide = root.getQueryLeftSide();
        assertNotNull(leftSide);
        String expressionVal = leftSide.getExpressionValue();
        assertNotNull(expressionVal);
        assertEquals("CellInfo", expressionVal);
        List<Expression> leftSideExpressions = root.leftSideOfExpressions();
        String[] expressions = {"'area'", "'area'", "'h'", "h3"};
        assertEquals(expressions.length, leftSideExpressions.size());
        int i = 0;
        for (Expression item : leftSideExpressions) {
            assertEquals(item.getExpressionValue(), expressions[i++]);
        }

        Query query = p.parse("epochs=('start_time'>'200' & stop_time<400 | 'stop_time'>'1600')");
        HDF5Connector connector = new HDF5Connector();
        try {
            List<NwbResult> res = connector.executeQuery(query, fname);
            assertTrue(res.size() > 0);
            res.forEach(name -> {
                double value = (double) name.getValue();
                assertFalse(value < 200 || value > 400 && value < 1600);
            });
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

}