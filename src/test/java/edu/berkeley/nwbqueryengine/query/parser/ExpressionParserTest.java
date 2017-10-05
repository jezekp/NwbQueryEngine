package edu.berkeley.nwbqueryengine.query.parser;

import edu.berkeley.nwbqueryengine.connectors.HDF5Connector;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.NwbProcessor;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.data.NwbResult;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.util.LinkedList;
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

    private Log logger = LogFactory.getLog(getClass());

    @BeforeAll
    static void init() {
        System.loadLibrary("HDFql");
    }

    protected List<NwbResult> execute(String expression) {
        List<NwbResult> res = new LinkedList<>();
        try {
            QueryParser p = new QueryParser();
            Query query = p.parse(expression);
            HDF5Connector connector = new HDF5Connector(new File(fname));
            NwbProcessor processor = new NwbProcessor(connector);
            res = processor.evaluate(query);
        } catch (Exception e) {
            logger.error(e);
            fail(e.getMessage());
        }
        return res;
    }

    @Test
    void parseQeryWithSingleOperand() {
        List<NwbResult> res = execute("epochs=(start_time>200)");
        assertTrue(res.size() == 327);
        res.forEach(name -> {
            double value = (double) name.getValue();
            logger.debug("Test with single Operand: " + value);
            assertFalse(value <= 200);
        });

    }

    @Test
    void parseQueryWithOperands() {
        List<NwbResult> res = execute("epochs=(start_time>200 & stop_time<400 | stop_time>1600)");
        assertTrue(res.size() == 87);
        res.forEach(name -> {
            double value = (double) name.getValue();
            logger.debug("Test with more operands: " + value);
            assertFalse(value <= 200 || value >= 400 && value <= 1600);
        });
    }
    @Test
    void parseQueryWithoutOperands() {
        List<NwbResult> res = execute("epochs=(start_time | stop_time)");
        assertTrue(res.size() == 736);
    }


    @Test
    void parseGenericQuery() {
        QueryParser p = new QueryParser();
        Query root = p.parse("CellInfo=('area'=='c1'|'area'=='c2'&'h'=='c3'|h3==c8)");

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
            assertEquals(expressions[i++], item.getExpressionValue());
        }

    }

}