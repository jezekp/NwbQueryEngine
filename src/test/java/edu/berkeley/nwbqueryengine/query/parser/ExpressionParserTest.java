package edu.berkeley.nwbqueryengine.query.parser;

import edu.berkeley.nwbqueryengine.connectors.HDF5Connector;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.NwbProcessor;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.data.NwbResult;
import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import javax.print.DocFlavor;
import java.io.File;
import java.io.FileInputStream;
import java.util.LinkedList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static edu.berkeley.nwbqueryengine.util.ValuesUtil.*;

/**
 * Created by petr-jezek on 19.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
class ExpressionParserTest {

    private static String file = "ANM184389_20130207.nwb";
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
            java.net.URL u = getClass().getClassLoader().getResource(file);
            HDF5Connector connector = new HDF5Connector(new File(u.getFile()));
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
        //List<NwbResult> res = execute("epochs=(start_time>200)");
        List<NwbResult> res = execute("epochs=\"start_time>200\"");
        assertTrue(res.size() == 327);
        res.forEach(name -> {
            double value = (double) name.getValue();
            logger.debug("Test with single Operand: " + value);
            assertFalse(value <= 200);
        });

    }

    @Test
    void parseQeryWithTwoOperand() {
        List<NwbResult> res = execute("epochs=\"start_time>200 & start_time<500\"");
        assertTrue(res.size() > 0);
        res.forEach(name -> {
            double value = (double) name.getValue();
            logger.debug("Test with single Operand: " + value);
            assertTrue(value > 200);
            assertTrue(value < 500);
        });

    }

    @Test
    void parseSubQueriesTest() {
        QueryParser parser = new QueryParser();
        Query query = parser.parse("epochs=\"start_time > 10 | stop_time < 20\" | epochs2=\"start_time2 > 10 | stop_time2 < 20\" | epochs3=\"start_time3 > 10 | stop_time3 < 20\"& epochs4=\"start_time4 > 10 | stop_time4 < 20\"");
        List<Query> expressions = query.getSubQueries();
        assertTrue(expressions.size() > 0);
        expressions.forEach(name -> {
            final StringBuilder s = new StringBuilder();
            name.leftSideOfExpressions().forEach(expression -> s.append(expression + " "));
            logger.debug("name: " + name.getQueryLeftSide() + ", operator: " + name.getQueryLeftSide().getParent().getRightSideSibling().getOperator());
            logger.debug("Expressions: " + s);
        });
        assertTrue(expressions.size() > 0);
    }

    @Test
    void parseQueryWithOperands() {
        List<NwbResult> res = execute("epochs=\"start_time>200 & stop_time<400 | stop_time>1600\"");
        assertTrue(res.size() > 0);
        res.forEach(name -> {
            double value = (double) name.getValue();
            logger.debug("Test with more operands: " + value);
            boolean found = false;
            if (getDatasetName(name).equals("start_time")) {
                assertTrue(value > 200);
                found = true;
            }
            if (getDatasetName(name).equals("stop_time")) {
                assertTrue(value < 400 || value > 1600);
                found = true;
            }

            assertTrue(found);

        });
    }

    @Test
    void parseQueryWithoutOperands() {
        List<NwbResult> res = execute("epochs=\"start_time | stop_time\"");
        assertTrue(res.size() == 736);
    }

    @Test
    void like() {
        List<NwbResult> res = execute("analysis=\"description LIKE whisker\"");
        assertTrue(res.size() > 0);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains("whisker")));
    }

    @Test
    void spacesInQuery() {
        List<NwbResult> res = execute("analysis  =    \"       description       LIKE     whisker       \"");
        assertTrue(res.size() > 0);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains("whisker")));
    }

    @Test
    void noSpacesInQuery() {
        List<NwbResult> res = execute("analysis=\"descriptionLIKEwhisker\"");
        assertTrue(res.size() > 0);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains("whisker")));
    }


    @Test
    void parseGenericQuery() {
        QueryParser p = new QueryParser();
        Query root = p.parse("CellInfo=\"area==c1|area==c2&h==c3|h3==c8\"");

        Expression leftSide = root.getQueryLeftSide();
        assertNotNull(leftSide);
        String expressionVal = leftSide.getExpressionValue();
        assertNotNull(expressionVal);
        assertEquals("CellInfo", expressionVal);
        List<Expression> leftSideExpressions = root.leftSideOfExpressions();
        String[] expressions = {"area", "area", "h", "h3"};
        assertEquals(expressions.length, leftSideExpressions.size());
        int i = 0;
        for (Expression item : leftSideExpressions) {
            assertEquals(expressions[i++], item.getExpressionValue());
        }
    }

    @Test
    void andLikeCondition() {
        List<NwbResult> res = execute("epochs=\"tags LIKE Mis\" & epochs=\"tags LIKE Hi\"");
//        List<NwbResult> res = execute("epochs=(tags LIKE Mis) & epochs=(tags LIKE Hi)");
        assertTrue(res.size() == 2);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains("Mis") ||
                ((String) item.getValue()).contains("Hi")));

    }

    @Test
    void andLikeCondition2() {
        List<NwbResult> res = execute("epochs=\"tags LIKE Mis & tags LIKE Hi\"");
        assertTrue(res.size() == 2);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains("Mis") ||
                ((String) item.getValue()).contains("Hi")));

    }

    @Test
    void complexLikeQuery() {
        List<NwbResult> res = execute("/general/subject=\"age LIKE 3 months 16 days & species LIKE Mus musculu\" & /=\"file_create_date LIKE 2017-04\"");
        //todo
    }

    @Test
    void andOverTwoDatasets() {
        List<NwbResult> res = execute("epochs/Trial_306=\"start_time < 1530\" & epochs/Trial_307=\"stop_time>1530\"");
        //TODO fix assertTrue(res.size() == 2);
        //res.forEach(item -> assertTrue((double)item.getValue() < 1530));

    }

}