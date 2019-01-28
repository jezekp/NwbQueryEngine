
package edu.berkeley.nwbqueryengine.query.parser;

import edu.berkeley.nwbqueryengine.connectors.HDF5Connector;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.NwbProcessor;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.data.NwbResult;
import edu.berkeley.nwbqueryengine.util.DateUtil;
import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import javax.print.DocFlavor;
import java.io.File;
import java.io.FileInputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
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
    private final SimpleDateFormat sdfyyyyMMdd = new SimpleDateFormat("yyyy-MM-dd");
    private final SimpleDateFormat sdfyyyy = new SimpleDateFormat("yyyy");

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
        List<NwbResult> res = execute("epochs=(start_time>200)");
        assertTrue(res.size() == 327);
        res.forEach(name -> {
            double value = (double) name.getValue();
            logger.debug("Test with single Operand: " + value);
            assertFalse(value <= 200);
        });

    }

    @Test
    void parseQeryWithTwoOperand() {
        List<NwbResult> res = execute("epochs=(start_time>200 & start_time<500)");
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
        Query query = parser.parse("epochs=(start_time > 10 | stop_time < 20) | epochs2=(start_time2 > 10 | stop_time2 < 20) | epochs3=(start_time3 > 10 | stop_time3 < 20)& epochs4=(start_time4 > 10 | stop_time4 < 20)");
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
        List<NwbResult> res = execute("epochs=(start_time>200 & stop_time<400 | stop_time>1600)");
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
        List<NwbResult> res = execute("epochs=(start_time | stop_time)");
        assertTrue(res.size() == 736);
    }

    @Test
    void like() {
        List<NwbResult> res = execute("analysis=(description LIKE whisker)");
        assertTrue(res.size() > 0);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains("whisker")));
    }

    @Test
    void spacesInQuery() {
        List<NwbResult> res = execute("analysis  =    (       description       LIKE     whisker        )");
        assertTrue(res.size() > 0);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains("whisker")));
    }

    @Test
    void noSpacesInQuery() {
        List<NwbResult> res = execute("analysis=(descriptionLIKEwhisker)");
        assertTrue(res.size() > 0);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains("whisker")));
    }


    @Test
    void parseGenericQuery() {
        QueryParser p = new QueryParser();
        Query root = p.parse("CellInfo=(area==c1|area==c2&h==c3|h3==c8)");

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
        List<NwbResult> res = execute("epochs=(tags LIKE Mis) & epochs=(tags LIKE Hi)");
        assertTrue(res.size() == 2);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains("Mis") ||
                ((String) item.getValue()).contains("Hi")));

    }

    @Test
    void andLikeCondition2() {
        List<NwbResult> res = execute("epochs=(tags LIKE Mis & tags LIKE Hi)");
        assertTrue(res.size() == 2);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains("Mis") ||
                ((String) item.getValue()).contains("Hi")));

    }

    @Test
    void complexLikeQuery() {
        List<NwbResult> res = execute("/general/subject=(age LIKE 25 & species LIKE Mus) & /=(file_create_date LIKE 2017-02-14T16:40:38.8414)");
        assertTrue(res.size() == 3);
        res.forEach(item -> {
            String value = (String) item.getValue();
            String dataset = item.getDataSet();
            assertTrue(value.contains("25") && dataset.equals("/general/subject/age") ||
                    value.contains("2017-02-14T16:40:38.8414") && dataset.equals("/file_create_date") ||
                    value.contains("Mus") && dataset.equals("/general/subject/species"));
        });
    }

    @Test
    void complexLikeInQuotesQuery() {
        List<NwbResult> res = execute("/general/subject=(age LIKE 25 & species LIKE \"Mus\") & /=(file_create_date LIKE \"2017-02-14T16:40:38.8414\")");
        assertTrue(res.size() == 3);
        res.forEach(item -> {
            String value = (String) item.getValue();
            String dataset = item.getDataSet();
            assertTrue(value.contains("25") && dataset.equals("/general/subject/age") ||
                    value.contains("2017-02-14T16:40:38.8414") && dataset.equals("/file_create_date") ||
                    value.contains("Mus") && dataset.equals("/general/subject/species"));
        });
    }

    @Test
    void andOverTwoDatasets() {
        List<NwbResult> res = execute("epochs/Trial_306=(start_time < 1530) & epochs/Trial_307=(stop_time>1530)");
        assertTrue(res.size() == 2);
        res.forEach(item -> assertTrue((double)item.getValue() < 1530 && item.getDataSet().equals("epochs/Trial_306/start_time")
    || (double)item.getValue() > 1530 && item.getDataSet().equals("epochs/Trial_307/stop_time")));

    }

    @Test
    void bracketsTest() {
        String find = "(PW)";
        List<NwbResult> res = execute("/=(session_description LIKE "+ find +")");
        assertTrue(res.size() == 1);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains(find)));


    }

    @Test
    void timeLikeTest() {
        String find = "2013-02-07";
        List<NwbResult> res = execute("/=(session_start_time LIKE " + find +")");
        assertTrue(res.size() == 1);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains(find)));
    }

    @Test
    void dateBeforeTest() throws ParseException {
        String find = "2015-02-07";
        List<NwbResult> res = execute("/=(session_start_time <  " + find +")");
        assertTrue(res.size() == 1);
        Date d = sdfyyyyMMdd.parse(find);
        res.forEach(item -> assertTrue(((Date) DateUtil.tryParse((String)item.getValue())).before(d)));

    }

    @Test
    void dateAfterTest() throws ParseException {
        String find = "2013-02-06";
        List<NwbResult> res = execute("/=(session_start_time > " + find +")");
        assertTrue(res.size() == 1);
        Date d = sdfyyyyMMdd.parse(find);
        res.forEach(item -> assertTrue(((Date) DateUtil.tryParse((String)item.getValue())).after(d)));
    }

    @Test
    void dateBeforeFalseTest() {
        String find = "2013-02-06";
        List<NwbResult> res = execute("/=(session_start_time < " + find +")");
        assertTrue(res.size() == 0);
    }

    @Test
    void dateAfter201801() throws ParseException {
        String find = "2018-01";
        List<NwbResult> res = execute("/=(session_start_time > " + find +")");
        assertTrue(res.size() == 0);
    }

    @Test
    void dateBeforeTrueTest() throws ParseException {
        String find = "2018";
        List<NwbResult> res = execute("/=(session_start_time < " + find +")");
        assertTrue(res.size() == 1);
        Date d = sdfyyyy.parse(find);
        res.forEach(item -> assertTrue(((Date) DateUtil.tryParse((String)item.getValue())).before(d)));

    }

    @Test
    void attributeInHierarchy() {
        List<NwbResult> res = execute("extracellular_units=(neurodata_type LIKE Modul)");
        assertTrue(res.size() == 1);
        res.forEach(item -> assertTrue(((String) item.getValue()).contains("Modul")));
    }

    @Test
    void andOverOneGroup() {
        List<NwbResult> res = execute("/general/subject=(species LIKE Mus musculu & age LIKE 25 w)");
        assertTrue(res.size() == 2);
        res.forEach(item -> {
            String value = (String) item.getValue();
            assertTrue(value.contains("musculu") || value.contains("25 w"));
        });
    }

    @Test
    void andOverOneGroupFalse() {
        List<NwbResult> res = execute("/general/subject=(species LIKE Mus musculu & age LIKE 25 weeee");
        assertTrue(res.size() == 0);

    }


    @Test
    void notLeftSideOnSubQueries() {
        List<NwbResult> res = execute("/general/subject=(species LIKE Mus musculu) & (age LIKE 25 w)");
        assertTrue(res.size() == 2);
        res.forEach(item -> {
            String value = (String) item.getValue();
            assertTrue(value.contains("musculu") || value.contains("25 w"));
        });
    }

}