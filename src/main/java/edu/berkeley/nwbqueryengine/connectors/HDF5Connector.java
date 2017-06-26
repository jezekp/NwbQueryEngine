package edu.berkeley.nwbqueryengine.connectors;

import as.hdfql.HDFql;
import as.hdfql.HDFqlCursor;
import edu.berkeley.nwbqueryengine.PartialExpression;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Operators;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.query.result.NwbResult;
import edu.berkeley.nwbqueryengine.query.result.Restrictions;
import edu.berkeley.nwbqueryengine.util.HDFqlUtil;
import org.apache.commons.jexl3.JexlEngine;
import org.apache.commons.jexl3.JexlExpression;
import org.apache.commons.jexl3.MapContext;
import org.apache.commons.jexl3.internal.Engine;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.File;
import java.io.FileFilter;
import java.util.*;

/**
 * Created by petr-jezek on 18.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class HDF5Connector implements Connector<String> {

    private Log logger = LogFactory.getLog(getClass());
    private File obj;

    public HDF5Connector(File obj) {
        this.obj = obj;
    }


    private List<PartialExpression> executeLikeQuery(Query q, String fileName) {
        List<PartialExpression> partialExpressions = new LinkedList<>();
        Map<String, List<String>> showExpressions = new HashMap<>();
        //cursor.delete();
        for (Expression item : new LinkedList<>(q.leftSideOfExpressions())) {
            List<String> showResults;
            String expressionValue = item.getExpressionValue();
            logger.debug("Processing  expression: " + expressionValue + " " + item.getOperator());
            if (showExpressions.containsKey(expressionValue)) {
                showResults = showExpressions.get(expressionValue);
            } else {
                showResults = new LinkedList<>();
                String query = "SHOW LIKE **/" + q.getQueryLeftSide() + "/**/" + StringUtils.strip(item.getExpressionValue().trim(), "''\"\"");
                logger.debug(query);
                int executeLikeRes;
                int attempts = 1;
                HDFql.cursorClear();
                HDFql.cursorInitialize();
                while ((executeLikeRes = HDFql.execute(query)) != HDFql.SUCCESS && attempts-- > 0) {
                    HDFql.cursorClear();
                    HDFql.cursorInitialize();
                    logger.error("Error: " + HDFql.errorGetMessage());
                }
                logger.debug("ExecuteLikeRes: " + executeLikeRes);

                int cursorRes;
                while ((cursorRes = HDFql.cursorNext()) == HDFql.SUCCESS) {
                    String cursorGetChar = HDFql.cursorGetChar();
                    showResults.add(cursorGetChar);
                    logger.debug("Like: " + cursorGetChar + " -- " + HDFql.cursorGetDatatype());
                }
                showExpressions.put(expressionValue, new LinkedList<>(showResults));
                logger.debug("cursorRes:" + cursorRes);

            }
            partialExpressions.add(new PartialExpression(showResults, item, fileName));
        }
        return partialExpressions;
    }


    public void connect(File obj) throws ConnectorException {
        HDFql.cursorClear();
        HDFql.cursorInitialize();
        if (logger.isDebugEnabled()) {
            HDFql.execute("ENABLE DEBUG");
        }
        String useFileQuery = "USE READONLY FILE " + obj.getAbsolutePath();
        logger.debug("Use file query: " + useFileQuery);
        HDFql.execute(useFileQuery);
        HDFql.execute("SHOW USE FILE");
        HDFql.cursorFirst();
        logger.debug("File in use: " + HDFql.cursorGetChar());
    }


    public void disconect(File obj) throws ConnectorException {
        int closeResult = HDFql.execute("CLOSE FILE " + obj.getAbsolutePath());
        logger.debug("Closing file: " + obj.getAbsolutePath() + ", resultCode: " + closeResult);
        HDFql.cursorClear();
    }

    public List<PartialExpression> processSearch(Query query) throws ConnectorException {
        List<PartialExpression> res = new LinkedList<>();
        Map<String, List<PartialExpression>> files = new HashMap<>();
        logger.debug("File: " + obj.getAbsolutePath());
        if (obj.isFile()) {
            connect(obj);
            List<PartialExpression> pe = executeLikeQuery(query, obj.getAbsolutePath());
            files.put(obj.getAbsolutePath(), pe);
            disconect(obj);
        } else {
            for (File item : obj.listFiles(new FileFilter() {
                @Override
                public boolean accept(File pathname) {
                    return pathname.getName().toLowerCase().endsWith(".nwb");
                }
            })) {
                logger.debug("Individual file: " + item.getAbsolutePath());
                connect(item);
                List<PartialExpression> peTmp = executeLikeQuery(query, item.getAbsolutePath());
                disconect(item);
                files.put(item.getAbsolutePath(), peTmp);
            }
        }

        for (Map.Entry<String, List<PartialExpression>> entry : files.entrySet()) {
            res.addAll(entry.getValue());
        }

        return res;
    }

    @Override
    public List<Object> getValues(String entity) throws ConnectorException {
        connect(obj); //TODO this doesn't work when a directory is used
        List<Object> values = new ArrayList<>();
        logger.debug("valuesType: " + values.getClass().getName());
        String selectQuery = "SELECT FROM " + entity;
        logger.debug("Select: " + selectQuery);
        int selectStatus = HDFql.execute(selectQuery);
        logger.debug("selectStatus: " + selectStatus);
        int selectCursorResult;
        while ((selectCursorResult = HDFql.cursorNext()) == HDFql.SUCCESS) {
            HDFqlUtil util = new HDFqlUtil();
            Object value = util.getValue();
            values.add(value);
            logger.debug("selected value: " + value + ", " + value.getClass().getName());
        }
        logger.debug("SelectCursorResult: " + selectCursorResult);
        logger.debug("objectValue: " + values);
        disconect(obj);
        return values;
    }
}

