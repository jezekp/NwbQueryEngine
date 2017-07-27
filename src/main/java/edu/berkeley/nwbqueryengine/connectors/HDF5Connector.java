package edu.berkeley.nwbqueryengine.connectors;

import as.hdfql.HDFql;
import as.hdfql.HDFqlCursor;
import edu.berkeley.nwbqueryengine.data.EntityWrapper;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.util.HDFqlUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.File;
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

    private  List<EntityWrapper> executeLikeQuery(Query q, String fileName) {
        List<EntityWrapper> partialExpressions = new LinkedList<>();
        Map<String, List<String>> showExpressions = new HashMap<>();
        //cursor.delete();
        for (Expression item : new LinkedList<>(q.leftSideOfExpressions())) {
            //dataset name and complete path (e. g. "epochs/Trial_001/start_time");
            List<String> showResults;
            String expressionValue = item.getExpressionValue();
            logger.debug("Processing  expression: " + expressionValue + " ,operator: " + item.getOperator());
            if (showExpressions.containsKey(expressionValue)) {
                showResults = showExpressions.get(expressionValue);
            } else {
                showResults = new LinkedList<>();
                String query = "SHOW LIKE **/" + q.getQueryLeftSide() + "/**/" + StringUtils.strip(item.getExpressionValue().trim(), "''\"\"");
                logger.debug(query);
                int executeLikeRes;
                int attempts = 1;

                HDFqlCursor cursor1 = new HDFqlCursor();
                synchronized (cursor1) {
                    HDFql.cursorUse(cursor1);
                    while ((executeLikeRes = HDFql.execute(query)) != HDFql.SUCCESS && attempts-- > 0) {
//                    HDFql.cursorClear(cursor);
//                    HDFql.cursorInitialize(cursor);
                        logger.error("Error: " + HDFql.errorGetMessage());
                    }
                    logger.debug("ExecuteLikeRes: " + executeLikeRes);

                    int cursorRes;
                    while ((cursorRes = HDFql.cursorNext(cursor1)) == HDFql.SUCCESS) {
                        String cursorGetChar = HDFql.cursorGetChar(cursor1);
                        showResults.add(cursorGetChar);
                        logger.debug("Like: " + cursorGetChar + " -- " + HDFql.cursorGetDatatype(cursor1));
                    }

                    showExpressions.put(expressionValue, new LinkedList<>(showResults));
                    logger.debug("cursorRes:" + cursorRes);
                }


            }
            partialExpressions.add(new EntityWrapper(showResults, item, fileName));
        }
        return partialExpressions;
    }


    public void connect(File obj) throws ConnectorException {
        HDFqlCursor cursor = new HDFqlCursor();
        synchronized (cursor) {
            HDFql.cursorUse(cursor);
            HDFql.cursorClear(cursor);
            HDFql.cursorInitialize(cursor);
            if (logger.isDebugEnabled()) {
                HDFql.execute("ENABLE DEBUG");
            }
            String useFileQuery = "USE READONLY FILE " + obj.getAbsolutePath();
            logger.debug("Use file query: " + useFileQuery);
            HDFql.execute(useFileQuery);
            HDFql.execute("SHOW USE FILE");
            HDFql.cursorFirst(cursor);
            logger.debug("File in use: " + HDFql.cursorGetChar(cursor));
        }
    }


    public void disconnect(File obj) throws ConnectorException {
        int closeResult = HDFql.execute("CLOSE FILE " + obj.getAbsolutePath());
        logger.debug("Closing file: " + obj.getAbsolutePath() + ", resultCode: " + closeResult);

    }

    public  List<EntityWrapper> processSearch(Query query) throws ConnectorException {
        List<EntityWrapper> res;
        logger.debug("File: " + obj.getAbsolutePath());
        if (obj.isFile()) {
            connect(obj);
            res = executeLikeQuery(query, obj.getAbsolutePath());
            disconnect(obj);
        } else {
            throw new ConnectorException("obj must be a file not a directory");
        }
        return res;
    }

    @Override
    public List<Object> getValues(String entity) throws ConnectorException {
        connect(obj);
        List<Object> values = new ArrayList<>();
        HDFqlCursor cursor = new HDFqlCursor();
        synchronized (cursor) {
            HDFql.cursorUse(cursor);

            logger.debug("valuesType: " + values.getClass().getName());
            String selectQuery = "SELECT FROM " + entity;
            logger.debug("Select: " + selectQuery);
            int selectStatus = HDFql.execute(selectQuery);
            logger.debug("selectStatus: " + selectStatus);
            int selectCursorResult;
            while ((selectCursorResult = HDFql.cursorNext(cursor)) == HDFql.SUCCESS) {
                HDFqlUtil util = new HDFqlUtil();
                Object value = util.getValue(cursor);
                values.add(value);
                logger.debug("selected value: " + value + ", " + value.getClass().getName());
            }
            logger.debug("SelectCursorResult: " + selectCursorResult);
            logger.debug("objectValue: " + values);
            disconnect(obj);
        }
        return values;
    }
}

