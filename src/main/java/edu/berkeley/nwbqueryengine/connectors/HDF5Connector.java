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
    private HDFqlCursor cursor = new HDFqlCursor();


    public HDF5Connector(File obj) {
        HDFql.cursorUse(cursor);
        this.obj = obj;

    }

    private List<EntityWrapper> executeLikeQuery(Query query, String fileName) throws ConnectorException {
        List<EntityWrapper> partialExpressions = new LinkedList<>();
        for (Query subQuery : query.getSubQueries()) {
            Map<String, List<String>> showExpressions = new HashMap<>();
            for (Expression item : new LinkedList<>(subQuery.leftSideOfExpressions())) {
                //dataset name and complete path (e. g. "epochs/Trial_001/start_time");
                List<String> showResults;
                String expressionValue = item.getExpressionValue();
                logger.debug("Processing  expression: " + expressionValue + " ,operator: " + item.getOperator());
                if (showExpressions.containsKey(expressionValue)) {
                    showResults = showExpressions.get(expressionValue);
                } else {
                    showResults = new LinkedList<>();
                    String leftSide = subQuery.getQueryLeftSide().getExpressionValue();
                    String queryPrefix = "**/";
                    if (leftSide.startsWith("/")) {
                        queryPrefix = "";
                    } else if (leftSide.equals("*")) {
                        leftSide = "**";
                    }
                    String queryString = "SHOW LIKE " + queryPrefix + "" + leftSide + "/" + queryPrefix + "" + StringUtils.strip(expressionValue.trim() + "/", "''\"\"");
                    logger.debug(queryString);
                    int executeLikeRes;
                    int attempts = 1;
                    connect(obj);
                    synchronized (this) {
                        while ((executeLikeRes = HDFql.execute(queryString)) != HDFql.SUCCESS && attempts-- > 0) {
//                    HDFql.cursorClear(cursor);
//                    HDFql.cursorInitialize(cursor);
                            logger.error("Error: " + HDFql.errorGetMessage());
                        }
                        logger.debug("ExecuteLikeRes: " + executeLikeRes);

                        int cursorRes;
                        while ((cursorRes = HDFql.cursorNext(cursor)) == HDFql.SUCCESS) {
                            String cursorGetChar = HDFql.cursorGetChar(cursor);
                            if (cursorGetChar.endsWith(expressionValue)) {
                                showResults.add(cursorGetChar);
                                logger.debug("Like: " + cursorGetChar + " -- " + HDFql.cursorGetDatatype(cursor));
                            }
                        }

                        showExpressions.put(expressionValue, new LinkedList<>(showResults));
                        logger.debug("cursorRes:" + cursorRes);

                    }
                    disconnect(obj);


                }
                partialExpressions.add(new EntityWrapper(showResults, item, fileName, subQuery));
            }
        }
        return partialExpressions;
    }


    public void connect(File obj) throws ConnectorException {
        synchronized (this) {
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

    public List<EntityWrapper> processSearch(Query query) throws ConnectorException {
        List<EntityWrapper> res;
        logger.debug("File: " + obj.getAbsolutePath());
        if (obj.isFile()) {
            //connect(obj);
            res = executeLikeQuery(query, obj.getAbsolutePath());
            //        disconnect(obj);
        } else {
            throw new ConnectorException("obj must be a file not a directory");
        }
        return res;
    }

    public Object next() throws ConnectorException {
        Object value = null;
        int selectCursorResult = HDFql.cursorNext(cursor);
        if(selectCursorResult == HDFql.SUCCESS) {
            HDFqlUtil util = new HDFqlUtil();
            try {
                value = util.getValue(cursor);
            } catch (Exception e) {
                throw new ConnectorException(e);
            }
        }


        logger.debug("selected value: " + value + ", " + ((value == null) ? "null" : value.getClass().getName()));
        return value;
    }

    @Override
    public void getValues(String entity) throws ConnectorException {
        synchronized (this) {
            connect(obj);
            String selectQuery = "SELECT FROM " + entity;
            logger.debug("Select: " + selectQuery);
            int selectStatus = HDFql.execute(selectQuery);
            logger.debug("selectStatus: " + selectStatus);

            disconnect(obj);
        }
    }
}

