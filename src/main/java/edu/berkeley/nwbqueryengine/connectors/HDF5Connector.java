package edu.berkeley.nwbqueryengine.connectors;

import as.hdfql.HDFql;
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
                    String leftSideCopy = StringUtils.strip(leftSide, null); //only removes white spaces
                    String queryPrefix = "";
                    if (leftSideCopy.startsWith("/")) {
                        queryPrefix = "";
                    } else if (leftSideCopy.equals("*")) {
                        leftSide = "**";
                    } else if (leftSideCopy.startsWith("*")) {
                        queryPrefix = "**/";
                        leftSide = StringUtils.strip(leftSide,"*/");
                    }
                    String querySuffix = "";
                    if(leftSideCopy.endsWith("*")) {
                        querySuffix = "**/";
                        leftSide = StringUtils.strip(leftSide, "/*");
                    }

                    String queryString = "SHOW LIKE " + queryPrefix + "" + leftSide + "/" + querySuffix + "" + StringUtils.strip(expressionValue.trim() + "/", "''\"\"");
                    logger.debug(queryString);
                    int executeLikeRes;

                    synchronized (this) {
                        HDFql.cursorClear();
                        HDFql.cursorInitialize();
                        if ((executeLikeRes = HDFql.execute(queryString)) != HDFql.SUCCESS) {
                            logger.error("Error: " + HDFql.errorGetMessage());
                        }
                        logger.debug("ExecuteLikeRes: " + executeLikeRes);

                        int cursorRes;
                        while ((cursorRes = HDFql.cursorNext()) == HDFql.SUCCESS) {
                            String cursorGetChar = HDFql.cursorGetChar();
                            if (cursorGetChar.endsWith(expressionValue)) {
                                showResults.add(cursorGetChar);
                                logger.debug("Like: " + cursorGetChar + " -- " + HDFql.cursorGetDataType());
                            }
                        }

                        showExpressions.put(expressionValue, new LinkedList<>(showResults));
                        logger.debug("cursorRes:" + cursorRes);

                    }
                }
                partialExpressions.add(new EntityWrapper(showResults, item, fileName, subQuery));
            }
        }
        return partialExpressions;
    }


    protected void connect(File obj) throws ConnectorException {
        synchronized (this) {
            if (logger.isDebugEnabled()) {
                HDFql.cursorClear();
                HDFql.cursorInitialize();
                HDFql.execute("ENABLE DEBUG");
            }
            String useFileQuery = "USE READONLY FILE \"" + obj.getAbsolutePath() + "\"";
            logger.debug("Use file query: " + useFileQuery);
            HDFql.cursorClear();
            HDFql.cursorInitialize();
            HDFql.execute(useFileQuery);
            HDFql.cursorClear();
            HDFql.cursorInitialize();
            HDFql.execute("SHOW USE FILE");
         //   logger.debug("File in use: " + HDFql.cursorGetChar());
        }
    }


    protected void disconnect(File obj) throws ConnectorException {
        HDFql.cursorClear();
        HDFql.cursorInitialize();
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
        if(HDFql.cursorNext() == HDFql.SUCCESS) {
            HDFqlUtil util = new HDFqlUtil();
            try {
                value = util.getValue();
            } catch (Exception e) {
                throw new ConnectorException(e);
            }
        }


        logger.debug("selected value: " + value + ", " + ((value == null) ? "null" : value.getClass().getName()));
        return value;
    }

    @Override
    public void disconnect() throws ConnectorException {
        disconnect(obj);
    }

    @Override
    public void connect() throws ConnectorException {
        connect(obj);


    }

    @Override
    public void getValues(String entity) throws ConnectorException {
        synchronized (this) {
            String selectQuery = "SELECT FROM " + entity;
            logger.debug("Select: " + selectQuery);
            HDFql.cursorClear();
            HDFql.cursorInitialize();
            int selectStatus = HDFql.execute(selectQuery);
            logger.debug("selectStatus: " + selectStatus);
        }
    }
}

