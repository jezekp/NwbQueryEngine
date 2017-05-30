package edu.berkeley.nwbqueryengine.connectors;

import as.hdfql.HDFql;
import as.hdfql.HDFqlCursor;
import edu.berkeley.nwbqueryengine.PartialExpression;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Operators;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.query.result.NwbResult;
import edu.berkeley.nwbqueryengine.query.result.Restrictions;
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
public class HDF5Connector implements Connector<File> {

    private Log logger = LogFactory.getLog(getClass());
    private JexlEngine jexl = new Engine();
    private MapContext mc = new MapContext();

    public List<PartialExpression> executeLikeQuery(Query q, String fileName) {
        HDFql.cursorClear();
        HDFql.cursorInitialize();
        HDFql.execute("ENABLE DEBUG");
        String useFileQuery = "USE READONLY FILE " + fileName;
        logger.debug("Use file query: " + useFileQuery);
        HDFql.execute(useFileQuery);
        HDFql.execute("SHOW USE FILE");
        HDFql.cursorFirst();
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
//                HDFqlCursor cursor2 = new HDFqlCursor();
//                HDFql.cursorInitialize(cursor2);
//                int cursorUse2 = HDFql.cursorUse(cursor2);
//                logger.debug("cursor2Use: " + cursorUse2);
                int executeLikeRes;
                int attempts = 1;
                HDFql.cursorClear();
                HDFql.cursorInitialize();
                while ((executeLikeRes = HDFql.execute(query)) != HDFql.SUCCESS && attempts-- > 0) {
//                    int closeResult = HDFql.execute("CLOSE FILE " + fname);
//                    logger.debug("Use file query: " + useFileQuery);
//                    HDFql.execute(useFileQuery);
                    HDFql.cursorClear();
                    HDFql.cursorInitialize();
                    //wait(300);
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
                //HDFql.cursorClear();
                //cursor2.delete();
                logger.debug("cursorRes:" + cursorRes);

            }
            partialExpressions.add(new PartialExpression(showResults, item, fileName));
        }
        int closeResult = HDFql.execute("CLOSE FILE " + fileName);
        logger.debug("Closing file: " + fileName + ", resultCode: " + closeResult);
        //HDFql.cursorClear();
        return partialExpressions;
    }

    public List<NwbResult> executeQuery(Query q, String fname, List<PartialExpression> partialExpressions) throws Exception {
        HDFql.cursorClear();
        HDFql.cursorInitialize();
        List<NwbResult> nwbResults = new LinkedList<>();
        Map<String, List<Object>> dataSets = new HashMap<>();
        boolean firstRun = true;
        String operator = "";
//        HDFqlCursor cursor = new HDFqlCursor();
//        HDFql.cursorInitialize(cursor);
//        int cursorUse1 = HDFql.cursorUse(cursor);
        HDFql.execute("ENABLE DEBUG");
//               HDFql.execute("ENABLE FLUSH LOCAL");
//        HDFql.execute("SET FILE CACHE SLOTS 1024 SIZE 2024 PREEMPTION 0.1");
//        HDFql.execute("SET DATASET CACHE SLOTS 1024 PREEMPTION 0.1");
//        logger.debug("cursor1Use: " + cursorUse1);
        String useFileQuery = "USE READONLY FILE " + fname;
        logger.debug("Use file query: " + useFileQuery);
        HDFql.execute(useFileQuery);
        // populate HDFql default cursor with name of the HDF file in use and display it
        logger.debug("File name: " + fname);
        HDFql.execute("SHOW USE FILE");
        HDFql.cursorFirst();
        logger.debug("File in use: " + HDFql.cursorGetChar());

        for(PartialExpression partialExpression : partialExpressions) {
            List<NwbResult> partialResult = new LinkedList<>();
            List<Object> values;
            Expression item = partialExpression.getExpression();
            String arithmeticalOperator = item.getOperator();
            List<String> showResults = partialExpression.getShowResults();
            for (String datasetsForSelect : showResults) {
                if (dataSets.containsKey(datasetsForSelect)) {
                    values = dataSets.get(datasetsForSelect);

                } else {
                    values = new ArrayList<>();
                    logger.debug("valuesType: " + values.getClass().getName());
                    //int registeringStatus = HDFql.variableRegister(valtmp);
//                logger.debug("Registering variable: " + registeringStatus);
//                    int size = HDFql.variableGetSize(values);
                    //logger.debug("Size: " + size);
                    //String selectQuery = "SELECT FROM " + datasetsForSelect + " INTO MEMORY " + HDFql.variableGetNumber(valtmp);
                    String selectQuery = "SELECT FROM " + datasetsForSelect;
//                    HDFqlCursor cursor3 = new HDFqlCursor();
//                    HDFql.cursorInitialize(cursor3);
//                    int cursorUse3 = HDFql.cursorUse(cursor3);
//                    logger.debug("cursor3Use: " + cursorUse3);
                    logger.debug("Select: " + selectQuery);
                    int selectStatus = HDFql.execute(selectQuery);
                    logger.debug("selectStatus: " + selectStatus);
                    int selectCursorResult;
                    //Somewhere here is a bug
                    while ((selectCursorResult = HDFql.cursorNext()) == HDFql.SUCCESS) {
                        Object value = getValue();
                        values.add(value);
                        logger.debug("selected value: " + value + ", " + value.getClass().getName());
                    }
                    logger.debug("SelectCursorResult: " + selectCursorResult);
                    //HDFql.cursorClear(cursor);
                    //cursor3.delete();
                    //logger.debug("VariableDataType:" + HDFql.variableGetDatatype(values));
                    logger.debug("objectValue: " + values);
                    dataSets.put(datasetsForSelect, values);
                }
                //HDFql.cursorClear();
                Expression clone = (Expression) item.clone();
                Expression rightSide = q.getRightSide(clone);
                logger.debug("Operator: " + clone.getOperator() + ", RightSide: " + rightSide);

                JexlExpression func = jexl.createExpression("x1" + arithmeticalOperator + "x2");
                mc.set("x2", rightSide.getExpressionValue());
                for (Object tmp : new ArrayList<>(values)) {
                    logger.debug("Value: " + tmp);
                    mc.set("x1", tmp);
                    Object eval = func.evaluate(mc);
                    if (((Boolean) eval).booleanValue()) {
                        partialResult.add(new NwbResult(datasetsForSelect, tmp));
                    }
                }

            }


            logger.debug(item + ", AND-OR-Operator: " + operator);

            if (("\\" + operator).equals(Operators.OR.op())) {
                logger.debug("...OR....");
                nwbResults = Restrictions.or(nwbResults, partialResult);
                nwbResults.forEach(name -> logger.debug(name));
            }
            if (operator.equals(Operators.AND.op())) {
                logger.debug("...AND....");
                nwbResults = Restrictions.and(nwbResults, partialResult);
                nwbResults.forEach(name -> logger.debug(name));
            }
            if (firstRun) {
                nwbResults.addAll(partialResult);
                firstRun = false;
            }
            operator = item.getParent().getOperator();
        }
        nwbResults.forEach(name -> logger.debug(name));
//        HDFqlCursor cursor4 = new HDFqlCursor();
//        HDFql.cursorInitialize(cursor4);
//        HDFql.cursorUse(cursor4);
        int closeResult = HDFql.execute("CLOSE FILE " + fname);
        logger.debug("Closing file: " + fname + ", resultCode: " + closeResult);
//        int cursor4clearStatus = HDFql.cursorClear(cursor4);
        //      logger.debug("cursor4ClearStatus:" + cursor4clearStatus);
        //      cursor4.delete();
        HDFql.cursorClear();
        return nwbResults;
    }

    private synchronized Object getValue() {
        Object res = null;
        int datatype = HDFql.cursorGetDatatype();
        logger.debug("DataType: " + datatype);
        if (datatype == HDFql.TINYINT) {
            res = HDFql.cursorGetTinyInt();
        }
        if (datatype == HDFql.UNSIGNED_TINYINT) {
            res = HDFql.cursorGetUnsignedTinyInt();
        }
        if (datatype == HDFql.SMALLINT) {
            res = HDFql.cursorGetSmallInt();
        }
        if (datatype == HDFql.UNSIGNED_SMALLINT) {
            res = HDFql.cursorGetUnsignedSmallInt();
        }
        if (datatype == HDFql.INT) {
            res = HDFql.cursorGetInt();
        }
        if (datatype == HDFql.UNSIGNED_INT) {
            res = HDFql.cursorGetUnsignedInt();
        }
        if (datatype == HDFql.BIGINT) {
            res = HDFql.cursorGetBigInt();
        }
        if (datatype == HDFql.UNSIGNED_BIGINT) {
            res = HDFql.cursorGetUnsignedBigInt();
        }
        if (datatype == HDFql.DOUBLE) {
            res = HDFql.cursorGetDouble();
        }
        if (datatype == HDFql.FLOAT) {
            res = HDFql.cursorGetFloat();
        }
        if (datatype == HDFql.CHAR) {
            res = HDFql.cursorGetChar();
        }
        if (res == null) {
            logger.error("IsNull");
        }
        return res;
    }


    /**
     * Recursively print a group and its members.
     * TODO not used now, but maybe later
     *
     * @throws Exception
     */
//    private static void printGroup(Group g, String indent) throws Exception {
//        if (g == null) return;
//
//        List<HObject> members = g.getMemberList();
//
//        int n = members.size();
//        indent += "    ";
//        HObject obj = null;
//        for (int i = 0; i < n; i++) {
//
//            obj = (HObject) members.get(i);
//            System.out.println(indent + obj);
//            List<Attribute> metadata = obj.getMetadata();
//            for (Attribute m : metadata) {
//                //System.out.println("metadta> " + m.toString(","));
//
//            }
//
//            if (obj instanceof Group) {
//                printGroup((Group) obj, indent);
//            }
//            if (obj instanceof Dataset) {
//                Dataset dataset = (Dataset) obj;
//
//                long[] start = dataset.getStartDims();
//                long[] stride = dataset.getStride();
//                long[] sizes = dataset.getSelectedDims();
//
//                System.out.println("start: " + Arrays.toString(start));
//                System.out.println("stride: " + Arrays.toString(stride));
//                System.out.println("sizes: " + Arrays.toString(sizes));
//
//
//                int size = dataset.getSize(dataset.getDatatype().getDatatypeClass());
//                if (size > 0) {
//                    Object o = ((Dataset) obj).read();
//                }
//                //System.out.println("datatype: " + ((Dataset) obj).getDatatype().getDatatypeClass());
//                // Datatype.CLASS_STRING;
//            }
//        }
//    }

    public void test(String fname) {

        String useFileQuery = "USE FILE " + fname;
        logger.debug("Use file query: " + useFileQuery);
        int executeFile = HDFql.execute(useFileQuery);
        logger.debug("ExecutingFile: " + executeFile);
        // populate HDFql default cursor with name of the HDF file in use and display it
        logger.debug("File name: " + fname);

        String query = "SHOW LIKE **/epochs/**/start_time";
        logger.debug(query);
        int executeLikeRes;
//        for (int i = 0; i < 1000; i++) {
//            int maxCount = 10;
//            while ((executeLikeRes = HDFql.execute(query)) != HDFql.SUCCESS && maxCount-- > 0) {
//                logger.error("Error: " + HDFql.errorGetMessage() + ", code: " + HDFql.errorGetPosition() + ", possition: " + HDFql.errorGetPosition());
//            }
//            logger.debug("ExecuteLikeRes: " + executeLikeRes);
//            if (executeLikeRes < 0) {
//                logger.error("EEE " + executeLikeRes);
//            }
//
//            int ii;
//            while ((ii = HDFql.cursorNext()) == HDFql.SUCCESS) {
//                String cursorGetChar = HDFql.cursorGetChar();
//                logger.debug("IIII " + ii);
//                logger.debug("Like: " + cursorGetChar + " -- " + HDFql.cursorGetDatatype());
//            }
//        }
        for (int i = 0; i < 10000; i++) {

            String s = "SELECT FROM epochs/Trial_360/stop_time";
            HDFqlCursor c = new HDFqlCursor();
            HDFql.cursorInitialize(c);
            HDFql.cursorUse(c);
            int res = HDFql.execute(s);
            if (res < 0) {
                logger.error("err: " + res);
            }
            logger.debug("TestSelectRes: " + res);
            int selectCursorResult;
            while ((selectCursorResult = HDFql.cursorNext(c)) == HDFql.SUCCESS) {
                int datatype = HDFql.cursorGetDatatype(c);
                Object value = getValue();
                logger.debug("select dataset: " + datatype + " " + value + ", " + value.getClass().getName());
            }
            logger.debug("selectCurorRes: " + selectCursorResult);
            HDFql.cursorClear(c);
            //c.delete();
        }


    }

    @Override
    public List<NwbResult> executeQuery(Query query, File obj) throws Exception {
        List<NwbResult> res = new LinkedList<>();
        Map<String, List<PartialExpression>> files = new HashMap<>();
        logger.debug("File: " + obj.getAbsolutePath());
        if (obj.isFile()) {
            List<PartialExpression> pe = executeLikeQuery(query, obj.getAbsolutePath());
            files.put(obj.getAbsolutePath(), pe);
        } else {
            for (File item : obj.listFiles(new FileFilter() {
                @Override
                public boolean accept(File pathname) {
                    return pathname.getName().toLowerCase().endsWith(".nwb");
                }
            })) {
                logger.debug("Individual file: " + item.getAbsolutePath());
                List<PartialExpression> peTmp = executeLikeQuery(query, item.getAbsolutePath());
                //List<NwbResult> partialRes = executeQuery(query, item.getAbsolutePath());

                files.put(item.getAbsolutePath(), peTmp);
            }
        }
        for (Map.Entry<String, List<PartialExpression>> cursor : files.entrySet()) {
            List<NwbResult> tmpRes = executeQuery(query, cursor.getKey(), cursor.getValue());
            logger.debug("I have partial res: " + tmpRes.size() + ", file: " + cursor.getKey());
            res.addAll(tmpRes);
        }

        return res;
    }
}

