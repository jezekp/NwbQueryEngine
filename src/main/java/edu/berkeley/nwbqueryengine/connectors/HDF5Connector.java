package edu.berkeley.nwbqueryengine.connectors;

import com.hdfql.HDFql;
import com.hdfql.HDFqlConstants;
import com.hdfql.HDFqlCursor;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Operators;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.query.result.NwbResult;
import edu.berkeley.nwbqueryengine.query.result.Restrictions;
import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.H5File;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import scala.collection.TraversableOnce;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import java.io.File;
import java.io.FileFilter;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Created by petr-jezek on 18.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class HDF5Connector implements Connector<File> {

    private Log logger = LogFactory.getLog(getClass());


    public List<NwbResult> executeQuery(Query q, String fname) throws Exception {
        List<NwbResult> nwbResults = new LinkedList<>();
        HDFqlCursor cursor = new HDFqlCursor();
        HDFql.cursorInitialize(cursor);
        HDFql.cursorUse(cursor);
        boolean firstRun = true;
        String operator = "";
        String useFileQuery = "USE FILE " + fname;
        logger.debug("Use file query: " + useFileQuery);
        HDFql.execute(useFileQuery);
        // populate HDFql default cursor with name of the HDF file in use and display it
        logger.debug("File name: " + fname);
        HDFql.execute("SHOW USE FILE");
        HDFql.cursorFirst(cursor);
        logger.debug("File in use: " + HDFql.cursorGetChar(cursor));
        HDFql.cursorClear(cursor);
        for (Expression item : q.leftSideOfExpressions()) {
            logger.debug("Processing  expression: " + item.getExpressionValue() + " " + item.getOperator());
            String query = "SHOW LIKE **/" + q.getQueryLeftSide() + "/**/" + StringUtils.strip(item.getExpressionValue().trim(), "''\"\"");
            logger.debug(query);
            int executeLikeRes;
            int maxCount = 10;
            while ((executeLikeRes = HDFql.execute(query)) != HDFql.SUCCESS && maxCount-- > 0) {
                logger.error("Error: " + HDFql.errorGetMessage() + ", code: " + HDFql.errorGetPosition() + ", possition: " + HDFql.errorGetPosition());
            }
            logger.debug("ExecuteLikeRes: " + executeLikeRes);
            List<String> showResults = new LinkedList<>();
            int cursorRes;
            while ((cursorRes = HDFql.cursorNext(cursor)) == HDFql.SUCCESS) {
                String cursorGetChar = HDFql.cursorGetChar(cursor);
                showResults.add(cursorGetChar);
                logger.debug("Like: " + cursorGetChar + " -- " + HDFql.cursorGetDatatype(cursor));
            }
            HDFql.cursorInitialize(cursor);
            HDFql.cursorUse(cursor);
            logger.debug("cursorRes:" + cursorRes);
            List<NwbResult> partialResult = new LinkedList<>();
            for (String showResult : showResults) {
                List<Object> values = new LinkedList<>();
                logger.debug("valuesType: " + values.getClass().getName());
//                int registeringStatus = HDFql.variableRegister(values);
//                logger.debug("Registering variable: " + registeringStatus);
                int size = HDFql.variableGetSize(values);
                logger.debug("Size: " + size);
                //String selectQuery = "SELECT FROM " + showResult + " INTO MEMORY " + HDFql.variableGetNumber(values);
                String selectQuery = "SELECT FROM " + showResult;
                logger.debug("Select: " + selectQuery);
                int selectStatus = HDFql.execute(selectQuery);
                logger.debug("selectStatus: " + selectStatus);
                while (HDFql.cursorNext(cursor) == HDFql.SUCCESS) {
                    Object value = getValue(HDFql.cursorGetDatatype(cursor), cursor);
                    values.add(value);
                    logger.debug("select dataset: " + HDFql.cursorGetDatatype(cursor) + " " + value + ", " + value.getClass().getName());
                }
                logger.debug("VariableDataType:" + HDFql.variableGetDatatype(values));
                logger.debug("objectValue: " + values);
                for (Object tmp : values) {
                    logger.debug("Value: " + tmp);
                    Expression rightSide = q.getRightSide(item);
                    logger.debug("Operator: " + item.getOperator() + ", RightSide: " + rightSide);

                    ScriptEngineManager mgr = new ScriptEngineManager();
                    ScriptEngine engine = mgr.getEngineByName("JavaScript");
                    String exp = StringUtils.strip(tmp + "" + item.getOperator() + "" + rightSide.getExpressionValue());
                    Object eval = engine.eval(exp);
                    logger.debug("Expression: " + exp + ", res: " + eval);
                    if (((Boolean) eval).booleanValue()) {
                        logger.debug("Store:  " + exp);
                        partialResult.add(new NwbResult(showResult, tmp));
                    }

                }
//                HDFql.variableUnregister(values);
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
        HDFql.cursorClear(cursor);
        cursor.delete();
        int closeResult = HDFql.execute("CLOSE FILE " + fname);
        logger.debug("Closing file: " + fname + ", resultCode: " + closeResult);
        return nwbResults;
    }

    private Object getValue(int datatype, HDFqlCursor cursor) {
        Object res = null;
        if (datatype == HDFql.TINYINT) {
            res = HDFql.cursorGetTinyInt(cursor);
        }
        if (datatype == HDFql.UNSIGNED_TINYINT) {
            res = HDFql.cursorGetUnsignedTinyInt(cursor);
        }
        if (datatype == HDFql.SMALLINT) {
            res = HDFql.cursorGetSmallInt(cursor);
        }
        if (datatype == HDFql.UNSIGNED_SMALLINT) {
            res = HDFql.cursorGetUnsignedSmallInt(cursor);
        }
        if (datatype == HDFql.INT) {
            res = HDFql.cursorGetInt(cursor);
        }
        if (datatype == HDFql.UNSIGNED_INT) {
            res = HDFql.cursorGetUnsignedInt(cursor);
        }
        if (datatype == HDFql.BIGINT) {
            res = HDFql.cursorGetBigInt(cursor);
        }
        if (datatype == HDFql.UNSIGNED_BIGINT) {
            res = HDFql.cursorGetUnsignedBigInt(cursor);
        }
        if (datatype == HDFql.DOUBLE) {
            res = HDFql.cursorGetDouble(cursor);
        }
        if (datatype == HDFql.FLOAT) {
            res = HDFql.cursorGetFloat(cursor);
        }
        if (datatype == HDFql.CHAR) {
            res = HDFql.cursorGetChar(cursor);
        }
        return res;
    }


    /**
     * Recursively print a group and its members.
     * TODO not used now, but maybe later
     *
     * @throws Exception
     */
    private static void printGroup(Group g, String indent) throws Exception {
        if (g == null) return;

        List<HObject> members = g.getMemberList();

        int n = members.size();
        indent += "    ";
        HObject obj = null;
        for (int i = 0; i < n; i++) {

            obj = (HObject) members.get(i);
            System.out.println(indent + obj);
            List<Attribute> metadata = obj.getMetadata();
            for (Attribute m : metadata) {
                //System.out.println("metadta> " + m.toString(","));

            }

            if (obj instanceof Group) {
                printGroup((Group) obj, indent);
            }
            if (obj instanceof Dataset) {
                Dataset dataset = (Dataset) obj;

                long[] start = dataset.getStartDims();
                long[] stride = dataset.getStride();
                long[] sizes = dataset.getSelectedDims();

                System.out.println("start: " + Arrays.toString(start));
                System.out.println("stride: " + Arrays.toString(stride));
                System.out.println("sizes: " + Arrays.toString(sizes));


                int size = dataset.getSize(dataset.getDatatype().getDatatypeClass());
                if (size > 0) {
                    Object o = ((Dataset) obj).read();
                }
                //System.out.println("datatype: " + ((Dataset) obj).getDatatype().getDatatypeClass());
                // Datatype.CLASS_STRING;
            }
        }
    }

    @Override
    public List<NwbResult> executeQuery(Query query, File obj) throws Exception {
        List<NwbResult> res = new LinkedList<>();
        logger.debug("File: " + obj.getAbsolutePath());
        if (obj.isFile()) {
            res = executeQuery(query, obj.getAbsolutePath());
        } else {
            for (File item : obj.listFiles(new FileFilter() {
                @Override
                public boolean accept(File pathname) {
                    return pathname.getName().toLowerCase().endsWith(".nwb");
                }
            })) {
                logger.debug("Individual file: " + item.getAbsolutePath());
                List<NwbResult> partialRes = executeQuery(query, item.getAbsolutePath());
                logger.debug("I have partial res: " + partialRes.size() + ", file: " + item.getName());
                res.addAll(partialRes);
            }
        }
        return res;
    }
}

