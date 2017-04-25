package edu.berkeley.nwbqueryengine.connectors;

import com.hdfql.HDFql;
import com.hdfql.HDFqlConstants;
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
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Created by petr-jezek on 18.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class HDF5Connector {

    private Log logger = LogFactory.getLog(getClass());


    public FileFormat connect(String fileName) throws Exception {
        FileFormat fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);
        FileFormat file = fileFormat.createInstance(fileName, FileFormat.READ);
        file.open();
        return file;
    }

    public List<NwbResult> executeQuery(Query q, String fname) throws Exception {
        List<NwbResult> nwbResults = new LinkedList<>();
        boolean firstRun = true;
        String operator = "";
        String useFileQuery = "USE FILE " + fname;
        logger.debug("Use file query: " + useFileQuery);
        HDFql.execute(useFileQuery);
        // populate HDFql default cursor with name of the HDF file in use and display it
        logger.debug("File name: " + fname);
        HDFql.execute("SHOW USE FILE");
        HDFql.cursorFirst();
        logger.debug("File in use: " + HDFql.cursorGetChar());
        for (Expression item : q.leftSideOfExpressions()) {
            logger.debug("Processing  expression: " + item.getExpressionValue() + " " + item.getOperator());
            String query = "SHOW LIKE **/" + q.getQueryLeftSide() + "/**/" + StringUtils.strip(item.getExpressionValue().trim(), "''\"\"");
            logger.debug(query);

            HDFql.execute(query);

            List<String> showResults = new LinkedList<>();
            while (HDFql.cursorNext() == HDFql.SUCCESS) {
                String cursorGetChar = HDFql.cursorGetChar();
                showResults.add(cursorGetChar);
                logger.debug("Like: " + cursorGetChar + " -- " + HDFql.cursorGetDatatype());
            }
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
                while (HDFql.cursorNext() == HDFql.SUCCESS) {
                    Object value = getValue(HDFql.cursorGetDatatype());
                    values.add(value);
                    logger.debug("select dataset: " + HDFql.cursorGetDatatype() + " " + value + ", " + value.getClass().getName());
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
            }
            if (operator.equals(Operators.AND.op())) {
                logger.debug("...AND....");
                nwbResults = Restrictions.and(nwbResults, partialResult);
            }
            if (firstRun) {
                nwbResults.addAll(partialResult);
                firstRun = false;
            }
            operator = item.getParent().getOperator();
        }


        return nwbResults;
    }

    private Object getValue(int datatype) {
        Object res = null;
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
        return res;
    }


    public static void x() {

        try {
            Class fileclass = Class.forName("ncsa.hdf.object.h5.H5File");
            FileFormat fileformat = (FileFormat) fileclass.newInstance();
            if (fileformat != null)
                FileFormat.addFileFormat("HDF5", fileformat);
        } catch (Throwable err) {
            err.printStackTrace();
        }

        //todo filename
        H5File h5file = new H5File("");
        try {
            h5file.open();
        } catch (Exception e) {
            e.printStackTrace();
        }


        FileFormat fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);
        if (fileFormat == null) {
            System.err.println("Cannot find HDF5 FileFormat.");
            return;
        }
        try {
            //todo filename
            FileFormat testFile = fileFormat.createInstance("", FileFormat.READ);
            testFile.open();
            //Group root = (Group) ((javax.swing.tree.DefaultMutableTreeNode) testFile.getRootNode()).getUserObject();
            Group root = (Group) testFile.get("/");
            printGroup(root, "");
            // close file resource
            testFile.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Recursively print a group and its members.
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
}

