package edu.berkeley.nwbqueryengine.connectors;

import com.hdfql.HDFql;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Query;
import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.H5File;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

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


    private static String path = "/home/petr-jezek/Data/nwb_datasets/nwbMatlab_DG";
    private static String file = "ANM184389_20130207.nwb";
    private static String fname = path + "/" + file;


    public FileFormat connect(String fileName) throws Exception {
        FileFormat fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);
        FileFormat file = fileFormat.createInstance(fileName, FileFormat.READ);
        file.open();
        return file;
    }

    public String executeQuery(Query q) {
        HDFql.execute("USE FILE " + fname);
        // populate HDFql default cursor with name of the HDF file in use and display it
        logger.debug("File name: " + fname);
        HDFql.execute("SHOW USE FILE");
        HDFql.cursorFirst();
        logger.debug("File in use: " + HDFql.cursorGetChar());
        for (Expression item : q.leftSideOfExpressions()) {
            String query = "SHOW LIKE **/" + q.getLeftSide() + "/**/" + StringUtils.strip(item.getExpressionValue(), "'|\"\"");
            logger.debug(query);

            HDFql.execute(query);

            HDFql.cursorFirst();
            List<String> showResults = new LinkedList<>();
            while (HDFql.cursorNext() == HDFql.SUCCESS) {
                String res = HDFql.cursorGetChar();
                showResults.add(res);
                logger.debug("Like: " + res + " -- " + HDFql.cursorGetDatatype());
            }

            for (String r : showResults) {
                double[] values = new double[1];
                int registeringStatus = HDFql.variableRegister(values);
                logger.debug("Registering variable: " + registeringStatus);
                int size = HDFql.variableGetSize(values);
                logger.debug("Size: " + size);
                String selectQuery = "SELECT FROM " + r + " INTO MEMORY " + HDFql.variableGetNumber(values);
                logger.debug("Select: " + selectQuery);
                int selectStatus = HDFql.execute(selectQuery);
                logger.debug("selectStatus: " + selectStatus);
                for (int i = 0; i < values.length; i++) {
                    logger.debug("Value: " + values[i]);
                    Expression rightSide = q.getRightSide(item);
                    logger.debug("RightSide: " + rightSide);
                }
                HDFql.variableUnregister(values);
            }
        }


        return null;
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

        H5File h5file = new H5File(fname);
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
            FileFormat testFile = fileFormat.createInstance(fname, FileFormat.READ);
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

