package edu.berkeley.nwbqueryengine.util;

import as.hdfql.HDFql;
import as.hdfql.HDFqlCursor;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Created by petr-jezek on 19.6.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class HDFqlUtil {

    private Log logger = LogFactory.getLog(getClass());

    public Object getValue() throws Exception {
        Object res = null;
        int datatype = HDFql.cursorGetDataType();
        logger.debug("DataType: " + datatype);
        if (datatype == HDFql.TINYINT) {
            res = HDFql.cursorGetTinyInt();
        } else if (datatype == HDFql.UNSIGNED_TINYINT) {
            res = HDFql.cursorGetUnsignedTinyInt();
        } else if (datatype == HDFql.SMALLINT) {
            res = HDFql.cursorGetSmallInt();
        } else if (datatype == HDFql.UNSIGNED_SMALLINT) {
            res = HDFql.cursorGetUnsignedSmallInt();
        } else if (datatype == HDFql.INT) {
            res = HDFql.cursorGetInt();
        } else if (datatype == HDFql.UNSIGNED_INT) {
            res = HDFql.cursorGetUnsignedInt();
        } else if (datatype == HDFql.BIGINT) {
            res = HDFql.cursorGetBigInt();
        } else if (datatype == HDFql.UNSIGNED_BIGINT) {
            res = HDFql.cursorGetUnsignedBigInt();
        } else if (datatype == HDFql.DOUBLE) {
            res = HDFql.cursorGetDouble();
        } else if (datatype == HDFql.FLOAT) {
            res = HDFql.cursorGetFloat();
        } else if (datatype == HDFql.CHAR) {
            res = processString();
        } else if (datatype == HDFql.VARCHAR) {
            res = HDFql.cursorGetChar();
        } else if (res == null) {
            logger.error("IsNull");
            throw new Exception("Not recognized data type: " + datatype);
        }
        return res;
    }

    private String processString() {
        StringBuilder stringBuilder = new StringBuilder();
        while (HDFql.subcursorNext() == HDFql.SUCCESS) {
            Byte b = HDFql.subcursorGetUnsignedTinyInt();
            stringBuilder.append((char) b.doubleValue());
        }
        return stringBuilder.toString();
    }

}
