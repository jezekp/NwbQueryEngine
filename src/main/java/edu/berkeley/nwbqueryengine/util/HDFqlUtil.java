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

    public Object getValue(HDFqlCursor cursor) throws Exception {
        Object res = null;
        int datatype = HDFql.cursorGetDatatype(cursor);
        logger.debug("DataType: " + datatype);
        if (datatype == HDFql.TINYINT) {
            res = HDFql.cursorGetTinyInt(cursor);
        }
        else if (datatype == HDFql.UNSIGNED_TINYINT) {
            res = HDFql.cursorGetUnsignedTinyInt(cursor);
        }
        else if (datatype == HDFql.SMALLINT) {
            res = HDFql.cursorGetSmallInt(cursor);
        }
        else if (datatype == HDFql.UNSIGNED_SMALLINT) {
            res = HDFql.cursorGetUnsignedSmallInt(cursor);
        }
        else if (datatype == HDFql.INT) {
            res = HDFql.cursorGetInt(cursor);
        }
        else if (datatype == HDFql.UNSIGNED_INT) {
            res = HDFql.cursorGetUnsignedInt(cursor);
        }
        else if (datatype == HDFql.BIGINT) {
            res = HDFql.cursorGetBigInt(cursor);
        }
        else if (datatype == HDFql.UNSIGNED_BIGINT) {
            res = HDFql.cursorGetUnsignedBigInt(cursor);
        }
        else if (datatype == HDFql.DOUBLE) {
            res = HDFql.cursorGetDouble(cursor);
        }
        else if (datatype == HDFql.FLOAT) {
            res = HDFql.cursorGetFloat(cursor);
        }
        else if (datatype == HDFql.CHAR) {
            res = HDFql.cursorGetChar(cursor);
        }
        else if (datatype == HDFql.VARCHAR) {
            res = HDFql.cursorGetChar(cursor);
        }
        else if (res == null) {
            logger.error("IsNull");
            throw new Exception("Not recognized data type: " + datatype);
        }
        return res;
    }

}
