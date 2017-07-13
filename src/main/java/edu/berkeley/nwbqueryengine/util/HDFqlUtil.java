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

    public Object getValue(HDFqlCursor cursor) {
        Object res = null;
        int datatype = HDFql.cursorGetDatatype(cursor);
        logger.debug("DataType: " + datatype);
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
        if (res == null) {
            logger.error("IsNull");
        }
        return res;
    }

}
