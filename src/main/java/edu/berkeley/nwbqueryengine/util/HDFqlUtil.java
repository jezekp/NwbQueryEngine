package edu.berkeley.nwbqueryengine.util;

import as.hdfql.HDFql;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Created by petr-jezek on 19.6.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class HDFqlUtil {

    private Log logger = LogFactory.getLog(getClass());

    public Object getValue() {
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

}
