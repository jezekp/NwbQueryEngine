/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package as.hdfql;

public class HDFqlJNI {

		static
		{
			System.loadLibrary("HDFql");
		}
	
  public final static native long new_HDFqlCursor();
  public final static native void delete_HDFqlCursor(long jarg1);
  public final static native String VERSION_get();
  public final static native int YES_get();
  public final static native int NO_get();
  public final static native int ENABLED_get();
  public final static native int DISABLED_get();
  public final static native int UNLIMITED_get();
  public final static native int UNDEFINED_get();
  public final static native int GLOBAL_get();
  public final static native int LOCAL_get();
  public final static native int TRACKED_get();
  public final static native int INDEXED_get();
  public final static native int CONTIGUOUS_get();
  public final static native int COMPACT_get();
  public final static native int CHUNKED_get();
  public final static native int EARLY_get();
  public final static native int INCREMENTAL_get();
  public final static native int LATE_get();
  public final static native int DIRECTORY_get();
  public final static native int FILE_get();
  public final static native int GROUP_get();
  public final static native int DATASET_get();
  public final static native int ATTRIBUTE_get();
  public final static native int SOFT_LINK_get();
  public final static native int EXTERNAL_LINK_get();
  public final static native int TINYINT_get();
  public final static native int UNSIGNED_TINYINT_get();
  public final static native int SMALLINT_get();
  public final static native int UNSIGNED_SMALLINT_get();
  public final static native int INT_get();
  public final static native int UNSIGNED_INT_get();
  public final static native int BIGINT_get();
  public final static native int UNSIGNED_BIGINT_get();
  public final static native int FLOAT_get();
  public final static native int DOUBLE_get();
  public final static native int CHAR_get();
  public final static native int VARTINYINT_get();
  public final static native int UNSIGNED_VARTINYINT_get();
  public final static native int VARSMALLINT_get();
  public final static native int UNSIGNED_VARSMALLINT_get();
  public final static native int VARINT_get();
  public final static native int UNSIGNED_VARINT_get();
  public final static native int VARBIGINT_get();
  public final static native int UNSIGNED_VARBIGINT_get();
  public final static native int VARFLOAT_get();
  public final static native int VARDOUBLE_get();
  public final static native int VARCHAR_get();
  public final static native int OPAQUE_get();
  public final static native int BITFIELD_get();
  public final static native int ENUMERATION_get();
  public final static native int COMPOUND_get();
  public final static native int ARRAY_get();
  public final static native int REFERENCE_get();
  public final static native int LITTLE_ENDIAN_get();
  public final static native int BIG_ENDIAN_get();
  public final static native int ASCII_get();
  public final static native int UTF8_get();
  public final static native int FILL_DEFAULT_get();
  public final static native int FILL_USER_DEFINED_get();
  public final static native int FILL_UNDEFINED_get();
  public final static native int EARLIEST_get();
  public final static native int LATEST_get();
  public final static native int VERSION_18_get();
  public final static native int SUCCESS_get();
  public final static native int ERROR_PARSE_get();
  public final static native int ERROR_NOT_FOUND_get();
  public final static native int ERROR_NO_ACCESS_get();
  public final static native int ERROR_NOT_OPEN_get();
  public final static native int ERROR_INVALID_FILE_get();
  public final static native int ERROR_NOT_SUPPORTED_get();
  public final static native int ERROR_NOT_ENOUGH_SPACE_get();
  public final static native int ERROR_NOT_ENOUGH_MEMORY_get();
  public final static native int ERROR_ALREADY_EXISTS_get();
  public final static native int ERROR_EMPTY_get();
  public final static native int ERROR_FULL_get();
  public final static native int ERROR_BEFORE_FIRST_get();
  public final static native int ERROR_AFTER_LAST_get();
  public final static native int ERROR_OUTSIDE_LIMIT_get();
  public final static native int ERROR_NO_ADDRESS_get();
  public final static native int ERROR_UNEXPECTED_TYPE_get();
  public final static native int ERROR_UNEXPECTED_DATA_TYPE_get();
  public final static native int ERROR_UNEXPECTED_STORAGE_TYPE_get();
  public final static native int ERROR_DANGLING_LINK_get();
  public final static native int ERROR_NOT_REGISTERED_get();
  public final static native int ERROR_INVALID_REGULAR_EXPRESSION_get();
  public final static native int ERROR_UNKNOWN_get();
  public final static native int JAVA_get();
  public final static native int executeGetStatus();
  public final static native int errorGetLine();
  public final static native int errorGetPosition();
  public final static native String errorGetMessage();
  public final static native int mpiGetSize();
  public final static native int mpiGetRank();
  public final static native int execute(String jarg1, int jarg2, int jarg3);
  public final static native int executeReset();
  public final static native void variableRegister(int jarg1, long jarg2, int jarg3);
  public final static native void variableUnregister(int jarg1);
  public final static native int variableGetDataType(int jarg1);
  public final static native int variableGetCount(int jarg1);
  public final static native int variableGetSize(int jarg1);
  public final static native int variableGetDimensionCount(int jarg1);
  public final static native int variableGetDimension(int jarg1, int jarg2);
  public final static native int cursorInitialize(long jarg1, HDFqlCursor jarg1_);
  public final static native int cursorUse(long jarg1, HDFqlCursor jarg1_);
  public final static native int cursorUseDefault();
  public final static native int cursorClear(long jarg1, HDFqlCursor jarg1_);
  public final static native int cursorClone(long jarg1, HDFqlCursor jarg1_, long jarg2, HDFqlCursor jarg2_);
  public final static native int cursorGetDataType(long jarg1, HDFqlCursor jarg1_);
  public final static native int cursorGetCount(long jarg1, HDFqlCursor jarg1_);
  public final static native int subcursorGetCount(long jarg1, HDFqlCursor jarg1_);
  public final static native int cursorGetPosition(long jarg1, HDFqlCursor jarg1_);
  public final static native int subcursorGetPosition(long jarg1, HDFqlCursor jarg1_);
  public final static native int cursorFirst(long jarg1, HDFqlCursor jarg1_);
  public final static native int subcursorFirst(long jarg1, HDFqlCursor jarg1_);
  public final static native int cursorLast(long jarg1, HDFqlCursor jarg1_);
  public final static native int subcursorLast(long jarg1, HDFqlCursor jarg1_);
  public final static native int cursorNext(long jarg1, HDFqlCursor jarg1_);
  public final static native int subcursorNext(long jarg1, HDFqlCursor jarg1_);
  public final static native int cursorPrevious(long jarg1, HDFqlCursor jarg1_);
  public final static native int subcursorPrevious(long jarg1, HDFqlCursor jarg1_);
  public final static native int cursorAbsolute(long jarg1, HDFqlCursor jarg1_, int jarg2);
  public final static native int subcursorAbsolute(long jarg1, HDFqlCursor jarg1_, int jarg2);
  public final static native int cursorRelative(long jarg1, HDFqlCursor jarg1_, int jarg2);
  public final static native int subcursorRelative(long jarg1, HDFqlCursor jarg1_, int jarg2);
  public final static native int cursorGetSize(long jarg1, HDFqlCursor jarg1_);
  public final static native int subcursorGetSize(long jarg1, HDFqlCursor jarg1_);
  public final static native long cursorGetTinyInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long subcursorGetTinyInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long cursorGetUnsignedTinyInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long subcursorGetUnsignedTinyInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long cursorGetSmallInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long subcursorGetSmallInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long cursorGetUnsignedSmallInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long subcursorGetUnsignedSmallInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long cursorGetInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long subcursorGetInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long cursorGetUnsignedInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long subcursorGetUnsignedInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long cursorGetBigInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long subcursorGetBigInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long cursorGetUnsignedBigInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long subcursorGetUnsignedBigInt(long jarg1, HDFqlCursor jarg1_);
  public final static native long cursorGetFloat(long jarg1, HDFqlCursor jarg1_);
  public final static native long subcursorGetFloat(long jarg1, HDFqlCursor jarg1_);
  public final static native long cursorGetDouble(long jarg1, HDFqlCursor jarg1_);
  public final static native long subcursorGetDouble(long jarg1, HDFqlCursor jarg1_);
  public final static native String cursorGetChar(long jarg1, HDFqlCursor jarg1_);
  public final static native String subcursorGetChar(long jarg1, HDFqlCursor jarg1_);
  public final static native void javaSetEnvironment();
  public final static native void variableSetChar(int jarg1, char jarg2, int jarg3);
  public final static native void variableSetShort(int jarg1, short jarg2, int jarg3);
  public final static native void variableSetInt(int jarg1, int jarg2, int jarg3);
  public final static native void variableSetLong(int jarg1, long jarg2, int jarg3);
  public final static native void variableSetFloat(int jarg1, float jarg2, int jarg3);
  public final static native void variableSetDouble(int jarg1, double jarg2, int jarg3);
  public final static native void variableSetString(int jarg1, String jarg2, int jarg3, int jarg4);
  public final static native char variableGetChar(int jarg1, int jarg2);
  public final static native short variableGetShort(int jarg1, int jarg2);
  public final static native int variableGetInt(int jarg1, int jarg2);
  public final static native long variableGetLong(int jarg1, int jarg2);
  public final static native float variableGetFloat(int jarg1, int jarg2);
  public final static native double variableGetDouble(int jarg1, int jarg2);
  public final static native String variableGetString(int jarg1, int jarg2);
  public final static native String getCanonicalPath(String jarg1);
  public final static native char convertChar(long jarg1);
  public final static native short convertShort(long jarg1);
  public final static native int convertInt(long jarg1);
  public final static native long convertLong(long jarg1);
  public final static native float convertFloat(long jarg1);
  public final static native double convertDouble(long jarg1);
}
