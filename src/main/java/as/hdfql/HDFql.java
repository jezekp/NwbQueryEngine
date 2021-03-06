/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package as.hdfql;

public class HDFql implements HDFqlConstants {

		private static Variable variableList[] = {null, null, null, null, null, null, null, null};

		static
		{
			javaSetEnvironment();
		}

		private HDFql()   // INFO: set constructor visibility to private to disable instantiation of the class
		{
		}

		private static class Variable
		{
			public Object variable;
			public int dataType;

			public Variable(Object variable, int dataType)
			{
				this.variable = variable;
				this.dataType = dataType;
			}
		}

		private static int variableOperation(int type, int number, int size)
		{
			int dataType;

			dataType = variableList[number].dataType;
			if (type == 0)   // INFO: CREATE DATASET, CREATE ATTRIBUTE OR INSERT operations
			{
				if (dataType == TINYINT)
				{
					return variableCopyFromChar(variableList[number].variable, number, size, 0);
				}
				else if (dataType == SMALLINT)
				{
					return variableCopyFromShort(variableList[number].variable, number, size, 0);
				}
				else if (dataType == INT)
				{
					return variableCopyFromInt(variableList[number].variable, number, size, 0);
				}
				else if (dataType == BIGINT)
				{
					return variableCopyFromLong(variableList[number].variable, number, size, 0);
				}
				else if (dataType == FLOAT)
				{
					return variableCopyFromFloat(variableList[number].variable, number, size, 0);
				}
				else if (dataType == DOUBLE)
				{
					return variableCopyFromDouble(variableList[number].variable, number, size, 0);
				}
				else   // VARCHAR
				{
					return variableCopyFromString(variableList[number].variable, number, size, 0);
				}
			}
			else   // INFO: SELECT OR SHOW operations
			{
				if (dataType == TINYINT)
				{
					return variableCopyIntoChar(variableList[number].variable, number, size, 0);
				}
				else if (dataType == SMALLINT)
				{
					return variableCopyIntoShort(variableList[number].variable, number, size, 0);
				}
				else if (dataType == INT)
				{
					return variableCopyIntoInt(variableList[number].variable, number, size, 0);
				}
				else if (dataType == BIGINT)
				{
					return variableCopyIntoLong(variableList[number].variable, number, size, 0);
				}
				else if (dataType == FLOAT)
				{
					return variableCopyIntoFloat(variableList[number].variable, number, size, 0);
				}
				else if (dataType == DOUBLE)
				{
					return variableCopyIntoDouble(variableList[number].variable, number, size, 0);
				}
				else   // VARCHAR
				{
					return variableCopyIntoString(variableList[number].variable, number, size, 0);
				}
			}
		}

		public static int execute(String script)
		{
			int status;
			int i;

			if (script == null)
			{
				return executeReset();
			}

			status = execute(script, script.length(), JAVA);
			if ((status & 0x100) != 0)   // INFO: check if a transient variable was used
			{
				for(i = 0; i < 8; i++)
				{
					if ((status & (1 << (i + 9))) != 0)
					{
						variableList[i] = null;
					}
				}
				status &= 0x000000FF;
			}
			if (status > 127)
			{
				return status - 256;
			}
			else
			{
				return status;
			}
		}

		public static int cursorInitialize()
		{
			return cursorInitialize(null);
		}

		public static int cursorClear()
		{
			return cursorClear(null);
		}

		public static int cursorClone(HDFqlCursor cursorClone)
		{
			return cursorClone(null, cursorClone);
		}

		public static int cursorGetDataType()
		{
			return cursorGetDataType(null);
		}

		public static int cursorGetCount()
		{
			return cursorGetCount(null);
		}

		public static int subcursorGetCount()
		{
			return subcursorGetCount(null);
		}

		public static int cursorGetPosition()
		{
			return cursorGetPosition(null);
		}

		public static int subcursorGetPosition()
		{
			return subcursorGetPosition(null);
		}

		public static int cursorFirst()
		{
			return cursorFirst(null);
		}

		public static int subcursorFirst()
		{
			return subcursorFirst(null);
		}

		public static int cursorLast()
		{
			return cursorLast(null);
		}

		public static int subcursorLast()
		{
			return subcursorLast(null);
		}

		public static int cursorNext()
		{
			return cursorNext(null);
		}

		public static int subcursorNext()
		{
			return subcursorNext(null);
		}

		public static int cursorPrevious()
		{
			return cursorPrevious(null);
		}

		public static int subcursorPrevious()
		{
			return subcursorPrevious(null);
		}

		public static int cursorAbsolute(int position)
		{
			return cursorAbsolute(null, position);
		}

		public static int subcursorAbsolute(int position)
		{
			return subcursorAbsolute(null, position);
		}

		public static int cursorRelative(int position)
		{
			return cursorRelative(null, position);
		}

		public static int subcursorRelative(int position)
		{
			return subcursorRelative(null, position);
		}

		public static int cursorGetSize()
		{
			return cursorGetSize(null);
		}

		public static int subcursorGetSize()
		{
			return subcursorGetSize(null);
		}

		public static Byte cursorGetTinyInt()
		{
			return cursorGetTinyInt(null);
		}

		public static Byte subcursorGetTinyInt()
		{
			return subcursorGetTinyInt(null);
		}

		public static Byte cursorGetUnsignedTinyInt()
		{
			return cursorGetUnsignedTinyInt(null);
		}

		public static Byte subcursorGetUnsignedTinyInt()
		{
			return subcursorGetUnsignedTinyInt(null);
		}

		public static Short cursorGetSmallInt()
		{
			return cursorGetSmallInt(null);
		}

		public static Short subcursorGetSmallInt()
		{
			return subcursorGetSmallInt(null);
		}

		public static Short cursorGetUnsignedSmallInt()
		{
			return cursorGetUnsignedSmallInt(null);
		}

		public static Short subcursorGetUnsignedSmallInt()
		{
			return subcursorGetUnsignedSmallInt(null);
		}

		public static Integer cursorGetInt()
		{
			return cursorGetInt(null);
		}

		public static Integer subcursorGetInt()
		{
			return subcursorGetInt(null);
		}

		public static Integer cursorGetUnsignedInt()
		{
			return cursorGetUnsignedInt(null);
		}

		public static Integer subcursorGetUnsignedInt()
		{
			return subcursorGetUnsignedInt(null);
		}

		public static Long cursorGetBigInt()
		{
			return cursorGetBigInt(null);
		}

		public static Long subcursorGetBigInt()
		{
			return subcursorGetBigInt(null);
		}

		public static Long cursorGetUnsignedBigInt()
		{
			return cursorGetUnsignedBigInt(null);
		}

		public static Long subcursorGetUnsignedBigInt()
		{
			return subcursorGetUnsignedBigInt(null);
		}

		public static Float cursorGetFloat()
		{
			return cursorGetFloat(null);
		}

		public static Float subcursorGetFloat()
		{
			return subcursorGetFloat(null);
		}

		public static Double cursorGetDouble()
		{
			return cursorGetDouble(null);
		}

		public static Double subcursorGetDouble()
		{
			return subcursorGetDouble(null);
		}

		public static String cursorGetChar()
		{
			return cursorGetChar(null);
		}

		public static String subcursorGetChar()
		{
			return subcursorGetChar(null);
		}

		public static int variableRegister(Object variable)
		{
			Object type;
			Object tmp;
			int dataType;
			int number;
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			if (variable.getClass().isArray() == false)   // INFO: only arrays are allowed (otherwise it is not possible to have a "real" reference of the object being registered)
			{
				return ERROR_UNEXPECTED_DATA_TYPE;
			}
			number = -1;
			for(i = 0; i < 8; i++)
			{
				if (variableList[i] == null)
				{
					if (number == -1)
					{
						number = i;
					}
				}
				else
				{
					if (variable.equals(variableList[i].variable))
					{
						variableRegister(i, 0, NO);
						return i;
					}
				}
			}
			if (number == -1)
			{
				return ERROR_FULL;
			}
			type = null;
			tmp = variable;
			try
			{
				while(true)
				{
					java.lang.reflect.Array.getLength(tmp);
					type = tmp.getClass().getComponentType();
					tmp = java.lang.reflect.Array.get(tmp, 0);
				}
			}
			catch(Exception e)
			{
			}
			if (type == byte.class || type == Byte.class)   // INFO: it includes the OPAQUE data type as well
			{
				dataType = TINYINT;
			}
			else if (type == short.class || type == Short.class)
			{
				dataType = SMALLINT;
			}
			else if (type == int.class || type == Integer.class)
			{
				dataType = INT;
			}
			else if (type == long.class || type == Long.class)
			{
				dataType = BIGINT;
			}
			else if (type == float.class || type == Float.class)
			{
				dataType = FLOAT;
			}
			else if (type == double.class || type == Double.class)
			{
				dataType = DOUBLE;
			}
			else if (type == String.class)
			{
				dataType = VARCHAR;
			}
			else
			{
				return ERROR_UNEXPECTED_DATA_TYPE;
			}
			variableList[number] = new Variable(variable, dataType);
			variableRegister(number, 0, NO);
			return number;
		}

		public static int variableTransientRegister(Object variable)
		{
			Object type;
			Object tmp;
			int dataType;
			int number;
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			if (variable.getClass().isArray() == false)   // INFO: only arrays are allowed (otherwise it is not possible to have a "real" reference of the object being registered)
			{
				return ERROR_UNEXPECTED_DATA_TYPE;
			}
			number = -1;
			for(i = 0; i < 8; i++)
			{
				if (variableList[i] == null)
				{
					if (number == -1)
					{
						number = i;
					}
				}
				else
				{
					if (variable.equals(variableList[i].variable))
					{
						variableRegister(i, 0, YES);
						return i;
					}
				}
			}
			if (number == -1)
			{
				return ERROR_FULL;
			}
			type = null;
			tmp = variable;
			try
			{
				while(true)
				{
					java.lang.reflect.Array.getLength(tmp);
					type = tmp.getClass().getComponentType();
					tmp = java.lang.reflect.Array.get(tmp, 0);
				}
			}
			catch(Exception e)
			{
			}
			if (type == byte.class || type == Byte.class)   // INFO: it includes the OPAQUE data type as well
			{
				dataType = TINYINT;
			}
			else if (type == short.class || type == Short.class)
			{
				dataType = SMALLINT;
			}
			else if (type == int.class || type == Integer.class)
			{
				dataType = INT;
			}
			else if (type == long.class || type == Long.class)
			{
				dataType = BIGINT;
			}
			else if (type == float.class || type == Float.class)
			{
				dataType = FLOAT;
			}
			else if (type == double.class || type == Double.class)
			{
				dataType = DOUBLE;
			}
			else if (type == String.class)
			{
				dataType = VARCHAR;
			}
			else
			{
				return ERROR_UNEXPECTED_DATA_TYPE;
			}
			variableList[number] = new Variable(variable, dataType);
			variableRegister(number, 0, YES);
			return number;
		}

		public static int variableUnregister(Object variable)
		{
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			for(i = 0; i < 8; i++)
			{
				if (variableList[i] != null && variable.equals(variableList[i].variable))
				{
					variableUnregister(i);
					variableList[i] = null;
					return SUCCESS;
				}
			}
			return ERROR_NOT_REGISTERED;
		}

		public static int variableUnregisterAll()
		{
			int i;

			for(i = 0; i < 8; i++)
			{
				if (variableList[i] != null)
				{
					variableUnregister(i);
					variableList[i] = null;
				}
			}
			return SUCCESS;
		}

		public static int variableGetNumber(Object variable)
		{
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			for(i = 0; i < 8; i++)
			{
				if (variableList[i] != null && variable.equals(variableList[i].variable))
				{
					return i;
				}
			}
			return ERROR_NOT_REGISTERED;
		}

		public static int variableGetDataType(Object variable)
		{
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			for(i = 0; i < 8; i++)
			{
				if (variableList[i] != null && variable.equals(variableList[i].variable))
				{
					return variableGetDataType(i);
				}
			}
			return ERROR_NOT_REGISTERED;
		}

		public static int variableGetCount(Object variable)
		{
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			for(i = 0; i < 8; i++)
			{
				if (variableList[i] != null && variable.equals(variableList[i].variable))
				{
					return variableGetCount(i);
				}
			}
			return ERROR_NOT_REGISTERED;
		}

		public static int variableGetSize(Object variable)
		{
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			for(i = 0; i < 8; i++)
			{
				if (variableList[i] != null && variable.equals(variableList[i].variable))
				{
					return variableGetSize(i);
				}
			}
			return ERROR_NOT_REGISTERED;
		}

		public static int variableGetDimensionCount(Object variable)
		{
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			for(i = 0; i < 8; i++)
			{
				if (variableList[i] != null && variable.equals(variableList[i].variable))
				{
					return variableGetDimensionCount(i);
				}
			}
			return ERROR_NOT_REGISTERED;
		}

		public static int variableGetDimension(Object variable, int index)
		{
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			for(i = 0; i < 8; i++)
			{
				if (variableList[i] != null && variable.equals(variableList[i].variable))
				{
					return variableGetDimension(i, index);
				}
			}
			return ERROR_NOT_REGISTERED;
		}

		private static int variableCopyFromChar(Object variable, int number, int size, int count)
		{
			Object element;
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyFromChar(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetChar(number, (char) 0, count);
					}
					else
					{
						variableSetChar(number, (char) ((byte) element), count);
					}
					count++;
				}
			}
			return count;
		}

		private static int variableCopyFromShort(Object variable, int number, int size, int count)
		{
			Object element;
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyFromShort(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetShort(number, (short) 0, count);
					}
					else
					{
						variableSetShort(number, (short) element, count);
					}
					count += 2;
				}
			}
			return count;
		}

		private static int variableCopyFromInt(Object variable, int number, int size, int count)
		{
			Object element;
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyFromInt(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetInt(number, 0, count);
					}
					else
					{
						variableSetInt(number, (int) element, count);
					}
					count += 4;
				}
			}
			return count;
		}

		private static int variableCopyFromLong(Object variable, int number, int size, int count)
		{
			Object element;
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyFromLong(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetLong(number, (long) 0, count);
					}
					else
					{
						variableSetLong(number, (long) element, count);
					}
					count += 8;
				}
			}
			return count;
		}

		private static int variableCopyFromFloat(Object variable, int number, int size, int count)
		{
			Object element;
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyFromFloat(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetFloat(number, (float) 0, count);
					}
					else
					{
						variableSetFloat(number, (float) element, count);
					}
					count += 4;
				}
			}
			return count;
		}

		private static int variableCopyFromDouble(Object variable, int number, int size, int count)
		{
			Object element;
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyFromDouble(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetDouble(number, (double) 0, count);
					}
					else
					{
						variableSetDouble(number, (double) element, count);
					}
					count += 8;
				}
			}
			return count;
		}

		private static int variableCopyFromString(Object variable, int number, int size, int count)
		{
			Object element;
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyFromString(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetString(number, "", count, 0);
					}
					else
					{
						variableSetString(number, (String) element, count, ((String) element).length());
					}
					count++;
				}
			}
			return count;
		}

		private static int variableCopyIntoChar(Object variable, int number, int size, int count)
		{
			boolean flag;
			int length;
			int i;
			int j;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyIntoChar(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					j = variableGetChar(number, count);
					java.lang.reflect.Array.set(variable, i, (byte) j);
					count++;
				}
			}
			return count;
		}

		private static int variableCopyIntoShort(Object variable, int number, int size, int count)
		{
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyIntoShort(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					java.lang.reflect.Array.set(variable, i, variableGetShort(number, count));
					count += 2;
				}
			}
			return count;
		}

		private static int variableCopyIntoInt(Object variable, int number, int size, int count)
		{
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyIntoInt(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					java.lang.reflect.Array.set(variable, i, variableGetInt(number, count));
					count += 4;
				}
			}
			return count;
		}

		private static int variableCopyIntoLong(Object variable, int number, int size, int count)
		{
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyIntoLong(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					java.lang.reflect.Array.set(variable, i, variableGetLong(number, count));
					count += 8;
				}
			}
			return count;
		}

		private static int variableCopyIntoFloat(Object variable, int number, int size, int count)
		{
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyIntoFloat(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					java.lang.reflect.Array.set(variable, i, variableGetFloat(number, count));
					count += 4;
				}
			}
			return count;
		}

		private static int variableCopyIntoDouble(Object variable, int number, int size, int count)
		{
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyIntoDouble(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					java.lang.reflect.Array.set(variable, i, variableGetDouble(number, count));
					count += 8;
				}
			}
			return count;
		}

		private static int variableCopyIntoString(Object variable, int number, int size, int count)
		{
			boolean flag;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			try
			{
				flag = java.lang.reflect.Array.get(variable, 0).getClass().isArray();
			}
			catch(Exception e)
			{
				flag = false;
			}
			if (flag)
			{
				for(i = 0; i < length && count < size; i++)
				{
					count = variableCopyIntoString(java.lang.reflect.Array.get(variable, i), number, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					java.lang.reflect.Array.set(variable, i, variableGetString(number, count));
					count++;
				}
			}
			return count;
		}
	
  public static int executeGetStatus() {
    return HDFqlJNI.executeGetStatus();
  }

  public static int errorGetLine() {
    return HDFqlJNI.errorGetLine();
  }

  public static int errorGetPosition() {
    return HDFqlJNI.errorGetPosition();
  }

  public static String errorGetMessage() {
    return HDFqlJNI.errorGetMessage();
  }

  public static int mpiGetSize() {
    return HDFqlJNI.mpiGetSize();
  }

  public static int mpiGetRank() {
    return HDFqlJNI.mpiGetRank();
  }

  private static int execute(String script, int script_size, int programming_language) {
    return HDFqlJNI.execute(script, script_size, programming_language);
  }

  private static int executeReset() {
    return HDFqlJNI.executeReset();
  }

  private static void variableRegister(int number, long address, int arg2) {
    HDFqlJNI.variableRegister(number, address, arg2);
  }

  private static void variableUnregister(int number) {
    HDFqlJNI.variableUnregister(number);
  }

  private static int variableGetDataType(int number) {
    return HDFqlJNI.variableGetDataType(number);
  }

  private static int variableGetCount(int number) {
    return HDFqlJNI.variableGetCount(number);
  }

  private static int variableGetSize(int number) {
    return HDFqlJNI.variableGetSize(number);
  }

  private static int variableGetDimensionCount(int number) {
    return HDFqlJNI.variableGetDimensionCount(number);
  }

  private static int variableGetDimension(int number, int index) {
    return HDFqlJNI.variableGetDimension(number, index);
  }

  public static int cursorInitialize(HDFqlCursor cursor) {
    return HDFqlJNI.cursorInitialize(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int cursorUse(HDFqlCursor cursor) {
    return HDFqlJNI.cursorUse(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int cursorUseDefault() {
    return HDFqlJNI.cursorUseDefault();
  }

  public static int cursorClear(HDFqlCursor cursor) {
    return HDFqlJNI.cursorClear(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int cursorClone(HDFqlCursor cursorOriginal, HDFqlCursor cursorClone) {
    return HDFqlJNI.cursorClone(HDFqlCursor.getCPtr(cursorOriginal), cursorOriginal, HDFqlCursor.getCPtr(cursorClone), cursorClone);
  }

  public static int cursorGetDataType(HDFqlCursor cursor) {
    return HDFqlJNI.cursorGetDataType(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int cursorGetCount(HDFqlCursor cursor) {
    return HDFqlJNI.cursorGetCount(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int subcursorGetCount(HDFqlCursor cursor) {
    return HDFqlJNI.subcursorGetCount(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int cursorGetPosition(HDFqlCursor cursor) {
    return HDFqlJNI.cursorGetPosition(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int subcursorGetPosition(HDFqlCursor cursor) {
    return HDFqlJNI.subcursorGetPosition(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int cursorFirst(HDFqlCursor cursor) {
    return HDFqlJNI.cursorFirst(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int subcursorFirst(HDFqlCursor cursor) {
    return HDFqlJNI.subcursorFirst(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int cursorLast(HDFqlCursor cursor) {
    return HDFqlJNI.cursorLast(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int subcursorLast(HDFqlCursor cursor) {
    return HDFqlJNI.subcursorLast(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int cursorNext(HDFqlCursor cursor) {
    return HDFqlJNI.cursorNext(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int subcursorNext(HDFqlCursor cursor) {
    return HDFqlJNI.subcursorNext(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int cursorPrevious(HDFqlCursor cursor) {
    return HDFqlJNI.cursorPrevious(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int subcursorPrevious(HDFqlCursor cursor) {
    return HDFqlJNI.subcursorPrevious(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int cursorAbsolute(HDFqlCursor cursor, int position) {
    return HDFqlJNI.cursorAbsolute(HDFqlCursor.getCPtr(cursor), cursor, position);
  }

  public static int subcursorAbsolute(HDFqlCursor cursor, int position) {
    return HDFqlJNI.subcursorAbsolute(HDFqlCursor.getCPtr(cursor), cursor, position);
  }

  public static int cursorRelative(HDFqlCursor cursor, int position) {
    return HDFqlJNI.cursorRelative(HDFqlCursor.getCPtr(cursor), cursor, position);
  }

  public static int subcursorRelative(HDFqlCursor cursor, int position) {
    return HDFqlJNI.subcursorRelative(HDFqlCursor.getCPtr(cursor), cursor, position);
  }

  public static int cursorGetSize(HDFqlCursor cursor) {
    return HDFqlJNI.cursorGetSize(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static int subcursorGetSize(HDFqlCursor cursor) {
    return HDFqlJNI.subcursorGetSize(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static Byte cursorGetTinyInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetTinyInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Byte((byte) convertChar(pointer));
	}

  public static Byte subcursorGetTinyInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetTinyInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Byte((byte) convertChar(pointer));
	}

  public static Byte cursorGetUnsignedTinyInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetUnsignedTinyInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Byte((byte) convertChar(pointer));
	}

  public static Byte subcursorGetUnsignedTinyInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetUnsignedTinyInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Byte((byte) convertChar(pointer));
	}

  public static Short cursorGetSmallInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetSmallInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Short(convertShort(pointer));
	}

  public static Short subcursorGetSmallInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetSmallInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Short(convertShort(pointer));
	}

  public static Short cursorGetUnsignedSmallInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetUnsignedSmallInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Short(convertShort(pointer));
	}

  public static Short subcursorGetUnsignedSmallInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetUnsignedSmallInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Short(convertShort(pointer));
	}

  public static Integer cursorGetInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Integer(convertInt(pointer));
	}

  public static Integer subcursorGetInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Integer(convertInt(pointer));
	}

  public static Integer cursorGetUnsignedInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetUnsignedInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Integer(convertInt(pointer));
	}

  public static Integer subcursorGetUnsignedInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetUnsignedInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Integer(convertInt(pointer));
	}

  public static Long cursorGetBigInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetBigInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Long(convertLong(pointer));
	}

  public static Long subcursorGetBigInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetBigInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Long(convertLong(pointer));
	}

  public static Long cursorGetUnsignedBigInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetUnsignedBigInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Long(convertLong(pointer));
	}

  public static Long subcursorGetUnsignedBigInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetUnsignedBigInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Long(convertLong(pointer));
	}

  public static Float cursorGetFloat(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetFloat(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Float(convertFloat(pointer));
	}

  public static Float subcursorGetFloat(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetFloat(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Float(convertFloat(pointer));
	}

  public static Double cursorGetDouble(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetDouble(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Double(convertDouble(pointer));
	}

  public static Double subcursorGetDouble(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetDouble(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}

		return new Double(convertDouble(pointer));
	}

  public static String cursorGetChar(HDFqlCursor cursor) {
    return HDFqlJNI.cursorGetChar(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static String subcursorGetChar(HDFqlCursor cursor) {
    return HDFqlJNI.subcursorGetChar(HDFqlCursor.getCPtr(cursor), cursor);
  }

  private static void javaSetEnvironment() {
    HDFqlJNI.javaSetEnvironment();
  }

  private static void variableSetChar(int number, char data, int position) {
    HDFqlJNI.variableSetChar(number, data, position);
  }

  private static void variableSetShort(int number, short data, int position) {
    HDFqlJNI.variableSetShort(number, data, position);
  }

  private static void variableSetInt(int number, int data, int position) {
    HDFqlJNI.variableSetInt(number, data, position);
  }

  private static void variableSetLong(int number, long data, int position) {
    HDFqlJNI.variableSetLong(number, data, position);
  }

  private static void variableSetFloat(int number, float data, int position) {
    HDFqlJNI.variableSetFloat(number, data, position);
  }

  private static void variableSetDouble(int number, double data, int position) {
    HDFqlJNI.variableSetDouble(number, data, position);
  }

  private static void variableSetString(int number, String data, int position, int size) {
    HDFqlJNI.variableSetString(number, data, position, size);
  }

  private static char variableGetChar(int number, int position) {
    return HDFqlJNI.variableGetChar(number, position);
  }

  private static short variableGetShort(int number, int position) {
    return HDFqlJNI.variableGetShort(number, position);
  }

  private static int variableGetInt(int number, int position) {
    return HDFqlJNI.variableGetInt(number, position);
  }

  private static long variableGetLong(int number, int position) {
    return HDFqlJNI.variableGetLong(number, position);
  }

  private static float variableGetFloat(int number, int position) {
    return HDFqlJNI.variableGetFloat(number, position);
  }

  private static double variableGetDouble(int number, int position) {
    return HDFqlJNI.variableGetDouble(number, position);
  }

  private static String variableGetString(int number, int position) {
    return HDFqlJNI.variableGetString(number, position);
  }

  public static String getCanonicalPath(String objectName) {
    return HDFqlJNI.getCanonicalPath(objectName);
  }

  private static char convertChar(long pointer) {
    return HDFqlJNI.convertChar(pointer);
  }

  private static short convertShort(long pointer) {
    return HDFqlJNI.convertShort(pointer);
  }

  private static int convertInt(long pointer) {
    return HDFqlJNI.convertInt(pointer);
  }

  private static long convertLong(long pointer) {
    return HDFqlJNI.convertLong(pointer);
  }

  private static float convertFloat(long pointer) {
    return HDFqlJNI.convertFloat(pointer);
  }

  private static double convertDouble(long pointer) {
    return HDFqlJNI.convertDouble(pointer);
  }

}
