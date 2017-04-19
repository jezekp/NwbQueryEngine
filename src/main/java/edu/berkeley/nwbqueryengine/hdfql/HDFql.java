package edu.berkeley.nwbqueryengine.hdfql;

import java.util.regex.Pattern;
		import java.util.regex.Matcher;
	
public class HDFql implements HDFqlConstants {

		private static Variable variableList[] = {null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null};
		private static Pattern patternFromMemory = Pattern.compile("(?i)FROM\\s+MEMORY\\s+(\\d+)(?:\\s+SIZE\\s+(\\d+))?");
		private static Pattern patternIntoMemory = Pattern.compile("(?i)INTO\\s+MEMORY\\s+(\\d+)(?:\\s+SIZE\\s+(\\d+))?");

		private static class Variable
		{
			public Object variable;
			public int type;
			public int size;
			public int fromSize;
			public int intoSize;
			public int fromMemory;
			public int intoMemory;

			public Variable(Object variable, int type, int size)
			{
				this.variable = variable;
				this.type = type;
				this.size = size;
				this.fromSize = Integer.MAX_VALUE;
				this.intoSize = Integer.MAX_VALUE;
				this.fromMemory = 0;
				this.intoMemory = 0;
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

		public static int cursorGetDatatype()
		{
			return cursorGetDatatype(null);
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
			int size;
			int slot;
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			if (variable.getClass().isArray() == false)	// INFO: only arrays are allowed (otherwise it is not possible to have a "real" reference of the object being registered)
			{
				return ERROR_UNKNOWN;
			}
			slot = -1;
			for(i = 0; i < 16; i++)
			{
				if (variableList[i] == null)
				{
					if (slot == -1)
					{
						slot = i;
					}
				}
				else
				{
					if (variable.equals(variableList[i].variable))
					{
						return SUCCESS;
					}
				}
			}
			if (slot == -1)
			{
				return ERROR_FULL;
			}
			type = null;
			tmp = variable;
			size = 1;
			try
			{
				while(true)
				{
					size *= java.lang.reflect.Array.getLength(tmp);
					type = tmp.getClass().getComponentType();
					tmp = java.lang.reflect.Array.get(tmp, 0);
				}
			}
			catch(Exception e)
			{
			}
			if (type == byte.class || type == Byte.class)
			{
				i = TINYINT;
				size *= 1;
			}
			else if (type == short.class || type == Short.class)
			{
				i = SMALLINT;
				size *= 2;
			}
			else if (type == int.class || type == Integer.class)
			{
				i = INT;
				size *= 4;
			}
			else if (type == long.class || type == Long.class)
			{
				i = BIGINT;
				size *= 8;
			}
			else if (type == float.class || type == Float.class)
			{
				i = FLOAT;
				size *= 4;
			}
			else if (type == double.class || type == Double.class)
			{
				i = DOUBLE;
				size *= 8;
			}
			//else if (type == String.class)
			//{
			//	i = CHAR;
			//	size *= 1;
			//}
			else
			{
				return ERROR_UNKNOWN;
			}
			variableList[slot] = new Variable(variable, i, size);
			variableRegister(slot, 0);
			return SUCCESS;
		}

		public static int variableUnregister(Object variable)
		{
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			for(i = 0; i < 16; i++)
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

		public static int variableGetNumber(Object variable)
		{
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			for(i = 0; i < 16; i++)
			{
				if (variableList[i] != null && variable.equals(variableList[i].variable))
				{
					return i;
				}
			}
			return ERROR_NOT_REGISTERED;
		}

		public static int variableGetDatatype(Object variable)
		{
			int i;

			if (variable == null)
			{
				return ERROR_NO_ADDRESS;
			}
			for(i = 0; i < 16; i++)
			{
				if (variableList[i] != null && variable.equals(variableList[i].variable))
				{
					return variableGetDatatype(i);
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
			for(i = 0; i < 16; i++)
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
			for(i = 0; i < 16; i++)
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
			for(i = 0; i < 16; i++)
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
			for(i = 0; i < 16; i++)
			{
				if (variableList[i] != null && variable.equals(variableList[i].variable))
				{
					return variableGetDimension(i, index);
				}
			}
			return ERROR_NOT_REGISTERED;
		}

		private static int variableCopyFromChar(Object variable, int index, int size, int count)
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
					count = variableCopyFromChar(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetChar(index, (char) 0, count);
					}
					else
					{
						variableSetChar(index, (char) ((byte) element), count);
					}
					count++;
				}
			}
			return count;
		}

		private static int variableCopyFromShort(Object variable, int index, int size, int count)
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
					count = variableCopyFromShort(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetShort(index, (short) 0, count);
					}
					else
					{
						variableSetShort(index, (short) element, count);
					}
					count++;
				}
			}
			return count;
		}

		private static int variableCopyFromInt(Object variable, int index, int size, int count)
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
					count = variableCopyFromInt(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetInt(index, 0, count);
					}
					else
					{
						variableSetInt(index, (int) element, count);
					}
					count++;
				}
			}
			return count;
		}

		private static int variableCopyFromLong(Object variable, int index, int size, int count)
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
					count = variableCopyFromLong(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetLong(index, (long) 0, count);
					}
					else
					{
						variableSetLong(index, (long) element, count);
					}
					count++;
				}
			}
			return count;
		}

		private static int variableCopyFromFloat(Object variable, int index, int size, int count)
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
					count = variableCopyFromFloat(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetFloat(index, (float) 0, count);
					}
					else
					{
						variableSetFloat(index, (float) element, count);
					}
					count++;
				}
			}
			return count;
		}

		private static int variableCopyFromDouble(Object variable, int index, int size, int count)
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
					count = variableCopyFromDouble(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						variableSetDouble(index, (double) 0, count);
					}
					else
					{
						variableSetDouble(index, (double) element, count);
					}
					count++;
				}
			}
			return count;
		}

		private static int variableCopyFromString(Object variable, int index, int size, int count)
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
					count = variableCopyFromString(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					// TODO: to be implemented
					element = java.lang.reflect.Array.get(variable, i);
					if (element == null)
					{
						//variableSetString(index, "", count);
					}
					else
					{
						//variableSetString(index, (String) element, count);
					}
					count++;
				}
			}
			return count;
		}

		private static int variableCopyIntoChar(Object variable, int index, int size, int count)
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
				for(i = 0; i < length; i++)
				{
					count = variableCopyIntoChar(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count < size; i++)
				{
					j = variableGetChar(index, count);
					java.lang.reflect.Array.set(variable, i, (byte) j);
					count++;
				}
			}
			return count;
		}

		private static int variableCopyIntoShort(Object variable, int index, int size, int count)
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
				for(i = 0; i < length; i++)
				{
					count = variableCopyIntoShort(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count * 2 < size; i++)
				{
					java.lang.reflect.Array.set(variable, i, variableGetShort(index, count));
					count++;
				}
			}
			return count;
		}

		private static int variableCopyIntoInt(Object variable, int index, int size, int count)
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
				for(i = 0; i < length; i++)
				{
					count = variableCopyIntoInt(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count * 4 < size; i++)
				{
					java.lang.reflect.Array.set(variable, i, variableGetInt(index, count));
					count++;
				}
			}
			return count;
		}

		private static int variableCopyIntoLong(Object variable, int index, int size, int count)
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
				for(i = 0; i < length; i++)
				{
					count = variableCopyIntoLong(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count * 8 < size; i++)
				{
					java.lang.reflect.Array.set(variable, i, (long) variableGetLong(index, count));
					count++;
				}
			}
			return count;
		}

		private static int variableCopyIntoFloat(Object variable, int index, int size, int count)
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
				for(i = 0; i < length && count * 4 < size; i++)
				{
					count = variableCopyIntoFloat(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length; i++)
				{
					java.lang.reflect.Array.set(variable, i, variableGetFloat(index, count));
					count++;
				}
			}
			return count;
		}

		private static int variableCopyIntoDouble(Object variable, int index, int size, int count)
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
				for(i = 0; i < length; i++)
				{
					count = variableCopyIntoDouble(java.lang.reflect.Array.get(variable, i), index, size, count);
				}
			}
			else
			{
				for(i = 0; i < length && count * 8 < size; i++)
				{
					java.lang.reflect.Array.set(variable, i, variableGetDouble(index, count));
					count++;
				}
			}
			return count;
		}

		private static int variableCopyIntoString(Object variable, int index, int size, int count)
		{
			Object tmp;
			int length;
			int i;

			length = java.lang.reflect.Array.getLength(variable);
			for(i = 0; i < length; i++)
			{
				tmp = java.lang.reflect.Array.get(variable, i);
				if (tmp.getClass().isArray())
				{
					count = variableCopyIntoString(tmp, index, size, count);
				}
				else
				{
					// TODO: to be implemented
					//java.lang.reflect.Array.set(variable, i, (String) variableGetChar(index, count));
					//count++;
				}
			}
			return count;
		}
	
  public static int execute(String script) {
		Matcher matcher;
		int status;
		int type;
		int i;

		matcher = patternFromMemory.matcher(script);	// "INSERT" OPERATION
		while(matcher.find())
		{
			i = Integer.parseInt(matcher.group(1));
			if (i > -1 && i < 16)
			{
				if (variableList[i] != null && variableList[i].fromMemory == 0)
				{
					variableCreate(i, variableList[i].size);
					variableList[i].fromMemory = 1;
					if (matcher.group(2) != null)
					{
						variableList[i].fromSize = Integer.parseInt(matcher.group(2));
					}
					type = variableList[i].type;
					if (type == TINYINT)
					{
						variableCopyFromChar(variableList[i].variable, i, variableList[i].fromSize, 0);
					}
					else if (type == SMALLINT)
					{
						variableCopyFromShort(variableList[i].variable, i, variableList[i].fromSize, 0);
					}
					else if (type == INT)
					{
						variableCopyFromInt(variableList[i].variable, i, variableList[i].fromSize, 0);
					}
					else if (type == BIGINT)
					{
						variableCopyFromLong(variableList[i].variable, i, variableList[i].fromSize, 0);
					}
					else if (type == FLOAT)
					{
						variableCopyFromFloat(variableList[i].variable, i, variableList[i].fromSize, 0);
					}
					else if (type == DOUBLE)
					{
						variableCopyFromDouble(variableList[i].variable, i, variableList[i].fromSize, 0);
					}
					else
					{
						// TODO: to be implemented
						//variableCopyFromString(variableList[i].variable, i, variableList[i].fromSize, 0);
					}
				}
			}
		}

		matcher = patternIntoMemory.matcher(script);	// "SELECT" OPERATION
		while(matcher.find())
		{
			i = Integer.parseInt(matcher.group(1));
			if (i > -1 && i < 16)
			{
				if (variableList[i] != null && variableList[i].fromMemory == 0 && variableList[i].intoMemory == 0)
				{
					variableCreate(i, variableList[i].size);
					variableList[i].intoMemory = 1;
				}
				if (matcher.group(2) != null)
				{
					variableList[i].intoSize = Integer.parseInt(matcher.group(2));
				}
			}
		}

		status = HDFqlJNI.execute(script);

		for(i = 0; i < 16; i++)
		{
			if (variableList[i] != null && variableList[i].intoMemory == 1)
			{
				type = variableList[i].type;
				if (type == TINYINT)
				{
					variableCopyIntoChar(variableList[i].variable, i, variableList[i].intoSize, 0);
				}
				else if (type == SMALLINT)
				{
					variableCopyIntoShort(variableList[i].variable, i, variableList[i].intoSize, 0);
				}
				else if (type == INT)
				{
					variableCopyIntoInt(variableList[i].variable, i, variableList[i].intoSize, 0);
				}
				else if (type == BIGINT)
				{
					variableCopyIntoLong(variableList[i].variable, i, variableList[i].intoSize, 0);
				}
				else if (type == FLOAT)
				{
					variableCopyIntoFloat(variableList[i].variable, i, variableList[i].intoSize, 0);
				}
				else if (type == DOUBLE)
				{
					variableCopyIntoDouble(variableList[i].variable, i, variableList[i].intoSize, 0);
				}
				else
				{
					variableCopyIntoString(variableList[i].variable, i, variableList[i].intoSize, 0);
				}
			}
		}

		for(i = 0; i < 16; i++)
		{
			if (variableList[i] != null)
			{
				if (variableList[i].fromMemory == 1 || variableList[i].intoMemory == 1)
				{
					variableList[i].fromSize = Integer.MAX_VALUE;
					variableList[i].intoSize = Integer.MAX_VALUE;
					variableList[i].fromMemory = 0;
					variableList[i].intoMemory = 0;
					variableDestroy(i);
				}
			}
		}

		return status;
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

  private static void variableRegister(int number, int address) {
    HDFqlJNI.variableRegister(number, address);
  }

  private static void variableUnregister(int number) {
    HDFqlJNI.variableUnregister(number);
  }

  private static int variableGetDatatype(int number) {
    return HDFqlJNI.variableGetDatatype(number);
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

  public static int cursorGetDatatype(HDFqlCursor cursor) {
    return HDFqlJNI.cursorGetDatatype(HDFqlCursor.getCPtr(cursor), cursor);
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
		else
		{
			return new Byte((byte) convertChar(pointer));
		}
	}

  public static Byte subcursorGetTinyInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetTinyInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Byte((byte) convertChar(pointer));
		}
	}

  public static Byte cursorGetUnsignedTinyInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetUnsignedTinyInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Byte((byte) convertChar(pointer));
		}
	}

  public static Byte subcursorGetUnsignedTinyInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetUnsignedTinyInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Byte((byte) convertChar(pointer));
		}
	}

  public static Short cursorGetSmallInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetSmallInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Short(convertShort(pointer));
		}
	}

  public static Short subcursorGetSmallInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetSmallInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Short(convertShort(pointer));
		}
	}

  public static Short cursorGetUnsignedSmallInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetUnsignedSmallInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Short(convertShort(pointer));
		}
	}

  public static Short subcursorGetUnsignedSmallInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetUnsignedSmallInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Short(convertShort(pointer));
		}
	}

  public static Integer cursorGetInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Integer(convertInt(pointer));
		}
	}

  public static Integer subcursorGetInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Integer(convertInt(pointer));
		}
	}

  public static Integer cursorGetUnsignedInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetUnsignedInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Integer(convertInt(pointer));
		}
	}

  public static Integer subcursorGetUnsignedInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetUnsignedInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Integer(convertInt(pointer));
		}
	}

  public static Long cursorGetBigInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetBigInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Long(convertLong(pointer));
		}
	}

  public static Long subcursorGetBigInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetBigInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Long(convertLong(pointer));
		}
	}

  public static Long cursorGetUnsignedBigInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetUnsignedBigInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Long(convertLong(pointer));
		}
	}

  public static Long subcursorGetUnsignedBigInt(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetUnsignedBigInt(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Long(convertLong(pointer));
		}
	}

  public static Float cursorGetFloat(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetFloat(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Float(convertFloat(pointer));
		}
	}

  public static Float subcursorGetFloat(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetFloat(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Float(convertFloat(pointer));
		}
	}

  public static Double cursorGetDouble(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.cursorGetDouble(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Double(convertDouble(pointer));
		}
	}

  public static Double subcursorGetDouble(HDFqlCursor cursor) {
		long pointer = HDFqlJNI.subcursorGetDouble(HDFqlCursor.getCPtr(cursor), cursor);

		if (pointer == 0)
		{
			return null;
		}
		else
		{
			return new Double(convertDouble(pointer));
		}
	}

  public static String cursorGetChar(HDFqlCursor cursor) {
    return HDFqlJNI.cursorGetChar(HDFqlCursor.getCPtr(cursor), cursor);
  }

  public static String subcursorGetChar(HDFqlCursor cursor) {
    return HDFqlJNI.subcursorGetChar(HDFqlCursor.getCPtr(cursor), cursor);
  }

  private static void variableCreate(int number, int size) {
    HDFqlJNI.variableCreate(number, size);
  }

  private static void variableDestroy(int number) {
    HDFqlJNI.variableDestroy(number);
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

  private static char variableGetChar(int number, int position) {
    return HDFqlJNI.variableGetChar(number, position);
  }

  private static short variableGetShort(int number, int position) {
    return HDFqlJNI.variableGetShort(number, position);
  }

  private static int variableGetInt(int number, int position) {
    return HDFqlJNI.variableGetInt(number, position);
  }

  private static int variableGetLong(int number, int position) {
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

  private static char convertChar(long arg0) {
    return HDFqlJNI.convertChar(arg0);
  }

  private static short convertShort(long arg0) {
    return HDFqlJNI.convertShort(arg0);
  }

  private static int convertInt(long arg0) {
    return HDFqlJNI.convertInt(arg0);
  }

  private static long convertLong(long arg0) {
    return HDFqlJNI.convertLong(arg0);
  }

  private static float convertFloat(long arg0) {
    return HDFqlJNI.convertFloat(arg0);
  }

  private static double convertDouble(long arg0) {
    return HDFqlJNI.convertDouble(arg0);
  }

}
