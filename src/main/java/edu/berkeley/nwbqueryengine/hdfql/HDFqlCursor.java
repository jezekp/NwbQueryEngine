package edu.berkeley.nwbqueryengine.hdfql;

public class HDFqlCursor {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected HDFqlCursor(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(HDFqlCursor obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        HDFqlJNI.delete_HDFqlCursor(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public HDFqlCursor() {
		this(HDFqlJNI.new_HDFqlCursor(), true);
		HDFqlJNI.cursorInitialize(swigCPtr, this);
	}

}
