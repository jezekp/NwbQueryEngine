package edu.berkeley.nwbqueryengine.data;

/**
 * Created by petr-jezek on 24.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class NwbResult implements Comparable<NwbResult> {

    private String dataSet;
    private Object value;
    private Object dataStorageName;

    public NwbResult(String dataSet, Object value, Object dataStorageName) {
        this.dataSet = dataSet;
        this.value = value;
        this.setDataStorageName(dataStorageName);
    }

    public String getDataSet() {
        return dataSet;
    }

    public void setDataSet(String dataSet) {
        this.dataSet = dataSet;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        NwbResult eq = (NwbResult) obj;
        return eq.value.equals(value)
                && eq.dataSet.equals(dataSet);
    }

    @Override
    public int hashCode() {
        int result = 17;
        result = 31 * result + value.hashCode();
        result = 31 * result + dataSet.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "Dataset: " + dataSet + ", Value: " + value + ", DataStorageName: " + getDataStorageName();
    }

    @Override
    public int compareTo(NwbResult o) {

        Object valueToCompare = o.getValue();

        if (valueToCompare instanceof String) {
            String compare = (String) valueToCompare;
            String thisValue = (String) value;
            return thisValue.compareTo(compare);
        }
        if (valueToCompare instanceof Number) {
            double compare = ((Number) valueToCompare).doubleValue();
            double thisValue = ((Number) value).doubleValue();;
            if (compare > thisValue) {
                return -1;
            }
            if (compare < thisValue) {
                return 1;
            }
            return 0;
        }
        return 0;
    }

    public Object getDataStorageName() {
        return dataStorageName;
    }

    public void setDataStorageName(Object dataStorageName) {
        this.dataStorageName = dataStorageName;
    }
}
