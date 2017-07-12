package edu.berkeley.nwbqueryengine.data;

/**
 * Created by petr-jezek on 24.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class NwbResult {

    private String dataSet;
    private Object value;

    public NwbResult(String dataSet, Object value) {
        this.dataSet = dataSet;
        this.value = value;
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
        if(obj == this) {
            return true;
        }

        NwbResult eq = (NwbResult) obj;
        return eq.value.equals(value);
    }

    @Override
    public int hashCode() {
        int result = 17;
        result = 31 * result + value.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "Dataset: " + dataSet + ", Value: " + value;
    }
}
