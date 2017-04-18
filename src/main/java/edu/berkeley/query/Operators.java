package edu.berkeley.query;

/**
 * Created by petr-jezek on 18.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public enum Operators {

    EQ("="), GT(">"), LT("<"), GE(">="), LE("<="), AND("&"), OR("\\|"), NE("!=");

    private String value;

    private Operators(String value) {
        this.value = value;
    }

    public String op() {
        return value;
    }
}
