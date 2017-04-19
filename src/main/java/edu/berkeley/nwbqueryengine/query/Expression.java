package edu.berkeley.nwbqueryengine.query;

/**
 * Created by petr-jezek on 18.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class Expression {

    private Expression leftSide;
    private Expression rightSide;
    private String operator;
    private String expressionValue;

    public Expression(String val) {
        this.expressionValue = val;
    }
    public Expression(String val, String operator) {
        this(val);
        this.operator = operator;
    }

    public Expression getLeftSide() {
        return leftSide;
    }

    public void setLeftSide(Expression leftSide) {
        this.leftSide = leftSide;
    }

    public Expression getRightSide() {
        return rightSide;
    }

    public void setRightSide(Expression rightSide) {
        this.rightSide = rightSide;
    }

    public String getOperator() {
        return operator;
    }

    public void setOperator(String operator) {
        this.operator = operator;
    }

    public String getExpressionValue() {
        return expressionValue;
    }

    public void setExpressionValue(String expressionValue) {
        this.expressionValue = expressionValue;
    }

    @Override
    public String toString() {
        return expressionValue;
    }
}
