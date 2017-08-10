package edu.berkeley.nwbqueryengine.query;

/**

 * Created by petr-jezek on 18.4.17*
 * <p>
 * jezekp@kiv.zcu.cz

   Expression is a node of a tree parsed by a query parser.

   Query such as: epochs=(start_time>200 & stop_time<400 | stop_time>1600)
   is parsed to a following structure:
 

               epochs=(start_time>200 & stop_time<400 | stop_time>1600) ()                               
              / \               
             /   \              
            /     \             
           /       \            
          /         \           
         /           \          
        /             \         
       /               \
   epochs ()  start_time>200 & stop_time<400 | stop_time>1600 ()
                      / \       
                     /   \      
                    /     \     
                   /       \    
         start_time>200  (&) stop_time<400 | stop_time>1600 (&)
                  / \     / \   
                 /   \   /   \  
   start_time (>)   200  (>) stop_time<400  (|)    stop_time>1600 (|)
                        / \ / \ 
          stop_time (<) 400  (<) stop_time (>) 1600 (>)
 */
public class Expression implements Cloneable{

    private Expression parent;
    private Expression leftSide;
    private Expression rightSide;
    private String operator = "";
    private String expressionValue;

    public Expression(String val) {
        this.expressionValue = val;
    }

    public Expression(String val, Expression parent) {
        this(val);
        this.parent = parent;
    }
    public Expression(String val, String operator, Expression parent) {
        this(val, parent);
        this.operator = operator;
    }
    private Expression(String val, String operator, Expression parent, Expression leftSide, Expression rightSide) {
        this(val, operator, parent);
        this.leftSide = leftSide;
        this.rightSide = rightSide;
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

    public Expression getParent() {
        return parent;
    }

    public void setParent(Expression parent) {
        this.parent = parent;
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        Expression left = null;
        Expression right = null;
        Expression parent = null;
        if (this.leftSide != null) {
            left = this.leftSide;
        }
        if (this.rightSide != null) {
            right = this.rightSide;
        }
        if(this.parent != null) {
            parent = this.parent;
        }
        return new Expression(expressionValue, operator, parent, left, right);
    }

    public Expression getRightSideSibling() {
        return getReverseSide(false);
    }

    public Expression getReverseSide(boolean left) {
        Expression res = null;

        if (parent != null) {
            res = left ? parent.getLeftSide() : parent.getRightSide();
        }
        return res;
    }

    public Expression getLeftSideSibling(Expression expression) {
        return getReverseSide(true);
    }
}
