package edu.berkeley.nwbqueryengine.query;

import java.util.LinkedList;
import java.util.List;

/**
 * Created by petr-jezek on 18.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class Query {

    private List<Expression> expressionsList = new LinkedList<Expression>();

    public void add(Expression expression) {
        expressionsList.add(expression);
    }

    public List<Expression> getAllExpressions() {
        return expressionsList;
    }

    public Expression getLeftSide() {
        Expression res = null;
        if(expressionsList.size() > 0) {
            res = expressionsList.get(0);
        }
        return res;
    }

    public List<Expression> getRightSideExpressions() {
        List<Expression> res = new LinkedList<Expression>();
        if(expressionsList.size() > 1) {
            List<Expression> copy = new LinkedList<Expression>(expressionsList);
            copy.remove(0);
            for(Expression item : copy) {
                if(item.getLeftSide() == null && item.getRightSide() == null) {
                    res.add(item);
                }
            }
        }
        return res;
    }

}
