package edu.berkeley.nwbqueryengine.query;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.LinkedList;
import java.util.List;

/**
 * Created by petr-jezek on 18.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class Query {

    private Expression root;
    private Log logger = LogFactory.getLog(getClass());
    private List<Expression> expressionsLeftSide;
    private Expression leftSide;

    public Query(Expression root) {
        this.root = root;
    }

    public Expression getQueryLeftSide() {
        if(leftSide == null) {
            expressionsLeftSide = leftSideOfExpressions();
        }
        return leftSide;

    }

    // recursive function to print left view
    private void leftListsViewInternal(List<Expression> res, Expression node, int level, int max_level, boolean isLeft) {
        // Base Case
        if (node == null) return;

        // If this is the first node of its level
        if (max_level < level && isLeft && node.getLeftSide() == null && node.getRightSide() == null) {
            logger.debug("[" + node.getExpressionValue() + ", " + node.getOperator() + ", left: (" + node.getLeftSide() + "), right: (" + node.getRightSide() + ")], ");
            res.add(node);
            max_level = level;
        }

        // Recur for left and right subtrees
        leftListsViewInternal(res, node.getLeftSide(), level + 1, max_level, true);
        leftListsViewInternal(res, node.getRightSide(), level + 1, max_level, false);
    }

    // A wrapper over leftListsViewInternal()
    public List<Expression> leftSideOfExpressions() {
        if(expressionsLeftSide == null) {
            expressionsLeftSide = new LinkedList<>();
            leftListsViewInternal(expressionsLeftSide, root, 1, 0, true);
            leftSide = expressionsLeftSide.get(0);
            expressionsLeftSide.remove(0);
        }
        return expressionsLeftSide;
    }

    public Expression getRightSide(Expression expression) {
        return getReverseSide(expression, false);
    }

    public Expression getReverseSide(Expression expression, boolean left) {
        Expression res = null;
        Expression parent = expression.getParent();
        if (parent != null) {
            res = left ? parent.getLeftSide() : parent.getRightSide();
        }
        return res;
    }

    public Expression getQueryLeftSide(Expression expression) {
        return getReverseSide(expression, true);
    }


}
