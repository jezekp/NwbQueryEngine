package edu.berkeley.nwbqueryengine.query.parser;

import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Operators;
import edu.berkeley.nwbqueryengine.query.Query;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by petr-jezek on 18.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class ExpressionParser {


    private Log logger = LogFactory.getLog(getClass());

    private Query q;

    private void inorder(Expression node) {
        if (node != null) {
            inorder(node.getLeftSide());
            inorder(node.getRightSide());
            logger.debug("[" + node.getExpressionValue() + ", " + node.getOperator() + ", left: (" + node.getLeftSide() + "), right: (" + node.getRightSide() + ")], ");
            q.add(node);
        }
    }

    public Query parse(String expression) {
        q = new Query();
        Expression root = parseInternal(new Expression(expression));
        inorder(root);
        return q;

    }

    private Expression parseInternal(Expression e) {
        Expression node = new Expression(e.getExpressionValue(), e.getOperator());
        String input = node.getExpressionValue();
        Matcher brackets = Pattern.compile("\\(([^)]+)\\)").matcher(input);
        while (brackets.find()) {
            String value = brackets.group(1);
            node.setRightSide(parseInternal(new Expression(value)));
            node.setLeftSide(parseInternal(new Expression(input.split(Operators.EQ.op())[0])));
        }

        String delimiter = Operators.AND.op() + "|" + Operators.OR.op();
        parseSubString(input, node, "((?<=" + delimiter + ")|(?=" + delimiter + "))");
    //    parseSubString(input, node, Operators.EQ.op() + "|" +
    //            Operators.GT.op() + "|" + Operators.GE.op() + "|" + Operators.LT.op() + "|" + Operators.LE.op() + "|" + Operators.NE.op());

        return node;
    }

    private void parseSubString(String input, Expression node, String delimiter) {
        String[] st = input.split(delimiter, 3);
        if(!st[0].equals(node.getExpressionValue()) && node.getLeftSide() == null) {
            node.setLeftSide(parseInternal(new Expression(st[0], st[1])));
        }
        if(st.length > 2  && node.getRightSide() == null) {
            node.setRightSide(parseInternal(new Expression(st[2], st[1])));
        }
    }
}
