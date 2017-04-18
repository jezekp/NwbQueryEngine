package edu.berkeley.query.parser;

import edu.berkeley.query.Expression;
import edu.berkeley.query.Operators;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by petr-jezek on 18.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class QueryParser {

    public Expression parse(Expression e) {
        Expression node = new Expression(e.getExpressionValue(), e.getOperator());
        String input = node.getExpressionValue();
        Matcher brackets = Pattern.compile("\\(([^)]+)\\)").matcher(input);
        while (brackets.find()) {
            String value = brackets.group(1);
            node.setRightSide(parse(new Expression(value)));
            node.setLeftSide(parse(new Expression(input.split(Operators.EQ.op())[0])));
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
            node.setLeftSide(parse(new Expression(st[0], st[1])));
        }
        if(st.length > 2  && node.getRightSide() == null) {
            node.setRightSide(parse(new Expression(st[2], st[1])));
        }
    }
}
