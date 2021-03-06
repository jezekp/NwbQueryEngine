package edu.berkeley.nwbqueryengine.query.parser;

import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Operators;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.util.BTreePrinter;
import edu.berkeley.nwbqueryengine.util.BracketsUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Created by petr-jezek on 18.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class QueryParser implements Parser {


    private Log logger = LogFactory.getLog(getClass());

    public static String ASSIGN = Operators.ASSIGN.op();
    public static String AND_OR = Operators.AND.op() + "|" + Operators.OR.op();
    public static String OTHERS = Operators.GE.op() + "|" + Operators.LT.op() + "|" +
            Operators.GT.op() + "|" + Operators.LE.op() + "|" + Operators.NE.op() + "|" + Operators.EQ.op() + "|" +
            Operators.MATCH.op() + "|" + Operators.CONTAINS.op();
    public static String BRACKETS_AND_QUOTES = "\"|'|(|)";

    //(?=foo) lookahead and (?<=foo) lookbehind are used to a delimiter be included as well.
    public static String AND_OR_DELIMITER = "((?<=" + AND_OR + ")|(?=" + AND_OR + "))";
    public static String OTHERS_DELIMITER = "((?<=" + OTHERS + ")|(?=" + OTHERS + "))";
    public static String ASSIGN_DELIMITER = "((?<=" + ASSIGN + ")|(?=" + ASSIGN + "))";



    public Query parse(String expression) {
        Expression root = parseInternal(new Expression(expression));
        Query q = new Query(root);
        if (logger.isDebugEnabled()) {
            BTreePrinter bTreePrinter = new BTreePrinter();
            bTreePrinter.printNode(root);
        }
        return q;
    }

    private Expression parseInternal(Expression e) {
        Expression node = new Expression(e.getExpressionValue(), e.getOperator(), e.getParent());
        String input = node.getExpressionValue().trim();
        int subValueStartingIndex = 0;
        Expression res = node;
        String previousOperator = "";
        String previousLeftSide = "";
        BracketsUtil bracketsUtil = new BracketsUtil(input);
        while (bracketsUtil.next()) {
            String subValue = input.substring(subValueStartingIndex, bracketsUtil.currentEnd());
            //left side of each subtreee is a group_name, right side is an expression like expression | expression or expression & expression
            //parse it recursively, goes over all expressions like epochs=(a|b|c)
            subValueStartingIndex += subValue.length();
            String operator = (subValueStartingIndex < input.length()) ? "" + (input.substring(subValueStartingIndex).trim().charAt(0)) : "";
            String valueWithoutOperator = StringUtils.strip(subValue.trim(), AND_OR);
            node.setOperator(operator);
            //for queries like: /general/subject=(species LIKE Mus musculu) & (age LIKE 25 w)")
            //modify it: /general/subject=(species LIKE Mus musculu) & /general/subject=(age LIKE 25 w)")
            if(!valueWithoutOperator.contains(ASSIGN)) {
                valueWithoutOperator = previousLeftSide + ASSIGN + valueWithoutOperator;
            }
            node.setLeftSide(parseSubString(new Expression(valueWithoutOperator, node), ASSIGN_DELIMITER, previousOperator));
            Expression newNode = new Expression("", previousOperator, node);
            node.setRightSide(newNode);
            node = newNode;
            previousOperator = operator;
            previousLeftSide = res.getLeftSide().getLeftSide().getExpressionValue();
        }

        return res;
    }

    private Expression parseSubString(Expression node, String delimiter, String previousOperator) {
        //st contains [0] - left side, [1] - operator, [2] - right side
        String input = node.getExpressionValue();
        String[] st = StringUtils.stripAll(input.split(delimiter, 3));
        st = StringUtils.stripAll(st, BRACKETS_AND_QUOTES);

        logger.debug("Input: " + input + ", delimiter: " + delimiter + ", left: " + ((st.length > 0) ? st[0] : "") + ", operator: " + ((st.length > 1) ? st[1] : "") + ", right: " + ((st.length > 2) ? st[2] : ""));
        boolean isOthers = delimiter.equals(OTHERS_DELIMITER);
        boolean isAssign = delimiter.equals(ASSIGN_DELIMITER);
        // if st > 1 and the operator is OTHERS (<, >, <=, >= etc. ) just assign the left side of the expression and the right side
        // if the operator is AND or OR or ASSIGN (=) parse right side recursively
        //   - left side is a subexpression like a>b, a<c, a>=b, a<=b etc
        //   - right side is a subexpression like subexpression | subexpression & subexpression etc.
        logger.debug("Parse substring: " + st.length + ", " + isOthers);

        if (st.length > 1) {
            if (isOthers) {
                node.setLeftSide(new Expression(st[0], st[1], node));
                node.setRightSide(new Expression(st[2], st[1], node));
            } else if (isAssign) {
                node.setLeftSide(new Expression(st[0], st[1], node));
                String rightSide = StringUtils.strip(st[2], BRACKETS_AND_QUOTES);
                node.setRightSide(parseSubString(new Expression(rightSide, "", node), AND_OR_DELIMITER, st[1]));
            } else {
                node.setRightSide(parseSubString(new Expression(st[2], st[1], node), AND_OR_DELIMITER, st[1]));
            }
            //If no operator is found it is either execution of AND, OR operator and a subexpression containing
            //  <, >, <=, >=  must be found or an expression without filter like: echoch=(startime) is given and
            // no additional parsing is needed
        } else {
            if (isOthers) {
                String tmpOperator = node.getRightSide() == null ? "" : node.getRightSide().getOperator();
                node.setLeftSide(new Expression(st[0], tmpOperator, node));
            }
        }
        if (!isOthers && !isAssign) {
            String tmpOperator = previousOperator.equals(ASSIGN) ? "" : previousOperator;
            node.setLeftSide(parseSubString(new Expression(st[0], tmpOperator, node), OTHERS_DELIMITER, previousOperator));
        }
        return node;

    }
}
