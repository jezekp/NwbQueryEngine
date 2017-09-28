package edu.berkeley.nwbqueryengine.query.parser;

import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Operators;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.util.BTreePrinter;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

    public Query parse(String expression) {
        Expression root = parseInternal(new Expression(expression), ASSIGN, "");
        Query q = new Query(root);
        if (logger.isDebugEnabled()) {
            BTreePrinter bTreePrinter = new BTreePrinter();
            bTreePrinter.printNode(root);
        }
        return q;
    }

    private Expression parseInternal(Expression e, String operator, String previousOperator) {
        Expression node = new Expression(e.getExpressionValue(), e.getOperator(), e.getParent());
        String input = node.getExpressionValue();
        //Expression is group_name=(expression)
        //Find expression inside brackets [] or ()
        Matcher brackets = Pattern.compile("\\(([^)]+)\\)").matcher(input);
        while (brackets.find()) {
            String value = brackets.group(1);
            value = value.replaceAll("\\(", "").replaceAll("\\)", "").replaceAll("\\.", "");
            //left side is group_name, right side is an expression like expression | expression or expression & expression
            //parse it recursively
            node.setLeftSide(new Expression(input.split(Operators.ASSIGN.op())[0], node));
            node.setRightSide(parseInternal(new Expression(value, node), AND_OR, previousOperator));
        }
        //operator assign has been solved already
        //now parse all the other operators
        if (!operator.equals(ASSIGN)) {
            //(?=foo) lookahead and (?<=foo) lookbehind are used to a delimiter be included as well.
            parseSubString(input, node, "((?<=" + operator + ")|(?=" + operator + "))", previousOperator);
        }
        return node;
    }
    private void parseSubString(String input, Expression node, String delimiter, String previousOperator) {
        //st contains [0] - left side, [1] - operator, [2] - right side
        String[] st = input.split(delimiter, 3);
        logger.debug("Input: " + input + ", delimiter: " + delimiter + ", left: " + ((st.length > 0) ? st[0] : "") + ", operator: " + ((st.length > 1) ? st[1] : "") + ", right: " + ((st.length > 2) ? st[2] : ""));
        boolean isOthers = delimiter.contains(OTHERS);
        // if st > 1 and the operator is OTHERS (<, >, <=, >= etc. ) just assign the left side of the expression and the right side
        // if the operator is AND or OR parse it recursively
        //   - left side is a subexpression like a>b, a<c, a>=b, a<=b etc
        //   - right side is a subexpression like subexpression | subexpression & subexpression etc.
        logger.debug("Parse substring: " + st.length + ", " + isOthers);
        if (st.length > 1) {
            if (isOthers) {
                node.setLeftSide(new Expression(st[0], st[1], node));
                node.setRightSide(new Expression(st[2], st[1], node));
            } else {
                node.setLeftSide(parseInternal(new Expression(st[0], previousOperator, node), OTHERS, previousOperator));
                node.setRightSide(parseInternal(new Expression(st[2], st[1], node), AND_OR, st[1]));
            }
            //If no operator is found it is either execution of AND, OR operator and a subexpression containing
            //  <, >, <=, >=  must be found or an expression without filter like: echoch=(startime) is given and
            // no additional parsing is needed
        } else {
            if (!isOthers) {
                node.setLeftSide(parseInternal(new Expression(st[0], previousOperator, node), OTHERS, previousOperator));
            } else {
                node.setLeftSide(new Expression(st[0], "", node));
            }
        }

    }
}
