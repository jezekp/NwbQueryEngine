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
public class QueryParser implements Parser{


    private Log logger = LogFactory.getLog(getClass());

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
        String input = node.getExpressionValue();
        //Find expression inside brackets [] or ()
        Matcher brackets = Pattern.compile("\\(([^)]+)\\)").matcher(input);
        while (brackets.find()) {
            String value = brackets.group(1);
            value = value.replaceAll("\\(", "").replaceAll("\\)", "").replaceAll("\\.", "");
            node.setRightSide(parseInternal(new Expression(value, node)));
            node.setLeftSide(parseInternal(new Expression(input.split(Operators.ASSIGN.op())[0], node)));
        }

        //(?=foo) lookahead and (?<=foo) lookbehind are used to a delimiter was included as well.
        String delimiter = Operators.AND.op() + "|" + Operators.OR.op();
        parseSubString(input, node, "((?<=" + delimiter + ")|(?=" + delimiter + "))");
        String[] delimiters = {
                Operators.GE.op() + "|" + Operators.LT.op(),
                Operators.GT.op() +  "|" + Operators.LE.op() + "|" + Operators.NE.op() + "|" + Operators.EQ.op(),
                Operators.MATCH.op() + "|" + Operators.CONTAINS.op()
        };
        for(String item : delimiters) {
            parseSubString(input, node, "((?<=" + item + ")|(?=" + item + "))");
        }
        return node;
    }

    private void parseSubString(String input, Expression node, String delimiter) {
        //st contains [0] - left side, [1] - operator, [2] - right side
        String[] st = input.split(delimiter, 3);
        logger.debug("Input: " + input + ", delimiter: " + delimiter + ", left: "  + ((st.length > 0) ? st[0] : "") + ", operator: " + ((st.length > 1) ? st[1] : "") + ", right: " + ((st.length > 2) ? st[2] : ""));
        if (!st[0].equals(node.getExpressionValue()) && node.getLeftSide() == null) {
            node.setLeftSide(parseInternal(new Expression(st[0], st[1], node)));
        }
        if (st.length > 2 && node.getRightSide() == null) {
            node.setRightSide(parseInternal(new Expression(st[2], st[1], node)));
        }
    }
}
