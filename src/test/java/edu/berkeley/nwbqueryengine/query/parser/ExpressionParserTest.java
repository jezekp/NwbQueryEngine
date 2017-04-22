package edu.berkeley.nwbqueryengine.query.parser;

import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Query;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Created by petr-jezek on 19.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
class ExpressionParserTest {
    @Test
    void parse() {
        ExpressionParser p = new ExpressionParser();
        Query root = p.parse("CellInfo=('area'='c1'|'area'='c2'&'h'='c3'|h3=c8)");

        Expression leftSide = root.getLeftSide();
        assertNotNull(leftSide);
        String expressionVal = leftSide.getExpressionValue();
        assertNotNull(expressionVal);
        assertEquals("CellInfo", expressionVal);
        List<Expression> rightSide = root.leftSideOfExpressions();
        String[] expressions = {"'area'","'c1'", "'area'","'c2'", "'h'","'c3'", "h3","c8"};
        assertEquals(expressions.length, rightSide.size());
        int i = 0;
        for(Expression item : rightSide) {
            assertEquals(item.getExpressionValue(), expressions[i++]);
        }
    }

}