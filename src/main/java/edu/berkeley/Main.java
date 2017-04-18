package edu.berkeley;

import edu.berkeley.query.Expression;
import edu.berkeley.query.parser.QueryParser;
import edu.berkeley.util.BTreePrinter;


/**
 * Created by petr-jezek on 6.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class Main {



    public static void main(String[] args) {
        QueryParser p = new QueryParser();
        Expression root = p.parse(new Expression("CellInfo=('area'='c1'|'area'='c2'&'h'='c3'|h3=c8)"));
        BTreePrinter bt = new BTreePrinter();
        bt.printNode(root);
        bt.inorder(root);

    }
}
