package edu.berkeley.nwbqueryengine.util;

import edu.berkeley.nwbqueryengine.query.Expression;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by petr-jezek on 18.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class BTreePrinter {

    private Log logger = LogFactory.getLog(getClass());
    private String tree;

        public void printNode(Expression root) {
            tree = "\n";
            int maxLevel = BTreePrinter.maxLevel(root);

            printNodeInternal(Collections.singletonList(root), 1, maxLevel);
            logger.debug(tree);
        }

        private void printNodeInternal(List<Expression> nodes, int level, int maxLevel) {
            if (nodes.isEmpty() || BTreePrinter.isAllElementsNull(nodes))
                return;

            int floor = maxLevel - level;
            int endgeLines = (int) Math.pow(2, (Math.max(floor - 1, 0)));
            int firstSpaces = (int) Math.pow(2, (floor)) - 1;
            int betweenSpaces = (int) Math.pow(2, (floor + 1)) - 1;

            printWhitespaces(firstSpaces);

            List<Expression> newNodes = new ArrayList<Expression>();
            for (Expression node : nodes) {
                if (node != null) {
                    tree += (node.getExpressionValue() + " (" + node.getOperator() + ")");
                    newNodes.add(node.getLeftSide());
                    newNodes.add(node.getRightSide());
                } else {
                    newNodes.add(null);
                    newNodes.add(null);
                    //System.out.print("");
                }

                printWhitespaces(betweenSpaces);
            }
            tree += "\n";

            for (int i = 1; i <= endgeLines; i++) {
                for (int j = 0; j < nodes.size(); j++) {
                    printWhitespaces(firstSpaces - i);
                    if (nodes.get(j) == null) {
                        printWhitespaces(endgeLines + endgeLines + i + 1);
                        continue;
                    }

                    if (nodes.get(j).getLeftSide() != null)
                        tree += ("/");
                    else
                        printWhitespaces(1);

                    printWhitespaces(i + i - 1);

                    if (nodes.get(j).getRightSide() != null)
                        tree += ("\\");
                    else
                        printWhitespaces(1);

                    printWhitespaces(endgeLines + endgeLines - i);
                }

                tree += "\n";
            }

            printNodeInternal(newNodes, level + 1, maxLevel);
        }

        private void printWhitespaces(int count) {
            for (int i = 0; i < count; i++)
                tree += (" ");
        }

        private static int maxLevel(Expression node) {
            if (node == null)
                return 0;

            return Math.max(BTreePrinter.maxLevel(node.getLeftSide()), BTreePrinter.maxLevel(node.getRightSide())) + 1;
        }

        private static <T> boolean isAllElementsNull(List<T> list) {
            for (Object object : list) {
                if (object != null)
                    return false;
            }

            return true;
        }
    }

