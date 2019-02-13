package edu.berkeley.nwbqueryengine;

import edu.berkeley.nwbqueryengine.connectors.Connector;
import edu.berkeley.nwbqueryengine.connectors.ConnectorException;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Operators;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.data.NwbResult;
import edu.berkeley.nwbqueryengine.data.EntityWrapper;
import edu.berkeley.nwbqueryengine.data.Restrictions;
import edu.berkeley.nwbqueryengine.query.parser.QueryParser;
import edu.berkeley.nwbqueryengine.util.DateUtil;
import edu.berkeley.nwbqueryengine.util.ValuesUtil;
import org.apache.commons.jexl3.JexlEngine;
import org.apache.commons.jexl3.JexlExpression;
import org.apache.commons.jexl3.MapContext;
import org.apache.commons.jexl3.internal.Engine;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.*;
import java.util.regex.Pattern;

/**
 * Created by petr-jezek on 26.6.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class NwbProcessor implements Processor<NwbResult> {

    private Log logger = LogFactory.getLog(getClass());
    private JexlEngine jexl = new Engine();
    private MapContext mc = new MapContext();

    private Connector storageConnector;

    public NwbProcessor(Connector storageConnector) {
        this.storageConnector = storageConnector;
    }

    public List<NwbResult> evaluate(Query query) throws ProcessorException {
        List<NwbResult> nwbResults = new LinkedList<>();
        List<List<NwbResult>> queryResults = new LinkedList<>();
        String andOrOperator;
        List<EntityWrapper> entityWrappers;

        try {
            entityWrappers = storageConnector.processSearch(query);
        } catch (Exception e) {
            logger.error(e);
            throw new ProcessorException(e);
        }
        Query previousSubQuery = null;
        for (EntityWrapper partialExpression : entityWrappers) {
            List<NwbResult> partialResult = new LinkedList<>();
            Expression item = partialExpression.getExpression();

            andOrOperator = item.getParent().getOperator();
            Query currentSubQuery = partialExpression.getQuery();
            boolean isNextSubQuery = previousSubQuery != null && previousSubQuery != currentSubQuery;
            logger.debug("Next subquery: " + isNextSubQuery);

            //if operator is "&" and the previous result is an empty set I mustn't continue
            if (!(StringUtils.equals(andOrOperator, Operators.AND.op()) && nwbResults.size() == 0)) {
                List<String> entities = partialExpression.getEntity();
                for (String entity : entities) {
                    try {
                        storageConnector.getValues(entity);

                        String arithmeticalOperator = item.getOperator();
                        Expression rightSide = item.getRightSideSibling();
                        logger.debug("Operator: " + item.getOperator() + ", RightSide: " + rightSide);
                        Object value = null;
                        if (StringUtils.isBlank(arithmeticalOperator)) {
                            while ((value = storageConnector.next()) != null) {
                                logger.debug("Value: " + value);
                                partialResult.add(new NwbResult(entity, value, partialExpression.getStorage()));
                            }
                        } else {
                            String expressionValue = rightSide.getExpressionValue();
                            String jexlExpression;
                            boolean isLike;
                            if (arithmeticalOperator.equals(Operators.CONTAINS.op())) {
                                jexlExpression = "x1=~x2";
                                expressionValue = ".*" + Pattern.quote(expressionValue) + ".*"; //find all substrings - is it a good or bad solution?
                                isLike = true;
                            } else {
                                jexlExpression = "x1" + arithmeticalOperator + "x2";
                                isLike = false;
                            }
                            JexlExpression func = jexl.createExpression(jexlExpression);
                            Object x2 = ValuesUtil.getModifiedCopy(expressionValue);
                            while ((value = storageConnector.next()) != null) {
                                logger.debug("Value: " + value);
                                Object x1 = ValuesUtil.getModifiedCopy(value);
                                if (!isLike && !(x1 instanceof Number)) {
                                    x1 = ValuesUtil.getDateIfPossible(x1);
                                    x2 = ValuesUtil.getDateIfPossible(x2);
                                }
                                mc.set("x1", x1);
                                mc.set("x2", x2);
                                Object eval = func.evaluate(mc);
                                boolean res = ((Boolean) eval).booleanValue();
                                logger.debug("Evaluation: " + value + ", Operator: " + arithmeticalOperator + ", Expression value: " + expressionValue + ", data: " + res);
                                if (res) {
                                    partialResult.add(new NwbResult(entity, value, partialExpression.getStorage()));
                                }
                            }
                        }
                    } catch (ConnectorException e) {
                        throw new ProcessorException(e);
                    }
                }
            }


            logger.debug(item + ", AND-OR-Operator: " + andOrOperator);


            boolean isDifferentSubqueryExpression = previousSubQuery != null &&
                    !previousSubQuery.getQueryLeftSide().getExpressionValue()
                            .equals(currentSubQuery.getQueryLeftSide().getExpressionValue());
            logger.debug("isNextQuery: " + isDifferentSubqueryExpression);

            if (isNextSubQuery) {
                queryResults.add(nwbResults);
                nwbResults = new LinkedList<>();
            }

            if (StringUtils.equals("\\" + andOrOperator, Operators.OR.op())) {
                logger.debug("...OR....");
                nwbResults = Restrictions.or(nwbResults, partialResult);
                nwbResults.forEach(name -> logger.debug(name));
            } else if (StringUtils.equals(andOrOperator, Operators.AND.op())) {
                logger.debug("...AND....");
                nwbResults = Restrictions.and(nwbResults, partialResult, isDifferentSubqueryExpression);
                nwbResults.forEach(name -> logger.debug(name));
            } else {
                nwbResults.addAll(partialResult);
            }


            previousSubQuery = currentSubQuery;
        }
        queryResults.add(nwbResults);
        List<NwbResult> previousResult = queryResults.remove(0);
        List<NwbResult> completeResult = new LinkedList<>();
        String operator = "";
        boolean subQueryFirstRun = true;
        String previousExpressionValue = "";
        for (Query q : query.getSubQueries()) {
            String expressionValue = q.getQueryLeftSide().getExpressionValue();
            boolean isDifferent = !expressionValue.equals(previousExpressionValue);
            String tmpOperator = q.getRoot().getParent().getOperator();
            if (tmpOperator.matches(QueryParser.AND_OR)) {
                operator = tmpOperator;
            }
            if (!operator.equals(QueryParser.AND_OR) && subQueryFirstRun) {
                completeResult.addAll(previousResult);
                subQueryFirstRun = false;
            } else {
                previousResult = queryResults.remove(0);
                if (StringUtils.equals("\\" + operator, Operators.OR.op())) {
                    logger.debug("...OR....");
                    completeResult = Restrictions.or(completeResult, previousResult);
                    completeResult.forEach(name -> logger.debug(name));
                } else if (StringUtils.equals(operator, Operators.AND.op())) {
                    logger.debug("...AND....");
                    completeResult = Restrictions.and(completeResult, previousResult, isDifferent);
                    completeResult.forEach(name -> logger.debug(name));
                }
            }
            previousExpressionValue = q.getQueryLeftSide().getExpressionValue();

        }

        completeResult.forEach(name -> logger.debug(name));

        return completeResult;
    }

}
