package edu.berkeley.nwbqueryengine;

import edu.berkeley.nwbqueryengine.connectors.Connector;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Operators;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.data.NwbResult;
import edu.berkeley.nwbqueryengine.data.EntityWrapper;
import edu.berkeley.nwbqueryengine.data.Restrictions;
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
        Map<String, List<Object>> dataSets = new HashMap<>();
        String andOrOperator;
        boolean isNext = false;
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

            //get an operator between two subqueries
            if (andOrOperator.equals("") && isNext) {
                andOrOperator = item.getParent().getParent().getParent().getParent().getParent().getOperator();
            }

            isNext = true;

            //if operator is "&" and the previous result is an empty set I mustn't continue
            if (!(StringUtils.equals(andOrOperator, Operators.AND.op()) && nwbResults.size() == 0)) {
                List<String> entities = partialExpression.getEntity();
                for (String entity : entities) {
                    List<Object> values = getValues(entity, dataSets);
                    String arithmeticalOperator = item.getOperator();
                    Expression rightSide = item.getRightSideSibling();
                    logger.debug("Operator: " + item.getOperator() + ", RightSide: " + rightSide);
                    if (StringUtils.isBlank(arithmeticalOperator)) {
                        for (Object value : new LinkedList<>(values)) {
                            logger.debug("Value: " + value);
                            partialResult.add(new NwbResult(entity, value, partialExpression.getStorage()));
                        }
                    } else {
                        String expressionValue =  rightSide.getExpressionValue();
                        String jexlExpression;
                        if (arithmeticalOperator.equals(Operators.CONTAINS.op())) {
                            jexlExpression = "x1=~x2";
                            expressionValue = ".*" + Pattern.quote(expressionValue) + ".*"; //find all substrings - is it a good or bad solution?
                        } else {
                            jexlExpression = "x1" + arithmeticalOperator + "x2";
                        }
                        JexlExpression func = jexl.createExpression(jexlExpression);
                        mc.set("x2", ValuesUtil.getModifiedCopy(expressionValue));
                        for (Object value : new LinkedList<>(values)) {
                            logger.debug("Value: " + value);
                            mc.set("x1", ValuesUtil.getModifiedCopy(value));
                            Object eval = func.evaluate(mc);
                            boolean res = ((Boolean) eval).booleanValue();
                            logger.debug("Evaluation: " + value + ", Operator: " + arithmeticalOperator + ", Expression value: " + expressionValue + ", data: " + res);
                            if (res) {
                                partialResult.add(new NwbResult(entity, value, partialExpression.getStorage()));
                            }
                        }
                    }

                }
            }


            logger.debug(item + ", AND-OR-Operator: " + andOrOperator);


            Query currentSubQuery = partialExpression.getQuery();
            boolean isNextSubquery = previousSubQuery == null ||
                    previousSubQuery.getQueryLeftSide().getExpressionValue()
                            .equals(currentSubQuery.getQueryLeftSide().getExpressionValue()) ? false : true;
            logger.debug("isNextQuery: " + isNextSubquery);

            if (StringUtils.equals("\\" + andOrOperator, Operators.OR.op())) {
                logger.debug("...OR....");
                nwbResults = Restrictions.or(nwbResults, partialResult);
                nwbResults.forEach(name -> logger.debug(name));
            } else if (StringUtils.equals(andOrOperator, Operators.AND.op())) {
                logger.debug("...AND....");
                nwbResults = Restrictions.and(nwbResults, partialResult, isNextSubquery);
                nwbResults.forEach(name -> logger.debug(name));
            } else {
                nwbResults.addAll(partialResult);
            }

            previousSubQuery = currentSubQuery;
        }
        nwbResults.forEach(name -> logger.debug(name));

        return nwbResults;
    }


    private List<Object> getValues(String entity, Map<String, List<Object>> dataSets) throws ProcessorException {
        List<Object> values;
        if (dataSets.containsKey(entity)) {
            values = dataSets.get(entity);
        } else {
            try {
                values = storageConnector.getValues(entity);
                dataSets.put(entity, values);
            } catch (Exception e) {
                logger.error(e);
                e.printStackTrace();
                throw new ProcessorException(e);
            }
        }
        return values;
    }
}
