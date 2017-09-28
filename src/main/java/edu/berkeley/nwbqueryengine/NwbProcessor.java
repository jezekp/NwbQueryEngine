package edu.berkeley.nwbqueryengine;

import edu.berkeley.nwbqueryengine.connectors.Connector;
import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Operators;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.data.NwbResult;
import edu.berkeley.nwbqueryengine.data.EntityWrapper;
import edu.berkeley.nwbqueryengine.data.Restrictions;
import org.apache.commons.jexl3.JexlEngine;
import org.apache.commons.jexl3.JexlExpression;
import org.apache.commons.jexl3.MapContext;
import org.apache.commons.jexl3.internal.Engine;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.*;

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
        List<EntityWrapper> entityWrappers;
        try {
            entityWrappers = storageConnector.processSearch(query);
        } catch (Exception e) {
            logger.error(e);
            throw new ProcessorException(e);
        }
        for (EntityWrapper partialExpression : entityWrappers) {
            List<NwbResult> partialResult = new LinkedList<>();
            Expression item = partialExpression.getExpression();

            andOrOperator = item.getParent().getOperator();

            //if operator is and and previous result is an empty set I mustn't continue
            if(!(StringUtils.equals(andOrOperator, Operators.AND.op()) && nwbResults.size() == 0)) {
                List<String> entities = partialExpression.getEntity();
                for (String entity : entities) {
                    List<Object> values = getValues(entity, dataSets);
                    String arithmeticalOperator = item.getOperator();
                    Expression rightSide = item.getRightSideSibling();
                    logger.debug("Operator: " + item.getOperator() + ", RightSide: " + rightSide);
//todo solve case when x1|x2 without value - modify tree parser to list does not contain operator - stop_time (|)  ?
                    if (StringUtils.isBlank(arithmeticalOperator)) {
                        for (Object value : new LinkedList<>(values)) {
                            logger.debug("Value: " + value);
                            partialResult.add(new NwbResult(entity, value));
                        }
                    } else {
                        String jexlExpression;
                        if (arithmeticalOperator.equals(Operators.CONTAINS.op())) {
                            jexlExpression = "x1.contains(x2)";
                        } else {
                            jexlExpression = "x1" + arithmeticalOperator + "x2";
                        }
                        JexlExpression func = jexl.createExpression(jexlExpression);
                        String expressionValue = rightSide.getExpressionValue();
                        mc.set("x2", expressionValue);
                        for (Object value : new LinkedList<>(values)) {
                            logger.debug("Value: " + value);
                            mc.set("x1", value);
                            Object eval = func.evaluate(mc);
                            boolean res = ((Boolean) eval).booleanValue();
                            logger.debug("Evaluation: " + value + " " + arithmeticalOperator + " " + expressionValue + ", data: " + res);
                            if (res) {
                                partialResult.add(new NwbResult(entity, value));
                            }
                        }
                    }

                }
            }

            logger.debug(item + ", AND-OR-Operator: " + andOrOperator);

            if (StringUtils.equals("\\" + andOrOperator, Operators.OR.op())) {
                logger.debug("...OR....");
                nwbResults = Restrictions.or(nwbResults, partialResult);
                nwbResults.forEach(name -> logger.debug(name));
            }
            if (StringUtils.equals(andOrOperator, Operators.AND.op())) {
                logger.debug("...AND....");
                nwbResults = Restrictions.and(nwbResults, partialResult);
                nwbResults.forEach(name -> logger.debug(name));
            }
            if (StringUtils.isBlank(andOrOperator)) {
                nwbResults.addAll(partialResult);
            }

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
                throw new ProcessorException(e);
            }
        }
        return values;
    }
}
