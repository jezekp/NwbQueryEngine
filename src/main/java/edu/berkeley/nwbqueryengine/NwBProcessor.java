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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.*;

/**
 * Created by petr-jezek on 26.6.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class NwBProcessor implements Processor<NwbResult>{

    private Log logger = LogFactory.getLog(getClass());
    private JexlEngine jexl = new Engine();
    private MapContext mc = new MapContext();

    public Connector storageConnector;

    public NwBProcessor(Connector storageConnector) {
        this.storageConnector = storageConnector;
    }

    public List<NwbResult> evaluate(Query query) throws ProcessorException{
        List<NwbResult> nwbResults = new LinkedList<>();
        Map<String, List<Object>> dataSets = new HashMap<>();
        boolean firstRun = true;
        String andOrOperator = "";
        List<EntityWrapper> entities;
        try {
            entities = storageConnector.processSearch(query);
        } catch (Exception e) {
            logger.error(e);
            throw new ProcessorException(e);
        }
        for (EntityWrapper partialExpression : entities) {
            List<NwbResult> partialResult = new LinkedList<>();
            List<Object> values;
            Expression item = partialExpression.getExpression();
            List<String> showResults = partialExpression.getEntity();
            for (String datasetsForSelect : showResults) {
                if (dataSets.containsKey(datasetsForSelect)) {
                    values = dataSets.get(datasetsForSelect);
                } else {
                    try {
                        values = storageConnector.getValues(datasetsForSelect);
                        dataSets.put(datasetsForSelect, values);
                    } catch (Exception e) {
                        logger.error(e);
                        throw new ProcessorException(e);
                    }
                }


                String arithmeticalOperator = item.getOperator();
                Expression rightSide = query.getRightSide(item);
                logger.debug("Operator: " + item.getOperator() + ", RightSide: " + rightSide);

                String jexlExpression;
                if (arithmeticalOperator.equals(Operators.CONTAINS.op())) {
                    jexlExpression = "x1.contains(x2)";
                } else {
                    jexlExpression = "x1" + arithmeticalOperator + "x2";
                }
                JexlExpression func = jexl.createExpression(jexlExpression);
                String expressionValue = rightSide.getExpressionValue();
                mc.set("x2", expressionValue);
                for (Object tmp : new LinkedList<>(values)) {
                    logger.debug("Value: " + tmp);
                    mc.set("x1", tmp);
                    Object eval = func.evaluate(mc);
                    boolean res = ((Boolean) eval).booleanValue();
                    logger.debug("Evaluation: " + tmp + "" + arithmeticalOperator + "" + expressionValue + ", data:" + res);
                    if (res) {
                        partialResult.add(new NwbResult(datasetsForSelect, tmp));
                    }
                }

            }


            logger.debug(item + ", AND-OR-Operator: " + andOrOperator);

            if (("\\" + andOrOperator).equals(Operators.OR.op())) {
                logger.debug("...OR....");
                nwbResults = Restrictions.or(nwbResults, partialResult);
                nwbResults.forEach(name -> logger.debug(name));
            }
            if (andOrOperator.equals(Operators.AND.op())) {
                logger.debug("...AND....");
                nwbResults = Restrictions.and(nwbResults, partialResult);
                nwbResults.forEach(name -> logger.debug(name));
            }
            if (firstRun) {
                nwbResults.addAll(partialResult);
                firstRun = false;
            }
            andOrOperator = item.getParent().getOperator();
        }
        nwbResults.forEach(name -> logger.debug(name));

        return nwbResults;
    }
}
