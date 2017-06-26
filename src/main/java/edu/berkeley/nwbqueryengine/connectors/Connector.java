package edu.berkeley.nwbqueryengine.connectors;

import edu.berkeley.nwbqueryengine.PartialExpression;
import edu.berkeley.nwbqueryengine.query.Query;
import edu.berkeley.nwbqueryengine.query.result.NwbResult;

import java.util.List;

/**
 * Created by petr-jezek on 8.5.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public interface Connector<T> {

    List<Object> getValues(T entity) throws ConnectorException;

    List<PartialExpression> processSearch(Query query) throws ConnectorException;
}
