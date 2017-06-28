package edu.berkeley.nwbqueryengine.connectors;

import edu.berkeley.nwbqueryengine.data.EntityWrapper;
import edu.berkeley.nwbqueryengine.query.Query;

import java.util.List;

/**
 * Created by petr-jezek on 8.5.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public interface Connector<Entity> {

    List<Object> getValues(Entity entity) throws ConnectorException;

    List<EntityWrapper> processSearch(Query query) throws ConnectorException;
}
