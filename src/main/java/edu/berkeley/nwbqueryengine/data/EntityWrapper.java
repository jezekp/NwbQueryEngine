package edu.berkeley.nwbqueryengine.data;

import edu.berkeley.nwbqueryengine.query.Expression;
import edu.berkeley.nwbqueryengine.query.Query;

import java.util.List;

/**
 * Created by petr-jezek on 22.5.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class EntityWrapper<T, DB> {

    private List<T> entity;
    private Expression expression;
    private DB storage;
    private Query query;

    public EntityWrapper(List<T> entity, Expression expression, DB storage, Query query) {
        this.entity = entity;
        this.expression = expression;
        this.setStorage(storage);
        this.setQuery(query);
    }

    public Expression getExpression() {
        return expression;
    }

    public void setExpression(Expression expression) {
        this.expression = expression;
    }

    public List<T> getEntity() {
        return entity;
    }

    public void setEntity(List<T> entity) {
        this.entity = entity;
    }

    public DB getStorage() {
        return storage;
    }

    public void setStorage(DB storage) {
        this.storage = storage;
    }

    public Query getQuery() {
        return query;
    }

    public void setQuery(Query query) {
        this.query = query;
    }
}
