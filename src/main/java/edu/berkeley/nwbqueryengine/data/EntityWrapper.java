package edu.berkeley.nwbqueryengine.data;

import edu.berkeley.nwbqueryengine.query.Expression;

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

    public EntityWrapper(List<T> showResults, Expression expression, DB storage) {
        this.entity = showResults;
        this.expression = expression;
        this.setStorage(storage);
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
}
