package edu.berkeley.nwbqueryengine;

import edu.berkeley.nwbqueryengine.query.Expression;

import java.util.List;

/**
 * Created by petr-jezek on 22.5.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class PartialExpression {

    private List<String> showResults;
    private Expression expression;
    private String fileName;

    public PartialExpression(List<String> showResults, Expression expression, String fileName) {
        this.showResults = showResults;
        this.expression = expression;
        this.setFileName(fileName);
    }

    public Expression getExpression() {
        return expression;
    }

    public void setExpression(Expression expression) {
        this.expression = expression;
    }

    public List<String> getShowResults() {
        return showResults;
    }

    public void setShowResults(List<String> showResults) {
        this.showResults = showResults;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }
}
