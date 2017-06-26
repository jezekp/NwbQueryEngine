package edu.berkeley.nwbqueryengine.query.parser;

import edu.berkeley.nwbqueryengine.query.Query;

/**
 * Created by petr-jezek on 26.6.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public interface Parser {
     Query parse(String expression);
}
