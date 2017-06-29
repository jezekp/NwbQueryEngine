package edu.berkeley.nwbqueryengine;

import edu.berkeley.nwbqueryengine.data.NwbResult;
import edu.berkeley.nwbqueryengine.query.Query;

import java.util.List;

/**
 * Created by petr-jezek on 29.6.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public interface Processor<T> {

    List<T> evaluate(Query query) throws ProcessorException;
}
