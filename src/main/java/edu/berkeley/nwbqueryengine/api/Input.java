package edu.berkeley.nwbqueryengine.api;

import edu.berkeley.nwbqueryengine.data.NwbResult;

import java.util.List;

/**
 * Created by petr-jezek on 10.7.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public interface Input<DB, Q> {

    List<NwbResult> executeQuery(DB storage, Q query);
}
