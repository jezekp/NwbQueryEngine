package edu.berkeley.nwbqueryengine.io;

/**
 * Created by petr-jezek on 10.7.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public interface Facade<DB, Q> {

    void executeQuery(DB storage, Q query);
}
