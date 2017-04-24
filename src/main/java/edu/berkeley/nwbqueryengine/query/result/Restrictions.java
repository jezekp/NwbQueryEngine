package edu.berkeley.nwbqueryengine.query.result;

import java.util.LinkedList;
import java.util.List;

/**
 * Created by petr-jezek on 24.4.17*
 * <p>
 * jezekp@kiv.zcu.cz
 */
public class Restrictions {

    public static List<NwbResult> or(List<NwbResult> first, List<NwbResult> second) {
        List<NwbResult> res = new LinkedList<>();
        res.addAll(first);
        res.addAll(second);
        return res;
    }

    public static List<NwbResult> and(List<NwbResult> first, List<NwbResult> second) {
        final List<NwbResult> copy = new LinkedList<>(first);
        copy.retainAll(second);
        return copy;
    }
}
