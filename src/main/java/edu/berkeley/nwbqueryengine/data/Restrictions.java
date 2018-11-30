package edu.berkeley.nwbqueryengine.data;

import java.util.*;

import static edu.berkeley.nwbqueryengine.util.ValuesUtil.*;

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



    public static List<NwbResult> and(List<NwbResult> first, List<NwbResult> second, boolean isNextQuery) {
        final List<NwbResult> result = new LinkedList<>();

        if (first.size() > 0 && second.size() > 0) {

            //If one value is String I suppose all the others are too - Its applied for LIKE searches
            //I take both - duplicities are removed
            //This is applied for searching over different datasets too
            if (first.get(0).getValue() instanceof String
        || !getDatasetName(first.get(0)).equals(getDatasetName(second.get(0)))) {
                if(isNextQuery) {
                    result.addAll(first);
                    result.addAll(second);
                } else {
                    result.addAll(removeDatasetWithDuplicitPath(first, second));
                }
            } else {
                //This code takes both intervals (first and second) finds a highest lower bound and
                // a lowest higher bound from both.
                //Then iterates over both collections and find a value inside these bounds
                //Finally duplicities are removed
                double firstMax = (double) (Collections.max(first)).getValue();
                double secondMax = (double) (Collections.max(second)).getValue();

                double firstMin = (double) (Collections.min(first)).getValue();
                double secondMin = (double) (Collections.min(second)).getValue();

                Double[] minims = {firstMin, secondMin};
                Double[] maxims = {firstMax, secondMax};

                double highestMin = Collections.max(Arrays.asList(minims));
                double lowestMax = Collections.min(Arrays.asList(maxims));

                if (highestMin <= lowestMax) {
                    result.addAll(getResultsInBounds(first, highestMin, lowestMax));
                    result.addAll(getResultsInBounds(second, highestMin, lowestMax));
                }
            }
        }
        return removeDuplicities(result);
    }


}
