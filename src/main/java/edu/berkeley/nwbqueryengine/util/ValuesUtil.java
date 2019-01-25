package edu.berkeley.nwbqueryengine.util;

import edu.berkeley.nwbqueryengine.data.NwbResult;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.*;

/***********************************************************************************************************************
 *
 * This file is part of the NwbQueryEngine project

 * ==========================================
 *
 * Copyright (C) 2018 by Petr Jezek
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 ***********************************************************************************************************************
 *
 * ValuesUtil, 2018/02/26 14:28 petr-jezek
 *
 **********************************************************************************************************************/
public class ValuesUtil {

    private static Log logger = LogFactory.getLog(ValuesUtil.class);

    public static Object getModifiedCopy(Object value) {
        Object copy = value;
        if (value instanceof String) {
            String stringValue = ((String) value);
            copy = stringValue.replaceAll("\n", " ");
        }
        return copy;
    }

    public static Object getDateIfPossible(Object value) {
        Object copy = value;
        if (value instanceof String) {
            String stringValue = ((String) value);
            copy = DateUtil.tryParse(stringValue);
        }
        return copy;
    }

    public static String getDatasetName(NwbResult item) {
        String[] path = item.getDataSet().split("/");
        return path[path.length - 1];
    }

    public static String getPathWithoutDatasetName(NwbResult item) {
        int index = item.getDataSet().lastIndexOf("/");
        logger.debug("Dataset: " + item);
        return index  < 0 ? item.getDataSet() : item.getDataSet().substring(0, index);
    }

    public static List<NwbResult> removeDatasetWithDuplicitPath(List<NwbResult> first, List<NwbResult> second) {
        List<NwbResult> results = new LinkedList<>();
        List<Wrapper> firstWrapper = new LinkedList<>();
        List<Wrapper> secondWrapper = new LinkedList<>();
        first.forEach(item -> firstWrapper.add(new Wrapper(getPathWithoutDatasetName(item), item)));
        second.forEach(item -> secondWrapper.add(new Wrapper(getPathWithoutDatasetName(item), item)));
        firstWrapper.forEach(item -> {
            if (secondWrapper.contains(item)) {
                results.add(item.getResult());
            }
        });
        secondWrapper.forEach(item -> {
            if (firstWrapper.contains(item)) {
                results.add(item.getResult());
            }
        });
        return results;
    }

    public static List<NwbResult> getResultsInBounds(List<NwbResult> input, double low, double high) {
        List<NwbResult> result = new LinkedList<>();
        for (NwbResult value : input) {
            double compare = (double) value.getValue();
            if ((compare >= low) && (compare <= high)) {
                result.add(value);
            }
        }
        return result;
    }

    public static List<NwbResult> removeDuplicities(List<NwbResult> input) {
        List<NwbResult> al = new ArrayList<>(input);
        Set<NwbResult> hs = new HashSet<>();
        hs.addAll(al);
        al.clear();
        al.addAll(hs);
        return al;
    }

    static class Wrapper {

        private String path;
        private NwbResult result;

        public Wrapper(String path, NwbResult result) {
            this.setPath(path);
            this.setResult(result);
        }


        public String getPath() {
            return path;
        }

        public void setPath(String path) {
            this.path = path;
        }

        public NwbResult getResult() {
            return result;
        }

        public void setResult(NwbResult result) {
            this.result = result;
        }

        @Override
        public boolean equals(Object obj) {
            boolean result = false;
            if (obj != null && obj instanceof Wrapper) {
                Wrapper compare = (Wrapper) obj;
                result = compare.path.equals(path);
            }
            return result;
        }

        @Override
        public int hashCode() {
            return 37 * path.hashCode();
        }
    }
}
