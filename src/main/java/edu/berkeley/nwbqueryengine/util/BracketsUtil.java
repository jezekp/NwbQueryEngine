package edu.berkeley.nwbqueryengine.util;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

/***********************************************************************************************************************
 *
 * This file is part of the nwbqueryengine project

 * ==========================================
 *
 * Copyright (C) 2019 by University of West Bohemia (http://www.zcu.cz/en/)
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
 * BracketsUtil, 2019/02/14 12:48 petr-jezek
 *
 **********************************************************************************************************************/
public class BracketsUtil {

    private final String text;

    public BracketsUtil(final String text) {
        this.text = text;
    }

    private List<String> parentheses;
    private List<Integer> indexes;
    private int index = 0;
    private int size = 0;

    public List<String> parse() {
        parentheses = new ArrayList<String>();
        indexes = new LinkedList<>();
        int[] endsAt = new int[1];
        endsAt[0] = 0;
        int previousIndex = 0;
        while(true) {
            int startsAt = text.indexOf("(", endsAt[0]);
            if (startsAt == -1) {
                break;
            }
            String item = parse(text, startsAt, endsAt);
            if(item != null) {
                int currentIndex = text.indexOf(item, previousIndex) + item.length();
                indexes.add(currentIndex);
                previousIndex = currentIndex;
            }
            if (endsAt[0] == 0) {
                break;
            }
            parentheses.add(item);
        }
        return parentheses;
    }

    public int end(int group) {
        return indexes.get(group);
    }

    public boolean next() {
        return size++ < parentheses.size();
    }

    public int nextEnd() {
        int localIndex = index++;
        return localIndex < indexes.size() ? indexes.get(localIndex) : -1;
    }

    private String parse(String str, int startsAt, int[] endsAt) {

        Stack<Integer> opStack = new Stack<Integer>();
        int i = startsAt + 1;
        while (i < str.length()) {

            if (str.charAt(i) == ')') {
                if (opStack.isEmpty()) {
                    endsAt[0] = i + 1;
                    return str.substring(startsAt, i + 1);
                } else {
                    opStack.pop();
                }
            }else if (str.charAt(i) == '(') {
                opStack.push(i);
            }

            i++;
        }

        return null;
    }
}
