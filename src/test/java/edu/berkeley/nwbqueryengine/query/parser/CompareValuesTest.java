package edu.berkeley.nwbqueryengine.query.parser;

import edu.berkeley.nwbqueryengine.data.NwbResult;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

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
 * CompareValuesTest, 2018/10/03 10:26 petr-jezek
 *
 **********************************************************************************************************************/
public class CompareValuesTest {

    @Test
    void compareDoubles() {
        NwbResult lowerDouble = new NwbResult("lower", 10.0, null);
        NwbResult higherDouble = new NwbResult("higher", 20.0, null);

        List<NwbResult> list = new LinkedList<>();
        list.add(lowerDouble);
        list.add(higherDouble);
        NwbResult maxResult = Collections.max(list);
        NwbResult minResult = Collections.min(list);

        assertEquals((double) minResult.getValue(), 10.0);
        assertEquals((double) maxResult.getValue(), 20.0);

        int result = lowerDouble.compareTo(higherDouble);
        assertTrue(result < 0);
    }

    @Test
    void compareIntegers() {
        NwbResult lowerDouble = new NwbResult("lower", 10, null);
        NwbResult higherDouble = new NwbResult("higher", 20, null);

        List<NwbResult> list = new LinkedList<>();
        list.add(lowerDouble);
        list.add(higherDouble);
        NwbResult maxResult = Collections.max(list);
        NwbResult minResult = Collections.min(list);

        assertEquals((int) minResult.getValue(), 10);
        assertEquals((int) maxResult.getValue(), 20);

        int result = lowerDouble.compareTo(higherDouble);
        assertTrue(result < 0);
    }

    @Test
    void compareStrings() {
        NwbResult lowerDouble = new NwbResult("lower", "Alphabet", null);
        NwbResult higherDouble = new NwbResult("higher", "ZZ-top", null);

        List<NwbResult> list = new LinkedList<>();
        list.add(lowerDouble);
        list.add(higherDouble);
        NwbResult maxResult = Collections.max(list);
        NwbResult minResult = Collections.min(list);

        assertEquals(minResult.getValue(), "Alphabet");
        assertEquals(maxResult.getValue(), "ZZ-top");

        int result = lowerDouble.compareTo(higherDouble);
        assertTrue(result < 0);
    }
}
