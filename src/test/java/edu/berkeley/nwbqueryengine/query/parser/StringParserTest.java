package edu.berkeley.nwbqueryengine.query.parser;

import edu.berkeley.nwbqueryengine.util.BracketsUtil;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
 * StringParserTest, 2019/02/14 12:52 petr-jezek
 *
 **********************************************************************************************************************/
public class StringParserTest {

    private BracketsUtil get(String s) {
        return new BracketsUtil(s);
    }

    @Test
    void complexQueryParserTest() {
        BracketsUtil util = get("epochs/Trial_306:(start_time < 1530) &  epochs/Trial_307:(stop_time>1530)");
        util.next();
        assertTrue (util.getExpression().equals("(start_time < 1530)"));
        util.next();
        assertTrue (util.getExpression().equals("(stop_time>1530)"));
        assertFalse(util.next());
    }

    @Test
    void innerBracketsQueryParserTest() {
        BracketsUtil util = get("/:(session_description LIKE %(PW)%)");
        util.next();
        assertTrue("(session_description LIKE %(PW)%)".equals(util.getExpression()));
        assertFalse(util.next());
    }

    @Test
    void testIndexBracketsQueryParserTest() {
        String s = "epochs/Trial_306:(start_time < 1530)   &    epochs/Trial_307:(stop_time>1530)   |   epochs/Trial_306:(start_time < 1530)";
        BracketsUtil util = get(s);

        int group0 = util.end(0);
        int group1 = util.end(1);
        int group2 = util.end(2);
        assertTrue(group0 == 36);
        assertTrue(group1 == 77);
        assertTrue(group2 == 120);
    }
}
