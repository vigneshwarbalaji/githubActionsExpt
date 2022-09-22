package com.yoco.commons.utils;

import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

class JsonUtilTest {

    @Test
    void getJson_passing_empty_HashMap_test () {
        Assert.assertEquals("{}", JsonUtil.getJson(new HashMap()));
    }

    @Test
    void getJson_passing_valid_HashMap_test () {

        HashMap<String, Object> payload = new HashMap<>();
        payload.put("name", "yoco");
        String expectedString = "{\"name\":\"yoco\"}";

        Assert.assertEquals(expectedString, JsonUtil.getJson(payload));
    }

    @Test
    void convertJsonToMap_passing_null_test () {
        Assert.assertEquals(null, JsonUtil.convertJsonToMap(null));
    }

    @Test
    void convertJsonToMap_passing_empty_test () {
        Assert.assertEquals(null, JsonUtil.convertJsonToMap(""));
    }

    @Test
    void convertJsonToMap_passing_valid_test () {
        String payloadString = "{\"name\":\"yoco\"}";
        Map<String, Object> resp= JsonUtil.convertJsonToMap(payloadString);
        Assertions.assertTrue(resp.containsKey("name"));
    }

}
