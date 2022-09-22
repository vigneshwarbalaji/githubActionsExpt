package com.yoco.commons.utils;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.Map;

class CloudTaskUtilTest {
    @Test
    void ObjectByteArrayConversion_test() throws IOException, ClassNotFoundException {
        Map<String,Object> payload = Map.of("key1","value","key2",true);
        byte[] payloadBytes = CloudTaskUtil.convertObjectToByteArray(payload);
        Object actual = CloudTaskUtil.convertByteArrayToObject(payloadBytes);
        Assertions.assertEquals(payload,actual);
    }
}
