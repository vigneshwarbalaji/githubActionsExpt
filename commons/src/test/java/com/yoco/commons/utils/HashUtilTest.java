package com.yoco.commons.utils;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.security.NoSuchAlgorithmException;
import java.util.Base64;

class HashUtilTest {

    @Test
    void generateSHA1_valid_test() throws NoSuchAlgorithmException {
        String message = "sampleData";

        String sha1Hash1 = HashUtil.generateSHA1(message);
        String sha1Hash2 = HashUtil.generateSHA1(message);

        Assertions.assertEquals(sha1Hash1,sha1Hash2);
    }

    @Test
    void generateSHA256_valid_test() throws NoSuchAlgorithmException {
        String message = "sampleData";

        String sha256Hash1 = HashUtil.generateSHA256(message);
        String sha256Hash2 = HashUtil.generateSHA256(message);

        Assertions.assertEquals(sha256Hash2,sha256Hash1);
    }

    @Test
    void encodeToBase64_test(){
        byte[] data = "test".getBytes();
        String resp = HashUtil.encodeToBase64(data);
        Assertions.assertEquals(Base64.getEncoder().encodeToString(data),resp);
    }
}
