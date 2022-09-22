package com.yoco.commons.utils;

import lombok.extern.slf4j.Slf4j;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.UUID;

@Slf4j
public class HashUtil {

    private static final String SHA_1_ALGORITHM = "SHA-1";
    private static final String SHA_256_ALGORITHM = "SHA-256";

    private HashUtil(){}

    public static String generateSHA1(String message) throws NoSuchAlgorithmException {
        return hashString(message, SHA_1_ALGORITHM);
    }

    public static String generateSHA256(String message) throws NoSuchAlgorithmException {
        return hashString(message, SHA_256_ALGORITHM);
    }

    private static String hashString(String message, String algorithm) throws NoSuchAlgorithmException {
        var digest = MessageDigest.getInstance(algorithm);
        byte[] hashedBytes = digest.digest(message.getBytes(StandardCharsets.UTF_8));

        return convertByteArrayToHexString(hashedBytes);
    }

    private static String convertByteArrayToHexString(byte[] arrayBytes) {
        var stBuilder = new StringBuilder();
        for (var i = 0; i < arrayBytes.length; i++) {
            stBuilder.append(Integer.toString((arrayBytes[i] & 0xff) + 0x100, 16)
                    .substring(1));
        }
        return stBuilder.toString();
    }

    public static String generateUUID(){
        return UUID.randomUUID().toString();
    }

    public static String encodeToBase64(byte[] data){
        return Base64.getEncoder().encodeToString(data);
    }

}
