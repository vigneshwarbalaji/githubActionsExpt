package com.yoco.commons.utils;

import java.io.*;

public class CloudTaskUtil {
    private CloudTaskUtil(){}

    public static byte[] convertObjectToByteArray(Object payload) throws IOException {
        var byteArrayOutputStream = new ByteArrayOutputStream();
        new ObjectOutputStream(byteArrayOutputStream).writeObject(payload);
        return byteArrayOutputStream.toByteArray();
    }

    public static Object convertByteArrayToObject(byte[] bytes) throws IOException, ClassNotFoundException {
        var byteIn = new ByteArrayInputStream(bytes);
        return new ObjectInputStream(byteIn).readObject();
    }
}
