package com.yoco.commons.utils;

import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.TypeFactory;
import com.yoco.commons.modal.JacksonObjectMapper;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class JsonUtil {

    private JsonUtil(){}
    public static String getJson(Object object) {
        try {
            return new JacksonObjectMapper().enable(JsonReadFeature.ALLOW_UNESCAPED_CONTROL_CHARS.mappedFeature()).writeValueAsString(object);
        } catch (Exception e) {
            return null;
        }
    }

    public static Map<String, Object> convertJsonToMap(String json) {

        try {
            return new JacksonObjectMapper().readValue(json, new TypeReference<HashMap<String, Object>>() {});
        } catch (Exception e) {
            return null;
        }
    }

    public static Set<Object> convertJsonToSet(String json) {

        try {
            return new JacksonObjectMapper().readValue(json, new TypeReference<Set<Object>>(){});
        } catch (Exception e) {
            return null;
        }
    }

    public static Boolean isValidJson(String string){
        if(ObjUtils.isNullOrEmpty(string)){
            return false;
        }
        try{
            new ObjectMapper().readTree(string);
        } catch(Exception e){
            return false;
        }
        return true;
    }

    public static <T> T convertSafeToType(String json, Class<T> clazz) {
        try {
            return new JacksonObjectMapper().readValue(json, clazz);
        } catch (IOException e) {
            return null;
        }
    }

    public static <T> T convertObjToType(Object data, Class<T> clazz) {

        var objMapper = new JacksonObjectMapper();
        return objMapper.convertValue(data, clazz);
    }

    public static <T> List<T> convertJsonToList(String json, Class<T> type) throws IOException {
        return convertJsonString(json, TypeFactory.defaultInstance().constructCollectionType(List.class, type));
    }

    public static <T> T convertJsonString(String json) throws IOException {
        var jacksonObjectMapper = new ObjectMapper().enable(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY);
        return jacksonObjectMapper.readValue(json, new TypeReference<>() {});
    }

    public static <T> T convertJsonString(String json, JavaType type) throws IOException {
        var jacksonObjectMapper = new ObjectMapper().enable(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY);
        return jacksonObjectMapper.readValue(json, type);
    }

    public static  <T> Map<String, Object> convertFileToMap (File file, Class<T> type ) throws IOException{
        return ( Map<String, Object> ) new ObjectMapper().readValue( file, type);
    }

}
