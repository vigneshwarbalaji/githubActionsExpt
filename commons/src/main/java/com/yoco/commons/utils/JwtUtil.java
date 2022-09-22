package com.yoco.commons.utils;

import lombok.extern.slf4j.Slf4j;

import java.util.Map;

@Slf4j
public class JwtUtil {
    private JwtUtil(){}

    public static Map<String,Object> extractPayloadFromJWT(String jwt){
        try{
            if(jwt == null || jwt.trim().length() < 1){
                return null;
            }
            var decoder = java.util.Base64.getDecoder();
            var payloadJson = new String(decoder.decode(jwt.split("\\.")[1]));
            return JsonUtil.convertJsonToMap(payloadJson);
        }catch (Exception e){
            log.info(e.getMessage());
            return null;
        }
    }

    public static boolean isTokenTypeUser(Map<String,Object> payload){
        if(payload == null || payload.isEmpty()){
            return false;
        }
        return "user".equalsIgnoreCase((String)payload.get("type"));
    }

    public static boolean isTokenTypeServer(Map<String,Object> payload){
        if(payload == null || payload.isEmpty()){
            return false;
        }
        return "server".equalsIgnoreCase((String)payload.get("type"));
    }

    public static String extractUserContactIDFromJwt(Map<String,Object> payload){
        if(payload == null || payload.isEmpty()){
            return null;
        }
        return (String)payload.get("sub");
    }

}
