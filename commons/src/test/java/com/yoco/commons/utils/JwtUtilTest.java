package com.yoco.commons.utils;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import java.util.Map;

class JwtUtilTest {

    @Test
    void extractPayloadFromJWT_nullJwt_test(){
        Assertions.assertNull(JwtUtil.extractPayloadFromJWT(null));
    }

    @Test
    void extractPayloadFromJWT_emptyJwt_test(){
        Assertions.assertNull(JwtUtil.extractPayloadFromJWT(""));
    }

    @Test
    void extractPayloadFromJWT_ValidJwt_test(){
        Assertions.assertNotNull(JwtUtil.extractPayloadFromJWT("eyQ.eyJpc3MiOiJodHRwczovL2Z1bGxjcmVhdGl2ZS5mdWxsYXV0aC5jb20iLCJpYXQiOjE2MzIxMjUyMDAsInByb2pfaWQiOiJ5b2NvYm9hcmQiLCJ0eXBlIjoidXNlciIsInN1YiI6IjdmMmE2N2NhLTRjMjItNDJhNS04YjkxLWIzYjI4ZTVjNzc1YiIsImV4cCI6MTYzMjczMDAwMCwianRpIjoiOTc3M2FkbnBHQ1MyRDVBTiJ9.AErW"));
    }

    @Test
    void extractUserContactIDFromJwt_nullPayload_test(){
        Assertions.assertNull(JwtUtil.extractUserContactIDFromJwt(null));
    }

    @Test
    void extractUserContactIDFromJwt_emptyPayload_test(){
        Assertions.assertNull(JwtUtil.extractUserContactIDFromJwt(Map.of()));
    }

    @Test
    void isTokenTypeUser_nullPayload_test(){
        Assertions.assertFalse(JwtUtil.isTokenTypeUser(null));
    }

    @Test
    void isTokenTypeUser_emptyPayload_test(){
        Assertions.assertFalse(JwtUtil.isTokenTypeUser(Map.of()));
    }

    @Test
    void isTokenTypeServer_nullPayload_test(){
        Assertions.assertFalse(JwtUtil.isTokenTypeServer(null));
    }

    @Test
    void isTokenTypeServer_emptyPayload_test(){
        Assertions.assertFalse(JwtUtil.isTokenTypeServer(Map.of()));
    }
}
