package com.yoco.commons.annotation.helper;

import com.fullauth.api.enums.AccessTokenType;
import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.utils.JwtUtil;
import com.yoco.commons.utils.MemoryStoreUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.util.SerializationUtils;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.HashMap;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;

class AccessTokenCacheServiceTest {
    @Test
    void putAccessToken_nullToken_test(){
        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class)){
            accessTokenCacheServiceMockedStatic.when(()->AccessTokenCacheService.putAccessToken(null)).thenCallRealMethod();
            AccessTokenCacheService.putAccessToken(null);
            accessTokenCacheServiceMockedStatic.verify(() -> AccessTokenCacheService.putUserAccessToken(null),times(0));
            accessTokenCacheServiceMockedStatic.verify(() -> AccessTokenCacheService.putServerAccessToken(null),times(0));
        }
    }

    @Test
    void putAccessToken_Exception_test(){
        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class)){
            accessTokenCacheServiceMockedStatic.when(()->AccessTokenCacheService.putAccessToken(any(OauthAccessToken.class))).thenCallRealMethod();
            AccessTokenCacheService.putAccessToken(new OauthAccessToken());
            accessTokenCacheServiceMockedStatic.verify(() -> AccessTokenCacheService.putUserAccessToken(any(OauthAccessToken.class)),times(0));
            accessTokenCacheServiceMockedStatic.verify(() -> AccessTokenCacheService.putServerAccessToken(any(OauthAccessToken.class)),times(0));
        }
    }

    @Test
    void putAccessToken_typeUser_test(){
        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class)){
            OauthAccessToken token = new OauthAccessToken();
            token.setType("user");
            accessTokenCacheServiceMockedStatic.when(()->AccessTokenCacheService.putAccessToken(token)).thenCallRealMethod();
            AccessTokenCacheService.putAccessToken(token);
            accessTokenCacheServiceMockedStatic.verify(() -> AccessTokenCacheService.putUserAccessToken(token));
            accessTokenCacheServiceMockedStatic.verify(() -> AccessTokenCacheService.putServerAccessToken(token),times(0));
        }
    }

    @Test
    void putAccessToken_typeServer_test(){
        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class)){
            OauthAccessToken token = new OauthAccessToken();
            token.setType("server");
            accessTokenCacheServiceMockedStatic.when(()->AccessTokenCacheService.putAccessToken(token)).thenCallRealMethod();
            AccessTokenCacheService.putAccessToken(token);
            accessTokenCacheServiceMockedStatic.verify(() -> AccessTokenCacheService.putUserAccessToken(token),times(0));
            accessTokenCacheServiceMockedStatic.verify(() -> AccessTokenCacheService.putServerAccessToken(token));
        }
    }

    @Test
    void putAccessToken_typeInvalid_test(){
        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class)){
            OauthAccessToken token = Mockito.mock(OauthAccessToken.class);
            AccessTokenType accessTokenTypeMock = Mockito.mock(AccessTokenType.class);
            Mockito.when(accessTokenTypeMock.toString()).thenReturn("invalid");
            Mockito.when(token.getType()).thenReturn(accessTokenTypeMock);
            accessTokenCacheServiceMockedStatic.when(()->AccessTokenCacheService.putAccessToken(token)).thenCallRealMethod();
            AccessTokenCacheService.putAccessToken(token);
            accessTokenCacheServiceMockedStatic.verify(() -> AccessTokenCacheService.putUserAccessToken(token),times(0));
            accessTokenCacheServiceMockedStatic.verify(() -> AccessTokenCacheService.putServerAccessToken(token),times(0));
        }
    }

    @Test
    void getAccessToken_nullJwttest(){
        Assertions.assertNull(AccessTokenCacheService.getAccessToken(null));
    }

    @Test
    void getAccessToken_emptyJwttest(){
        Assertions.assertNull(AccessTokenCacheService.getAccessToken(""));
    }

    @Test
    void getAccessToken_invalidTypetest(){
        Assertions.assertNull(AccessTokenCacheService.getAccessToken("jwt"));
    }

    @Test
    void getAccessToken_ExceptionTest(){
        try(MockedStatic<JwtUtil> jwtUtilMockedStatic = mockStatic(JwtUtil.class)){
            jwtUtilMockedStatic.when(()->JwtUtil.extractPayloadFromJWT("jwt")).thenThrow(new IllegalArgumentException());
            Assertions.assertNull(AccessTokenCacheService.getAccessToken("jwt"));
        }
    }

    @Test
    void getAccessToken_userTypeTest(){
        try(MockedStatic<JwtUtil> jwtUtilMockedStatic = mockStatic(JwtUtil.class);
            MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class);){
            jwtUtilMockedStatic.when(()->JwtUtil.extractPayloadFromJWT("jwt")).thenReturn(new HashMap(){{put("type","user");put("sub","contactID");}});
            jwtUtilMockedStatic.when(()->JwtUtil.isTokenTypeUser(any(HashMap.class))).thenCallRealMethod();
            jwtUtilMockedStatic.when(()->JwtUtil.extractUserContactIDFromJwt(any(HashMap.class))).thenCallRealMethod();
            OauthAccessToken token = new OauthAccessToken();
            accessTokenCacheServiceMockedStatic.when(()->AccessTokenCacheService.fetchUserAccessToken("jwt","contactID")).thenReturn(token);
            accessTokenCacheServiceMockedStatic.when(()->AccessTokenCacheService.getAccessToken("jwt")).thenCallRealMethod();
            Assertions.assertEquals(token,AccessTokenCacheService.getAccessToken("jwt"));
        }
    }

    @Test
    void getAccessToken_serverTypeTest(){
        try(MockedStatic<JwtUtil> jwtUtilMockedStatic = mockStatic(JwtUtil.class);
            MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class);){
            jwtUtilMockedStatic.when(()->JwtUtil.extractPayloadFromJWT("jwt")).thenReturn(new HashMap(){{put("type","server");}});
            jwtUtilMockedStatic.when(()->JwtUtil.isTokenTypeUser(any(HashMap.class))).thenCallRealMethod();
            jwtUtilMockedStatic.when(()->JwtUtil.isTokenTypeServer(any(HashMap.class))).thenCallRealMethod();
            OauthAccessToken token = new OauthAccessToken();
            accessTokenCacheServiceMockedStatic.when(()->AccessTokenCacheService.fetchServerAccessToken("jwt")).thenReturn(token);
            accessTokenCacheServiceMockedStatic.when(()->AccessTokenCacheService.getAccessToken("jwt")).thenCallRealMethod();
            Assertions.assertEquals(token,AccessTokenCacheService.getAccessToken("jwt"));
        }
    }

    @Test
    void fetchUserAccessToken_nullTokenTest() throws NoSuchAlgorithmException {
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.get(anyString(),anyString())).thenReturn(null);
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        Assertions.assertNull(AccessTokenCacheService.fetchUserAccessToken("jwt","contactID"));
    }

    @Test
    void fetchUserAccessToken_TokenExpiredTest() throws NoSuchAlgorithmException {
        OauthAccessToken token = new OauthAccessToken();
        token.setExpires(123L);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.get(anyString(),anyString())).thenReturn(SerializationUtils.serialize(token));
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        Assertions.assertNull(AccessTokenCacheService.fetchUserAccessToken("jwt","contactID"));
        Mockito.verify(memoryStoreUtilMock).deleteHashKey(anyString(),anyString());
    }

    @Test
    void fetchUserAccessToken_ValidTokenTest() throws NoSuchAlgorithmException {
        OauthAccessToken token = new OauthAccessToken();
        token.setExpires(new Date().getTime() + 1000000);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.get(anyString(),anyString())).thenReturn(SerializationUtils.serialize(token));
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        Assertions.assertEquals(token,AccessTokenCacheService.fetchUserAccessToken("jwt","contactID"));
        Mockito.verify(memoryStoreUtilMock).setExpiryInDays(anyString(),eq(MemoryStoreUtil.DEFAULT_KEY_EXPIRYTIME_DAYS));
    }

    @Test
    void fetchServerAccessToken_nullTokenTest() throws NoSuchAlgorithmException {
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.get(anyString(),anyString())).thenReturn(null);
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        Assertions.assertNull(AccessTokenCacheService.fetchServerAccessToken("jwt"));
    }

    @Test
    void fetchServerAccessToken_TokenExpiredTest() throws NoSuchAlgorithmException {
        OauthAccessToken token = new OauthAccessToken();
        token.setExpires(123L);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.get(anyString(),anyString())).thenReturn(SerializationUtils.serialize(token));
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        Assertions.assertNull(AccessTokenCacheService.fetchServerAccessToken("jwt"));
        Mockito.verify(memoryStoreUtilMock).deleteHashKey(eq(AccessTokenCacheService.SERVERACCESSTOKEN_NAMESPACE),anyString());
    }

    @Test
    void fetchServerAccessToken_ValidTokenTest() throws NoSuchAlgorithmException {
        OauthAccessToken token = new OauthAccessToken();
        token.setExpires(new Date().getTime() + 1000000);
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        Mockito.when(memoryStoreUtilMock.get(anyString(),anyString())).thenReturn(SerializationUtils.serialize(token));
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        Assertions.assertEquals(token,AccessTokenCacheService.fetchServerAccessToken("jwt"));
        Mockito.verify(memoryStoreUtilMock).setExpiryInDays(AccessTokenCacheService.SERVERACCESSTOKEN_NAMESPACE,MemoryStoreUtil.DEFAULT_KEY_EXPIRYTIME_DAYS);
    }

    @Test
    void putUserAccessToken_nullToken_test() throws NoSuchAlgorithmException {
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        AccessTokenCacheService.putUserAccessToken(null);
        Mockito.verifyNoInteractions(memoryStoreUtilMock);
    }

    @Test
    void putUserAccessToken_NullUserID_test() throws NoSuchAlgorithmException {
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        AccessTokenCacheService.putUserAccessToken(new OauthAccessToken());
        Mockito.verifyNoInteractions(memoryStoreUtilMock);
    }

    @Test
    void putUserAccessToken_ValidUserID_test() throws NoSuchAlgorithmException {
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        OauthAccessToken token = new OauthAccessToken();
        token.setUserId("userID");
        token.setAccessToken("accessToken");
        AccessTokenCacheService.putUserAccessToken(token);
        Mockito.verify(memoryStoreUtilMock).put(anyString(),anyString(),eq(token));
        Mockito.verify(memoryStoreUtilMock).setExpiryInDays(anyString(),eq(MemoryStoreUtil.DEFAULT_KEY_EXPIRYTIME_DAYS));
    }

    @Test
    void putServerAccessToken_nullToken_test() throws NoSuchAlgorithmException {
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        AccessTokenCacheService.putServerAccessToken(null);
        Mockito.verifyNoInteractions(memoryStoreUtilMock);
    }

    @Test
    void putServerAccessToken_ValidToken_test() throws NoSuchAlgorithmException {
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        OauthAccessToken token = new OauthAccessToken();
        token.setAccessToken("accessToken");
        AccessTokenCacheService.putServerAccessToken(token);
        Mockito.verify(memoryStoreUtilMock).put(eq(AccessTokenCacheService.SERVERACCESSTOKEN_NAMESPACE),anyString(),eq(token));
        Mockito.verify(memoryStoreUtilMock).setExpiryInDays(AccessTokenCacheService.SERVERACCESSTOKEN_NAMESPACE,MemoryStoreUtil.DEFAULT_KEY_EXPIRYTIME_DAYS);
    }

    @Test
    void putServerAccessToken_nullTokenWithKey_test() throws NoSuchAlgorithmException {
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        AccessTokenCacheService.putServerAccessToken(null,"serverKey");
        Mockito.verifyNoInteractions(memoryStoreUtilMock);
    }

    @Test
    void putServerAccessToken_validTokenWithKey_test() throws NoSuchAlgorithmException {
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        OauthAccessToken token = new OauthAccessToken();
        token.setAccessToken("accessToken");
        AccessTokenCacheService.putServerAccessToken(token,"serverKey");
        Mockito.verify(memoryStoreUtilMock).put(eq(AccessTokenCacheService.SERVERACCESSTOKEN_NAMESPACE),anyString(),eq(token));
        Mockito.verify(memoryStoreUtilMock).setExpiryInDays(AccessTokenCacheService.SERVERACCESSTOKEN_NAMESPACE,MemoryStoreUtil.DEFAULT_KEY_EXPIRYTIME_DAYS);
    }

    @Test
    void clearAccessTokensForUser_nullTokenToExclude_test() throws NoSuchAlgorithmException {
        MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
        AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
        AccessTokenCacheService.clearAccessTokensForUser(null,"userID");
        Mockito.verify(memoryStoreUtilMock).deleteKey(anyString());
    }

    @Test
    void clearAccessTokensForUser_ValidTokenToExclude_test() throws NoSuchAlgorithmException {
        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class)){
            MemoryStoreUtil memoryStoreUtilMock = Mockito.mock(MemoryStoreUtil.class);
            accessTokenCacheServiceMockedStatic.when(()->AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock)).thenCallRealMethod();
            AccessTokenCacheService.setMemoryStoreUtil(memoryStoreUtilMock);
            OauthAccessToken token = new OauthAccessToken();
            accessTokenCacheServiceMockedStatic.when(()->AccessTokenCacheService.clearAccessTokensForUser(token,"userID")).thenCallRealMethod();
            AccessTokenCacheService.clearAccessTokensForUser(token,"userID");
            Mockito.verify(memoryStoreUtilMock).deleteKey(anyString());
            accessTokenCacheServiceMockedStatic.verify(()->AccessTokenCacheService.putUserAccessToken(token));
        }
    }
}
