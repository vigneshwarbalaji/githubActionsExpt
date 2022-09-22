package com.yoco.account.helper;

import com.fullauth.api.exception.TokenResponseException;
import com.fullauth.api.model.oauth.OauthAccessToken;
import com.google.api.client.googleapis.auth.oauth2.GoogleIdToken;
import com.google.api.client.googleapis.auth.oauth2.GoogleIdTokenVerifier;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.fullservices.FullAuthService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.HashMap;
import java.util.Map;

class OneTapHelperTest {
    @Test
    void validateAndExtractUserDetails_valid_test() throws GeneralSecurityException, IOException {
        try(MockedStatic<OneTapHelper> oneTapHelperMockedStatic = Mockito.mockStatic(OneTapHelper.class)){
            oneTapHelperMockedStatic.when(()->OneTapHelper.validateAndExtractOneTapPayload("payload")).thenReturn(Map.of("clientId","id"));
            GoogleIdTokenVerifier googleIdTokenVerifier = Mockito.mock(GoogleIdTokenVerifier.class);
            oneTapHelperMockedStatic.when(()->OneTapHelper.getGoogleIdTokenVerifierInstance("id")).thenReturn(googleIdTokenVerifier);
            oneTapHelperMockedStatic.when(()->OneTapHelper.verifyAndExtractUserDetails(Map.of("clientId","id"),googleIdTokenVerifier)).thenReturn(Map.of("key","value"));
            oneTapHelperMockedStatic.when(()->OneTapHelper.validateAndExtractUserDetails("payload")).thenCallRealMethod();
            Assertions.assertEquals(Map.of("key","value"),OneTapHelper.validateAndExtractUserDetails("payload"));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"invalid", "{}","{\"key\":\"value\"}","{\"credential\":\"value\"}"})
    void validateAndExtractOneTapPayload_invalidJson_test(String payload){
        Throwable exception = Assertions.assertThrows(IllegalArgumentException.class,()->OneTapHelper.validateAndExtractOneTapPayload(payload));
        Assertions.assertEquals("Payload cannot be empty or null.",exception.getMessage());
    }

    @Test
    void validateAndExtractOneTapPayload_valid_test(){
        Assertions.assertEquals(Map.of("credential","value","clientId","id"),OneTapHelper.validateAndExtractOneTapPayload("{\"credential\":\"value\",\"clientId\":\"id\"}"));
    }

    @Test
    void verifyAndExtractUserDetails_nullEmail_test() throws GeneralSecurityException, IOException {
        try{
            GoogleIdTokenVerifier googleIdTokenVerifierMock = Mockito.mock(GoogleIdTokenVerifier.class);
            GoogleIdToken idTokenMock = Mockito.mock(GoogleIdToken.class);
            Mockito.when(googleIdTokenVerifierMock.verify("cred")).thenReturn(idTokenMock);
            GoogleIdToken.Payload payloadMock = Mockito.mock(GoogleIdToken.Payload.class);
            Mockito.when(payloadMock.getEmail()).thenReturn(null);
            Mockito.when(idTokenMock.getPayload()).thenReturn(payloadMock);
            OneTapHelper.verifyAndExtractUserDetails(new HashMap(){{put("credential","cred");}},googleIdTokenVerifierMock);
        }catch (Exception e){
            Assertions.assertEquals("Payload cannot be empty or null.",e.getMessage());
        }
    }

    @Test
    void verifyAndExtractUserDetails_nullName_test() throws GeneralSecurityException, IOException {
        try{
            GoogleIdTokenVerifier googleIdTokenVerifierMock = Mockito.mock(GoogleIdTokenVerifier.class);
            GoogleIdToken idTokenMock = Mockito.mock(GoogleIdToken.class);
            Mockito.when(googleIdTokenVerifierMock.verify("cred")).thenReturn(idTokenMock);
            GoogleIdToken.Payload payloadMock = Mockito.mock(GoogleIdToken.Payload.class);
            Mockito.when(payloadMock.getEmail()).thenReturn("email");
            Mockito.when(payloadMock.get("name")).thenReturn(null);
            Mockito.when(idTokenMock.getPayload()).thenReturn(payloadMock);
            OneTapHelper.verifyAndExtractUserDetails(new HashMap(){{put("credential","cred");}},googleIdTokenVerifierMock);
        }catch (Exception e){
            Assertions.assertEquals("Payload cannot be empty or null.",e.getMessage());
        }
    }

    @Test
    void verifyAndExtractUserDetails_valid_test() throws GeneralSecurityException, IOException {
        GoogleIdTokenVerifier googleIdTokenVerifierMock = Mockito.mock(GoogleIdTokenVerifier.class);
        GoogleIdToken idTokenMock = Mockito.mock(GoogleIdToken.class);
        Mockito.when(googleIdTokenVerifierMock.verify("cred")).thenReturn(idTokenMock);
        GoogleIdToken.Payload payloadMock = Mockito.mock(GoogleIdToken.Payload.class);
        Mockito.when(payloadMock.getEmail()).thenReturn("email");
        Mockito.when(payloadMock.get("name")).thenReturn("name");
        Mockito.when(payloadMock.get("picture")).thenReturn("picture");
        Mockito.when(idTokenMock.getPayload()).thenReturn(payloadMock);
        Assertions.assertEquals(Map.of("credential","cred","emailID","email","userName","name","photoID","picture"),OneTapHelper.verifyAndExtractUserDetails(new HashMap(){{put("credential","cred");}},googleIdTokenVerifierMock));
    }

    @Test
    void getGoogleIdTokenVerifierInstance_valid_test(){
        GoogleIdTokenVerifier googleIdTokenVerifier = OneTapHelper.getGoogleIdTokenVerifierInstance("id");
        Assertions.assertNotNull(googleIdTokenVerifier);
        Assertions.assertEquals("id",googleIdTokenVerifier.getAudience().iterator().next());
    }

    @Test
    void generateSsoSetupUrl_nullToken_test() throws TokenResponseException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){
                commonAppPropertiesMockedStatic.when(CommonAppProperties::getYoCoDashboardUrl).thenReturn("url");
                fullAuthServiceMockedStatic.when(()->FullAuthService.generateAccessTokenForOneTapToken("token","id")).thenReturn(null);
                Assertions.assertEquals("url",OneTapHelper.generateSsoSetupUrl("token","id"));
        }
    }

    @Test
    void generateSsoSetupUrl_validToken_test() throws TokenResponseException {
        OauthAccessToken token = new OauthAccessToken();
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getYoCoDashboardUrl).thenReturn("url");
            fullAuthServiceMockedStatic.when(()->FullAuthService.generateAccessTokenForOneTapToken("token","id")).thenReturn(token);
            fullAuthServiceMockedStatic.when(()->FullAuthService.generateSsoSetupUrl(token,"url")).thenReturn("ssoUrl");
            Assertions.assertEquals("ssoUrl",OneTapHelper.generateSsoSetupUrl("token","id"));
        }
    }
}
