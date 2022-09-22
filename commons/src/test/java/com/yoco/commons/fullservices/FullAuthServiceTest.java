package com.yoco.commons.fullservices;

import com.fullauth.api.enums.OauthAccessType;
import com.fullauth.api.exception.TokenResponseException;
import com.fullauth.api.model.oauth.OauthAccessToken;
import com.fullauth.api.service.FullAuthOauthService;
import com.fullauth.api.service.ServiceAccountCredentials;
import com.yoco.commons.annotation.helper.AccessTokenCacheService;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.utils.GaeUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Set;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mockStatic;

class FullAuthServiceTest {

    @Test
    void getFullAuthOauthServiceForFetchingAccessTokenInfo_notInstanstiated_LiveMode_test(){
        FullAuthService.setFullAuthOauthServiceForFetchingAccessTokenInfo(null);
        try(MockedStatic<FullAuthOauthService> fullAuthOauthServiceMockedStatic = mockStatic(FullAuthOauthService.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(true);
            FullAuthOauthService.FullAuthOauthServiceBuilder fullAuthOauthServiceBuilderMock = Mockito.mock( FullAuthOauthService.FullAuthOauthServiceBuilder.class);
            FullAuthOauthService fullAuthOauthServiceMock = Mockito.mock(FullAuthOauthService.class);
            Mockito.when(fullAuthOauthServiceBuilderMock.build()).thenReturn(fullAuthOauthServiceMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.useAppspot(true)).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.authDomain("fullcreative")).thenReturn(fullAuthOauthServiceBuilderMock);
            fullAuthOauthServiceMockedStatic.when(()->FullAuthOauthService.builder(true)).thenReturn(fullAuthOauthServiceBuilderMock);
            Assertions.assertEquals(fullAuthOauthServiceMock,FullAuthService.getFullAuthOauthServiceForFetchingAccessTokenInfo());
        }
    }

    @Test
    void getFullAuthOauthServiceForFetchingAccessTokenInfo_notInstanstiated_StagingMode_test(){
        FullAuthService.setFullAuthOauthServiceForFetchingAccessTokenInfo(null);
        try(MockedStatic<FullAuthOauthService> fullAuthOauthServiceMockedStatic = mockStatic(FullAuthOauthService.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            FullAuthOauthService.FullAuthOauthServiceBuilder fullAuthOauthServiceBuilderMock = Mockito.mock( FullAuthOauthService.FullAuthOauthServiceBuilder.class);
            FullAuthOauthService fullAuthOauthServiceMock = Mockito.mock(FullAuthOauthService.class);
            Mockito.when(fullAuthOauthServiceBuilderMock.build()).thenReturn(fullAuthOauthServiceMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.useAppspot(true)).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.authDomain("anywhere")).thenReturn(fullAuthOauthServiceBuilderMock);
            fullAuthOauthServiceMockedStatic.when(()->FullAuthOauthService.builder(false)).thenReturn(fullAuthOauthServiceBuilderMock);
            Assertions.assertEquals(fullAuthOauthServiceMock,FullAuthService.getFullAuthOauthServiceForFetchingAccessTokenInfo());
        }
    }

    @Test
    void getFullAuthOauthServiceForFetchingAccessTokenInfo_Instantiated_test(){
        FullAuthOauthService fullAuthOauthServiceMock = Mockito.mock(FullAuthOauthService.class);
        FullAuthService.setFullAuthOauthServiceForFetchingAccessTokenInfo(fullAuthOauthServiceMock);
        Assertions.assertEquals(fullAuthOauthServiceMock,FullAuthService.getFullAuthOauthServiceForFetchingAccessTokenInfo());
    }

    @Test
    void getFullAuthOauthServiceForFetchingServerTokenInfo_Instantiated_test(){
        FullAuthOauthService fullAuthOauthServiceMock = Mockito.mock(FullAuthOauthService.class);
        FullAuthService.setFullAuthOauthServiceForFetchingServerTokenInfo(fullAuthOauthServiceMock);
        Assertions.assertEquals(fullAuthOauthServiceMock,FullAuthService.getFullAuthOauthServiceForFetchingServerTokenInfo());
    }

    @Test
    void getAccessTokenInfo_nullToken_test() throws TokenResponseException {
        Assertions.assertNull(FullAuthService.getAccessTokenInfo(null));
    }

    @Test
    void getAccessTokenInfo_ValidToken_test() throws TokenResponseException {
        OauthAccessToken token = new OauthAccessToken();
        FullAuthOauthService fullAuthOauthServiceMock = Mockito.mock(FullAuthOauthService.class);
        Mockito.when(fullAuthOauthServiceMock.getTokenInfo("jwt")).thenReturn(token);
        FullAuthService.setFullAuthOauthServiceForFetchingAccessTokenInfo(fullAuthOauthServiceMock);
        Assertions.assertEquals(token,FullAuthService.getAccessTokenInfo("jwt"));
    }

    @Test
    void getAuthDomain_live_test(){
        Assertions.assertEquals("fullcreative",FullAuthService.getAuthDomain(true));
    }

    @Test
    void getAuthDomain_non_live_test(){
        Assertions.assertEquals("anywhere",FullAuthService.getAuthDomain(false));
    }

    @Test
    void getCustomAuthDomain_live_test(){
        Assertions.assertEquals("auth.my.yocoboard.com",FullAuthService.getCustomAuthDomain(true));
    }

    @Test
    void getCustomAuthDomain_non_live_test(){
        Assertions.assertEquals("auth.staging.yocoboard.com",FullAuthService.getCustomAuthDomain(false));
    }

    @Test
    void getServiceAccessToken_fullAuth_test() throws IOException, NoSuchAlgorithmException {

        FullAuthService.setFullAuthOauthServiceForFetchingServerTokenInfo(null);
        OauthAccessToken token = new OauthAccessToken();
        token.setAccessToken("serverToken");

        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class);
            MockedStatic<FullAuthOauthService> fullAuthOauthServiceMockedStatic = mockStatic(FullAuthOauthService.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = mockStatic(GaeUtils.class);
            MockedStatic<ServiceAccountCredentials> serviceAccountCredentialsMockedStatic = mockStatic(ServiceAccountCredentials.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = mockStatic(CommonAppProperties.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullOauthClientId).thenReturn("fullOAuthClientId");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullOauthClientSecret).thenReturn("fullOAuthClientSecret");

            accessTokenCacheServiceMockedStatic.when(() -> AccessTokenCacheService.fetchServerAccessToken("serverTokenFullAccess")).thenReturn(null);

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            FullAuthOauthService.FullAuthOauthServiceBuilder fullAuthOauthServiceBuilderMock = Mockito.mock( FullAuthOauthService.FullAuthOauthServiceBuilder.class);
            FullAuthOauthService fullAuthOauthServiceMock = Mockito.mock(FullAuthOauthService.class);
            Mockito.when(fullAuthOauthServiceBuilderMock.build()).thenReturn(fullAuthOauthServiceMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.useAppspot(true)).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.clientSecret("fullOAuthClientSecret")).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.clientId("fullOAuthClientId")).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.authDomain("anywhere")).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.build().requestAccessTokenWithJWT("jwt")).thenReturn(token);
            fullAuthOauthServiceMockedStatic.when(()->FullAuthOauthService.builder(false)).thenReturn(fullAuthOauthServiceBuilderMock);

            ServiceAccountCredentials serviceAccountCredentials = Mockito.mock(ServiceAccountCredentials.class);
            Mockito.when(serviceAccountCredentials.jwtString(anyCollection())).thenReturn("jwt");
            serviceAccountCredentialsMockedStatic.when(()-> ServiceAccountCredentials.fromJson(anyMap())).thenReturn(serviceAccountCredentials);

            Assertions.assertEquals(token.getAccessToken(),FullAuthService.getServerAccessToken());

            accessTokenCacheServiceMockedStatic.verify(() -> AccessTokenCacheService.putServerAccessToken(token,"serverTokenFullAccess"));
        }
    }

    @Test
    void getServiceAccessToken_cache_test() throws IOException, NoSuchAlgorithmException {
        OauthAccessToken token = new OauthAccessToken();
        token.setAccessToken("serverToken");
        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class)){
            accessTokenCacheServiceMockedStatic.when(() -> AccessTokenCacheService.fetchServerAccessToken("serverTokenFullAccess")).thenReturn(token);
            Assertions.assertEquals(token.getAccessToken(),FullAuthService.getServerAccessToken());
        }
    }

    @Test
    void getServiceAccessToken_null_test() throws IOException, NoSuchAlgorithmException {

        FullAuthService.setFullAuthOauthServiceForFetchingServerTokenInfo(null);

        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class);
            MockedStatic<FullAuthOauthService> fullAuthOauthServiceMockedStatic = mockStatic(FullAuthOauthService.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = mockStatic(CommonAppProperties.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullOauthClientId).thenReturn("fullOAuthClientId");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullOauthClientSecret).thenReturn("fullOAuthClientSecret");

            accessTokenCacheServiceMockedStatic.when(() -> AccessTokenCacheService.fetchServerAccessToken("serverTokenFullAccess")).thenReturn(null);

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            FullAuthOauthService.FullAuthOauthServiceBuilder fullAuthOauthServiceBuilderMock = Mockito.mock( FullAuthOauthService.FullAuthOauthServiceBuilder.class);
            Mockito.when(fullAuthOauthServiceBuilderMock.build()).thenReturn(null);
            Mockito.when(fullAuthOauthServiceBuilderMock.useAppspot(true)).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.clientSecret("fullOAuthClientSecret")).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.clientId("fullOAuthClientId")).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.authDomain("anywhere")).thenReturn(fullAuthOauthServiceBuilderMock);
            fullAuthOauthServiceMockedStatic.when(()->FullAuthOauthService.builder(false)).thenReturn(fullAuthOauthServiceBuilderMock);

            Assertions.assertEquals("",FullAuthService.getServerAccessToken());
        }
    }

    @Test
    void generateAccessTokenForCredentials_valid_test() throws TokenResponseException {
        OauthAccessToken token = new OauthAccessToken();
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){
            FullAuthOauthService fullAuthOauthService = Mockito.mock(FullAuthOauthService.class);
            Mockito.when(fullAuthOauthService.requestAccessTokenForResourceCredentials("email","password", Set.of("yoco-api.fullaccess","contacts-api.full_access"), OauthAccessType.OFFLINE, true)).thenReturn(token);
            fullAuthServiceMockedStatic.when(FullAuthService::getFullAuthOauthServiceForFetchingServerTokenInfo).thenReturn(fullAuthOauthService);
            fullAuthServiceMockedStatic.when(()->FullAuthService.generateAccessTokenForCredentials("email","password")).thenCallRealMethod();
            Assertions.assertEquals(token,FullAuthService.generateAccessTokenForCredentials("email","password"));
        }
    }

    @Test
    void generateAccessTokenForCredentials_googleToken_valid_test() throws TokenResponseException {
        OauthAccessToken token = new OauthAccessToken();
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){
            FullAuthOauthService fullAuthOauthService = Mockito.mock(FullAuthOauthService.class);
            Mockito.when(fullAuthOauthService.requestAccessTokenForGoogleToken("googleToken", Set.of("yoco-api.fullaccess","contacts-api.full_access"), OauthAccessType.OFFLINE, true)).thenReturn(token);
            fullAuthServiceMockedStatic.when(FullAuthService::getFullAuthOauthServiceForFetchingServerTokenInfo).thenReturn(fullAuthOauthService);
            fullAuthServiceMockedStatic.when(()->FullAuthService.generateAccessTokenForGoogleToken("googleToken")).thenCallRealMethod();
            Assertions.assertEquals(token,FullAuthService.generateAccessTokenForGoogleToken("googleToken"));
        }
    }

    @Test
    void generateSsoSetupUrl_valid_test(){
        OauthAccessToken token = new OauthAccessToken();
        token.setSsoToken("ssoToken");
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){
            FullAuthOauthService fullAuthOauthService = Mockito.mock(FullAuthOauthService.class);
            Mockito.when(fullAuthOauthService.generateSSOSetupUrl("ssoToken", "continueUrl")).thenReturn("ssoUrl");
            fullAuthServiceMockedStatic.when(FullAuthService::getFullAuthOauthServiceForGeneratingSsoUrl).thenReturn(fullAuthOauthService);
            fullAuthServiceMockedStatic.when(()->FullAuthService.generateSsoSetupUrl(token,"continueUrl")).thenCallRealMethod();
            Assertions.assertEquals("ssoUrl",FullAuthService.generateSsoSetupUrl(token,"continueUrl"));
        }
    }

    @Test
    void generateAccessTokenForOneTapToken_valid_test() throws TokenResponseException {
        OauthAccessToken token = new OauthAccessToken();
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){
            FullAuthOauthService fullAuthOauthService = Mockito.mock(FullAuthOauthService.class);
            Mockito.when(fullAuthOauthService.requestAccessTokenForGoogleOneTapToken("id","googleToken", Set.of("yoco-api.fullaccess","contacts-api.full_access"), OauthAccessType.OFFLINE, true)).thenReturn(token);
            fullAuthServiceMockedStatic.when(FullAuthService::getFullAuthOauthServiceForFetchingServerTokenInfo).thenReturn(fullAuthOauthService);
            fullAuthServiceMockedStatic.when(()->FullAuthService.generateAccessTokenForOneTapToken("googleToken","id")).thenCallRealMethod();
            Assertions.assertEquals(token,FullAuthService.generateAccessTokenForOneTapToken("googleToken","id"));
        }
    }

    @Test
    void getFullAuthOauthServiceForGeneratingSsoUrl_fullAuthOauthServiceForGeneratingSsoUrl_alreadyInstantiated_ShouldReturnTheInstantiatedObject(){
        FullAuthOauthService fullAuthOauthService = Mockito.mock(FullAuthOauthService.class);
        FullAuthService.setFullAuthOauthServiceForGeneratingSsoUrl(fullAuthOauthService);
        Assertions.assertEquals(fullAuthOauthService,FullAuthService.getFullAuthOauthServiceForGeneratingSsoUrl());
    }

    @Test
    void getFullAuthOauthServiceForGeneratingSsoUrl_serviceNotInstantiated_shouldCreateSetAndReturnFullOauthServiceObject(){
        FullAuthService.setFullAuthOauthServiceForGeneratingSsoUrl(null);
        try(MockedStatic<FullAuthOauthService> fullAuthOauthServiceMockedStatic = mockStatic(FullAuthOauthService.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = mockStatic(CommonAppProperties.class)){
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullOauthClientId).thenReturn("fullOAuthClientId");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullOauthClientSecret).thenReturn("fullOAuthClientSecret");
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            FullAuthOauthService.FullAuthOauthServiceBuilder fullAuthOauthServiceBuilderMock = Mockito.mock( FullAuthOauthService.FullAuthOauthServiceBuilder.class);
            FullAuthOauthService fullAuthOauthServiceMock = Mockito.mock(FullAuthOauthService.class);
            Mockito.when(fullAuthOauthServiceBuilderMock.authDomain("auth.staging.yocoboard.com")).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.isCustomDomain(true)).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.clientId("fullOAuthClientId")).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.clientSecret("fullOAuthClientSecret")).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.useAppspot(true)).thenReturn(fullAuthOauthServiceBuilderMock);
            Mockito.when(fullAuthOauthServiceBuilderMock.build()).thenReturn(fullAuthOauthServiceMock);
            fullAuthOauthServiceMockedStatic.when(()->FullAuthOauthService.builder(false)).thenReturn(fullAuthOauthServiceBuilderMock);
            fullAuthOauthServiceMockedStatic.when(FullAuthService::getFullAuthOauthServiceForGeneratingSsoUrl).thenCallRealMethod();
            Assertions.assertEquals(fullAuthOauthServiceMock,FullAuthService.getFullAuthOauthServiceForGeneratingSsoUrl());
        }
    }

}
