package com.yoco.commons.fullservices;

import com.fullauth.api.enums.OauthAccessType;
import com.fullauth.api.exception.TokenResponseException;
import com.fullauth.api.model.oauth.OauthAccessToken;
import com.fullauth.api.service.FullAuthOauthService;
import com.fullauth.api.service.ServiceAccountCredentials;
import com.yoco.commons.annotation.helper.AccessTokenCacheService;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.utils.GaeUtils;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

@Slf4j
public class FullAuthService {
    private FullAuthService(){}

    private static final String SERVICE_TOKEN_KEY = "serverTokenFullAccess";
    public static final String AW_ACCOUNT_CREATION_TOKEN_KEY = "awAccountCreationServerToken";
    public static final String RATE_SERVICE_TOKEN_KEY = "rateLimitingServerToken";
    private static FullAuthOauthService fullAuthOauthServiceForFetchingAccessTokenInfo =  null;
    private static FullAuthOauthService fullAuthOauthServiceForFetchingServerTokenInfo =  null;
    private static FullAuthOauthService fullAuthOauthServiceForGeneratingSsoUrl =  null;

    public static String getAuthDomain(boolean isLiveMode){
        return isLiveMode ? "fullcreative" : "anywhere";
    }

    public static String getCustomAuthDomain(boolean isLiveMode) {
        return isLiveMode ? "auth.my.yocoboard.com" : "auth.staging.yocoboard.com";
    }

    public static FullAuthOauthService getFullAuthOauthServiceForFetchingAccessTokenInfo(){
        if(fullAuthOauthServiceForFetchingAccessTokenInfo == null){
            boolean isLiveMode = GaeUtils.isAppModeLive();
            setFullAuthOauthServiceForFetchingAccessTokenInfo(FullAuthOauthService.builder(isLiveMode)
                    .authDomain(getAuthDomain(isLiveMode))
                    .useAppspot(true)
                    .build());
        }
        return fullAuthOauthServiceForFetchingAccessTokenInfo;
    }

    public static FullAuthOauthService getFullAuthOauthServiceForFetchingServerTokenInfo(){
        if(fullAuthOauthServiceForFetchingServerTokenInfo == null){
            boolean isLiveMode = GaeUtils.isAppModeLive();
            setFullAuthOauthServiceForFetchingServerTokenInfo(FullAuthOauthService.builder(isLiveMode)
                    .authDomain(getAuthDomain(isLiveMode))
                    .clientId(CommonAppProperties.getFullOauthClientId())
                    .clientSecret(CommonAppProperties.getFullOauthClientSecret())
                    .useAppspot(true)
                    .build());
        }
        return fullAuthOauthServiceForFetchingServerTokenInfo;
    }

    public static FullAuthOauthService getFullAuthOauthServiceForGeneratingSsoUrl(){
        if(fullAuthOauthServiceForGeneratingSsoUrl == null){
            boolean isLiveMode = GaeUtils.isAppModeLive();
            setFullAuthOauthServiceForGeneratingSsoUrl(FullAuthOauthService.builder(isLiveMode)
                    .authDomain(getCustomAuthDomain(isLiveMode))
                    .isCustomDomain(true)
                    .clientId(CommonAppProperties.getFullOauthClientId())
                    .clientSecret(CommonAppProperties.getFullOauthClientSecret())
                    .useAppspot(true)
                    .build());
        }
        return fullAuthOauthServiceForGeneratingSsoUrl;
    }

    public static OauthAccessToken getAccessTokenInfo(String jwt) throws TokenResponseException {
        if(jwt == null){
            return null;
        }
        return getFullAuthOauthServiceForFetchingAccessTokenInfo().getTokenInfo(jwt);
    }

    public static void setFullAuthOauthServiceForFetchingAccessTokenInfo(FullAuthOauthService fullAuthOauthServiceForFetchingAccessTokenInfo) {
        FullAuthService.fullAuthOauthServiceForFetchingAccessTokenInfo = fullAuthOauthServiceForFetchingAccessTokenInfo;
    }

    public static void setFullAuthOauthServiceForFetchingServerTokenInfo(FullAuthOauthService fullAuthOauthServiceForFetchingServerTokenInfo) {
        FullAuthService.fullAuthOauthServiceForFetchingServerTokenInfo = fullAuthOauthServiceForFetchingServerTokenInfo;
    }

    public static void setFullAuthOauthServiceForGeneratingSsoUrl(FullAuthOauthService fullAuthOauthServiceForGeneratingSsoUrl) {
        FullAuthService.fullAuthOauthServiceForGeneratingSsoUrl = fullAuthOauthServiceForGeneratingSsoUrl;
    }

    public static String getServerAccessToken() throws NoSuchAlgorithmException, IOException {
        return getServerAccessToken(SERVICE_TOKEN_KEY, ApiScope.getServiceTokenScopes());
    }

    public static String getServerAccessToken(String serviceKey, Set<String> scopes) throws NoSuchAlgorithmException, IOException {

        OauthAccessToken token = null;

        token = AccessTokenCacheService.fetchServerAccessToken(serviceKey);

        if(ObjUtils.isNull(token)){

            log.info("cache hit failed for service");

            var fullAuthOauthService = getFullAuthOauthServiceForFetchingServerTokenInfo();

            if(!ObjUtils.isNull(fullAuthOauthService)){

                String jwt = generateJwtWithServiceAccountCredentials(scopes);

                token = fullAuthOauthService.requestAccessTokenWithJWT(jwt);

                AccessTokenCacheService.putServerAccessToken(token,serviceKey);
            }
        }

        return token == null ? "" : token.getAccessToken();
    }

    private static String generateJwtWithServiceAccountCredentials(Set<String> scopes) throws IOException {
        Map<String, Object> credentialMap = new HashMap<>();
        credentialMap.put("client_id", CommonAppProperties.getServiceAccountClientId());
        credentialMap.put("private_key", CommonAppProperties.getServiceAccountPrivateKey());
        credentialMap.put("public_key_id", CommonAppProperties.getServiceAccountPublicKeyId());
        var serviceAccountCredential = ServiceAccountCredentials.fromJson( credentialMap );
        return serviceAccountCredential.jwtString(scopes);
    }

    public static OauthAccessToken generateAccessTokenForCredentials(String email, String password) throws TokenResponseException {
        var authOauthService = getFullAuthOauthServiceForFetchingServerTokenInfo();
        return authOauthService.requestAccessTokenForResourceCredentials(email, password, ApiScope.getAccessTokenScopes(), OauthAccessType.OFFLINE, true);
    }

    public static OauthAccessToken generateAccessTokenForGoogleToken(String googleAccessToken) throws TokenResponseException {
        var authOauthService = getFullAuthOauthServiceForFetchingServerTokenInfo();
        return authOauthService.requestAccessTokenForGoogleToken(googleAccessToken, ApiScope.getAccessTokenScopes(), OauthAccessType.OFFLINE, true);
    }

    public static OauthAccessToken generateAccessTokenForOneTapToken(String oneTapToken, String googleClientID) throws TokenResponseException {
        var authOauthService = getFullAuthOauthServiceForFetchingServerTokenInfo();
        return authOauthService.requestAccessTokenForGoogleOneTapToken(googleClientID,oneTapToken, ApiScope.getAccessTokenScopes(), OauthAccessType.OFFLINE, true);
    }

    public static String generateSsoSetupUrl(OauthAccessToken token, String continueUrl){
        var authOauthService = getFullAuthOauthServiceForGeneratingSsoUrl();
        return authOauthService.generateSSOSetupUrl(token.getSsoToken(),continueUrl);
    }
}
