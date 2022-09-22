package com.yoco.account.helper;

import com.fullauth.api.exception.TokenResponseException;
import com.fullauth.api.model.oauth.OauthAccessToken;
import com.google.api.client.googleapis.auth.oauth2.GoogleIdToken;
import com.google.api.client.googleapis.auth.oauth2.GoogleIdTokenVerifier;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.yoco.account.modal.AccountCreationPayloadDTO;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Collections;
import java.util.Map;
import static com.yoco.account.modal.AccountCreationPayloadDTO.EMAIL_KEY;
import static com.yoco.account.modal.AccountCreationPayloadDTO.USERNAME_KEY;

public class OneTapHelper {
    private OneTapHelper(){}
    public static final String CREDENTIAL_KEY = "credential";
    public static final String CLIENT_ID_KEY = "clientId";

    public static Map<String, Object> validateAndExtractUserDetails(String payload) throws GeneralSecurityException, IOException {
       Map<String,Object> oneTapPayload = validateAndExtractOneTapPayload(payload);
       return verifyAndExtractUserDetails(oneTapPayload,getGoogleIdTokenVerifierInstance((String)oneTapPayload.get(CLIENT_ID_KEY)));
    }

    public static Map<String,Object> validateAndExtractOneTapPayload(String payload){
        Validator.checkArgument(!JsonUtil.isValidJson(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payloadMap) || ObjUtils.isNullOrEmpty((String)payloadMap.get(CREDENTIAL_KEY)) || ObjUtils.isNullOrEmpty((String)payloadMap.get(CLIENT_ID_KEY)), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        return payloadMap;
    }

    public static Map<String, Object> verifyAndExtractUserDetails(Map<String,Object> oneTapPayload, GoogleIdTokenVerifier verifier) throws GeneralSecurityException, IOException {
        GoogleIdToken idToken = verifier.verify((String)oneTapPayload.get(CREDENTIAL_KEY));
        GoogleIdToken.Payload payload = idToken.getPayload();
        oneTapPayload.put(EMAIL_KEY, payload.getEmail());
        oneTapPayload.put(USERNAME_KEY, payload.get("name"));
        oneTapPayload.put(AccountCreationPayloadDTO.PHOTO_KEY, payload.get("picture"));
        Validator.checkArgument(ObjUtils.isNullOrEmpty((String)oneTapPayload.get(EMAIL_KEY)) || ObjUtils.isNullOrEmpty((String)oneTapPayload.get(USERNAME_KEY)), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        return oneTapPayload;
    }

    public static GoogleIdTokenVerifier getGoogleIdTokenVerifierInstance(String googleClientID){
        return new GoogleIdTokenVerifier.Builder(new NetHttpTransport(),new JacksonFactory()).setAudience(Collections.singletonList(googleClientID)).build();
    }

    public static String generateSsoSetupUrl(String oneTapToken, String googleClientID) throws TokenResponseException {
        OauthAccessToken token;
        token = FullAuthService.generateAccessTokenForOneTapToken(oneTapToken,googleClientID);
        if(token != null){
            return FullAuthService.generateSsoSetupUrl(token, CommonAppProperties.getYoCoDashboardUrl());
        }else{
            return CommonAppProperties.getYoCoDashboardUrl();
        }
    }
}
