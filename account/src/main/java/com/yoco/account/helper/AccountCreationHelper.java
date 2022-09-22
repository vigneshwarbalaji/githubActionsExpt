package com.yoco.account.helper;

import com.fullauth.api.exception.TokenResponseException;
import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.account.enums.ACCOUNT_ERROR_MESSAGE;
import com.yoco.account.modal.AccountCreationPayloadDTO;
import com.yoco.account.modal.GeoLocationDTO;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.constants.UniversalSignupConstants;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.fullservices.RateConfigurator;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.validations.Validator;
import lombok.extern.slf4j.Slf4j;
import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

@Slf4j
public class AccountCreationHelper {
    private AccountCreationHelper(){}
    static final int SIGNUP_REQUEST_LIMIT = 10;
    static final int SIGNUP_COUNT_EXPIRATION_LIMIT_SECONDS = (int) TimeUnit.HOURS.toSeconds(24);

    public static boolean isSignUpRateLimitExceeded(HttpServletRequest request){
        var key = HeaderUtil.extractIpAddressFromRequest(request) + "_signup";
        var signupRateLimitInfo = RateConfigurator.checkRateLimitUsage(key,SIGNUP_REQUEST_LIMIT,SIGNUP_COUNT_EXPIRATION_LIMIT_SECONDS);
        if(signupRateLimitInfo == null){
            log.info("Rate limiting api failure");
            return false;
        }
        return !signupRateLimitInfo.isAllow();
    }

    public static AccountCreationPayloadDTO validatePayloadAndExtractDTO(String payload, HttpServletRequest request, String source){
        Validator.checkArgument(!JsonUtil.isValidJson(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payloadMap), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        var accountCreationPayloadDTO = new AccountCreationPayloadDTO(payloadMap, request, source);
        Validator.checkArgument(UserPROUtil.isUserAssociatedToYoco(accountCreationPayloadDTO.getEmailID()), ACCOUNT_ERROR_MESSAGE.ACCOUNT_EXISTS.value());
        return accountCreationPayloadDTO;
    }

    public static Map<String, Object> generateSignupPayload(AccountCreationPayloadDTO payloadDTO) {
        Map<String,Object> payload = generateMandatoryFieldsForPayload(payloadDTO);
        if(!ObjUtils.isNullOrEmpty(payloadDTO.getPassword())){
            payload.put(UniversalSignupConstants.PAYLOAD_PASSWORD_KEY,payloadDTO.getPassword());
        }
        if(!ObjUtils.isNullOrEmpty(payloadDTO.getLastName())){
            payload.put(UniversalSignupConstants.PAYLOAD_LASTNAME_KEY,payloadDTO.getLastName());
        }
        if(!ObjUtils.isNullOrEmpty(payloadDTO.getCountryCode()) && !ObjUtils.isNullOrEmpty(payloadDTO.getPhoneNumber())){
            payload.put(UniversalSignupConstants.PAYLOAD_COUNTRY_CODE_KEY,payloadDTO.getCountryCode());
            payload.put(UniversalSignupConstants.PAYLOAD_PHONE_NUMBER_KEY,payloadDTO.getPhoneNumber());
        }
        if(!ObjUtils.isNullOrEmpty(payloadDTO.getIpAddress())){
            payload.put(UniversalSignupConstants.PAYLOAD_IP_KEY,payloadDTO.getIpAddress());
        }
        return payload;
    }

    public static Map<String, Object> generateMandatoryFieldsForPayload(AccountCreationPayloadDTO payloadDTO){
        Map<String,Object> payload = new HashMap<>();
        payload.put(UniversalSignupConstants.PAYLOAD_LOGIN_KEY,payloadDTO.getEmailID());
        payload.put(UniversalSignupConstants.PAYLOAD_ACC_NAME_KEY,payloadDTO.getAccountName());
        payload.put(UniversalSignupConstants.PAYLOAD_BRAND_ID_KEY, DcmConstants.YOCO_BRAND_ID);
        payload.put(UniversalSignupConstants.PAYLOAD_SOURCE_KEY,payloadDTO.getSource());
        payload.put(UniversalSignupConstants.PAYLOAD_FIRSTNAME_KEY,payloadDTO.getFirstName());
        payload.put(UniversalSignupConstants.PAYLOAD_TIMEZONE_KEY, payloadDTO.getTimeZone());
        payload.put(UniversalSignupConstants.PAYLOAD_GEOLOCATION_KEY,generateGeolocationPayload(payloadDTO.getGeoLocation()));
        return payload;
    }

    public static Map<String,Object> generateGeolocationPayload(GeoLocationDTO geoLocationDTO){
        Map<String,Object> geolocationPayload = new HashMap<>();
        geolocationPayload.put(UniversalSignupConstants.PAYLOAD_COUNTRY_KEY,ObjUtils.isNullOrEmpty(geoLocationDTO.getCountry()) ? "US" : geoLocationDTO.getCountry().trim());
        if(!ObjUtils.isNullOrEmpty(geoLocationDTO.getCity())){
            geolocationPayload.put(UniversalSignupConstants.PAYLOAD_CITY_KEY,geoLocationDTO.getCity());
        }
        if(!ObjUtils.isNullOrEmpty(geoLocationDTO.getRegion())){
            geolocationPayload.put(UniversalSignupConstants.PAYLOAD_REGION_KEY,geoLocationDTO.getRegion());
        }
        if(!ObjUtils.isNullOrEmpty(geoLocationDTO.getLatitude())){
            geolocationPayload.put(UniversalSignupConstants.PAYLOAD_LATITUDE_KEY,geoLocationDTO.getLatitude());
        }
        if(!ObjUtils.isNullOrEmpty(geoLocationDTO.getLongitude())){
            geolocationPayload.put(UniversalSignupConstants.PAYLOAD_LONGITUDE_KEY,geoLocationDTO.getLongitude());
        }
        return geolocationPayload;
    }

    public static String generateSsoSetupUrl(AccountCreationPayloadDTO payloadDTO) throws TokenResponseException {
        OauthAccessToken token;
        if(!ObjUtils.isNullOrEmpty(payloadDTO.getPassword())){
            token = FullAuthService.generateAccessTokenForCredentials(payloadDTO.getEmailID(),payloadDTO.getPassword());
        }else{
            token = FullAuthService.generateAccessTokenForGoogleToken(payloadDTO.getGoogleToken());
        }
        if(token != null){
            return FullAuthService.generateSsoSetupUrl(token, CommonAppProperties.getYoCoDashboardUrl());
        }else{
           return CommonAppProperties.getYoCoDashboardUrl();
        }
    }
}
