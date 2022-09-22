package com.yoco.commons.constants;

import com.yoco.commons.enums.AppMode;
import com.yoco.commons.utils.GaeUtils;

public class UniversalSignupConstants {
    private UniversalSignupConstants(){}
    public static final AppMode APP_MODE = GaeUtils.getAppMode();
    private static String baseUrl = "";
    private static String freeSignupApiUrl = "";

    public static final String PAYLOAD_LOGIN_KEY = "login";
    public static final String PAYLOAD_ACC_NAME_KEY = "accountName";
    public static final String PAYLOAD_BRAND_ID_KEY = "brandId";
    public static final String PAYLOAD_SOURCE_KEY = "source";
    public static final String PAYLOAD_FIRSTNAME_KEY = "firstName";
    public static final String PAYLOAD_TIMEZONE_KEY = "timeZone";
    public static final String PAYLOAD_GEOLOCATION_KEY = "geoLocation";
    public static final String PAYLOAD_COUNTRY_KEY = "country";
    public static final String PAYLOAD_CITY_KEY = "city";
    public static final String PAYLOAD_REGION_KEY = "region";
    public static final String PAYLOAD_LATITUDE_KEY = "latitude";
    public static final String PAYLOAD_LONGITUDE_KEY = "longitude";
    public static final String PAYLOAD_PASSWORD_KEY = "password";
    public static final String PAYLOAD_LASTNAME_KEY = "lastName";
    public static final String PAYLOAD_COUNTRY_CODE_KEY = "countryCode";
    public static final String PAYLOAD_PHONE_NUMBER_KEY = "nationalNumber";
    public static final String PAYLOAD_IP_KEY = "ipAddress";

    static{
        initializeUniversalSignupConstants(APP_MODE);
    }

    public static void initializeUniversalSignupConstants(AppMode appMode){
        if(AppMode.LIVE.equals(appMode)){
            setupUniversalSignupConstantsForLive();
        }else{
            setupUniversalSignupConstantsForStaging();
        }
        setupUniversalSignupUrls();
    }

    private static void setupUniversalSignupUrls() {
        freeSignupApiUrl = baseUrl + "/api/freesignup";
    }

    private static void setupUniversalSignupConstantsForStaging() {baseUrl = "https://staging-universalsignup.appspot.com";}

    private static void setupUniversalSignupConstantsForLive() {
        baseUrl = "https://live-universalsignup.appspot.com";
    }

    public static String getFreeSignupApiUrl() {
        return freeSignupApiUrl;
    }

}
