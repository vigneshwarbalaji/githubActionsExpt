package com.yoco.account.modal;

import com.yoco.account.enums.ACCOUNT_ERROR_MESSAGE;
import com.yoco.commons.constants.AccountConstants;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.*;
import com.yoco.commons.utils.account.PhoneNumberUtil;
import com.yoco.commons.validations.RangeValidator;
import com.yoco.commons.validations.Validator;
import com.yoco.contact.helper.password.PasswordDCMHelper;
import lombok.Data;
import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;

@Data
public class AccountCreationPayloadDTO {
    private String emailID          = "";
    private String userName         = "";
    private String firstName        = "";
    private String lastName         = "";
    private String accountName      = "";
    private String password         = "";
    private String sysOffset        = "";
    private String plan             = "";
    private String price            = "";
    private String srcRef           = "";
    private String photoID          = "";
    private String phoneNumber      = "";
    private String countryCode      = "";
    private String timeZone         = "";
    private String ipAddress        = "";
    private String source           = "";
    private String googleToken      = "";
    private String oneTapToken      = "";
    private GeoLocationDTO geoLocation = null;

    public static final String EMAIL_KEY = "emailID";
    public static final String USERNAME_KEY = "userName";
    public static final String PASSWORD_KEY = "password";
    public static final String ACC_NAME_KEY = "accountName";
    public static final String OFFSET_KEY = "sysOffset";
    public static final String PLAN_KEY = "plan";
    public static final String PRICE_KEY = "price";
    public static final String REFERRER_KEY = "srcRef";
    public static final String PHOTO_KEY = "photoID";
    public static final String PHONE_NUMBER_KEY = "phoneNumber";
    public static final String COUNTRY_CODE_KEY = "countryCode";
    public static final String IP_ADDRESS_KEY = "ipAddress";
    public static final String GOOGLE_TOKEN_KEY = "googleAccessToken";
    public static final String ONE_TAP_TOKEN_KEY = "credential";

    public AccountCreationPayloadDTO(){}

    public AccountCreationPayloadDTO(Map<String,Object> payloadMap, HttpServletRequest request, String source){
         emailID          = !ObjUtils.isNullOrEmpty((String)payloadMap.get(EMAIL_KEY)) ? payloadMap.get(EMAIL_KEY).toString().toLowerCase().trim() : "";
         userName         = !ObjUtils.isNullOrEmpty((String)payloadMap.get(USERNAME_KEY)) ? Validator.sanitizeText(payloadMap.get(USERNAME_KEY).toString()).trim() : "";
         password         = !ObjUtils.isNullOrEmpty((String)payloadMap.get(PASSWORD_KEY)) ? payloadMap.get(PASSWORD_KEY).toString() : "";
         googleToken      = !ObjUtils.isNullOrEmpty((String)payloadMap.get(GOOGLE_TOKEN_KEY)) ? payloadMap.get(GOOGLE_TOKEN_KEY).toString() : "";
         oneTapToken      = !ObjUtils.isNullOrEmpty((String)payloadMap.get(ONE_TAP_TOKEN_KEY)) ? payloadMap.get(ONE_TAP_TOKEN_KEY).toString() : "";
         this.validateMandatoryFields();
         accountName      = !ObjUtils.isNullOrEmpty((String)payloadMap.get(ACC_NAME_KEY)) ? payloadMap.get(ACC_NAME_KEY).toString().trim() : userName;
         this.setFirstAndLastName();
         sysOffset        = !ObjUtils.isNull(payloadMap.get(OFFSET_KEY)) ? payloadMap.get(OFFSET_KEY).toString().trim() : "0";
         plan             = !ObjUtils.isNullOrEmpty((String)payloadMap.get(PLAN_KEY)) ? payloadMap.get(PLAN_KEY).toString().trim() : AccountConstants.FREE_PLAN;
         price            = !ObjUtils.isNullOrEmpty((String)payloadMap.get(PRICE_KEY)) ? payloadMap.get(PRICE_KEY).toString().trim() : "0.0";
         srcRef           = !ObjUtils.isNullOrEmpty((String)payloadMap.get(REFERRER_KEY)) ? payloadMap.get(REFERRER_KEY).toString().trim() : "";
         photoID          = !ObjUtils.isNullOrEmpty((String)payloadMap.get(PHOTO_KEY)) ? payloadMap.get(PHOTO_KEY).toString().trim() : "";
         phoneNumber      = !ObjUtils.isNullOrEmpty((String)payloadMap.get(PHONE_NUMBER_KEY)) ? payloadMap.get(PHONE_NUMBER_KEY).toString().trim() : "";
         countryCode      = !ObjUtils.isNullOrEmpty((String)payloadMap.get(COUNTRY_CODE_KEY)) ? payloadMap.get(COUNTRY_CODE_KEY).toString().trim() : "";
         this.source      = ObjUtils.isNullOrEmpty(source) ? ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value() : source.trim();
         this.extractAndSetIpAddress(request);
         geoLocation = new GeoLocationDTO(request);
         this.extractAndSetTimezone();
         this.extractAndSetPhoneNumber();
    }

    public void validateMandatoryFields(){
        Validator.checkArgument(!Validator.isValidEmail(this.emailID), ACCOUNT_ERROR_MESSAGE.INVALID_EMAIL.value());
        Validator.checkArgument(Validator.isFullDomain(this.emailID),ACCOUNT_ERROR_MESSAGE.INTERNAL_DOMAIN.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(this.userName), ACCOUNT_ERROR_MESSAGE.INVALID_USERNAME.value());
        if(!ObjUtils.isNullOrEmpty(this.password)){
            PasswordDCMHelper.validatePassword(this.password);
        }else{
            Validator.checkArgument(ObjUtils.isNullOrEmpty(this.googleToken) && ObjUtils.isNullOrEmpty(this.oneTapToken), COMMON_ERROR_RESPONSE.UNABLE_TO_CREATE.value());
        }
    }

    public void setFirstAndLastName(){
        String[] nameArray = this.userName.split(" ");
        this.firstName = nameArray[0].trim();
        this.lastName = (nameArray.length < 2 || nameArray[1] == null) ? "" : nameArray[1].trim();
    }

    public void extractAndSetIpAddress(HttpServletRequest request){
        this.ipAddress = HeaderUtil.extractIpAddressFromRequest(request);
    }

    public void extractAndSetTimezone(){
        var timezoneID = "";
        if(Validator.isValidCoordinates(this.geoLocation.getLatitude(),this.geoLocation.getLongitude())){
            timezoneID = TimeZoneUtil.getTimezoneFromCoordinates(this.geoLocation.getLatitude(),this.geoLocation.getLongitude());
        }
        if(!RangeValidator.isValidZoneID(timezoneID)){
            timezoneID = TimeZoneUtil.getTimeZoneID(this.sysOffset);
        }
        this.timeZone = timezoneID;
    }

    public void extractAndSetPhoneNumber(){
        if(!ObjUtils.isNullOrEmpty(this.countryCode) && !ObjUtils.isNullOrEmpty(this.phoneNumber)){
            var number = PhoneNumberUtil.extractUserPhoneNumber(this.phoneNumber,this.countryCode);
            if(PhoneNumberUtil.isValidPhoneNumber(number)){
                this.countryCode = String.valueOf(number.getCountryCode());
                this.phoneNumber = String.valueOf(number.getNationalNumber());
            }else{
                this.countryCode = "";
                this.phoneNumber = "";
            }
        }
    }

    public Map<String, Object> generateRegistrationInfoDetails(){
        Map<String,Object> registrationInfo = new HashMap<>();
        registrationInfo.put(PRICE_KEY,this.price);
        registrationInfo.put(PLAN_KEY,this.plan);
        registrationInfo.put(IP_ADDRESS_KEY,this.ipAddress);
        registrationInfo.put(AccountConstants.MAX_USERS, AccountUtil.getMaxUsersCountBasedOnPlan(this.plan));
        registrationInfo.put("phone", ObjUtils.isNullOrEmpty(this.phoneNumber) ? "" : this.countryCode + this.phoneNumber);
        registrationInfo.put("geoInfo", JsonUtil.getJson(this.geoLocation.getGeoInfoMap()));
        return registrationInfo;
    }
}
