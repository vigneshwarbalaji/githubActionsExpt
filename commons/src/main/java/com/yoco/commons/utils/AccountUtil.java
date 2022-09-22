package com.yoco.commons.utils;

import com.yoco.commons.constants.AccountConstants;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.Status;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.validations.Validator;
import lombok.extern.slf4j.Slf4j;
import java.time.DayOfWeek;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public class AccountUtil {

    private AccountUtil(){}

    public static final String PHONE_KEY = "phone";
    public static final String LOGO_KEY = "logo";
    public static final String GEO_INFO_KEY = "geoInfo";
    public static final String COUNTRY_KEY = "country";
    public static final String DECIMAL = "decimal";

    public static Map<String,Object> getRegisteredAccountDetails(SettingsJDO settingsJDO){
        if(Boolean.TRUE.equals(JsonUtil.isValidJson(settingsJDO.getRegisteredPersonDetails()))){
            return JsonUtil.convertJsonToMap(settingsJDO.getRegisteredPersonDetails());
        }
        return new HashMap<>();
    }

    public static boolean isAccountActive(SettingsJDO account){
        return !ObjUtils.isNull(account) && Status.ACTIVE.toString().equalsIgnoreCase(account.getStatus());
    }

    public static boolean isActiveEnterpriseAccount(SettingsJDO settingsJDO){
        return isAccountActive(settingsJDO) && AccountConstants.ENTERPRISE_PLAN.equalsIgnoreCase(getAccountPlan(settingsJDO));
    }

    public static boolean isDecimalDurationActiveForTheGivenAccount(SettingsJDO account){
        return account.getDisplayTimeFormat().equalsIgnoreCase(DECIMAL);
    }

    public static boolean isActiveEnterpriseAccount(String accountId){
        var settingsJDO = AccountImpl.getAccountImplInstance().getById(accountId);
        return isActiveEnterpriseAccount(settingsJDO);
    }

    public static boolean isFreeAccount(SettingsJDO accountObject) {
        String accountPlan = AccountUtil.getAccountPlan(accountObject);
        return AccountConstants.FREE_PLAN.equalsIgnoreCase(accountPlan);
    }

    public static String getAccountPlan(SettingsJDO account){
        Map<String, Object> registerDetailsMap = getRegisteredAccountDetails(account);
        if(!ObjUtils.isNullOrEmpty(registerDetailsMap) && !ObjUtils.isBlank((String) registerDetailsMap.get(AccountConstants.PLAN))){
            return (String) registerDetailsMap.get(AccountConstants.PLAN);
        }
        return AccountConstants.FREE_PLAN;
    }

    public static String getMaxUsersCount(SettingsJDO settingsJDO){
        if(isAccountActive(settingsJDO)){
            Map<String,Object> registeredDetails = getRegisteredAccountDetails(settingsJDO);
            if(!ObjUtils.isNullOrEmpty(registeredDetails) && registeredDetails.containsKey(AccountConstants.MAX_USERS)){
                return registeredDetails.get(AccountConstants.MAX_USERS).toString();
            }
        }
        return AccountConstants.DEFAULT_USERS_LIMIT;
    }

    public static DayOfWeek getStartDayOfWeek(String accountID){
        return convertWeekStartDayIntoDayOfWeek(getStartDayOfWeekString(accountID));
    }

    public static String getStartDayOfWeekString(String accountID){
        var settingsJDO = AccountImpl.getAccountImplInstance().getById(accountID);
        return (settingsJDO == null || ObjUtils.isNullOrEmpty(settingsJDO.getWeekStartDay())) ? DateConstants.WEEK_DAY_SUN : settingsJDO.getWeekStartDay();
    }

    public static DayOfWeek convertWeekStartDayIntoDayOfWeek(String weekStartDay){
        if(DateConstants.WEEK_DAY_MON.equalsIgnoreCase(weekStartDay)){
            return DayOfWeek.MONDAY;
        }else {
            return DayOfWeek.SUNDAY;
        }
    }

    public static String getCompanyDisplayName(String accountID){
        SettingsJDO account = AccountImpl.getAccountImplInstance().getById(accountID);
        return getCompanyDisplayName(account);
    }

    public static String getCompanyDisplayName(SettingsJDO account){
        return (ObjUtils.isNull(account) || ObjUtils.isNullOrEmpty(account.getDisplayDomainName()))
                ? "" : account.getDisplayDomainName();
    }

    public static String getCompanyLogo(SettingsJDO account){
        return getCompanyLogo(getRegisteredAccountDetails(account));
    }

    public static String getCompanyLogo(Map<String,Object> registrationDetails){
        if(ObjUtils.isNullOrEmpty(registrationDetails) || ObjUtils.isNullOrEmpty((String)registrationDetails.get(LOGO_KEY))){
            return "";
        }
        return (String)registrationDetails.get(LOGO_KEY);
    }

    public static String getPayrollAccessSinceDate(SettingsJDO account){
        if(account.getPayrollAccessSince() <= 0L){
            return "";
        }
        return DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY, account.getPayrollAccessSince(), account.getTimeZone());
    }

    public static String getRegisteredPhoneNumber(SettingsJDO account){
        return getRegisteredPhoneNumber(getRegisteredAccountDetails(account));
    }

    public static String getRegisteredPhoneNumber(Map<String,Object> registrationDetails){
        if(ObjUtils.isNullOrEmpty(registrationDetails) || ObjUtils.isNullOrEmpty((String)registrationDetails.get(PHONE_KEY))){
            return "";
        }
        return (String) registrationDetails.get(PHONE_KEY);
    }

    public static Map<String,Object> getRegisteredGeoInfoDetails(SettingsJDO account){
        return getRegisteredGeoInfoDetails(getRegisteredAccountDetails(account));
    }

    public static Map<String,Object> getRegisteredGeoInfoDetails(Map<String,Object> registrationDetails){
        if(ObjUtils.isNullOrEmpty(registrationDetails) || Boolean.FALSE.equals(JsonUtil.isValidJson((String) registrationDetails.get(GEO_INFO_KEY)))){
            return Map.of();
        }
        return JsonUtil.convertJsonToMap((String) registrationDetails.get(GEO_INFO_KEY));
    }

    public static String getRegisteredCountryAlphaCode(SettingsJDO account) {
        return getRegisteredCountryAlphaCode(getRegisteredAccountDetails(account));
    }

    public static String getRegisteredCountryAlphaCode(Map<String,Object> registrationDetails) {
        Map<String,Object> registeredGeoInfo = getRegisteredGeoInfoDetails(registrationDetails);
        if(ObjUtils.isNullOrEmpty(registeredGeoInfo) || ObjUtils.isNullOrEmpty((String)registeredGeoInfo.get(COUNTRY_KEY))){
            return "US";
        }
        return (String)registeredGeoInfo.get(COUNTRY_KEY);
    }

    public static String getMaxUsersCountBasedOnPlan(String plan){
        if(AccountConstants.ENTERPRISE_PLAN.equalsIgnoreCase(plan)){
            return "unlimited";
        }else if(AccountConstants.STANDARD_PLAN.equalsIgnoreCase(plan)){
            return "25";
        }else{
            return "10";
        }
    }

    public static String formatOtHours(String otHours){
        if(ObjUtils.isNullOrEmpty(otHours)){
            return "0 Hours/Day";
        }
        String[] otHoursArray = otHours.split(",");
        String hours = "" + Long.parseLong(otHoursArray[0]) / 3600000;
        String rangeMillis = otHoursArray[2];
        String range;
        range = getRangeString(rangeMillis);
        return hours + " Hours/" + range;

    }

    private static String getRangeString(String rangeMillis) {
        String range;
        switch (rangeMillis) {
            case "86400000":
                range = "Day";
                break;
            case "604800000":
                range = "Week";
                break;
            case "2419200000":
                range = "Month";
                break;
            default:
                range = "";
        }
        return range;
    }

    public static SettingsJDO validateAndExtractAccount(String accountID){
        SettingsJDO account = AccountImpl.getAccountImplInstance().getById(accountID);
        Validator.checkArgument(ObjUtils.isNull(account),COMMON_ERROR_RESPONSE.ACCOUNT_NOT_FOUND.value());
        return account;
    }
}
