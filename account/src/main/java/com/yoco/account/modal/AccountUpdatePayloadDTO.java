package com.yoco.account.modal;

import com.yoco.account.enums.ACCOUNT_DISPLAY_TIMEFORMAT;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.modal.account.PhoneNumberDetailsDTO;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import lombok.Data;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Data
public class AccountUpdatePayloadDTO implements Serializable {
    private String accountName;
    private String logo;
    private String allowedIPAddresses;
    private String phoneNumberCountry;
    private String displayTimeFormat;
    private Boolean isPayrollEnabled;
    private String otThresholdHours = null;
    private Long payrollAccessSince = null;
    private String phoneNumber;
    private String timeZone;
    private String weekStartDay;

    private String activityMessage;
    private String oldPhoneNumber;

    public static final String ACC_NAME_KEY = "accountName";
    public static final String LOGO_KEY = "logo";
    public static final String COUNTRY_KEY = "country";
    public static final String DISPLAY_TIME_FORMAT_KEY = "displayTimeFormat";
    public static final String IS_PAYROLL_ENABLED_KEY = "isPayrollEnabled";
    public static final String OT_THRESHOLD_KEY = "otThresholdHours";
    public static final String PHONE_NUMBER_KEY = "phoneNumber";
    public static final String PAYROLL_ACCESS_SINCE_KEY = "payrollAccessSince";
    public static final String ALLOWED_IP_ADDRESS_KEY = "allowedIPAddresses";
    public static final String TIME_ZONE_KEY = "timeZone";
    public static final String WEEK_START_DAY_KEY = "weekStartDay";

    public  AccountUpdatePayloadDTO(){}

    public AccountUpdatePayloadDTO(Map<String,Object> payloadMap){
        var payloadAccName = Validator.sanitizeText((String)payloadMap.get(ACC_NAME_KEY));
        this.accountName = ObjUtils.isNullOrEmpty(payloadAccName) ? null : payloadAccName;
        var payloadLogoUrl = (String)payloadMap.get(LOGO_KEY);
        this.logo = ObjUtils.isNullOrEmpty(payloadLogoUrl) ? null : payloadLogoUrl;
        this.allowedIPAddresses = this.validateAndExtractAllowedIpAddresses((String)payloadMap.get(ALLOWED_IP_ADDRESS_KEY));
        this.phoneNumber = this.validateAndExtractPhoneNumber((String)payloadMap.get(COUNTRY_KEY),(String)payloadMap.get(PHONE_NUMBER_KEY));
        this.displayTimeFormat = this.validateAndExtractDisplayTimeFormat((String)payloadMap.get(DISPLAY_TIME_FORMAT_KEY));
        this.weekStartDay = this.validateAndExtractWeekStartDay((String)payloadMap.get(WEEK_START_DAY_KEY));
        this.timeZone = this.validateAndExtractTimeZone((String)payloadMap.get(TIME_ZONE_KEY));
        this.isPayrollEnabled = (Boolean)payloadMap.get(IS_PAYROLL_ENABLED_KEY);
        if(!Boolean.FALSE.equals(this.isPayrollEnabled)){
            this.payrollAccessSince = this.validateAndExtractPayrollAccessSince((String)payloadMap.get(PAYROLL_ACCESS_SINCE_KEY));
            this.otThresholdHours = this.validateAndExtractOtThresholdHours((String)payloadMap.get(OT_THRESHOLD_KEY));
        }
    }

    public String validateAndExtractOtThresholdHours(String otThresholdHours) {
        try{
            String hours      =   otThresholdHours.split("/")[0];
            String duration   =   otThresholdHours.split("/")[1].toLowerCase();
            int durationInDays = getDaysTimeUnitForDuration(duration);
            return  Integer.parseInt(hours) > durationInDays * 24 ? null : TimeUnit.MILLISECONDS.convert( Integer.parseInt( hours), TimeUnit.HOURS) + ",HOURS,"
                    + TimeUnit.MILLISECONDS.convert(durationInDays, TimeUnit.DAYS);
        }catch (Exception e){
            return null;
        }
    }

    public int getDaysTimeUnitForDuration(String duration){
        switch ( duration.toLowerCase() ){
            case "day" :
                return 1;
            case "week":
                return 7;
            case "month":
                return 28;
            default:
                throw new IllegalStateException("Unexpected value: " + duration);
        }
    }

    public Long validateAndExtractPayrollAccessSince(String payrollAccessSince) {
       if(!Validator.isValidLongNumber(payrollAccessSince)){
           return null;
       }
       long payrollAccessSinceMillis = Long.parseLong(payrollAccessSince);
       return payrollAccessSinceMillis > DateUtil.getCurrentTime() ? DateUtil.getCurrentTime() : payrollAccessSinceMillis;
    }

    public String validateAndExtractTimeZone(String timeZone) {
        return Validator.isValidTimeZone(timeZone) ? timeZone : null;
    }

    public String validateAndExtractWeekStartDay(String weekStartDay) {
        if(DateConstants.WEEK_DAY_MON.equals(weekStartDay) || DateConstants.WEEK_DAY_SUN.equals(weekStartDay)){
            return weekStartDay;
        }
        return null;
    }

    public String validateAndExtractDisplayTimeFormat(String displayTimeFormat) {
        if(ACCOUNT_DISPLAY_TIMEFORMAT.HH_MM.value().equals(displayTimeFormat) || ACCOUNT_DISPLAY_TIMEFORMAT.HH_MM_SS.value().equals(displayTimeFormat)
        || ACCOUNT_DISPLAY_TIMEFORMAT.DECIMAL.value().equals(displayTimeFormat)){
            return displayTimeFormat;
        }
        return null;
    }

    public String validateAndExtractAllowedIpAddresses(String allowedIPAddresses) {
        if(allowedIPAddresses == null){
            return null;
        }
        if(allowedIPAddresses.trim().equals("")){
            return "";
        }
        String[] ipArray = allowedIPAddresses.split(",");
        String[] validatedIPArray = Arrays.stream(ipArray).map(Validator::sanitizeText)
                .filter(ip->!ObjUtils.isNullOrEmpty(ip))
                .map(ip -> ip.replaceAll("\\s+",""))
                .collect(Collectors.toList()).toArray(String[]::new);
        String validatedIPArrayString = String.join(",",validatedIPArray);
        return ObjUtils.isNullOrEmpty(validatedIPArrayString) ? null : validatedIPArrayString;
    }

    public String validateAndExtractPhoneNumber(String phoneNumberCountry, String phoneNumber){
        if(phoneNumber == null){
            return null;
        }
        if(phoneNumber.trim().equals("")){
            return "";
        }
        PhoneNumberDetailsDTO phoneNumberDetailsDTO = new PhoneNumberDetailsDTO(phoneNumberCountry,phoneNumber);
        return phoneNumberDetailsDTO.isValid() ? phoneNumberDetailsDTO.getCountryPhoneCode() + phoneNumberDetailsDTO.getNationalNumber()
                : null;
    }
}
