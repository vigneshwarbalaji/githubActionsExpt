package com.yoco.account.helper;

import com.yoco.account.modal.AccountUpdatePayloadDTO;
import com.yoco.commons.constants.AccountConstants;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.AccountUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.TimeZoneUtil;
import com.yoco.commons.validations.Validator;
import java.util.Locale;
import java.util.Map;
import static com.yoco.account.enums.ACCOUNT_DISPLAY_TIMEFORMAT.DECIMAL;
import static com.yoco.account.modal.AccountUpdatePayloadDTO.LOGO_KEY;
import static com.yoco.commons.utils.AccountUtil.PHONE_KEY;

public class AccountUpdateHelper {

    private AccountUpdateHelper(){}

    public static AccountUpdatePayloadDTO validateAndExtractDTO(String payload){
        Validator.checkArgument(!JsonUtil.isValidJson(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payloadMap), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        return new AccountUpdatePayloadDTO(payloadMap);
    }

    public static void updateAccountAndSetActivityMessage(SettingsJDO account, AccountUpdatePayloadDTO accountUpdatePayloadDTO) {
        StringBuilder activityMessage = new StringBuilder();
        Map<String,Object> registrationDetails = AccountUtil.getRegisteredAccountDetails(account);
        String accountPlan = AccountUtil.getAccountPlan(account);
        updateGeneralFields(account,registrationDetails,accountUpdatePayloadDTO, activityMessage);
        updateFieldsBasedOnEnterprisePlan(account,accountPlan,registrationDetails,accountUpdatePayloadDTO, activityMessage);
        updateDisplayTimeFormatBasedOnPlan(account,accountPlan, accountUpdatePayloadDTO, activityMessage);
        account.setRegisteredPersonDetails(JsonUtil.getJson(registrationDetails));
        AccountImpl.getAccountImplInstance().saveAccount(account);
        accountUpdatePayloadDTO.setActivityMessage(activityMessage.toString());
    }

    public static void updateGeneralFields(SettingsJDO account, Map<String,Object> registrationDetails, AccountUpdatePayloadDTO accountUpdatePayloadDTO, StringBuilder activityMessage) {
        if(accountUpdatePayloadDTO.getAccountName() != null){
            account.setDisplayDomainName(accountUpdatePayloadDTO.getAccountName());
            activityMessage.append(" AccountName-").append(accountUpdatePayloadDTO.getAccountName());
        }
        if(accountUpdatePayloadDTO.getIsPayrollEnabled() != null){
            account.setIsPayrollEnabled(accountUpdatePayloadDTO.getIsPayrollEnabled());
            activityMessage.append(" PayrollEnabled-").append(accountUpdatePayloadDTO.getIsPayrollEnabled());
        }
        if(accountUpdatePayloadDTO.getOtThresholdHours() != null){
            account.setOtThresholdHours(accountUpdatePayloadDTO.getOtThresholdHours());
            activityMessage.append(" OtHours-").append(AccountUtil.formatOtHours(accountUpdatePayloadDTO.getOtThresholdHours()));
        }
        if(accountUpdatePayloadDTO.getPayrollAccessSince() != null){
            account.setPayrollAccessSince(accountUpdatePayloadDTO.getPayrollAccessSince());
            activityMessage.append(" PayrollAccessSince-").append(accountUpdatePayloadDTO.getPayrollAccessSince());
        }
        if(accountUpdatePayloadDTO.getPhoneNumber() != null){
            accountUpdatePayloadDTO.setOldPhoneNumber((String)registrationDetails.get(PHONE_KEY));
            registrationDetails.put(PHONE_KEY,accountUpdatePayloadDTO.getPhoneNumber());
            activityMessage.append(" Phone-").append(accountUpdatePayloadDTO.getPhoneNumber());
        }
        if(accountUpdatePayloadDTO.getTimeZone() != null){
            account.setTimeZone(accountUpdatePayloadDTO.getTimeZone());
            activityMessage.append(" TimeZone-").append(accountUpdatePayloadDTO.getTimeZone());
            account.setTimeZoneDisplayName(TimeZoneUtil.getZoneInFormat(accountUpdatePayloadDTO.getTimeZone(),TimeZoneUtil.OFFSET_ZONE_DISPLAY_NAME_LONG, Locale.ENGLISH));
        }
        if(accountUpdatePayloadDTO.getWeekStartDay() != null){
            account.setWeekStartDay(accountUpdatePayloadDTO.getWeekStartDay());
            activityMessage.append(" WeekStartDay-").append(accountUpdatePayloadDTO.getWeekStartDay());
        }
    }

    public static void updateDisplayTimeFormatBasedOnPlan(SettingsJDO account, String accountPlan, AccountUpdatePayloadDTO accountUpdatePayloadDTO, StringBuilder activityMessage) {
        boolean isPlanFree = AccountConstants.FREE_PLAN.equalsIgnoreCase(accountPlan);
        if(accountUpdatePayloadDTO.getDisplayTimeFormat() != null){
            if(DECIMAL.value().equalsIgnoreCase(accountUpdatePayloadDTO.getDisplayTimeFormat())){
                if(!isPlanFree){
                    account.setDisplayTimeFormat(DECIMAL.value());
                    activityMessage.append(" DisplayTimeFormat-").append(DECIMAL.value());
                }
            }else {
                account.setDisplayTimeFormat(accountUpdatePayloadDTO.getDisplayTimeFormat());
                activityMessage.append(" DisplayTimeFormat-").append(accountUpdatePayloadDTO.getDisplayTimeFormat());
            }
        }
    }

    public static void updateFieldsBasedOnEnterprisePlan(SettingsJDO account,String accountPlan, Map<String,Object> registrationDetails, AccountUpdatePayloadDTO accountUpdatePayloadDTO, StringBuilder activityMessage){
        if(AccountConstants.ENTERPRISE_PLAN.equalsIgnoreCase(accountPlan)){
            if(accountUpdatePayloadDTO.getLogo() != null){
                registrationDetails.put(LOGO_KEY,accountUpdatePayloadDTO.getLogo());
                activityMessage.append(" Logo-").append(accountUpdatePayloadDTO.getLogo());
            }
            if(accountUpdatePayloadDTO.getAllowedIPAddresses() != null){
                account.setAllowedIPAddresses(accountUpdatePayloadDTO.getAllowedIPAddresses());
                activityMessage.append(" AllowedIpAddress-").append(accountUpdatePayloadDTO.getAllowedIPAddresses());
            }
        }
    }

}
