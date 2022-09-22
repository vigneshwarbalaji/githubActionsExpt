package com.yoco.account.helper;

import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.Status;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.validations.Validator;
import java.util.Map;

public class AccountDeleteHelper {
    private AccountDeleteHelper(){}
    private static final String DELETE_ACC_ACTIVITY_TEMPLATE = "${contactID} has deleted account: ${accountID}. Reason : ${reason}";
    private static final String DELETE_ACC_ACTIVITY_KEY = "Account Deletion";
    public static String extractReasonFromPayload(String payload){
        if(Boolean.TRUE.equals(JsonUtil.isValidJson(payload))){
            Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);
            String reason = (String)payloadMap.get("reason");
            return Validator.sanitizeText(reason);
        }
        return "";
    }

    public static void updateSettingsJdoEntryForDeletion(SettingsJDO account){
        account.setStatus(Status.INACTIVE.toString());
        account.setDateDeletedLongTime(DateUtil.getCurrentTime());
        AccountImpl.getAccountImplInstance().saveAccount(account);
    }

    public static void saveAccountDeletionActivity(Contact requesterContact, SettingsJDO account, String reason){
        String activity = DELETE_ACC_ACTIVITY_TEMPLATE.replace("${contactID}", requesterContact.getId())
                .replace("${accountID}",account.getPeopleUniquePin()).replace("${reason}",reason);
        ActivityUtil.saveActivity(account.getPeopleUniquePin(),requesterContact.getId(),DELETE_ACC_ACTIVITY_KEY,requesterContact.getEmailID()
        ,activity, ActivityUtil.ACTIVITIES.DUMMY.value(),account.getDateDeletedLongTime());
    }
}
