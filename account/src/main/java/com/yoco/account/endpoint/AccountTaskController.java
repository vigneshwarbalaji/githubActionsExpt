package com.yoco.account.endpoint;

import com.yoco.account.helper.AccountCreateTaskHelper;
import com.yoco.account.helper.AccountDeleteTaskHelper;
import com.yoco.account.helper.AccountUpdateTaskHelper;
import com.yoco.account.modal.AccountUpdatePayloadDTO;
import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.modal.account.AccountDTO;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.constants.CommonConstants;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import java.util.Map;

@Slf4j
@ValidateAccessToken
@RequestMapping("/task/account")
@RestController
public class AccountTaskController {

    @PostMapping("/delete-handler")
    public void handleAccountDeletion(@RequestBody byte[] payload){
        try{
            Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
            AccountDeleteTaskHelper.handleAccountDeletion((AccountDTO)payloadMap.get(CommonConstants.ACCOUNT_KEY),(String)payloadMap.get("primaryAdminContactID"));
        }catch (Exception e){
            log.error("Error processing account deletion queue :: " + e.getMessage());
        }
    }

    @PostMapping("/delete-handler/staff")
    public void handleStaffDisablingOnAccountDeletion(@RequestBody byte[] payload){
        try{
            Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
            AccountDeleteTaskHelper.handleStaffDisablingOnAccountDeletion((AccountDTO)payloadMap.get(CommonConstants.ACCOUNT_KEY),(String)payloadMap.get("primaryAdminContactID"));
        }catch (Exception e){
            log.error("Error processing account deletion staff pro deletion queue :: " + e.getMessage());
        }
    }

    @PostMapping("/delete-handler/clock")
    public void handleClockEntriesOnAccountDeletion(@RequestBody byte[] payload){
        try{
            Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
            AccountDeleteTaskHelper.handleClockEntriesOnAccountDeletion((AccountDTO)payloadMap.get(CommonConstants.ACCOUNT_KEY));
        }catch (Exception e){
            log.error("Error processing account deletion clock handler queue :: " + e.getMessage());
        }
    }

    @PostMapping("/create-handler")
    public void handleAccountCreation(@RequestBody byte[] payload){
        try{
            Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
            AccountCreateTaskHelper.handleAccountCreation((SettingsJDO)payloadMap.get(CommonConstants.ACCOUNT_KEY), (PeopleRelationJDO)payloadMap.get(CommonConstants.USER_KEY));
        }catch (Exception e){
            log.error("Error processing account creation queue :: " + e.getMessage());
        }
    }

    @PostMapping("/{accountID}/update-handler")
    public void handleAccountUpdate(@PathVariable String accountID,@RequestBody byte[] payload){
        try{
            Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
            AccountUpdateTaskHelper.handleAccountUpdate(accountID, (Contact)payloadMap.get(CommonConstants.CONTACT_KEY),(AccountUpdatePayloadDTO)payloadMap.get("payloadDTO"));
        }catch (Exception e){
            log.error("Error processing account update queue :: " + e.getMessage());
        }
    }
}
