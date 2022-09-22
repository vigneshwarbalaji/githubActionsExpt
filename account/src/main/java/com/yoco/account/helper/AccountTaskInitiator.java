package com.yoco.account.helper;

import com.yoco.account.modal.AccountUpdatePayloadDTO;
import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.modal.account.AccountDTO;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.constants.CommonConstants;
import java.io.IOException;
import java.util.Map;

public class AccountTaskInitiator {
    private AccountTaskInitiator(){}

    private static final String STAFF_OPERATIONS_QUEUE = "staff-operation";

    private static String getAccountDeletionTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/account/delete-handler";
    }

    private static String getAccountDeleteStaffProDeletionTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/account/delete-handler/staff";
    }

    private static String getAccountDeleteStopClockTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/account/delete-handler/clock";
    }

    private static String getAccountCreationCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/account/create-handler";
    }

    private static String getAccountUpdateCallBackUrl(String accountID){
        return CommonAppProperties.getAppUrl() + "/task/account/" + accountID + "/update-handler";
    }

    public static void initiateAccountDeletionQueues(AccountDTO account,String primaryAdminContactID) throws IOException{
        Map<String,Object> payloadMap = Map.of(CommonConstants.ACCOUNT_KEY, account,"primaryAdminContactID",primaryAdminContactID);
        TaskCreator.createPostTask(STAFF_OPERATIONS_QUEUE,getAccountDeletionTaskCallBackUrl(), CloudTaskUtil.convertObjectToByteArray(payloadMap));
        TaskCreator.createPostTask(STAFF_OPERATIONS_QUEUE,getAccountDeleteStopClockTaskCallBackUrl(), CloudTaskUtil.convertObjectToByteArray(payloadMap));
    }

    public static void initiateAccountDeletionStaffDisablingQueue(AccountDTO account,String primaryAdminContactID) throws IOException{
        Map<String,Object> payloadMap = Map.of(CommonConstants.ACCOUNT_KEY, account,"primaryAdminContactID",primaryAdminContactID);
        TaskCreator.createPostTask(STAFF_OPERATIONS_QUEUE,getAccountDeleteStaffProDeletionTaskCallBackUrl(), CloudTaskUtil.convertObjectToByteArray(payloadMap));
    }

    public static void initiateAccountCreationQueue(SettingsJDO account, PeopleRelationJDO userpro) throws IOException{
        Map<String,Object> payloadMap = Map.of(CommonConstants.ACCOUNT_KEY, account,CommonConstants.USER_KEY,userpro);
        TaskCreator.createPostTask(STAFF_OPERATIONS_QUEUE,getAccountCreationCallBackUrl(), CloudTaskUtil.convertObjectToByteArray(payloadMap));
    }

    public static void initiateAccountUpdateQueue(String accountID, Contact adminContact, AccountUpdatePayloadDTO accountUpdatePayloadDTO) throws IOException {
        Map<String,Object> payloadMap = Map.of("payloadDTO", accountUpdatePayloadDTO,CommonConstants.CONTACT_KEY,adminContact);
        TaskCreator.createPostTask(STAFF_OPERATIONS_QUEUE,getAccountUpdateCallBackUrl(accountID), CloudTaskUtil.convertObjectToByteArray(payloadMap));
    }
}
