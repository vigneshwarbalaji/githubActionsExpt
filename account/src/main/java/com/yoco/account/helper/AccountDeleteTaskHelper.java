package com.yoco.account.helper;

import com.fullauth.api.manage.exception.FullAuthApiException;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.fullservices.FullReminders;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.account.AccountDTO;
import com.yoco.commons.services.EmailService;
import com.yoco.commons.services.FCMService;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.*;
import com.yoco.commons.utils.client.ClientUtil;
import com.yoco.commons.utils.events.ClockUtil;
import com.yoco.commons.utils.fcm.FcmUtil;
import com.yoco.commons.utils.integration.IntegrationUtil;
import com.yoco.user.helper.staff.UserStaffFullMetricHelper;
import com.yoco.user.helper.staff.UserStaffHelper;
import freemarker.template.TemplateException;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import static com.yoco.user.helper.staff.UserStaffTaskHelper.ACCOUNT_DELETED;

public class AccountDeleteTaskHelper {
    private AccountDeleteTaskHelper(){}
    private static final String ACCOUNT_DELETE_CHANNEL_KEY = "delete_account";

    public static void handleAccountDeletion(AccountDTO accountDTO,String primaryAdminContactID) throws NoSuchAlgorithmException, IOException, TemplateException {
        String accountID = accountDTO.getId();
        handlePrimaryAdminProDeletion(accountID,primaryAdminContactID,accountDTO.getDateDeletedTime());
        AccountTaskInitiator.initiateAccountDeletionStaffDisablingQueue(accountDTO,primaryAdminContactID);
        RTMService.publishToChannel(accountID,ACCOUNT_DELETE_CHANNEL_KEY,null,null);
        List<String> fcmDeviceTokens = FcmUtil.deleteAllDevicesInAccountAndReturnDeletedDeviceTokens(accountID);
        FCMService.getFCMService().publishToFCM(FCMService.FCM_SERVICE_CONSTANTS.FCM_DELETE_ACCOUNT.value(),fcmDeviceTokens);
        IntegrationUtil.deleteAllIntegrationsInAccount(accountID);
        ProjectUtil.deleteAllProjectAssociationsUnderAccount(accountID);
        ProjectUtil.archiveAllProjectsUnderAccount(accountID);
        ClientUtil.deactivateAllClientsUnderAccount(accountID,accountDTO.getDateDeletedTime(),primaryAdminContactID);
        DcmTeamUtil.deleteAllTeamsUnderAccount(accountID);
        AccountFullMetricHelper.getInstance().updateAccountDeletionMetric(accountID);
        FullReminders.deleteAllJobsForAccount(accountID);
    }

    public static void handlePrimaryAdminProDeletion(String accountID, String primaryAdminContactID,Long dateDeletedTime) throws NoSuchAlgorithmException, IOException, TemplateException {
        PeopleRelationJDO userPro = UserPROUtil.getUserProWithContact(accountID,primaryAdminContactID);
        UserStaffHelper.getInstance().updateProForAccountDeletion(userPro,dateDeletedTime);
        UserImpl.getUserImplInstance().savePro(userPro);
        AccessManager.deleteYoCoLevelPolicyForUser(accountID, primaryAdminContactID);
        DcmUtil.removeUserFromAccount(accountID, primaryAdminContactID);
        UserStaffFullMetricHelper.getInstance().updateUserDeletionMetric(accountID,"web_force");
        AccountEmailHelper.sendAccountDeletionEmailForPrimaryAdmin(userPro.getEmailID(),userPro.getContact().getFirstName());
    }

    public static void handleStaffDisablingOnAccountDeletion(AccountDTO accountDTO, String primaryAdminContactID) throws NoSuchAlgorithmException, IOException, FullAuthApiException, TemplateException {
        String accountID = accountDTO.getId();
        var userImplInstance = UserImpl.getUserImplInstance();
        Map<String,Object> staffProResponse = userImplInstance.getAllUsersWithContact(accountID,false,100,null);
        List<PeopleRelationJDO> staffProList = (List<PeopleRelationJDO>) staffProResponse.get(UserImpl.USERS);
        if(!ObjUtils.isNullOrEmpty(staffProList)) {
            batchDisableStaffPros(staffProList,accountDTO,userImplInstance,primaryAdminContactID);
            if(!ObjUtils.isNullOrEmpty((String)staffProResponse.get(UserImpl.CURSOR))){
                AccountTaskInitiator.initiateAccountDeletionStaffDisablingQueue(accountDTO,primaryAdminContactID);
            }
        }
    }

    public static void batchDisableStaffPros(List<PeopleRelationJDO> staffList, AccountDTO accountDTO, UserImpl userImplInstance, String primaryAdminContactID) throws FullAuthApiException, IOException, NoSuchAlgorithmException, TemplateException {
        var accountID = accountDTO.getId();
        var dateDeletedLongTime = accountDTO.getDateDeletedTime();
        var userStaffHelper = UserStaffHelper.getInstance();
        var primaryAdminEmail = UserPROUtil.getUserPro(accountID,primaryAdminContactID).getEmailID();
        var emailService = EmailService.getInstance();
        var subject = EmailUtil.getSubject("Thank you for using YoCoBoard.");
        Set<String> policyIDs = new HashSet<>();
        var userContactID = "";
        for(PeopleRelationJDO userPro : staffList){
            userContactID = userPro.getContactId();
            userStaffHelper.updateProForAccountDeletion(userPro,dateDeletedLongTime);
            policyIDs.add(AccessManager.getPolicyID(accountID,userContactID ));
            DcmUtil.removeUserFromAccount(accountID, userContactID);
            RTMService.publishToAW(accountID,userContactID, ACCOUNT_DELETED,null,null);
            AccountEmailHelper.sendAccountDeletionEmailForStaff(subject, userPro,accountDTO.getAccountName(),primaryAdminEmail,emailService);
        }
        userImplInstance.savePros(staffList);
        AccessManager.deleteAccessPolicies(policyIDs);
        UserStaffFullMetricHelper.getInstance().updateUserDeletionMetric(accountID,"web_force",staffList.size());
    }

    public static void handleClockEntriesOnAccountDeletion(AccountDTO accountDTO) throws IOException, NoSuchAlgorithmException {
        List<Map<String,Object>> runningEntries = ReportImpl.getReportImplInstance().getAllClockedInEntriesForAccount(accountDTO.getId());
        var accountDeletedLongTime = accountDTO.getDateDeletedTime();
        if(!ObjUtils.isNullOrEmpty(runningEntries)){
            for(Map<String,Object> entry : runningEntries){
                ClockUtil.stopRunningEntryForAccountDeletion(entry,accountDeletedLongTime);
            }
        }
    }
}
