package com.yoco.account.helper;

import com.fullauth.api.manage.exception.FullAuthApiException;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
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
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class AccountDeleteTaskHelperTest {
    @Test
    void handleAccountDeletion_valid_test() throws TemplateException, NoSuchAlgorithmException, IOException {
        try(MockedStatic<AccountDeleteTaskHelper> accountDeleteTaskHelperMockedStatic = Mockito.mockStatic(AccountDeleteTaskHelper.class);
            MockedStatic<AccountTaskInitiator> accountTaskInitiatorMockedStatic = Mockito.mockStatic(AccountTaskInitiator.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<FcmUtil> fcmUtilMockedStatic = Mockito.mockStatic(FcmUtil.class);
            MockedStatic<FCMService> fcmServiceMockedStatic = Mockito.mockStatic(FCMService.class);
            MockedStatic<IntegrationUtil> integrationUtilMockedStatic = Mockito.mockStatic(IntegrationUtil.class);
            MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);
            MockedStatic<ClientUtil> clientUtilMockedStatic = Mockito.mockStatic(ClientUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class);
            MockedStatic<AccountFullMetricHelper> accountFullMetricHelperMockedStatic = Mockito.mockStatic(AccountFullMetricHelper.class);
            MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class)){
            AccountDTO accountDTO = new AccountDTO();
            accountDTO.setId("id");
            accountDTO.setDateDeletedTime(1L);
            accountDeleteTaskHelperMockedStatic.when(()->AccountDeleteTaskHelper.handleAccountDeletion(accountDTO,"123")).thenCallRealMethod();
            FCMService fcmServiceMock = Mockito.mock(FCMService.class);
            fcmServiceMockedStatic.when(FCMService::getFCMService).thenReturn(fcmServiceMock);
            AccountFullMetricHelper accountFullMetricHelperMock = Mockito.mock(AccountFullMetricHelper.class);
            accountFullMetricHelperMockedStatic.when(AccountFullMetricHelper::getInstance).thenReturn(accountFullMetricHelperMock);
            AccountDeleteTaskHelper.handleAccountDeletion(accountDTO,"123");
            accountDeleteTaskHelperMockedStatic.verify(()->AccountDeleteTaskHelper.handlePrimaryAdminProDeletion("id","123",1L));
            accountTaskInitiatorMockedStatic.verify(()->AccountTaskInitiator.initiateAccountDeletionStaffDisablingQueue(accountDTO,"123"));
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("id","delete_account",null,null));
            fcmUtilMockedStatic.verify(()->FcmUtil.deleteAllDevicesInAccountAndReturnDeletedDeviceTokens("id"));
            Mockito.verify(fcmServiceMock).publishToFCM("deleteAccount", List.of());
            integrationUtilMockedStatic.verify(()-> IntegrationUtil.deleteAllIntegrationsInAccount("id"));
            projectUtilMockedStatic.verify(()-> ProjectUtil.deleteAllProjectAssociationsUnderAccount("id"));
            projectUtilMockedStatic.verify(()-> ProjectUtil.archiveAllProjectsUnderAccount("id"));
            clientUtilMockedStatic.verify(()->ClientUtil.deactivateAllClientsUnderAccount("id",1L,"123"));
            dcmTeamUtilMockedStatic.verify(()-> DcmTeamUtil.deleteAllTeamsUnderAccount("id"));
            Mockito.verify(accountFullMetricHelperMock).updateAccountDeletionMetric("id");
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteAllJobsForAccount("id"));
        }
    }

    @Test
    void handlePrimaryAdminProDeletion_valid_test() throws TemplateException, NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<UserStaffFullMetricHelper> userStaffFullMetricHelperMockedStatic = Mockito.mockStatic(UserStaffFullMetricHelper.class);
            MockedStatic<AccountEmailHelper> accountEmailHelperMockedStatic = Mockito.mockStatic(AccountEmailHelper.class);){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setEmailID("email");
            Contact contact = new Contact();
            contact.setFirstName("first");
            userPro.setContact(contact);
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserProWithContact("accID","123")).thenReturn(userPro);
            UserStaffHelper userStaffHelperMock = Mockito.mock(UserStaffHelper.class);
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelperMock);
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            UserStaffFullMetricHelper userStaffFullMetricHelperMock = Mockito.mock(UserStaffFullMetricHelper.class);
            userStaffFullMetricHelperMockedStatic.when(UserStaffFullMetricHelper::getInstance).thenReturn(userStaffFullMetricHelperMock);
            AccountDeleteTaskHelper.handlePrimaryAdminProDeletion("accID","123",1L);
            Mockito.verify(userStaffHelperMock).updateProForAccountDeletion(userPro,1L);
            Mockito.verify(userImplMock).savePro(userPro);
            accessManagerMockedStatic.verify(()->AccessManager.deleteYoCoLevelPolicyForUser("accID","123"));
            dcmUtilMockedStatic.verify(()->DcmUtil.removeUserFromAccount("accID","123"));
            Mockito.verify(userStaffFullMetricHelperMock).updateUserDeletionMetric("accID","web_force");
            accountEmailHelperMockedStatic.verify(()->AccountEmailHelper.sendAccountDeletionEmailForPrimaryAdmin("email","first"));
        }
    }

    @Test
    void handleStaffDisablingOnAccountDeletion_noUsers_test() throws FullAuthApiException, TemplateException, NoSuchAlgorithmException, IOException {
        try( MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
             MockedStatic<AccountDeleteTaskHelper> accountDeleteTaskHelperMockedStatic = Mockito.mockStatic(AccountDeleteTaskHelper.class);){
            AccountDTO accountDTO = new AccountDTO();
            accountDTO.setId("id");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUsersWithContact("id",false,100,"")).thenReturn(Map.of());
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            accountDeleteTaskHelperMockedStatic.when(()->AccountDeleteTaskHelper.handleStaffDisablingOnAccountDeletion(accountDTO,"123")).thenCallRealMethod();
            AccountDeleteTaskHelper.handleStaffDisablingOnAccountDeletion(accountDTO,"123");
            accountDeleteTaskHelperMockedStatic.verify(()->AccountDeleteTaskHelper.handleStaffDisablingOnAccountDeletion(accountDTO,"123"));
            accountDeleteTaskHelperMockedStatic.verifyNoMoreInteractions();
        }
    }

    @Test
    void handleStaffDisablingOnAccountDeletion_noCursor_test() throws FullAuthApiException, TemplateException, NoSuchAlgorithmException, IOException {
        try( MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
             MockedStatic<AccountDeleteTaskHelper> accountDeleteTaskHelperMockedStatic = Mockito.mockStatic(AccountDeleteTaskHelper.class);
             MockedStatic<AccountTaskInitiator> accountTaskInitiatorMockedStatic = Mockito.mockStatic(AccountTaskInitiator.class)){
            AccountDTO accountDTO = new AccountDTO();
            accountDTO.setId("id");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUsersWithContact("id",false,100,null)).thenReturn(Map.of("users",List.of(new PeopleRelationJDO())));
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            accountDeleteTaskHelperMockedStatic.when(()->AccountDeleteTaskHelper.handleStaffDisablingOnAccountDeletion(accountDTO,"123")).thenCallRealMethod();
            AccountDeleteTaskHelper.handleStaffDisablingOnAccountDeletion(accountDTO,"123");
            accountDeleteTaskHelperMockedStatic.verify(()->AccountDeleteTaskHelper.batchDisableStaffPros(List.of(new PeopleRelationJDO()),accountDTO,userImplMock,"123"));
            accountTaskInitiatorMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleStaffDisablingOnAccountDeletion_validCursor_test() throws FullAuthApiException, TemplateException, NoSuchAlgorithmException, IOException {
        try( MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
             MockedStatic<AccountDeleteTaskHelper> accountDeleteTaskHelperMockedStatic = Mockito.mockStatic(AccountDeleteTaskHelper.class);
             MockedStatic<AccountTaskInitiator> accountTaskInitiatorMockedStatic = Mockito.mockStatic(AccountTaskInitiator.class)){
            AccountDTO accountDTO = new AccountDTO();
            accountDTO.setId("id");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUsersWithContact("id",false,100,null)).thenReturn(Map.of("users",List.of(new PeopleRelationJDO()),"cursor","cursor"));
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            accountDeleteTaskHelperMockedStatic.when(()->AccountDeleteTaskHelper.handleStaffDisablingOnAccountDeletion(accountDTO,"123")).thenCallRealMethod();
            AccountDeleteTaskHelper.handleStaffDisablingOnAccountDeletion(accountDTO,"123");
            accountDeleteTaskHelperMockedStatic.verify(()->AccountDeleteTaskHelper.batchDisableStaffPros(List.of(new PeopleRelationJDO()),accountDTO,userImplMock,"123"));
            accountTaskInitiatorMockedStatic.verify(()->AccountTaskInitiator.initiateAccountDeletionStaffDisablingQueue(accountDTO,"123"));
        }
    }

    @Test
    void batchDisableStaffPros_test() throws FullAuthApiException, TemplateException, IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<EmailUtil> emailUtilMockedStatic = Mockito.mockStatic(EmailUtil.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<AccountEmailHelper> accountEmailHelperMockedStatic = Mockito.mockStatic(AccountEmailHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserStaffFullMetricHelper> userStaffFullMetricHelperMockedStatic = Mockito.mockStatic(UserStaffFullMetricHelper.class);){
            AccountDTO accountDTO = new AccountDTO();
            accountDTO.setId("accID");
            accountDTO.setDateDeletedTime(1L);
            accountDTO.setAccountName("company");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO user1 = new PeopleRelationJDO();
            user1.setContactId("1");
            PeopleRelationJDO user2 = new PeopleRelationJDO();
            user2.setContactId("2");
            UserStaffHelper userStaffHelperMock = Mockito.mock(UserStaffHelper.class);
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelperMock);
            emailUtilMockedStatic.when(()->EmailUtil.getSubject("Thank you for using YoCoBoard.")).thenReturn("subject");
            UserStaffFullMetricHelper userStaffFullMetricHelperMock = Mockito.mock(UserStaffFullMetricHelper.class);
            userStaffFullMetricHelperMockedStatic.when(UserStaffFullMetricHelper::getInstance).thenReturn(userStaffFullMetricHelperMock);
            accessManagerMockedStatic.when(()->AccessManager.getPolicyID("accID","1")).thenReturn("p1");
            accessManagerMockedStatic.when(()->AccessManager.getPolicyID("accID","2")).thenReturn("p2");
            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            adminPro.setEmailID("adminEmail");
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserPro("accID","123")).thenReturn(adminPro);
            AccountDeleteTaskHelper.batchDisableStaffPros(List.of(user1,user2),accountDTO,userImplMock,"123");
            Mockito.verify(userStaffHelperMock).updateProForAccountDeletion(user1,1L);
            Mockito.verify(userStaffHelperMock).updateProForAccountDeletion(user2,1L);
            dcmUtilMockedStatic.verify(()->DcmUtil.removeUserFromAccount("accID","1"));
            dcmUtilMockedStatic.verify(()->DcmUtil.removeUserFromAccount("accID","2"));
            rtmServiceMockedStatic.verify(()->RTMService.publishToAW("accID","1","DELETE_ACCOUNT",null,null));
            rtmServiceMockedStatic.verify(()->RTMService.publishToAW("accID","2","DELETE_ACCOUNT",null,null));
            accountEmailHelperMockedStatic.verify(()->AccountEmailHelper.sendAccountDeletionEmailForStaff(eq("subject"),eq(user1),eq("company"),eq("adminEmail"),any(EmailService.class)));
            accountEmailHelperMockedStatic.verify(()->AccountEmailHelper.sendAccountDeletionEmailForStaff(eq("subject"),eq(user2),eq("company"),eq("adminEmail"),any(EmailService.class)));
            Mockito.verify(userImplMock).savePros(List.of(user1,user2));
            accessManagerMockedStatic.verify(()->AccessManager.deleteAccessPolicies(Set.of("p1","p2")));
            Mockito.verify(userStaffFullMetricHelperMock).updateUserDeletionMetric("accID","web_force",2);
        }
    }

    @Test
    void handleClockEntriesOnAccountDeletion_noEntries_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<ClockUtil> clockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class);){
            AccountDTO accountDTO = new AccountDTO();
            accountDTO.setId("accID");
            accountDTO.setDateDeletedTime(1L);
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getAllClockedInEntriesForAccount("accID")).thenReturn(List.of());
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            AccountDeleteTaskHelper.handleClockEntriesOnAccountDeletion(accountDTO);
            clockUtilMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleClockEntriesOnAccountDeletion_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<ClockUtil> clockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class);){
            AccountDTO accountDTO = new AccountDTO();
            accountDTO.setId("accID");
            accountDTO.setDateDeletedTime(1L);
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getAllClockedInEntriesForAccount("accID")).thenReturn(List.of(Map.of(),Map.of()));
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            AccountDeleteTaskHelper.handleClockEntriesOnAccountDeletion(accountDTO);
            clockUtilMockedStatic.verify(()-> ClockUtil.stopRunningEntryForAccountDeletion(Map.of(),1L),Mockito.times(2));
        }
    }
}
