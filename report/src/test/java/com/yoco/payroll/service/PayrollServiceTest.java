package com.yoco.payroll.service;

import com.yoco.adjustment.helper.AdjustmentHelper;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.utils.AccountUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.payroll.helper.PayrollHelper;
import com.yoco.payroll.helper.PayrollTaskInitiator;
import com.yoco.payroll.helper.adminupdate.PayrollAdminUpdateHelper;
import com.yoco.payroll.modal.AdminUpdatePayloadDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.mockito.ArgumentMatchers.*;

class PayrollServiceTest {

    PayrollService payrollService = new PayrollService();

    @Test
    void getUserAcknowledgedEntries_valid_test() throws IOException, NoSuchAlgorithmException {
        Map<String, Object> userConfirmedMap = new HashMap<>();
        userConfirmedMap.put("entries", "entries");
        userConfirmedMap.put(Commons.SUCCESS, true);
        Contact contact = new Contact();
        contact.setId("123");
        contact.setEmailID("emailID");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("id");
        userPro.setUniquepin("123");
        userPro.setTimeZone("Asia/Kolkata");
        userPro.setSkillsets("payrollSkillset");
        try(MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
        });
            MockedConstruction<PayrollHelper> payrollHelperMock = Mockito.mockConstruction(PayrollHelper.class, (payrollHelper, context) -> {
                    Mockito.when(payrollHelper.getUserConfirmedPayrollDetails(anyString(), anyString(), any(), anyString())).thenReturn(userConfirmedMap);
        })
        ) {
            try(MockedStatic<PermissionManager> permissionManagerMockedStatic = Mockito.mockStatic(PermissionManager.class)) {
                permissionManagerMockedStatic.when(() -> PermissionManager.checkUserHasAdjustmentEditAccess(any())).thenReturn(true);
                Map<String, Object> responseMap = new PayrollService().getUserAcknowledgedEntries("accountId", contact, "by_date", "10/12/1970", "10/12/1970", "");
                Assertions.assertEquals(userConfirmedMap, responseMap);
            }
        }
    }

    @Test
    void getAdminUpdatedEntries_valid_test() throws IOException, NoSuchAlgorithmException {
        Map<String, Object> adminUpdatedMap = new HashMap<>();
        adminUpdatedMap.put("adjustments", "adjustments");
        Contact contact = new Contact();
        contact.setId("123");
        contact.setEmailID("emailID");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("id");
        userPro.setUniquepin("123");
        userPro.setTimeZone("Asia/Kolkata");
        userPro.setSkillsets("payrollSkillset");
        try(MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
        });
            MockedConstruction<PayrollHelper> payrollHelperMock = Mockito.mockConstruction(PayrollHelper.class, (payrollHelper, context) -> {
                Mockito.when(payrollHelper.getAdminUpdatedPayrollDetails(anyString(), anyString(), any())).thenReturn(adminUpdatedMap);
            })
        ) {
            try(MockedStatic<PermissionManager> permissionManagerMockedStatic = Mockito.mockStatic(PermissionManager.class)) {
                permissionManagerMockedStatic.when(() -> PermissionManager.checkUserHasAdjustmentEditAccess(any())).thenReturn(true);
                Map<String, Object> responseMap = new PayrollService().getAdminUpdatedEntries("accountId", contact, "by_date", "10/12/1970", "10/12/1970");
                Assertions.assertEquals(adminUpdatedMap, responseMap);
            }
        }
    }

    @Test
    void approvePayrollEvents_no_such_user_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<PayrollHelper> payrollHelperMockedStatic = Mockito.mockStatic(PayrollHelper.class)){
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(null);
            PayrollHelper payrollHelperMock = Mockito.mock(PayrollHelper.class);
            Mockito.doCallRealMethod().when(payrollHelperMock).validateAdminPro(anyString(),any(Contact.class));
            payrollHelperMockedStatic.when(PayrollHelper::getInstance).thenReturn(payrollHelperMock);
            payrollService.approvePayrollEvents("accId","contId",new Contact(),"payload");
        } catch (Exception e) {
           Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void approvePayrollEvents_no_access_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<PayrollHelper> payrollHelperMockedStatic = Mockito.mockStatic(PayrollHelper.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForStaff("accId",false)));
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(userPro);
            PayrollHelper payrollHelperMock = Mockito.mock(PayrollHelper.class);
            Mockito.doCallRealMethod().when(payrollHelperMock).validateAdminPro(anyString(),any(Contact.class));
            payrollHelperMockedStatic.when(PayrollHelper::getInstance).thenReturn(payrollHelperMock);
            payrollService.approvePayrollEvents("accId","contId",new Contact(),"payload");
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void approvePayrollEvents_no_events_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<PayrollHelper> payrollHelperMockedStatic = Mockito.mockStatic(PayrollHelper.class);
            MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)) {

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForAdmin()));
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserPro(anyString(), anyString())).thenReturn(userPro);

            PayrollHelper payrollHelperMock = Mockito.mock(PayrollHelper.class);
            Mockito.doCallRealMethod().when(payrollHelperMock).validateAdminPro(anyString(), any(Contact.class));
            Mockito.doCallRealMethod().when(payrollHelperMock).validateAndExtractApprovalEntryIDsFromPayload(anyString());
            Mockito.when(payrollHelperMock.getPayrollEvents(anyList())).thenReturn(List.of());
            payrollHelperMockedStatic.when(PayrollHelper::getInstance).thenReturn(payrollHelperMock);

            accountUtilMockedStatic.when(()-> AccountUtil.validateAndExtractAccount(anyString())).thenReturn(new SettingsJDO());

            Map<String,Object> payload = new HashMap<>();
            payload.put("entryID","id1,id2");
            Contact contact = new Contact();
            contact.setId("contId");
            Assertions.assertEquals(new HashMap<>(),payrollService.approvePayrollEvents("accId","contId",contact,JsonUtil.getJson(payload)));
        }
    }

    @Test
    void approvePayrollEvents_invalid_events_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<PayrollHelper> payrollHelperMockedStatic = Mockito.mockStatic(PayrollHelper.class);
            MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)) {

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForAdmin()));
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserPro(anyString(), anyString())).thenReturn(userPro);

            PayrollHelper payrollHelperMock = Mockito.mock(PayrollHelper.class);
            Mockito.doCallRealMethod().when(payrollHelperMock).validateAdminPro(anyString(), any(Contact.class));
            Mockito.doCallRealMethod().when(payrollHelperMock).validateAndExtractApprovalEntryIDsFromPayload(anyString());
            Mockito.when(payrollHelperMock.getPayrollEvents(anyList())).thenReturn(List.of(Map.of("id1","value1","id2","value2")));
            Mockito.when(payrollHelperMock.validateAndApprovePayrollEvents(anyList(),any(SettingsJDO.class),anyString())).thenReturn(List.of());
            payrollHelperMockedStatic.when(PayrollHelper::getInstance).thenReturn(payrollHelperMock);

            accountUtilMockedStatic.when(()-> AccountUtil.validateAndExtractAccount(anyString())).thenReturn(new SettingsJDO());

            Map<String,Object> payload = new HashMap<>();
            payload.put("entryID","id1,id2");
            Contact contact = new Contact();
            contact.setId("contId");
            Assertions.assertEquals(new HashMap<>(),payrollService.approvePayrollEvents("accId","contId",contact,JsonUtil.getJson(payload)));
        }
    }

    @Test
    void approvePayrollEvents_valid_events_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<PayrollHelper> payrollHelperMockedStatic = Mockito.mockStatic(PayrollHelper.class);
            MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class);
            MockedStatic<PayrollTaskInitiator> payrollTaskInitiatorMockedStatic = Mockito.mockStatic(PayrollTaskInitiator.class)) {

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForAdmin()));
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserPro(anyString(), anyString())).thenReturn(userPro);

            PayrollHelper payrollHelperMock = Mockito.mock(PayrollHelper.class);
            Mockito.doCallRealMethod().when(payrollHelperMock).validateAdminPro(anyString(), any(Contact.class));
            Mockito.doCallRealMethod().when(payrollHelperMock).validateAndExtractApprovalEntryIDsFromPayload(anyString());
            Mockito.when(payrollHelperMock.getPayrollEvents(anyList())).thenReturn(List.of(Map.of("id1","value1","id2","value2")));
            Mockito.when(payrollHelperMock.validateAndApprovePayrollEvents(anyList(),any(SettingsJDO.class),anyString())).thenReturn(List.of("id1","id2"));
            payrollHelperMockedStatic.when(PayrollHelper::getInstance).thenReturn(payrollHelperMock);

            SettingsJDO account = new SettingsJDO();
            account.setDisplayTimeFormat("HH MM");
            accountUtilMockedStatic.when(()-> AccountUtil.validateAndExtractAccount(anyString())).thenReturn(account);

            payrollTaskInitiatorMockedStatic.when(()-> PayrollTaskInitiator.initiateApprovePayrollQueue(any(Contact.class),anyList(),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> payload = new HashMap<>();
            payload.put("entryID","id1,id2");
            Contact contact = new Contact();
            contact.setId("contId");
            Assertions.assertEquals(new HashMap<>(){{put("eventIds",List.of("id1","id2"));}},payrollService.approvePayrollEvents("accId","contId",contact,JsonUtil.getJson(payload)));

            payrollTaskInitiatorMockedStatic.verify(()-> PayrollTaskInitiator.initiateApprovePayrollQueue(contact,List.of("id1","id2"),"HH MM"));
        }
    }

    @Test
    void updateUserVerifiedEntries_userNotAuthorizedForDeletedContact_test() throws IOException, NoSuchAlgorithmException {
        Map<String, Object> userVerifiedMap = new HashMap<>();
        userVerifiedMap.put("entry", "entry");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("contactID");
        userPro.setUniquepin("123");
        userPro.setTimeZone("Asia/Kolkata");
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put("entryID", "entryID");
        Contact contact = new Contact();
        contact.setId("contactID");
        contact.setEmailID("new@gmail.com");
        contact.setFirstName("first");
        contact.setLastName("last");
        SettingsJDO account = new SettingsJDO();
        account.setPeopleUniquePin("123");
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
        });
            MockedConstruction<PayrollHelper> payrollHelperMock = Mockito.mockConstruction(PayrollHelper.class, (payrollHelper, context) -> {
                Mockito.when(payrollHelper.updateEntriesWithUserVerifiedPaymentStatus(anyString(), anyString(), anyString(), any())).thenReturn(userVerifiedMap);
            })
        ) {
            userPROUtilMockedStatic.when(() -> UserPROUtil.isUserProActive(any())).thenReturn(false);
            Map<String, Object> responseMap = new PayrollService().updateUserVerifiedEntries("accountID", "contactID", "entryID", contact);
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void updateUserVerifiedEntries_userNotAuthorizedForInvalidContactID_test() throws IOException, NoSuchAlgorithmException {
        Map<String, Object> userVerifiedMap = new HashMap<>();
        userVerifiedMap.put("entry", "entry");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("contactID");
        userPro.setUniquepin("123");
        userPro.setTimeZone("Asia/Kolkata");
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put("entryID", "entryID");
        Contact contact = new Contact();
        contact.setId("contactID");
        contact.setEmailID("new@gmail.com");
        contact.setFirstName("first");
        contact.setLastName("last");
        SettingsJDO account = new SettingsJDO();
        account.setPeopleUniquePin("123");
        try(MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
        });
            MockedConstruction<AccountImpl> accountMock = Mockito.mockConstruction(AccountImpl.class, (accountImplMock, context) -> {
                Mockito.when(accountImplMock.getById(anyString())).thenReturn(account);
            });
            MockedConstruction<PayrollHelper> payrollHelperMock = Mockito.mockConstruction(PayrollHelper.class, (payrollHelper, context) -> {
                Mockito.when(payrollHelper.updateEntriesWithUserVerifiedPaymentStatus(anyString(), anyString(), anyString(), any())).thenReturn(userVerifiedMap);
            })
        ) {
            Map<String, Object> responseMap = new PayrollService().updateUserVerifiedEntries("accountID", "contact", "entryID", contact);
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void updateUserVerifiedEntries_valid_test() throws IOException, NoSuchAlgorithmException {
        Map<String, Object> userVerifiedMap = new HashMap<>();
        userVerifiedMap.put("entry", "entry");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("contactID");
        userPro.setUniquepin("123");
        userPro.setTimeZone("Asia/Kolkata");
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put("entryID", "entryID");
        Contact contact = new Contact();
        contact.setId("contactID");
        contact.setEmailID("new@gmail.com");
        contact.setFirstName("first");
        contact.setLastName("last");
        SettingsJDO account = new SettingsJDO();
        account.setPeopleUniquePin("123");
        try(MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
        });
            MockedConstruction<AccountImpl> accountMock = Mockito.mockConstruction(AccountImpl.class, (accountImplMock, context) -> {
                Mockito.when(accountImplMock.getById(anyString())).thenReturn(account);
            });
            MockedConstruction<PayrollHelper> payrollHelperMock = Mockito.mockConstruction(PayrollHelper.class, (payrollHelper, context) -> {
                Mockito.when(payrollHelper.updateEntriesWithUserVerifiedPaymentStatus(anyString(), anyString(), anyString(), any())).thenReturn(userVerifiedMap);
            })
        ) {
            Map<String, Object> responseMap = new PayrollService().updateUserVerifiedEntries("accountID", "contactID", "entryID", contact);
            Assertions.assertEquals(userVerifiedMap, responseMap);
        }
    }

    @Test
    void adminUpdateEntry_OverlapResponseMapNotEmpty_shouldReturnResponseFalse() throws IOException, NoSuchAlgorithmException {
        Contact loggedInContact = new Contact();
        PeopleRelationJDO requesterPro = new PeopleRelationJDO();
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        try(MockedStatic<AdjustmentHelper> adjustmentHelperMockedStatic = Mockito.mockStatic(AdjustmentHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedConstruction<AdminUpdatePayloadDTO> adminUpdatePayloadDTOMockedConstruction = Mockito.mockConstruction(AdminUpdatePayloadDTO.class);
            MockedConstruction<PayrollAdminUpdateHelper> payrollAdminUpdateHelperMockedConstruction = Mockito.mockConstruction(PayrollAdminUpdateHelper.class,((mock, context) -> {
                Mockito.when(mock.checkAndGenerateOverlapResponseForAdjustedTime(eq(requesterPro),eq(userPro),any(AdminUpdatePayloadDTO.class))).thenReturn(Map.of("message","overlap"));
            }))){
            adjustmentHelperMockedStatic.when(()->AdjustmentHelper.validateAndExtractLoggedInPro("accID",loggedInContact)).thenReturn(requesterPro);
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserProWithContact("accID","123")).thenReturn(userPro);
            GenericResponse actual = payrollService.adminUpdateEntry("accID","123","eID","",loggedInContact);
            GenericResponse expected = new GenericResponse();
            expected.setData(Map.of("message","overlap"));
            Assertions.assertEquals(expected,actual);
            Assertions.assertFalse(actual.isSuccess());
        }
    }

    @Test
    void adminUpdateEntry_OverlapResponseMapEmpty_shouldReturnUpdatedEventResponse() throws IOException, NoSuchAlgorithmException {
        Contact loggedInContact = new Contact();
        PeopleRelationJDO requesterPro = new PeopleRelationJDO();
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        try(MockedStatic<AdjustmentHelper> adjustmentHelperMockedStatic = Mockito.mockStatic(AdjustmentHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedConstruction<AdminUpdatePayloadDTO> adminUpdatePayloadDTOMockedConstruction = Mockito.mockConstruction(AdminUpdatePayloadDTO.class,((mock,context)->{
                Mockito.when(mock.getEvent()).thenReturn(Map.of("id","pID"));
            }));
            MockedConstruction<ReportImpl> reportMockedConstruction = Mockito.mockConstruction(ReportImpl.class);
            MockedConstruction<PayrollAdminUpdateHelper> payrollAdminUpdateHelperMockedConstruction = Mockito.mockConstruction(PayrollAdminUpdateHelper.class,((mock, context) -> {
                Mockito.when(mock.checkAndGenerateOverlapResponseForAdjustedTime(eq(requesterPro),eq(userPro),any(AdminUpdatePayloadDTO.class))).thenReturn(Map.of());
                Mockito.when(mock.createAdminUpdatedEventAndGenerateResponse(eq(requesterPro),eq(userPro),any(AdminUpdatePayloadDTO.class))).thenReturn(new GenericResponse(true,null,null));
            }))){
            adjustmentHelperMockedStatic.when(()->AdjustmentHelper.validateAndExtractLoggedInPro("accID",loggedInContact)).thenReturn(requesterPro);
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserProWithContact("accID","123")).thenReturn(userPro);
            GenericResponse actual = payrollService.adminUpdateEntry("accID","123","eID","",loggedInContact);
            GenericResponse expected = new GenericResponse(true,null,null);
            Assertions.assertEquals(expected,actual);
            Mockito.verify(reportMockedConstruction.constructed().get(0)).deleteEvents(List.of("pID"));
        }
    }
}
