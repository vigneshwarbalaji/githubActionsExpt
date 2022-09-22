package com.yoco.user.service;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.MockPRO;
import com.yoco.account.enums.ACCOUNT_ERROR_MESSAGE;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.enums.error.DCM_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.constants.CommonConstants;
import com.yoco.user.helper.UserTaskInitiator;
import com.yoco.user.helper.staff.UserStaffDCMHelper;
import com.yoco.user.helper.staff.UserStaffHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import static com.yoco.user.service.UserStaffService.CONTACT_LIST;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

class UserStaffServiceTest {

    @Test
    void createUserPRO_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setIsPayrollEnabled(false);
            settingsJDO.setTimeZone(DateConstants.ZONE_ID_IST);
            settingsJDO.setTimeZoneDisplayName("(GMT+05:30) Asia/Kolkata (India Time)");

            AccountImpl account = Mockito.mock(AccountImpl.class);
            Mockito.when(account.getById(anyString())).thenReturn(settingsJDO);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(account);

            PeopleRelationJDO user = new PeopleRelationJDO();
            user.setContactId("pContactId");
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(null);
            userPROUtilMockedStatic.when(() -> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(user);

            Contact contactObj = new Contact();
            contactObj.setEmailID("test@gmail.com");
            ContactImpl contact = Mockito.mock(ContactImpl.class);
            Mockito.when(contact.getByID(anyString())).thenReturn(contactObj);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contact);

            UserImpl userImpl = Mockito.mock(UserImpl.class);
            Mockito.when(userImpl.savePro(any(PeopleRelationJDO.class))).thenReturn(user);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImpl);

            AccessPolicy accessPolicy = new AccessPolicy();
            AccessManager accessManager = Mockito.mock(AccessManager.class);
            Mockito.when(accessManager.createNewPolicyForMember(anyString(),anyString(),anyBoolean())).thenReturn(accessPolicy);
            accessManagerMockedStatic.when(AccessManager::getInstance).thenReturn(accessManager);

            Map<String,Object> resp = new UserStaffService().createUserPRO("accountId","contactId");

            Map<String,Object> responseMap = new HashMap<>();
            responseMap.put(CommonConstants.USER_KEY,user);
            responseMap.put(CommonConstants.POLICY,accessPolicy);
            Assertions.assertEquals(responseMap,resp);

            Mockito.verify(account, times(1)).getById("accountId");
            userPROUtilMockedStatic.verify(() ->  UserPROUtil.getUserPro("accountId","contactId"),times(1));
            userPROUtilMockedStatic.verify(() ->  UserPROUtil.getPrimaryAdmin("accountId"),times(1));
            Mockito.verify(contact, times(1)).getByID("contactId");

            Mockito.verify(userImpl, times(1)).savePro(any(PeopleRelationJDO.class));
            Mockito.verify(accessManager, times(1)).createNewPolicyForMember("accountId","contactId",false);
        }
    }

    @Test
    void createUserPRO_nullContact_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);){

            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setIsPayrollEnabled(false);
            settingsJDO.setTimeZone(DateConstants.ZONE_ID_IST);
            settingsJDO.setTimeZoneDisplayName("(GMT+05:30) Asia/Kolkata (India Time)");

            AccountImpl account = Mockito.mock(AccountImpl.class);
            Mockito.when(account.getById(anyString())).thenReturn(settingsJDO);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(account);

            PeopleRelationJDO user = new PeopleRelationJDO();
            user.setContactId("pContactId");
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(null);
            userPROUtilMockedStatic.when(() -> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(user);

            ContactImpl contact = Mockito.mock(ContactImpl.class);
            Mockito.when(contact.getByID(anyString())).thenReturn(null);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contact);

            Map<String,Object> resp = new UserStaffService().createUserPRO("accountId","contactId");

            Map<String,Object> responseMap = new HashMap<>();
            Assertions.assertEquals(responseMap,resp);

            Mockito.verify(account, times(1)).getById("accountId");
            userPROUtilMockedStatic.verify(() ->  UserPROUtil.getUserPro("accountId","contactId"),times(1));
            userPROUtilMockedStatic.verify(() ->  UserPROUtil.getPrimaryAdmin("accountId"),times(1));
            Mockito.verify(contact, times(1)).getByID("contactId");
        }
    }

    @Test
    void createUserPRO_nullPrimaryAdmin_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setIsPayrollEnabled(false);
            settingsJDO.setTimeZone(DateConstants.ZONE_ID_IST);
            settingsJDO.setTimeZoneDisplayName("(GMT+05:30) Asia/Kolkata (India Time)");

            AccountImpl account = Mockito.mock(AccountImpl.class);
            Mockito.when(account.getById(anyString())).thenReturn(settingsJDO);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(account);

            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(null);
            userPROUtilMockedStatic.when(() -> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(null);

            Map<String,Object> resp = new UserStaffService().createUserPRO("accountId","contactId");

            Map<String,Object> responseMap = new HashMap<>();
            Assertions.assertEquals(responseMap,resp);

            Mockito.verify(account, times(1)).getById("accountId");
            userPROUtilMockedStatic.verify(() ->  UserPROUtil.getUserPro("accountId","contactId"),times(1));
            userPROUtilMockedStatic.verify(() ->  UserPROUtil.getPrimaryAdmin("accountId"),times(1));
        }
    }

    @Test
    void createUserPRO_userExists_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setIsPayrollEnabled(false);
            settingsJDO.setTimeZone(DateConstants.ZONE_ID_IST);
            settingsJDO.setTimeZoneDisplayName("(GMT+05:30) Asia/Kolkata (India Time)");

            AccountImpl account = Mockito.mock(AccountImpl.class);
            Mockito.when(account.getById(anyString())).thenReturn(settingsJDO);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(account);

            PeopleRelationJDO user = new PeopleRelationJDO();
            user.setContactId("pContactId");
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(user);

            Map<String,Object> resp = new UserStaffService().createUserPRO("accountId","contactId");

            Map<String,Object> responseMap = new HashMap<>();
            responseMap.put(Commons.MESSAGE,"user already exists");
            Assertions.assertEquals(responseMap,resp);

            Mockito.verify(account, times(1)).getById("accountId");
            userPROUtilMockedStatic.verify(() ->  UserPROUtil.getUserPro("accountId","contactId"),times(1));
        }
    }

    @Test
    void createUserPRO_noAccount_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){

            AccountImpl account = Mockito.mock(AccountImpl.class);
            Mockito.when(account.getById(anyString())).thenReturn(null);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(account);

            Map<String,Object> resp = new UserStaffService().createUserPRO("accountId","contactId");

            Map<String,Object> responseMap = new HashMap<>();
            Assertions.assertEquals(responseMap,resp);

            Mockito.verify(account, times(1)).getById("accountId");
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void createUser_empty_payload_test(String testValue){
        try{
            new UserStaffService().createUser("accountId",null,testValue,true);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

   @Test
    void createUser_invalid_payload_test(){
        try{
            new UserStaffService().createUser("accountId",null,"payload",true);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void createUser_not_an_admin_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(new PeopleRelationJDO());
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenReturn(false);

            Map<String,Object> payload = new HashMap<>();
            payload.put(ContactConstants.EMAIL_ID,"test@gmail.com");
            payload.put(ContactConstants.FIRST_NAME,"fName");

            Contact currentUser = new Contact();
            currentUser.setId("contactId");

            new UserStaffService().createUser("accountId",currentUser, JsonUtil.getJson(payload),true);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void createUser_invalid_account_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class)){

            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(new PeopleRelationJDO());
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenReturn(true);

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(Commons.SUCCESS,false);
            mockResp.put(Commons.MESSAGE, ACCOUNT_ERROR_MESSAGE.MAX_USERS_COUNT_LIMIT.value());

            UserStaffHelper userStaffHelper = Mockito.mock(UserStaffHelper.class);
            Mockito.when(userStaffHelper.validateUserAccountCheck(anyString(),anyMap())).thenReturn(mockResp);
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelper);

            Map<String,Object> payload = new HashMap<>();
            payload.put(ContactConstants.EMAIL_ID,"test@gmail.com");
            payload.put(ContactConstants.FIRST_NAME,"fName");

            Contact currentUser = new Contact();
            currentUser.setId("contactId");

            Map<String,Object> response = new UserStaffService().createUser("accountId",currentUser, JsonUtil.getJson(payload),true);

            Assertions.assertEquals(mockResp,response);
        }
    }

    @Test
    void createUser_invalid_dcmResponse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<UserStaffDCMHelper> userStaffDCMHelperMockedStatic = Mockito.mockStatic(UserStaffDCMHelper.class)){

            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(new PeopleRelationJDO());
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenReturn(true);

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(Commons.SUCCESS,true);
            mockResp.put(ContactConstants.EMAIL_ID, "test@gmail.com");
            mockResp.put(CommonConstants.ACCOUNT_KEY, new SettingsJDO());

            UserStaffHelper userStaffHelper = Mockito.mock(UserStaffHelper.class);
            Mockito.when(userStaffHelper.validateUserAccountCheck(anyString(),anyMap())).thenReturn(mockResp);
            Mockito.when(userStaffHelper.processUserCreation(anyMap(),any(Contact.class),any(SettingsJDO.class))).thenCallRealMethod();
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelper);

            Map<String,Object> mockDcmResp = new HashMap<>();
            mockDcmResp.put(Commons.SUCCESS,false);
            mockDcmResp.put(Commons.ERROR_RESPONSE, DCM_ERROR_RESPONSE.NOT_ALLOWED_IN_DCM.value());

            UserStaffDCMHelper userStaffDCMHelper = Mockito.mock(UserStaffDCMHelper.class);
            Mockito.when(userStaffDCMHelper.validateAndExtractContactFromDCM(anyString(),anyString(),anyMap())).thenReturn(mockDcmResp);
            userStaffDCMHelperMockedStatic.when(UserStaffDCMHelper::getInstance).thenReturn(userStaffDCMHelper);

            Map<String,Object> payload = new HashMap<>();
            payload.put(ContactConstants.EMAIL_ID,"test@gmail.com");
            payload.put(ContactConstants.FIRST_NAME,"fName");

            Contact currentUser = new Contact();
            currentUser.setId("contactId");

            Map<String,Object> response = new UserStaffService().createUser("accountId",currentUser, JsonUtil.getJson(payload),false);

            Assertions.assertEquals(mockDcmResp,response);
        }
    }

    @Test
    void createUser_pro_creation_failure_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<UserStaffDCMHelper> userStaffDCMHelperMockedStatic = Mockito.mockStatic(UserStaffDCMHelper.class)){

            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(new PeopleRelationJDO());
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenReturn(true);

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(Commons.SUCCESS,true);
            mockResp.put(ContactConstants.EMAIL_ID, "test@gmail.com");
            mockResp.put(CommonConstants.ACCOUNT_KEY, new SettingsJDO());

            UserStaffHelper userStaffHelper = Mockito.mock(UserStaffHelper.class);
            Mockito.when(userStaffHelper.validateUserAccountCheck(anyString(),anyMap())).thenReturn(mockResp);
            Mockito.when(userStaffHelper.createUser(any(DcmContactDTO.class),any(SettingsJDO.class),anyBoolean())).thenReturn(null);
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelper);

            Map<String,Object> mockDcmResp = new HashMap<>();
            mockDcmResp.put(Commons.SUCCESS,true);
            mockDcmResp.put("markAsDefault",false);
            mockDcmResp.put("dcmContact",new DcmContactDTO());
            mockDcmResp.put("isNewUser",false);

            UserStaffDCMHelper userStaffDCMHelper = Mockito.mock(UserStaffDCMHelper.class);
            Mockito.when(userStaffDCMHelper.validateAndExtractContactFromDCM(anyString(),anyString(),anyMap())).thenReturn(mockDcmResp);
            userStaffDCMHelperMockedStatic.when(UserStaffDCMHelper::getInstance).thenReturn(userStaffDCMHelper);

            Map<String,Object> payload = new HashMap<>();
            payload.put(ContactConstants.EMAIL_ID,"test@gmail.com");
            payload.put(ContactConstants.FIRST_NAME,"fName");

            Contact currentUser = new Contact();
            currentUser.setId("contactId");

            Map<String,Object> response = new UserStaffService().createUser("accountId",currentUser, JsonUtil.getJson(payload),true);

            Assertions.assertEquals(new HashMap<>(),response);
        }
    }

    @Test
    void createUser_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<UserStaffDCMHelper> userStaffDCMHelperMockedStatic = Mockito.mockStatic(UserStaffDCMHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(new PeopleRelationJDO());
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenReturn(true);

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(Commons.SUCCESS,true);
            mockResp.put(ContactConstants.EMAIL_ID, "test@gmail.com");
            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setIsPayrollEnabled(false);
            settingsJDO.setDomainName("accountA");
            mockResp.put(CommonConstants.ACCOUNT_KEY, settingsJDO);

            PeopleRelationJDO userPro = MockPRO.getMockPRO();

            UserStaffHelper userStaffHelper = Mockito.mock(UserStaffHelper.class);
            Mockito.when(userStaffHelper.validateUserAccountCheck(anyString(),anyMap())).thenReturn(mockResp);
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelper);

            AccessManager accessManager = Mockito.mock(AccessManager.class);
            AccessPolicy policy = new AccessPolicy();
            policy.setPermissions(PermissionManager.getMemberPermissions(false,"accountId"));

            Mockito.when(accessManager.createNewPolicyForMember(anyString(),anyString(),anyBoolean())).thenReturn(policy);
            accessManagerMockedStatic.when(AccessManager::getInstance).thenReturn(accessManager);

            Map<String,Object> mockDcmResp = new HashMap<>();
            mockDcmResp.put(Commons.SUCCESS,true);
            mockDcmResp.put("markAsDefault",false);
            mockDcmResp.put("dcmContact",new DcmContactDTO());
            mockDcmResp.put("isNewUser",false);

            UserStaffDCMHelper userStaffDCMHelper = Mockito.mock(UserStaffDCMHelper.class);
            Mockito.when(userStaffDCMHelper.validateAndExtractContactFromDCM(anyString(),anyString(),anyMap())).thenReturn(mockDcmResp);
            userStaffDCMHelperMockedStatic.when(UserStaffDCMHelper::getInstance).thenReturn(userStaffDCMHelper);

            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> mockResponse = new HashMap<>();
            mockResponse.put(Commons.SUCCESS,true);
            mockResponse.put("accessPolicy",policy);
            mockResponse.put(CommonConstants.USER_KEY, new UserDTO(userPro,policy.getPermissions()));

            Mockito.when(userStaffHelper.processUserCreation(anyMap(),any(Contact.class),any(SettingsJDO.class))).thenReturn(mockResponse);

            Map<String,Object> payload = new HashMap<>();
            payload.put(ContactConstants.EMAIL_ID,"test@gmail.com");
            payload.put(ContactConstants.FIRST_NAME,"fName");

            Contact currentUser = new Contact();
            currentUser.setId("contactId");

            Map<String,Object> response = new UserStaffService().createUser("accountId",currentUser, JsonUtil.getJson(payload),true);

            Assertions.assertEquals(mockResponse,response);
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void importUser_empty_payload_test(String testValue){
        try{
            new UserStaffService().importUser("accountId",null,testValue);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void importUser_invalid_payload_test(){
        try{
            new UserStaffService().importUser("accountId",null,"payload");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void importUser_invalid_payloadKey_test(){
        try{
            Map<String,Object> payload = new HashMap<>();
            payload.put(ContactConstants.EMAIL_ID,"test@gmail.com");
            new UserStaffService().importUser("accountId",null, JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void importUser_not_an_admin_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(new PeopleRelationJDO());
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenReturn(false);

            Map<String,Object> payload = new HashMap<>();
            payload.put(CONTACT_LIST,new ArrayList<>());
            payload.put(ContactConstants.EMAIL_ID,"test@gmail.com");
            payload.put(ContactConstants.FIRST_NAME,"fName");

            Contact currentUser = new Contact();
            currentUser.setId("contactId");

            new UserStaffService().importUser("accountId",currentUser, JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }


    @Test
    void importUser_empty_response_test() throws IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(new PeopleRelationJDO());
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenReturn(true);

            Map<String,Object> payload = new HashMap<>();
            payload.put(CONTACT_LIST,new ArrayList<>());

            Contact currentUser = new Contact();
            currentUser.setId("contactId");

            Map<String,Object> response = new UserStaffService().importUser("accountId",currentUser, JsonUtil.getJson(payload));
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        }
    }

    @Test
    void importUser_valid_response_test() throws IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(new PeopleRelationJDO());
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenReturn(true);

            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> payload = new HashMap<>();
            Map<String,Object> map = new HashMap<>();
            map.put("key","data");
            ArrayList<Object> arrayList = new ArrayList<>() {{
                add(map);
            }};
            payload.put(CONTACT_LIST, arrayList);

            Contact currentUser = new Contact();
            currentUser.setId("contactId");

            Map<String,Object> response = new UserStaffService().importUser("accountId",currentUser, JsonUtil.getJson(payload));
            Assertions.assertEquals("Mail will be sent to you shortly" , response.get("message"));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put(CommonConstants.ACTION,"staffImported");
            queueMap.put(CommonConstants.ACCOUNT_ID_KEY,"accountId");
            queueMap.put(ContactConstants.CONTACT,currentUser);
            queueMap.put(CONTACT_LIST, arrayList);

            userTaskInitiatorMockedStatic.verify(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap()));
        }
    }

    @Test
    void activateUser_noAccount_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<AccountImpl> accountUtilMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl accountMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountMock.getById(anyString())).thenReturn(null);
            accountUtilMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountMock);
            Map<String,Object> resp = new UserStaffService().activateUser("accountId","contactId",null);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
            Mockito.verify(accountMock).getById("accountId");
        }
    }

    @Test
    void activateUser_noPro_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<AccountImpl> accountUtilMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            AccountImpl accountMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountMock.getById(anyString())).thenReturn(new SettingsJDO());
            accountUtilMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountMock);

            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(null);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(MockPRO.getMockPRO());
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenReturn(true);

            Contact contact = new Contact();
            contact.setId("contactId1");
            Map<String,Object> resp = new UserStaffService().activateUser("accountId","contactId",contact);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));

            Mockito.verify(accountMock).getById("accountId");
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserProWithContact("accountId","contactId"));
        }
    }

    @Test
    void activateUser_activePro_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<AccountImpl> accountUtilMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            AccountImpl accountMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountMock.getById(anyString())).thenReturn(new SettingsJDO());
            accountUtilMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountMock);

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(false);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenReturn(true);

            Contact contact = new Contact();
            contact.setId("contactId1");
            Map<String,Object> resp = new UserStaffService().activateUser("accountId","contactId",contact);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));

            Mockito.verify(accountMock).getById("accountId");
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserProWithContact("accountId","contactId"));
        }
    }


    @Test
    void activateUser_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<AccountImpl> accountUtilMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){

            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setIsPayrollEnabled(true);
            AccountImpl accountMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountMock.getById(anyString())).thenReturn(settingsJDO);
            accountUtilMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountMock);

            PeopleRelationJDO userPro = MockPRO.getMockPRO();
            userPro.setDelete(true);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenReturn(true);

            UserStaffHelper userStaffHelperMock = Mockito.mock(UserStaffHelper.class);
            Mockito.when(userStaffHelperMock.validateAndSetIsDefault(anyString(),any(PeopleRelationJDO.class))).thenReturn(userPro);
            Mockito.when(userStaffHelperMock.activateUserSkillHelper(any(PeopleRelationJDO.class))).thenReturn(userPro);
            UserDTO userDTO = new UserDTO();
            Mockito.when(userStaffHelperMock.activateUserEventsHandler(any(SettingsJDO.class),any(PeopleRelationJDO.class),anyString(),anyString())).thenReturn(userDTO);
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelperMock);

            dcmUtilMockedStatic.when(()-> DcmUtil.linkContactToAccount(anyString(),anyString())).thenReturn(null);

            Contact contact = new Contact();
            contact.setId("contactId1");
            Map<String,Object> resp = new UserStaffService().activateUser("accountId","contactId",contact);
            Assertions.assertFalse(ObjUtils.isNullOrEmpty(resp));
            Assertions.assertEquals(userDTO,resp.get(CommonConstants.USER_KEY));

            Mockito.verify(accountMock).getById("accountId");
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserProWithContact("accountId","contactId"));
            Mockito.verify(userStaffHelperMock).validateAndSetIsDefault("contactId",userPro);
            Mockito.verify(userStaffHelperMock).activateUserSkillHelper(userPro);
            userPro.setDelete(false);
            Mockito.verify(userStaffHelperMock).activateUserEventsHandler(settingsJDO,userPro,"contactId has been enabled by contactId1","web");
        }
    }

    @Test
    void deleteUser_notAnAdmin_Test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO("accountID","requesterContactID")).thenReturn(null);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(null)).thenReturn(false);
            UserStaffService.getInstance().deleteUser("accountID","contactID","requesterContactID");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void deleteUser_userProNull_Test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO("accountID","requesterContactID")).thenReturn(null);
            userPROUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPROWithContact("accountID","contactID")).thenReturn(null);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(null)).thenReturn(true);
            UserStaffService.getInstance().deleteUser("accountID","contactID","requesterContactID");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value(),e.getMessage());
        }
    }

    @Test
    void deleteUser_userProPrimaryAdmin_Test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO("accountID","requesterContactID")).thenReturn(null);
            userPROUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPROWithContact("accountID","contactID")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(null)).thenReturn(true);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isPrimaryAdmin(userPro)).thenReturn(true);
            UserStaffService.getInstance().deleteUser("accountID","contactID","requesterContactID");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void deleteUser_valid_Test() throws IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO("accountID","requesterContactID")).thenReturn(null);
            userPROUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPROWithContact("accountID","contactID")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(null)).thenReturn(true);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isPrimaryAdmin(userPro)).thenReturn(false);
            UserStaffHelper userStaffHelperMock = Mockito.mock(UserStaffHelper.class);
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelperMock);
            UserStaffService.getInstance().deleteUser("accountID","contactID","requesterContactID");
            verify(userStaffHelperMock).updateAndSaveProForUserDisabling(userPro);
            userTaskInitiatorMockedStatic.verify(()->UserTaskInitiator.initiateDeleteUserQueue(null,userPro,"web"));
        }
    }

}