package com.yoco.user.helper.staff;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.MockPRO;
import com.yoco.account.enums.ACCOUNT_ERROR_MESSAGE;
import com.yoco.commons.constants.*;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.AccountUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.constants.CommonConstants;
import com.yoco.contact.helper.ContactHelper;
import com.yoco.user.enums.USER_ERROR_MESSAGE;
import com.yoco.user.helper.UserTaskInitiator;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class UserStaffHelperTest {

    UserStaffHelper userStaffHelper = UserStaffHelper.getInstance();

    @Test
    void validateUserPayload_ID_test(){
        try{
            Map<String,Object> payload = new HashMap<>();
            payload.put(ContactConstants.ID,"id");
            userStaffHelper.validateUserPayload(payload);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD_CONTAINS_ID.value(),e.getMessage());
        }
    }

    @Test
    void validateUserPayload_noEmailID_test(){
        try{
            userStaffHelper.validateUserPayload(new HashMap<>());
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void validateUserPayload_InvalidEmailID_test(){
        try{
            Map<String,Object> payload = new HashMap<>();
            payload.put(ContactConstants.EMAIL_ID,"email");
            userStaffHelper.validateUserPayload(payload);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.value(),e.getMessage());
        }
    }
    @Test
    void validateUserPayload_noFirstName_test(){
        try{
            Map<String,Object> payload = new HashMap<>();
            payload.put(ContactConstants.EMAIL_ID,"email@gmail.com");
            userStaffHelper.validateUserPayload(payload);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }
    @Test
    void validateUserPayload_InvalidFirstName_test(){
        try{
            Map<String,Object> payload = new HashMap<>();
            payload.put(ContactConstants.EMAIL_ID,"email@gmail.com");
            payload.put(ContactConstants.FIRST_NAME,"");
            userStaffHelper.validateUserPayload(payload);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void validateUserPayload_valid_test(){
        Map<String,Object> payload = new HashMap<>();
        payload.put(ContactConstants.EMAIL_ID,"email@gmail.com");
        payload.put(ContactConstants.FIRST_NAME,"fName");
        Assertions.assertDoesNotThrow(() -> userStaffHelper.validateUserPayload(payload));
    }

    @Test
    void validateAndExtractAccountInfo_noAccount_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl accountMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountMock.getById(anyString())).thenReturn(null);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountMock);
            Map<String,Object> response = userStaffHelper.validateAndExtractAccountInfo("accountId");
            Assertions.assertNotNull(response);
            Assertions.assertFalse((Boolean) response.get(Commons.SUCCESS));
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.ACCOUNT_NOT_FOUND.value(),response.get(Commons.ERROR_RESPONSE));
        }
    }

    @Test
    void validateAndExtractAccountInfo_limitReached_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            AccountImpl accountMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountMock.getById(anyString())).thenReturn(account);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountMock);

            UserStaffHelper userServiceHelper = Mockito.spy(userStaffHelper);
            Mockito.doReturn(false).when(userServiceHelper).isAccountEligibleToAddStaff(Mockito.any());

            Map<String,Object> response = userServiceHelper.validateAndExtractAccountInfo("accountId");
            Assertions.assertNotNull(response);
            Assertions.assertFalse((Boolean) response.get(Commons.SUCCESS));
            Assertions.assertEquals(ACCOUNT_ERROR_MESSAGE.MAX_USERS_COUNT_LIMIT.value(),response.get(Commons.ERROR_RESPONSE));
        }
    }

    @Test
    void validateAndExtractAccountInfo_valid_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            AccountImpl accountMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountMock.getById(anyString())).thenReturn(account);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountMock);

            UserStaffHelper userServiceHelper = Mockito.spy(userStaffHelper);
            Mockito.doReturn(true).when(userServiceHelper).isAccountEligibleToAddStaff(Mockito.any());

            Map<String,Object> response = userServiceHelper.validateAndExtractAccountInfo("accountId");
            Assertions.assertNotNull(response);
            Assertions.assertTrue((Boolean) response.get(Commons.SUCCESS));
            Assertions.assertEquals(account,response.get(CommonConstants.ACCOUNT_KEY));
        }
    }

    @Test
    void validateAndExtractUserInfo_userExists_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO user = new PeopleRelationJDO();
            user.setId("accountId");
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserProByEmailID(anyString(),anyString())).thenReturn(user);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put(ContactConstants.EMAIL_ID,"test@gmail.com");
            Map<String,Object> response = new HashMap<>();
            response = userStaffHelper.validateAndExtractUserInfo("accountId",payloadMap,response);
            Assertions.assertNotNull(response);
            Assertions.assertFalse((Boolean) response.get(Commons.SUCCESS));
            Assertions.assertEquals(USER_ERROR_MESSAGE.EMAIL_ID_EXISTS.value(),response.get(Commons.ERROR_RESPONSE));
            Assertions.assertTrue(response.containsKey(CommonConstants.USER_KEY));
        }
    }

    @Test
    void validateAndExtractUserInfo_noUser_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserProByEmailID(anyString(),anyString())).thenReturn(null);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put(ContactConstants.EMAIL_ID,"test@gmail.com");
            Map<String,Object> response = new HashMap<>();
            response = userStaffHelper.validateAndExtractUserInfo("accountId",payloadMap,response);
            Assertions.assertNotNull(response);
            Assertions.assertTrue((Boolean) response.get(Commons.SUCCESS));
            Assertions.assertEquals("test@gmail.com",response.get(ContactConstants.EMAIL_ID));
        }
    }

    @Test
    void validateUserAccountCheck_valid_test(){
        Map<String,Object> responseMap = new HashMap<>();
        responseMap.put(Commons.SUCCESS,true);
        responseMap.put(CommonConstants.ACCOUNT_KEY, "accountJdo");
        responseMap.put(ContactConstants.EMAIL_ID,"test@gmail.com");
        UserStaffHelper userServiceHelper = Mockito.spy(userStaffHelper);
        Mockito.doReturn(responseMap).when(userServiceHelper).validateAndExtractAccountInfo(Mockito.any());
        Mockito.doReturn(responseMap).when(userServiceHelper).validateAndExtractUserInfo(Mockito.any(),anyMap(),anyMap());

        Map<String,Object> payloadMap = new HashMap<>();
        payloadMap.put(ContactConstants.EMAIL_ID,"test@gmail.com");
        Map<String,Object> response = userServiceHelper.validateUserAccountCheck("accountId",payloadMap);
        Assertions.assertNotNull(response);
        Assertions.assertTrue((Boolean) response.get(Commons.SUCCESS));
        Assertions.assertEquals("accountJdo",response.get(CommonConstants.ACCOUNT_KEY));
        Assertions.assertEquals("test@gmail.com",response.get(ContactConstants.EMAIL_ID));
    }

    @Test
    void validateUserAccountCheck_InvalidAccount_test(){
        Map<String,Object> responseMap = new HashMap<>();
        responseMap.put(Commons.SUCCESS,false);
        responseMap.put(Commons.ERROR_RESPONSE,ACCOUNT_ERROR_MESSAGE.MAX_USERS_COUNT_LIMIT.value());

        UserStaffHelper userServiceHelper = Mockito.spy(userStaffHelper);
        Mockito.doReturn(responseMap).when(userServiceHelper).validateAndExtractAccountInfo(Mockito.any());

        Map<String,Object> payloadMap = new HashMap<>();
        payloadMap.put(ContactConstants.EMAIL_ID,"test@gmail.com");
        Map<String,Object> response = userServiceHelper.validateUserAccountCheck("accountId",payloadMap);
        Assertions.assertNotNull(response);
        Assertions.assertFalse((Boolean) response.get(Commons.SUCCESS));
        Assertions.assertEquals(ACCOUNT_ERROR_MESSAGE.MAX_USERS_COUNT_LIMIT.value(),response.get(Commons.ERROR_RESPONSE));
    }

    @Test
    void isAccountEligibleToAddStaff_unlimitedUsers_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accountUtilMockedStatic.when(() -> AccountUtil.getMaxUsersCount(any(SettingsJDO.class))).thenReturn(AccountConstants.UNLIMITED);
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            Assertions.assertTrue(userStaffHelper.isAccountEligibleToAddStaff(account));
            accountUtilMockedStatic.verify(()-> AccountUtil.getMaxUsersCount(account),times(1));
        }
    }

    @Test
    void isAccountEligibleToAddStaff_noProUsers_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUsers(anyString(),anyBoolean(),anyInt(),anyString())).thenReturn(null);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            accountUtilMockedStatic.when(() -> AccountUtil.getMaxUsersCount(any(SettingsJDO.class))).thenReturn("10");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            Assertions.assertTrue(userStaffHelper.isAccountEligibleToAddStaff(account));
            accountUtilMockedStatic.verify(()-> AccountUtil.getMaxUsersCount(account),times(1));
        }
    }

    @Test
    void isAccountEligibleToAddStaff_noUserKey_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            Map<String,Object> activeUsersMap = new HashMap<>();
            activeUsersMap.put("key","data");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUsers(anyString(),anyBoolean(),anyInt(),anyString())).thenReturn(activeUsersMap);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            accountUtilMockedStatic.when(() -> AccountUtil.getMaxUsersCount(any(SettingsJDO.class))).thenReturn("10");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            Assertions.assertTrue(userStaffHelper.isAccountEligibleToAddStaff(account));
            accountUtilMockedStatic.verify(()-> AccountUtil.getMaxUsersCount(account),times(1));
        }
    }

    @Test
    void isAccountEligibleToAddStaff_emptyList_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            Map<String,Object> activeUsersMap = new HashMap<>();
            List<PeopleRelationJDO> usersList = new ArrayList<>();
            activeUsersMap.put(CommonConstants.USERS_KEY,usersList);
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUsers(anyString(),anyBoolean(),anyInt(),anyString())).thenReturn(activeUsersMap);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            accountUtilMockedStatic.when(() -> AccountUtil.getMaxUsersCount(any(SettingsJDO.class))).thenReturn("10");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            Assertions.assertTrue(userStaffHelper.isAccountEligibleToAddStaff(account));
            accountUtilMockedStatic.verify(()-> AccountUtil.getMaxUsersCount(account),times(1));
        }
    }

    @Test
    void isAccountEligibleToAddStaff_limitLessThanUsers_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            Map<String,Object> activeUsersMap = new HashMap<>();
            List<PeopleRelationJDO> usersList = new ArrayList<>();
            usersList.add(new PeopleRelationJDO());
            activeUsersMap.put(CommonConstants.USERS_KEY,usersList);
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUsers(anyString(),anyBoolean(),anyInt(),anyString())).thenReturn(activeUsersMap);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            accountUtilMockedStatic.when(() -> AccountUtil.getMaxUsersCount(any(SettingsJDO.class))).thenReturn("10");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            Assertions.assertTrue(userStaffHelper.isAccountEligibleToAddStaff(account));
            accountUtilMockedStatic.verify(()-> AccountUtil.getMaxUsersCount(account),times(1));
        }
    }
    @Test
    void isAccountEligibleToAddStaff_limitMoreThanUsers_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            Map<String,Object> activeUsersMap = new HashMap<>();
            List<PeopleRelationJDO> usersList = new ArrayList<>();
            usersList.add(new PeopleRelationJDO());
            usersList.add(new PeopleRelationJDO());
            activeUsersMap.put(CommonConstants.USERS_KEY,usersList);
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUsers(anyString(),anyBoolean(),anyInt(),anyString())).thenReturn(activeUsersMap);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            accountUtilMockedStatic.when(() -> AccountUtil.getMaxUsersCount(any(SettingsJDO.class))).thenReturn("1");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            Assertions.assertFalse(userStaffHelper.isAccountEligibleToAddStaff(account));
            accountUtilMockedStatic.verify(()-> AccountUtil.getMaxUsersCount(account),times(1));
        }
    }

    @Test
    void processUserCreation_success_false_test(){
        UserStaffHelper userStaffHelper = Mockito.mock(UserStaffHelper.class);
        Mockito.doCallRealMethod().when(userStaffHelper).processUserCreation(anyMap(),any(),any());
        Map<String,Object> dcmResp = new HashMap<>();
        dcmResp.put(Commons.SUCCESS,false);
        Assertions.assertEquals(dcmResp,userStaffHelper.processUserCreation(dcmResp,null,null));
    }

    @Test
    void processUserCreation_null_pro_test(){
        UserStaffHelper userStaffHelper = Mockito.mock(UserStaffHelper.class);
        Mockito.doCallRealMethod().when(userStaffHelper).processUserCreation(anyMap(),any(),any());
        Mockito.when(userStaffHelper.createUser(any(DcmContactDTO.class),any(SettingsJDO.class),anyBoolean())).thenReturn(null);
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        Map<String,Object> dcmResp = new HashMap<>();
        dcmResp.put(Commons.SUCCESS,true);
        dcmResp.put("markAsDefault",false);
        dcmResp.put("dcmContact",dcmContactDTO);
        dcmResp.put("isNewUser",false);
        SettingsJDO account = new SettingsJDO();
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(userStaffHelper.processUserCreation(dcmResp,new Contact(),account)));
        Mockito.verify(userStaffHelper).createUser(dcmContactDTO,account,false);
    }

    @Test
    void processUserCreation_valid_test(){
        UserStaffHelper userStaffHelper = Mockito.mock(UserStaffHelper.class);
        Mockito.doCallRealMethod().when(userStaffHelper).processUserCreation(anyMap(),any(),any());
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        Mockito.when(userStaffHelper.createUser(any(DcmContactDTO.class),any(SettingsJDO.class),anyBoolean())).thenReturn(userPro);
        Mockito.when(userStaffHelper.createUserEventsHandler(any(Contact.class),any(SettingsJDO.class),anyBoolean(),anyString(),any(PeopleRelationJDO.class))).thenReturn(Map.of("success",true));
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        dcmContactDTO.setId("userID");
        Map<String,Object> dcmResp = new HashMap<>();
        dcmResp.put(Commons.SUCCESS,true);
        dcmResp.put("markAsDefault",false);
        dcmResp.put("dcmContact",dcmContactDTO);
        dcmResp.put("isNewUser",false);
        SettingsJDO account = new SettingsJDO();
        Contact contact = new Contact();
        contact.setId("adminID");
        Assertions.assertEquals(Map.of("success",true),userStaffHelper.processUserCreation(dcmResp,contact,account));
        Mockito.verify(userStaffHelper).createUser(dcmContactDTO,account,false);
        Mockito.verify(userStaffHelper).createUserEventsHandler(contact,account,false,"userID has  been added by adminID" ,userPro);
    }

    @Test
    void createUser_nullContact_test(){
        try(MockedStatic<ContactHelper> contactHelperMockedStatic = Mockito.mockStatic(ContactHelper.class)){
            ContactHelper contactHelper = Mockito.mock(ContactHelper.class);
            Mockito.when(contactHelper.validateAndCreateContact(any(DcmContactDTO.class))).thenReturn(null);
            contactHelperMockedStatic.when(ContactHelper::getInstance).thenReturn(contactHelper);
            Assertions.assertNull(userStaffHelper.createUser(new DcmContactDTO(),new SettingsJDO(),false));
        }
    }

    @Test
    void createUser_valid_test(){
        try(MockedStatic<ContactHelper> contactHelperMockedStatic = Mockito.mockStatic(ContactHelper.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            Contact contact = new Contact();
            contact.setId("contactId");
            contact.setEmailID("test@gmail.com");
            ContactHelper contactHelper = Mockito.mock(ContactHelper.class);
            Mockito.when(contactHelper.validateAndCreateContact(any(DcmContactDTO.class))).thenReturn(contact);
            contactHelperMockedStatic.when(ContactHelper::getInstance).thenReturn(contactHelper);

            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.savePro(any(PeopleRelationJDO.class))).thenReturn(new PeopleRelationJDO());
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            account.setTimeZone(DateConstants.ZONE_ID_IST);
            account.setTimeZoneDisplayName("(GMT+05:30) Asia/Kolkata (India Time)");
            account.setIsPayrollEnabled(false);
            Assertions.assertNotNull(userStaffHelper.createUser(new DcmContactDTO(),account,false));
            Mockito.verify(contactHelper,times(1)).validateAndCreateContact(any(DcmContactDTO.class));
            Mockito.verify(userMock,times(1)).savePro(any(PeopleRelationJDO.class));
        }
    }

    @Test
    void createUser_exception_test(){
        try(MockedStatic<ContactHelper> contactHelperMockedStatic = Mockito.mockStatic(ContactHelper.class)){
            Contact contact = new Contact();
            contact.setId("contactId");
            contact.setEmailID("test@gmail.com");
            ContactHelper contactHelper = Mockito.mock(ContactHelper.class);
            Mockito.when(contactHelper.validateAndCreateContact(any(DcmContactDTO.class))).thenThrow(new IllegalArgumentException());
            contactHelperMockedStatic.when(ContactHelper::getInstance).thenReturn(contactHelper);

            SettingsJDO account = new SettingsJDO();
            Assertions.assertNull(userStaffHelper.createUser(new DcmContactDTO(),account,false));
            Mockito.verify(contactHelper,times(1)).validateAndCreateContact(any(DcmContactDTO.class));
        }
    }

    @Test
    void generateAddStaffQueuePayload_test(){
        UserDTO userDTO = new UserDTO();
        AccessPolicy policy = new AccessPolicy();
        Contact contact = new Contact();
        Map<String,Object> response = userStaffHelper.generateAddStaffQueuePayload(userDTO,"accountA",contact,policy,false,"activity");

        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"staffAdded");
        queueMap.put("accountName","accountA");
        queueMap.put("adminContact",contact);
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put(CommonConstants.POLICY,policy);
        queueMap.put("isNewContact",false);
        queueMap.put(CommonConstants.ACTIVITY,"activity");

        Assertions.assertEquals(queueMap,response);
    }

    @Test
    void activateUserSkillHelper_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO pro = MockPRO.getMockPRO();
            userPROUtilMockedStatic.when(()-> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(pro);
            PeopleRelationJDO resp = userStaffHelper.activateUserSkillHelper(pro);
            Assertions.assertNotNull(resp);
            userPROUtilMockedStatic.verify(()-> UserPROUtil.extractPROSkills(pro));
            userPROUtilMockedStatic.verify(()-> UserPROUtil.extractDataProtectionSkill(pro));
            Map<String,Object> skillMap = UserPROUtil.extractPROSkills(pro);
            skillMap.put(Skillset.SKILL_SET_KEY_SUB_STATUS, Skillset.SKILLSET_VIEW);
            userPROUtilMockedStatic.verify(()-> UserPROUtil.updatePROSkill(pro,skillMap));
        }
    }

    @Test
    void activateUserSkillHelper_full_In_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO pro = MockPRO.getMockPRO();
            pro.setUniquepin(InternalUsage.FULL);
            userPROUtilMockedStatic.when(()-> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(pro);
            PeopleRelationJDO resp = userStaffHelper.activateUserSkillHelper(pro);
            Assertions.assertNotNull(resp);
            userPROUtilMockedStatic.verify(()-> UserPROUtil.extractPROSkills(pro));
            userPROUtilMockedStatic.verify(()-> UserPROUtil.extractDataProtectionSkill(pro));
            Map<String,Object> skillMap = UserPROUtil.extractPROSkills(pro);
            skillMap.put(Skillset.SKILL_SET_KEY_SUB_STATUS,"");
            userPROUtilMockedStatic.verify(()-> UserPROUtil.updatePROSkill(pro,skillMap));
        }
    }

    @Test
    void validateAndSetIsDefault_valid_defaultPro_test(){
        try(MockedStatic<UserImpl> userPROUtilMockedStatic = Mockito.mockStatic(UserImpl.class)){

            PeopleRelationJDO userDefaultPro = new PeopleRelationJDO();
            userDefaultPro.setDelete(false);
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(userDefaultPro);
            userPROUtilMockedStatic.when(()-> UserImpl.getUserImplInstance()).thenReturn(userImplMock);

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDefault(false);
            PeopleRelationJDO expectedPro = userStaffHelper.validateAndSetIsDefault("contactId",userPro);

            Assertions.assertFalse(expectedPro.isDefault());
        }
    }

    @Test
    void validateAndSetIsDefault_valid_defaultPro_delete_test(){
        try(MockedStatic<UserImpl> userPROUtilMockedStatic = Mockito.mockStatic(UserImpl.class)){

            PeopleRelationJDO userDefaultPro = new PeopleRelationJDO();
            userDefaultPro.setDelete(true);

            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(userDefaultPro);
            userPROUtilMockedStatic.when(()-> UserImpl.getUserImplInstance()).thenReturn(userImplMock);

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDefault(false);
            PeopleRelationJDO expectedPro = userStaffHelper.validateAndSetIsDefault("contactId",userPro);

            Assertions.assertTrue(expectedPro.isDefault());
        }
    }

    @Test
    void validateAndSetIsDefault_null_defaultPro_test(){
        try(MockedStatic<UserImpl> userPROUtilMockedStatic = Mockito.mockStatic(UserImpl.class)){

            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(null);
            userPROUtilMockedStatic.when(()-> UserImpl.getUserImplInstance()).thenReturn(userImplMock);

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDefault(false);
            PeopleRelationJDO expectedPro = userStaffHelper.validateAndSetIsDefault("contactId",userPro);

            Assertions.assertTrue(expectedPro.isDefault());
        }
    }

    @Test
    void activateUserEventsHandler_nullPolicy_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setIsPayrollEnabled(true);

            PeopleRelationJDO userPro = MockPRO.getMockPRO();
            userPro.setUniquepin("accountId");
            userPro.setContactId("contactId");
            userPro.setDelete(true);

            AccessManager accessManagerMock = Mockito.mock(AccessManager.class);
            Mockito.when(accessManagerMock.createNewPolicyForMember(anyString(),anyString(),anyBoolean())).thenReturn(null);
            accessManagerMockedStatic.when(AccessManager::getInstance).thenReturn(accessManagerMock);

            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            UserDTO resp = userStaffHelper.activateUserEventsHandler(settingsJDO,userPro,"activity","src");
            Assertions.assertNotNull(resp);
            Assertions.assertEquals(new UserDTO(userPro,null),resp);
            Mockito.verify(accessManagerMock).createNewPolicyForMember("accountId","contactId",true);
            userTaskInitiatorMockedStatic.verify(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap()));
        }
    }

    @Test
    void activateUserEventsHandler_emptyPolicyPermissions_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setIsPayrollEnabled(true);

            PeopleRelationJDO userPro = MockPRO.getMockPRO();
            userPro.setUniquepin("accountId");
            userPro.setContactId("contactId");
            userPro.setDelete(true);

            AccessPolicy policy = new AccessPolicy();
            AccessManager accessManagerMock = Mockito.mock(AccessManager.class);
            Mockito.when(accessManagerMock.createNewPolicyForMember(anyString(),anyString(),anyBoolean())).thenReturn(policy);
            accessManagerMockedStatic.when(AccessManager::getInstance).thenReturn(accessManagerMock);

            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            UserDTO resp = userStaffHelper.activateUserEventsHandler(settingsJDO,userPro,"activity","activitySrc");
            Assertions.assertNotNull(resp);
            Assertions.assertEquals(new UserDTO(userPro,null),resp);
            Mockito.verify(accessManagerMock).createNewPolicyForMember("accountId","contactId",true);
            userTaskInitiatorMockedStatic.verify(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap()));
        }
    }

    @Test
    void activateUserEventsHandler_validPolicyPermissions_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setIsPayrollEnabled(true);

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accountId");
            userPro.setContactId("contactId");
            userPro.setDelete(true);

            AccessPolicy policy = new AccessPolicy();
            Set<String> permissions = PermissionManager.getMemberPermissions(false, "accountId");
            policy.setPermissions(permissions);
            AccessManager accessManagerMock = Mockito.mock(AccessManager.class);
            Mockito.when(accessManagerMock.createNewPolicyForMember(anyString(),anyString(),anyBoolean())).thenReturn(policy);
            accessManagerMockedStatic.when(AccessManager::getInstance).thenReturn(accessManagerMock);

            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            UserDTO resp = userStaffHelper.activateUserEventsHandler(settingsJDO,userPro,"activity","activitySrc");
            Assertions.assertNotNull(resp);
            Assertions.assertEquals(new UserDTO(userPro,permissions),resp);

            Mockito.verify(accessManagerMock).createNewPolicyForMember("accountId","contactId",true);
            userTaskInitiatorMockedStatic.verify(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap()));
        }
    }

    @Test
    void activateUserEventsHandler_exception_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setIsPayrollEnabled(true);

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accountId");
            userPro.setContactId("contactId");
            userPro.setDelete(true);

            AccessPolicy policy = new AccessPolicy();
            Set<String> permissions = PermissionManager.getMemberPermissions(false, "accountId");
            policy.setPermissions(permissions);
            AccessManager accessManagerMock = Mockito.mock(AccessManager.class);
            Mockito.when(accessManagerMock.createNewPolicyForMember(anyString(),anyString(),anyBoolean())).thenReturn(policy);
            accessManagerMockedStatic.when(AccessManager::getInstance).thenReturn(accessManagerMock);

            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap())).thenThrow(new IOException("exception"));

            UserDTO resp = userStaffHelper.activateUserEventsHandler(settingsJDO,userPro,"activity","activitySrc");
            Assertions.assertNull(resp);

            Mockito.verify(accessManagerMock).createNewPolicyForMember("accountId","contactId",true);
            userTaskInitiatorMockedStatic.verify(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap()));
        }
    }

    @Test
    void generateActivateStaffQueuePayload_test(){
        UserDTO userDTO = new UserDTO();
        AccessPolicy policy = new AccessPolicy();
        Map<String,Object> response = userStaffHelper.generateActivateStaffQueuePayload(userDTO,policy,"activity","activitySrc");
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"staffEnabled");
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put(CommonConstants.POLICY,policy);
        queueMap.put(CommonConstants.ACTIVITY,"activity");
        queueMap.put(CommonConstants.SOURCE,"activitySrc");
        Assertions.assertEquals(queueMap,response);
    }

    @Test
    void updateAndSaveProForUserDisabling_nullPro_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            new UserStaffHelper().updateAndSaveProForUserDisabling(null);
            userPROUtilMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void updateAndSaveProForUserDisabling_validPro_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            new UserStaffHelper().updateAndSaveProForUserDisabling(userPRO);
            Assertions.assertTrue(userPRO.isDelete());
            Assertions.assertFalse(userPRO.isDefault());
            Assertions.assertEquals("",userPRO.getRfId());
            Assertions.assertEquals(Skillset.ROLE_STAFF, userPRO.getRole());
            Assertions.assertEquals(Skillset.generateSkillsetForDeletedStaff(),userPRO.getSkillsets());
            userPROUtilMockedStatic.verify(()->UserPROUtil.updatePRO(userPRO));
        }
    }

    @Test
    void updateProForAccountDeletion_nullPro_test(){
        UserStaffHelper userStaffHelper = Mockito.mock(UserStaffHelper.class);
        Mockito.doCallRealMethod().when(userStaffHelper).updateProForAccountDeletion(null,0L);
        userStaffHelper.updateProForAccountDeletion(null,0L);
        Mockito.verify(userStaffHelper).updateProForAccountDeletion(null,0L);
        Mockito.verifyNoMoreInteractions(userStaffHelper);
    }

    @Test
    void updateProForAccountDeletion_validPro_test(){
        UserStaffHelper userStaffHelper = Mockito.mock(UserStaffHelper.class);
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        Mockito.doCallRealMethod().when(userStaffHelper).updateProForAccountDeletion(userPro,1L);
        userStaffHelper.updateProForAccountDeletion(userPro,1L);
        Mockito.verify(userStaffHelper).updateProForUserDisabling(userPro);
        Assertions.assertEquals(1L,userPro.getDateModified());
    }

    @Test
    void createUserEventsHandler_null_policy_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            PeopleRelationJDO userPro = MockPRO.getMockPRO();

            AccessManager accessManager = Mockito.mock(AccessManager.class);
            Mockito.when(accessManager.createNewPolicyForMember(anyString(),anyString(),anyBoolean())).thenReturn(null);
            accessManagerMockedStatic.when(AccessManager::getInstance).thenReturn(accessManager);

            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            Contact currentUser = new Contact();
            currentUser.setId("contactId");

            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accId");
            account.setIsPayrollEnabled(true);
            account.setDisplayDomainName("accName");

            Map<String,Object> response = userStaffHelper.createUserEventsHandler(currentUser, account,true,"activity",userPro);

            Assertions.assertTrue((Boolean) response.get(Commons.SUCCESS));
            Assertions.assertEquals(null,response.get("accessPolicy"));
            Assertions.assertEquals(new UserDTO(userPro,null),response.get(CommonConstants.USER_KEY));
        }
    }

    @Test
    void createUserEventsHandler_empty_policyPermissions_test() {
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            PeopleRelationJDO userPro = MockPRO.getMockPRO();

            AccessManager accessManager = Mockito.mock(AccessManager.class);
            AccessPolicy policy = new AccessPolicy();
            policy.setPermissions(null);
            Mockito.when(accessManager.createNewPolicyForMember(anyString(),anyString(),anyBoolean())).thenReturn(policy);
            accessManagerMockedStatic.when(AccessManager::getInstance).thenReturn(accessManager);

            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            Contact currentUser = new Contact();
            currentUser.setId("contactId");

            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accId");
            account.setIsPayrollEnabled(true);
            account.setDisplayDomainName("accName");

            Map<String,Object> response = userStaffHelper.createUserEventsHandler(currentUser, account,true,"activity",userPro);

            Assertions.assertTrue((Boolean) response.get(Commons.SUCCESS));
            Assertions.assertEquals(policy,response.get("accessPolicy"));
            Assertions.assertEquals(new UserDTO(userPro,null),response.get(CommonConstants.USER_KEY));
        }
    }

    @Test
    void createUserEventsHandler_valid_policyPermissions_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            PeopleRelationJDO userPro = MockPRO.getMockPRO();

            AccessManager accessManager = Mockito.mock(AccessManager.class);
            AccessPolicy policy = new AccessPolicy();
            policy.setPermissions(PermissionManager.getMemberPermissions(false,"accountId"));
            Mockito.when(accessManager.createNewPolicyForMember(anyString(),anyString(),anyBoolean())).thenReturn(policy);
            accessManagerMockedStatic.when(AccessManager::getInstance).thenReturn(accessManager);

            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            Contact currentUser = new Contact();
            currentUser.setId("contactId");

            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accId");
            account.setIsPayrollEnabled(true);
            account.setDisplayDomainName("accName");

            Map<String,Object> response = userStaffHelper.createUserEventsHandler(currentUser,account,true,"activity",userPro);

            UserDTO userDTO = new UserDTO(userPro,policy.getPermissions());
            Assertions.assertTrue((Boolean) response.get(Commons.SUCCESS));
            Assertions.assertEquals(policy,response.get("accessPolicy"));
            Assertions.assertEquals(userDTO,response.get(CommonConstants.USER_KEY));
        }
    }

    @Test
    void createUserEventsHandler_exception_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            PeopleRelationJDO userPro = MockPRO.getMockPRO();

            AccessManager accessManager = Mockito.mock(AccessManager.class);
            AccessPolicy policy = new AccessPolicy();
            policy.setPermissions(PermissionManager.getMemberPermissions(false,"accountId"));
            Mockito.when(accessManager.createNewPolicyForMember(anyString(),anyString(),anyBoolean())).thenReturn(policy);
            accessManagerMockedStatic.when(AccessManager::getInstance).thenReturn(accessManager);

            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateStaffOperationsTaskQueue(anyMap())).thenThrow(new IOException("exception"));

            Contact currentUser = new Contact();
            currentUser.setId("contactId");

            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accId");
            account.setIsPayrollEnabled(true);
            account.setDisplayDomainName("accName");

            Map<String,Object> response = userStaffHelper.createUserEventsHandler(currentUser,account,true,"activity",userPro);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        }
    }

}