package com.yoco.user.helper.staff;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.contact.ContactDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.*;
import com.yoco.commons.utils.events.ClockUtil;
import com.yoco.constants.CommonConstants;
import com.yoco.contact.helper.hook.DeleteContactHookHelper;
import com.yoco.team.service.TeamService;
import com.yoco.user.service.UserStaffService;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import static com.yoco.user.service.UserStaffService.CONTACT_LIST;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class UserStaffTaskHelperTest {

    public UserStaffTaskHelper userStaffTaskHelper = UserStaffTaskHelper.getInstance();

    @Test
    void staffOperationsTaskHelper_test() throws NoSuchAlgorithmException, IOException {

        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic userProUtil = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserStaffEmailHelper> userStaffEmailHelperMockedStatic = Mockito.mockStatic(UserStaffEmailHelper.class);
            MockedStatic<UserStaffChannelPublishHelper> staffChannelPublishHelperMockedStatic = Mockito.mockStatic(UserStaffChannelPublishHelper.class);
            MockedStatic<UserStaffFullMetricHelper> staffFullMetricHelperMockedStatic = Mockito.mockStatic(UserStaffFullMetricHelper.class)){

            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            adminPro.setContactId("contactId");
            adminPro.setParentContactId("parentContactId");
            userProUtil.when(()-> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(adminPro);
            userProUtil.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(adminPro);
            userProUtil.when(()-> UserPROUtil.updatePRO(any(PeopleRelationJDO.class))).thenReturn(adminPro);

            UserStaffEmailHelper staffEmailHelper = Mockito.mock(UserStaffEmailHelper.class);
            Mockito.doNothing().when(staffEmailHelper).addStaffEmailHandler(anyMap(),anyString(),any(Contact.class),any(ContactDTO.class));
            userStaffEmailHelperMockedStatic.when(UserStaffEmailHelper::getInstance).thenReturn(staffEmailHelper);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            UserStaffChannelPublishHelper staffChannelPublishHelper = Mockito.mock(UserStaffChannelPublishHelper.class);
            Mockito.doNothing().when(staffChannelPublishHelper).publishToAdminChannelOnStaffOperations(any(AccessPolicy.class),any(UserDTO.class),anyString());
            staffChannelPublishHelperMockedStatic.when(UserStaffChannelPublishHelper::getInstance).thenReturn(staffChannelPublishHelper);

            UserStaffFullMetricHelper staffFullMetricHelper = Mockito.mock(UserStaffFullMetricHelper.class);
            Mockito.doNothing().when(staffFullMetricHelper).userCreationMetric(anyString(),anyString());
            staffFullMetricHelperMockedStatic.when(UserStaffFullMetricHelper::getInstance).thenReturn(staffFullMetricHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            ContactDTO contactDTO = new ContactDTO();
            contactDTO.setEmailID("test@gmail.com");
            userDTO.setContact(contactDTO);

            Contact adminContactDTO = new Contact();
            adminContactDTO.setId("adminContactId");

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"staffAdded");
            taskMap.put(CommonConstants.USER_KEY,userDTO);
            taskMap.put("adminContact",adminContactDTO);
            taskMap.put(CommonConstants.POLICY,accessPolicy);
            taskMap.put(CommonConstants.ACTIVITY,"contactId has  been added by adminContactId");

            userStaffTaskHelper.staffOperationsTaskHelper(taskMap);

            Mockito.verify(staffEmailHelper, times(1)).addStaffEmailHandler(taskMap,"accountId",adminContactDTO,contactDTO);

            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("accountId","contactId","STAFF_ADDED", "test@gmail.com",
                    "contactId has  been added by adminContactId","DUMMY"), times(1));

            Mockito.verify(staffChannelPublishHelper, times(1)).publishToAdminChannelOnStaffOperations(accessPolicy,userDTO,"staff_added");
            Mockito.verify(staffFullMetricHelper, times(1)).userCreationMetric("accountId","web");

            userProUtil.verify(()-> UserPROUtil.getUserPro(anyString(),anyString()),times(1));
            userProUtil.verify(()-> UserPROUtil.updatePRO(any(PeopleRelationJDO.class)),times(1));
        }
    }

    @Test
    void staffOperationsTaskHelper_empty_parent_contact_Id_test() throws NoSuchAlgorithmException, IOException {

        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic userProUtil = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserStaffEmailHelper> userStaffEmailHelperMockedStatic = Mockito.mockStatic(UserStaffEmailHelper.class);
            MockedStatic<UserStaffChannelPublishHelper> staffChannelPublishHelperMockedStatic = Mockito.mockStatic(UserStaffChannelPublishHelper.class);
            MockedStatic<UserStaffFullMetricHelper> staffFullMetricHelperMockedStatic = Mockito.mockStatic(UserStaffFullMetricHelper.class)){

            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            adminPro.setContactId("");
            userProUtil.when(()-> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(adminPro);

            UserStaffEmailHelper staffEmailHelper = Mockito.mock(UserStaffEmailHelper.class);
            Mockito.doNothing().when(staffEmailHelper).addStaffEmailHandler(anyMap(),anyString(),any(Contact.class),any(ContactDTO.class));
            userStaffEmailHelperMockedStatic.when(UserStaffEmailHelper::getInstance).thenReturn(staffEmailHelper);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            UserStaffChannelPublishHelper staffChannelPublishHelper = Mockito.mock(UserStaffChannelPublishHelper.class);
            Mockito.doNothing().when(staffChannelPublishHelper).publishToAdminChannelOnStaffOperations(any(AccessPolicy.class),any(UserDTO.class),anyString());
            staffChannelPublishHelperMockedStatic.when(UserStaffChannelPublishHelper::getInstance).thenReturn(staffChannelPublishHelper);

            UserStaffFullMetricHelper staffFullMetricHelper = Mockito.mock(UserStaffFullMetricHelper.class);
            Mockito.doNothing().when(staffFullMetricHelper).userCreationMetric(anyString(),anyString());
            staffFullMetricHelperMockedStatic.when(UserStaffFullMetricHelper::getInstance).thenThrow(new IllegalArgumentException());

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setEmailID("test@gmail.com");
            ContactDTO contactDTO = new ContactDTO();
            contactDTO.setEmailID("test@gmail.com");
            userDTO.setContact(contactDTO);

            Contact adminContactDTO = new Contact();
            adminContactDTO.setId("adminContactId");

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"staffAdded");
            taskMap.put(CommonConstants.USER_KEY,userDTO);
            taskMap.put("adminContact",adminContactDTO);
            taskMap.put(CommonConstants.POLICY,accessPolicy);
            taskMap.put(CommonConstants.ACTIVITY,"contactId has  been added by adminContactId");

            userStaffTaskHelper.staffOperationsTaskHelper(taskMap);

            Mockito.verify(staffEmailHelper, times(1)).addStaffEmailHandler(taskMap,"accountId",adminContactDTO,contactDTO);

            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("accountId","contactId","STAFF_ADDED", "test@gmail.com",
                    "contactId has  been added by adminContactId","DUMMY"), times(1));

            Mockito.verify(staffChannelPublishHelper, times(1)).publishToAdminChannelOnStaffOperations(accessPolicy,userDTO,"staff_added");
            Mockito.verify(staffFullMetricHelper, times(0)).userCreationMetric("accountId","web");

            userProUtil.verify(()-> UserPROUtil.getUserPro(anyString(),anyString()),times(0));
            userProUtil.verify(()-> UserPROUtil.updatePRO(any(PeopleRelationJDO.class)),times(0));
        }
    }

    @Test
    void staffOperationsTaskHelper_nullPrimaryAdmin_test() throws NoSuchAlgorithmException, IOException {

        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic userProUtil = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserStaffEmailHelper> userStaffEmailHelperMockedStatic = Mockito.mockStatic(UserStaffEmailHelper.class);
            MockedStatic<UserStaffChannelPublishHelper> staffChannelPublishHelperMockedStatic = Mockito.mockStatic(UserStaffChannelPublishHelper.class);
            MockedStatic<UserStaffFullMetricHelper> staffFullMetricHelperMockedStatic = Mockito.mockStatic(UserStaffFullMetricHelper.class)){

            userProUtil.when(()-> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(null);

            UserStaffEmailHelper staffEmailHelper = Mockito.mock(UserStaffEmailHelper.class);
            Mockito.doNothing().when(staffEmailHelper).addStaffEmailHandler(anyMap(),anyString(),any(Contact.class),any(ContactDTO.class));
            userStaffEmailHelperMockedStatic.when(UserStaffEmailHelper::getInstance).thenReturn(staffEmailHelper);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            UserStaffChannelPublishHelper staffChannelPublishHelper = Mockito.mock(UserStaffChannelPublishHelper.class);
            Mockito.doNothing().when(staffChannelPublishHelper).publishToAdminChannelOnStaffOperations(any(AccessPolicy.class),any(UserDTO.class),anyString());
            staffChannelPublishHelperMockedStatic.when(UserStaffChannelPublishHelper::getInstance).thenReturn(staffChannelPublishHelper);

            UserStaffFullMetricHelper staffFullMetricHelper = Mockito.mock(UserStaffFullMetricHelper.class);
            Mockito.doNothing().when(staffFullMetricHelper).userCreationMetric(anyString(),anyString());
            staffFullMetricHelperMockedStatic.when(UserStaffFullMetricHelper::getInstance).thenReturn(staffFullMetricHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            ContactDTO contactDTO = new ContactDTO();
            contactDTO.setEmailID("test@gmail.com");
            userDTO.setContact(contactDTO);

            Contact adminContactDTO = new Contact();
            adminContactDTO.setId("adminContactId");

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"staffAdded");
            taskMap.put(CommonConstants.USER_KEY,userDTO);
            taskMap.put("adminContact",adminContactDTO);
            taskMap.put(CommonConstants.POLICY,accessPolicy);
            taskMap.put(CommonConstants.ACTIVITY,"contactId has  been added by adminContactId");

            userStaffTaskHelper.staffOperationsTaskHelper(taskMap);

            Mockito.verify(staffEmailHelper, times(1)).addStaffEmailHandler(taskMap,"accountId",adminContactDTO,contactDTO);

            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("accountId","contactId","STAFF_ADDED", "test@gmail.com",
                    "contactId has  been added by adminContactId","DUMMY"), times(1));

            Mockito.verify(staffChannelPublishHelper, times(1)).publishToAdminChannelOnStaffOperations(accessPolicy,userDTO,"staff_added");
            Mockito.verify(staffFullMetricHelper, times(1)).userCreationMetric("accountId","web");

            userProUtil.verify(()-> UserPROUtil.getUserPro(anyString(),anyString()),times(0));
            userProUtil.verify(()-> UserPROUtil.updatePRO(any(PeopleRelationJDO.class)),times(0));
        }
    }

    @Test
    void staffOperationsTaskHelper_invalid_action_test() throws NoSuchAlgorithmException, IOException {

        try(MockedStatic<UserStaffEmailHelper> userStaffEmailHelperMockedStatic = Mockito.mockStatic(UserStaffEmailHelper.class)){

            UserStaffEmailHelper staffEmailHelper = Mockito.mock(UserStaffEmailHelper.class);
            userStaffEmailHelperMockedStatic.when(UserStaffEmailHelper::getInstance).thenReturn(staffEmailHelper);

            UserDTO userDTO = new UserDTO();
            Contact adminContactDTO = new Contact();

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"");
            taskMap.put(CommonConstants.USER_KEY,userDTO);
            taskMap.put("adminContact",adminContactDTO);
            taskMap.put(CommonConstants.POLICY,accessPolicy);
            taskMap.put(CommonConstants.ACTIVITY,"contactId has  been added by adminContactId");

            userStaffTaskHelper.staffOperationsTaskHelper(taskMap);

            Mockito.verify(staffEmailHelper, times(0)).addStaffEmailHandler(anyMap(),anyString(),any(Contact.class),any(ContactDTO.class));
        }
    }


    @Test
    void importStaffTaskHandler_success_false_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserStaffService> userStaffServiceMockedStatic = Mockito.mockStatic(UserStaffService.class);
            MockedStatic<UserStaffEmailHelper> userStaffEmailHelperMockedStatic = Mockito.mockStatic(UserStaffEmailHelper.class)){

            Map<String,Object> importedUserMockResp = new HashMap<>();
            importedUserMockResp.put(Commons.SUCCESS,false);
            importedUserMockResp.put(Commons.ERROR_RESPONSE,"Email ID already exists");

            UserStaffService userStaffServiceMock = Mockito.mock(UserStaffService.class);
            Mockito.when(userStaffServiceMock.createUser(anyString(),any(Contact.class),anyString(),anyBoolean())).
                    thenReturn(importedUserMockResp);
            userStaffServiceMockedStatic.when(UserStaffService::getInstance).thenReturn(userStaffServiceMock);

            UserStaffEmailHelper userStaffEmailHelperMock = Mockito.mock(UserStaffEmailHelper.class);
            Mockito.doNothing().when(userStaffEmailHelperMock).importStaffEmailHandler(anyMap(),any(Contact.class),anyInt());
            userStaffEmailHelperMockedStatic.when(UserStaffEmailHelper::getInstance).thenReturn(userStaffEmailHelperMock);

            Contact adminContact = new Contact();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"staffImported");
            taskMap.put(CommonConstants.ACCOUNT_ID_KEY,"accountId");
            taskMap.put(ContactConstants.CONTACT,adminContact);

            Map<String,Object> contactList = new HashMap<>();
            contactList.put(ContactConstants.EMAIL_ID,"test@gmail.com");

            taskMap.put(CONTACT_LIST,new ArrayList<>(){{add(contactList);}});

            userStaffTaskHelper.staffOperationsTaskHelper(taskMap);

            Mockito.verify(userStaffServiceMock).createUser("accountId",adminContact, JsonUtil.getJson(contactList),false);

            LinkedHashMap<String,Object> failedUsersMap = new LinkedHashMap<>();
            failedUsersMap.put("test@gmail.com","Email ID already exists");

            Mockito.verify(userStaffEmailHelperMock).importStaffEmailHandler(failedUsersMap,adminContact, 1);
        }
    }

    @Test
    void importStaffTaskHandler_empty_errorResponse_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserStaffService> userStaffServiceMockedStatic = Mockito.mockStatic(UserStaffService.class);
            MockedStatic<UserStaffEmailHelper> userStaffEmailHelperMockedStatic = Mockito.mockStatic(UserStaffEmailHelper.class)){

            Map<String,Object> importedUserMockResp = new HashMap<>();
            importedUserMockResp.put(Commons.SUCCESS,false);
            importedUserMockResp.put(Commons.ERROR_RESPONSE,"");

            UserStaffService userStaffServiceMock = Mockito.mock(UserStaffService.class);
            Mockito.when(userStaffServiceMock.createUser(anyString(),any(Contact.class),anyString(),anyBoolean())).
                    thenReturn(importedUserMockResp);
            userStaffServiceMockedStatic.when(UserStaffService::getInstance).thenReturn(userStaffServiceMock);

            UserStaffEmailHelper userStaffEmailHelperMock = Mockito.mock(UserStaffEmailHelper.class);
            Mockito.doNothing().when(userStaffEmailHelperMock).importStaffEmailHandler(anyMap(),any(Contact.class),anyInt());
            userStaffEmailHelperMockedStatic.when(UserStaffEmailHelper::getInstance).thenReturn(userStaffEmailHelperMock);

            Contact adminContact = new Contact();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"staffImported");
            taskMap.put(CommonConstants.ACCOUNT_ID_KEY,"accountId");
            taskMap.put(ContactConstants.CONTACT,adminContact);

            Map<String,Object> contactList = new HashMap<>();
            contactList.put(ContactConstants.EMAIL_ID,"test@gmail.com");

            taskMap.put(CONTACT_LIST,new ArrayList<>(){{add(contactList);}});

            userStaffTaskHelper.staffOperationsTaskHelper(taskMap);

            Mockito.verify(userStaffServiceMock).createUser("accountId",adminContact, JsonUtil.getJson(contactList),false);

            LinkedHashMap<String,Object> failedUsersMap = new LinkedHashMap<>();
            failedUsersMap.put("test@gmail.com","");

            Mockito.verify(userStaffEmailHelperMock).importStaffEmailHandler(failedUsersMap,adminContact, 1);
        }
    }

    @Test
    void importStaffTaskHandler_success_true_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserStaffService> userStaffServiceMockedStatic = Mockito.mockStatic(UserStaffService.class);
            MockedStatic<UserStaffEmailHelper> userStaffEmailHelperMockedStatic = Mockito.mockStatic(UserStaffEmailHelper.class)){

            Map<String,Object> importedUserMockResp1 = new HashMap<>();
            importedUserMockResp1.put(Commons.SUCCESS,true);

            UserStaffService userStaffServiceMock = Mockito.mock(UserStaffService.class);
            Mockito.when(userStaffServiceMock.createUser(anyString(),any(Contact.class),anyString(),anyBoolean())).thenReturn(importedUserMockResp1);
            userStaffServiceMockedStatic.when(UserStaffService::getInstance).thenReturn(userStaffServiceMock);

            UserStaffEmailHelper userStaffEmailHelperMock = Mockito.mock(UserStaffEmailHelper.class);
            Mockito.doNothing().when(userStaffEmailHelperMock).importStaffEmailHandler(anyMap(),any(Contact.class),anyInt());
            userStaffEmailHelperMockedStatic.when(UserStaffEmailHelper::getInstance).thenReturn(userStaffEmailHelperMock);

            Contact adminContact = new Contact();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"staffImported");
            taskMap.put(CommonConstants.ACCOUNT_ID_KEY,"accountId");
            taskMap.put(ContactConstants.CONTACT,adminContact);

            Map<String,Object> contactList = new HashMap<>();
            contactList.put(ContactConstants.EMAIL_ID,"test@gmail.com");

            taskMap.put(CONTACT_LIST,new ArrayList<>(){{add(contactList);}});

            userStaffTaskHelper.staffOperationsTaskHelper(taskMap);

            Mockito.verify(userStaffServiceMock).createUser("accountId",adminContact, JsonUtil.getJson(contactList),false);

            LinkedHashMap<String,Object> failedUsersMap = new LinkedHashMap<>();

            Mockito.verify(userStaffEmailHelperMock).importStaffEmailHandler(failedUsersMap,adminContact, 1);
        }
    }


    @Test
    void importStaffTaskHandler_exception_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserStaffService> userStaffServiceMockedStatic = Mockito.mockStatic(UserStaffService.class);
            MockedStatic<UserStaffEmailHelper> userStaffEmailHelperMockedStatic = Mockito.mockStatic(UserStaffEmailHelper.class)){

            Map<String,Object> importedUserMockResp1 = new HashMap<>();
            importedUserMockResp1.put(Commons.SUCCESS,true);

            UserStaffService userStaffServiceMock = Mockito.mock(UserStaffService.class);
            Mockito.when(userStaffServiceMock.createUser(anyString(),any(Contact.class),anyString(),anyBoolean())).thenThrow(new IOException());
            userStaffServiceMockedStatic.when(UserStaffService::getInstance).thenReturn(userStaffServiceMock);

            UserStaffEmailHelper userStaffEmailHelperMock = Mockito.mock(UserStaffEmailHelper.class);
            Mockito.doNothing().when(userStaffEmailHelperMock).importStaffEmailHandler(anyMap(),any(Contact.class),anyInt());
            userStaffEmailHelperMockedStatic.when(UserStaffEmailHelper::getInstance).thenReturn(userStaffEmailHelperMock);

            Contact adminContact = new Contact();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"staffImported");
            taskMap.put(CommonConstants.ACCOUNT_ID_KEY,"accountId");
            taskMap.put(ContactConstants.CONTACT,adminContact);

            Map<String,Object> contactList = new HashMap<>();
            contactList.put(ContactConstants.EMAIL_ID,"test@gmail.com");

            taskMap.put(CONTACT_LIST,new ArrayList<>(){{add(contactList);}});

            userStaffTaskHelper.staffOperationsTaskHelper(taskMap);

            Mockito.verify(userStaffServiceMock).createUser("accountId",adminContact, JsonUtil.getJson(contactList),false);

            LinkedHashMap<String,Object> failedUsersMap = new LinkedHashMap<>();
            failedUsersMap.put("test@gmail.com","");

            Mockito.verify(userStaffEmailHelperMock).importStaffEmailHandler(failedUsersMap,adminContact, 1);
        }
    }

    @Test
    void enableStaffTaskHelper_test(){
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserStaffChannelPublishHelper> userStaffChannelPublishHelperMockedStatic = Mockito.mockStatic(UserStaffChannelPublishHelper.class);
            MockedStatic<UserStaffFullMetricHelper> userStaffFullMetricHelperMockedStatic = Mockito.mockStatic(UserStaffFullMetricHelper.class)){

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            UserStaffChannelPublishHelper staffChannelPublishHelper = Mockito.mock(UserStaffChannelPublishHelper.class);
            Mockito.doNothing().when(staffChannelPublishHelper).publishToAdminAndUserChannelOnStaffOperations(any(AccessPolicy.class),any(UserDTO.class),anyString());
            userStaffChannelPublishHelperMockedStatic.when(UserStaffChannelPublishHelper::getInstance).thenReturn(staffChannelPublishHelper);

            UserStaffFullMetricHelper staffFullMetricHelper = Mockito.mock(UserStaffFullMetricHelper.class);
            Mockito.doNothing().when(staffFullMetricHelper).userActivationMetric(anyString(),anyString());
            userStaffFullMetricHelperMockedStatic.when(UserStaffFullMetricHelper::getInstance).thenReturn(staffFullMetricHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setEmailID("test@gmail.com");

            Contact adminContactDTO = new Contact();
            adminContactDTO.setId("adminContactId");
            AccessPolicy accessPolicy = new AccessPolicy();

            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"staffEnabled");
            taskMap.put(CommonConstants.USER_KEY,userDTO);
            taskMap.put(CommonConstants.POLICY,accessPolicy);
            taskMap.put(CommonConstants.ACTIVITY,"activity");
            taskMap.put(CommonConstants.SOURCE,"activitySrc");
            userStaffTaskHelper.staffOperationsTaskHelper(taskMap);

            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("accountId","contactId","STAFF_ENABLED", "test@gmail.com",
                    "activity","DUMMY"), times(1));

            Mockito.verify(staffChannelPublishHelper, times(1)).publishToAdminAndUserChannelOnStaffOperations(accessPolicy,userDTO,"staff_enabled");
            Mockito.verify(staffFullMetricHelper, times(1)).userActivationMetric("accountId","activitySrc");
        }
    }

    @Test
    void handleUserDeletion_Exception_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){
            UserStaffTaskHelper.getInstance().handleUserDeletion(null);
            accessManagerMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleUserDeletion_valid_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<ClockUtil> clockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class);
            MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);
            MockedConstruction<TeamService> mock = Mockito.mockConstruction(TeamService.class, (teamServiceMock, context) -> {
                Mockito.doNothing().when(teamServiceMock).removeUserFromAllTeamsInAnAccount("345","234");
            });
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<UserStaffFullMetricHelper> userStaffFullMetricHelperMockedStatic = Mockito.mockStatic(UserStaffFullMetricHelper.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserStaffChannelPublishHelper> userStaffChannelPublishHelperMockedStatic = Mockito.mockStatic(UserStaffChannelPublishHelper.class);){

            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            adminPro.setContactId("123");

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setContactId("234");
            userPro.setUniquepin("345");
            userPro.setEmailID("email");

            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO account = new SettingsJDO();
            Mockito.when(accountImplMock.getById("345")).thenReturn(account);
            accountImplMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);

            UserStaffFullMetricHelper userStaffFullMetricHelperMock = Mockito.mock(UserStaffFullMetricHelper.class);
            userStaffFullMetricHelperMockedStatic.when(UserStaffFullMetricHelper::getInstance).thenReturn(userStaffFullMetricHelperMock);

            UserStaffChannelPublishHelper userStaffChannelPublishHelper = Mockito.mock(UserStaffChannelPublishHelper.class);
            userStaffChannelPublishHelperMockedStatic.when(UserStaffChannelPublishHelper::getInstance).thenReturn(userStaffChannelPublishHelper);

            UserStaffTaskHelper.getInstance().handleUserDeletion(Map.of("adminPro",adminPro,"userPro",userPro,"requestSource","web"));

            accessManagerMockedStatic.verify(()-> AccessManager.deleteYoCoLevelPolicyForUser("345","234"));
            clockUtilMockedStatic.verify(()->ClockUtil.stopRunningEntryForDeletedUser(userPro,account,"web"));
            projectUtilMockedStatic.verify(()->ProjectUtil.disassociateUserFromAllProjectsInAccount("345","234"));
            dcmUtilMockedStatic.verify(()-> DcmUtil.removeUserFromAccount("345","234"));
            Mockito.verify(userStaffFullMetricHelperMock).updateUserDeletionMetric("345","web");
            rtmServiceMockedStatic.verify(()->RTMService.publishToAW("345","234","STAFF_DISABLED",null,null));
            activityUtilMockedStatic.verify(()-> ActivityUtil.saveActivity("345","234","STAFF_DISABLED","email","234 was deleted by 123","DUMMY"));
            Mockito.verify(userStaffChannelPublishHelper).publishToAdminAndUserChannelOnStaffOperations(null,new UserDTO(userPro,null),"STAFF_DISABLED");
        }
    }

    @Test
    void handleUserDeletion_valid_hook_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<ClockUtil> clockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class);
            MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);
            MockedConstruction<TeamService> mock = Mockito.mockConstruction(TeamService.class, (teamServiceMock, context) -> {
                Mockito.doNothing().when(teamServiceMock).removeUserFromAllTeamsInAnAccount("345","234");
            });
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<UserStaffFullMetricHelper> userStaffFullMetricHelperMockedStatic = Mockito.mockStatic(UserStaffFullMetricHelper.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserStaffChannelPublishHelper> userStaffChannelPublishHelperMockedStatic = Mockito.mockStatic(UserStaffChannelPublishHelper.class);){

            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            adminPro.setContactId("123");

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setContactId("234");
            userPro.setUniquepin("345");
            userPro.setEmailID("email");

            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO account = new SettingsJDO();
            Mockito.when(accountImplMock.getById("345")).thenReturn(account);
            accountImplMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);

            UserStaffFullMetricHelper userStaffFullMetricHelperMock = Mockito.mock(UserStaffFullMetricHelper.class);
            userStaffFullMetricHelperMockedStatic.when(UserStaffFullMetricHelper::getInstance).thenReturn(userStaffFullMetricHelperMock);

            UserStaffChannelPublishHelper userStaffChannelPublishHelper = Mockito.mock(UserStaffChannelPublishHelper.class);
            userStaffChannelPublishHelperMockedStatic.when(UserStaffChannelPublishHelper::getInstance).thenReturn(userStaffChannelPublishHelper);

            UserStaffTaskHelper.getInstance().handleUserDeletion(Map.of("adminPro",adminPro,"userPro",userPro,"requestSource", DeleteContactHookHelper.FULL_SYNC_DELETE));

            accessManagerMockedStatic.verifyNoInteractions();
            clockUtilMockedStatic.verify(()->ClockUtil.stopRunningEntryForDeletedUser(userPro,account,DeleteContactHookHelper.FULL_SYNC_DELETE));
            projectUtilMockedStatic.verify(()->ProjectUtil.disassociateUserFromAllProjectsInAccount("345","234"));
            dcmUtilMockedStatic.verify(()-> DcmUtil.removeUserFromAccount("345","234"));
            Mockito.verify(userStaffFullMetricHelperMock).updateUserDeletionMetric("345","hook");
            rtmServiceMockedStatic.verify(()->RTMService.publishToAW("345","234","STAFF_DISABLED",null,null));
            activityUtilMockedStatic.verify(()-> ActivityUtil.saveActivity("345","234","STAFF_DISABLED","email","234 was deleted by 123","DUMMY"));
            Mockito.verify(userStaffChannelPublishHelper).publishToAdminAndUserChannelOnStaffOperations(null,new UserDTO(userPro,null),"STAFF_DISABLED");
        }
    }

}