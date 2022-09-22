package com.yoco.user.service;

import com.yoco.MockPRO;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.contact.ContactDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.constants.CommonConstants;
import com.yoco.user.enums.USER_ERROR_MESSAGE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.MockitoRule;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

import static org.mockito.ArgumentMatchers.*;

class UserFetchServiceTest {

    UserFetchService userFetchService = new UserFetchService();

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {""," "})
    void getUser_invalid_accountID_test(final String testValue){
        try{
            userFetchService.getUser(testValue,"contactId");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value() ,e.getMessage());
        }
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {""," "})
    void getUser_invalid_contactID_test(final String testValue){
        try{
            userFetchService.getUser("accountId",testValue);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value() ,e.getMessage());
        }
    }

    @Test
    void getUser_noPRO_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserProWithContact(anyString(), anyString())).thenReturn(null);
            Map<String,Object> userMap = userFetchService.getUser("accountId","contactId");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(userMap));
        }
    }

    @Test
    void getUser_valid_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserProWithContact(anyString(), anyString())).thenReturn(MockPRO.getMockPRO());
            Map<String,Object> userMap = userFetchService.getUser("accountId","contactId");
            Assertions.assertEquals(new UserDTO(MockPRO.getMockPRO(),null),userMap.get("user"));
        }
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {""," "})
    void getAccessPolicy_invalid_accountID_test(final String testValue){
        try{
            userFetchService.getAccessPolicy(testValue,"contactId");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value() ,e.getMessage());
        }
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {""," "})
    void getAccessPolicy_invalid_contactID_test(final String testValue){
        try{
            userFetchService.getAccessPolicy("accountId",testValue);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value() ,e.getMessage());
        }
    }

    @Test
    void getAccessPolicy_noPRO_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserPro(anyString(), anyString())).thenReturn(null);
            Map<String,Object> userMap = userFetchService.getAccessPolicy("accountId","contactId");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(userMap));
        }
    }

    @Test
    void getAccessPolicy_valid_test(){
        Map<String,Object> policyMap = new HashMap<>();
        policyMap.put("policy","accessPolicy");
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedConstruction<AccessManager> mock = Mockito.mockConstruction(AccessManager.class, (accessManagerMock, context) -> {
                Mockito.when(accessManagerMock.getPolicy(any())).thenReturn(policyMap);
            })){
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserPro(anyString(), anyString())).thenReturn(MockPRO.getMockPRO());
            Map<String,Object> userMap = new UserFetchService().getAccessPolicy("accountId","contactId");
            Assertions.assertEquals(policyMap,userMap);
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getAllUsers_invalidAccountId_test(String testValue){
        try{
            new UserFetchService().getAllUsers(testValue,false,10,"");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getAllUsers_emptyUsers_test(Map<String,Object> testValue){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){

            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getAllUsers(anyString(),anyBoolean(),anyInt(),anyString())).thenReturn(testValue);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            Map<String,Object> usersMap = new UserFetchService().getAllUsers("accountId",false,10,"");
            Map<String,Object> expectedUsersMap = new HashMap<>();
            expectedUsersMap.put("users",new ArrayList<>());
            Assertions.assertEquals(expectedUsersMap,usersMap);
            Mockito.verify(userMock).getAllUsers("accountId",false,10,"");
        }
    }

    @Test
    void getAllUsers_noUsers_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){

            List<PeopleRelationJDO> allUserProList = new ArrayList<>();
            allUserProList.add(MockPRO.getMockPRO());
            allUserProList.add(MockPRO.getMockPRO());

            Map<String, Object> allUsersPro = new HashMap<>();
            allUsersPro.put(Commons.SUCCESS,false);

            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getAllUsers(anyString(),anyBoolean(),anyInt(),anyString())).thenReturn(allUsersPro);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            Map<String,Object> usersMap = new UserFetchService().getAllUsers("accountId",false,10,"");

            Map<String,Object> expectedUsersMap = new HashMap<>();
            expectedUsersMap.put("users",new ArrayList<>());

            Assertions.assertEquals(expectedUsersMap,usersMap);
            Mockito.verify(userMock).getAllUsers("accountId",false,10,"");
        }
    }

    @Test
    void getAllUsers_valid_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){

            List<PeopleRelationJDO> allUserProList = new ArrayList<>();
            allUserProList.add(MockPRO.getMockPRO());
            allUserProList.add(MockPRO.getMockPRO());

            Map<String, Object> allUsersPro = new HashMap<>();
            allUsersPro.put(CommonConstants.USERS_KEY,allUserProList);
            allUsersPro.put(CommonConstants.CURSOR,"cursorString");

            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getAllUsers(anyString(),anyBoolean(),anyInt(),anyString())).thenReturn(allUsersPro);

            Contact contactObj = new Contact();
            contactObj.setId("id");

            Contact contactObj2 = new Contact();
            contactObj2.setId("i2");

            List<Contact> contactList = new ArrayList<>();
            contactList.add(contactObj);
            contactList.add(contactObj2);

            Mockito.when(userMock.getContactsByKeys(anyList())).thenReturn(contactList);

            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            Map<String,Object> usersMap = new UserFetchService().getAllUsers("accountId",false,10,"");

            Map<String,Object> expectedUsersMap = new HashMap<>();
            expectedUsersMap.put(CommonConstants.USERS_KEY,new UserDTO().generateUserDTOList(allUserProList));
            expectedUsersMap.put(CommonConstants.CURSOR,"cursorString");

            Assertions.assertEquals(expectedUsersMap,usersMap);
            Mockito.verify(userMock).getAllUsers("accountId",false,10,"");
        }
    }

    @Test
    void getAllUsersWithOutContact_valid_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){

            List<PeopleRelationJDO> allUserProList = new ArrayList<>();
            allUserProList.add(MockPRO.getMockPRO());
            allUserProList.add(null);

            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getAllUsers(anyString(),anyBoolean())).thenReturn(allUserProList);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            Map<String,Object> usersMap = new UserFetchService().getAllUsersWithOutContact("accountId");

            Map<String,Object> expectedUsersMap = new HashMap<>();
            Map<String, Object> userMap = new HashMap<>();
            userMap.put(MockPRO.getMockPRO().getContactId(),new UserDTO(MockPRO.getMockPRO(),null));
            expectedUsersMap.put(CommonConstants.USERS_KEY,userMap);

            Assertions.assertEquals(expectedUsersMap,usersMap);
            Mockito.verify(userMock).getAllUsers("accountId",false);
        }
    }

    @Test
    void getAllUsersWithOutContact_noUsers_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){

            List<PeopleRelationJDO> allUserProList = new ArrayList<>();

            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getAllUsers(anyString(),anyBoolean())).thenReturn(allUserProList);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            Map<String,Object> usersMap = new UserFetchService().getAllUsersWithOutContact("accountId");

            Map<String,Object> expectedUsersMap = new HashMap<>();
            Assertions.assertEquals(expectedUsersMap,usersMap);
            Mockito.verify(userMock).getAllUsers("accountId",false);
        }
    }

    @Test
    void getRecentlyUpdatedPROs_valid_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){

            List<PeopleRelationJDO> allUserProList = new ArrayList<>();
            allUserProList.add(MockPRO.getMockPRO());

            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getRecentlyUpdatedPRO(anyString(),anyLong())).thenReturn(allUserProList);

            Contact contactObj = new Contact();
            contactObj.setId("id");

            Contact contactObj2 = new Contact();
            contactObj2.setId("i2");

            List<Contact> contactList = new ArrayList<>();
            contactList.add(contactObj);
            contactList.add(contactObj2);

            Mockito.when(userMock.getContactsByKeys(anyList())).thenReturn(contactList);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            Map<String,Object> usersMap = new UserFetchService().getRecentlyUpdatedPROs("accountId",1646384551000L);

            Map<String,Object> expectedUsersMap = new HashMap<>();
            expectedUsersMap.put(CommonConstants.USERS_KEY,new UserDTO().generateUserDTOList(allUserProList));

            Assertions.assertEquals(expectedUsersMap,usersMap);
            Mockito.verify(userMock).getRecentlyUpdatedPRO("accountId",1646384551000L);
        }
    }

    @Test
    void getRecentlyUpdatedPROs_noUsers_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            List<PeopleRelationJDO> allUserProList = new ArrayList<>();

            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getRecentlyUpdatedPRO(anyString(),anyLong())).thenReturn(allUserProList);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            Map<String,Object> usersMap = new UserFetchService().getRecentlyUpdatedPROs("accountId",1646384551000L);
            Map<String,Object> expectedUsersMap = new HashMap<>();

            Assertions.assertEquals(expectedUsersMap,usersMap);
            Mockito.verify(userMock).getRecentlyUpdatedPRO("accountId",1646384551000L);
        }
    }

    @Test
    void getMeInfo_defaultAccountID_nullUserPro_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractDefaultUserPRO("123")).thenReturn(null);
            Contact contact = new Contact();
            contact.setId("123");
            new UserFetchService().getMeInfo("default",contact);
        }catch(Exception e){
            Assertions.assertEquals(USER_ERROR_MESSAGE.USER_NOT_FOUND.value(),e.getMessage());
        }
    }

    @Test
    void getMeInfo_validAccountID_nullUserPro_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID", "123")).thenReturn(null);
            Contact contact = new Contact();
            contact.setId("123");
            new UserFetchService().getMeInfo("accID",contact);
        }catch(Exception e){
            Assertions.assertEquals(USER_ERROR_MESSAGE.USER_NOT_FOUND.value(),e.getMessage());
        }
    }

    @Test
    void getMeInfo_defaultAccountID_validUserPro_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accID");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractDefaultUserPRO("123")).thenReturn(userPro);
            dcmUtilMockedStatic.when(()->DcmUtil.getIsPasswordPresentForUserEmailID("email")).thenReturn(true);
            Set<String> permissions = new HashSet(){{
                add("permission1");
            }};
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserPermissions("accID","123")).thenReturn(permissions);
            Contact contact = new Contact();
            contact.setId("123");
            contact.setEmailID("email");
            Map<String,Object> response =  new UserFetchService().getMeInfo("default",contact);
            UserDTO responseDTO = (UserDTO) response.get("user");
            Assertions.assertEquals("accID",responseDTO.getAccountID());
            Assertions.assertEquals(new ContactDTO(contact),responseDTO.getContact());
            Assertions.assertEquals(permissions, responseDTO.getPermissions());
        }
    }

    @Test
    void getMeInfo_validAccountID_validUserPro_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accID");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID", "123")).thenReturn(userPro);
            dcmUtilMockedStatic.when(()->DcmUtil.getIsPasswordPresentForUserEmailID("email")).thenReturn(true);
            Set<String> permissions = new HashSet(){{
                add("permission1");
            }};
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserPermissions("accID","123")).thenReturn(permissions);
            Contact contact = new Contact();
            contact.setId("123");
            contact.setEmailID("email");
            Map<String,Object> response =  new UserFetchService().getMeInfo("accID",contact);
            UserDTO responseDTO = (UserDTO) response.get("user");
            Assertions.assertEquals("accID",responseDTO.getAccountID());
            Assertions.assertEquals(new ContactDTO(contact),responseDTO.getContact());
            Assertions.assertEquals(permissions, responseDTO.getPermissions());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getAllUsersInAccount_empty_null_test(String testValue){
        try{
            new UserFetchService().getAllUsersInAccount(testValue,"");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getAllUsersInAccount_empty_list_test(){
        try(MockedConstruction<UserImpl> userImpl = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
            Mockito.when(objUserImpl.getAllUsers("accID", null, 2000, "")).thenReturn(new HashMap());
        })){
            Map<String, Object> resp =  new UserFetchService().getAllUsersInAccount("accID","");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
        }
    }

    @Test
    void getAllUsersInAccount_valid_list_test(){
        HashMap<String, Object> sampleResp = new HashMap<>();
        ArrayList usersList = new ArrayList<>();
        usersList.add(MockPRO.getMockPRO());
        sampleResp.put("users", usersList);
        try(MockedConstruction<UserImpl> userImpl = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
            Mockito.when(objUserImpl.getAllUsers("accID", null, 2000, "")).thenReturn(sampleResp);
        })){
            Map<String, Object> resp =  new UserFetchService().getAllUsersInAccount("accID","");
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));
        }
    }
}