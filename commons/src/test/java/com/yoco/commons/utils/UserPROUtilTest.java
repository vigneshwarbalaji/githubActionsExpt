package com.yoco.commons.utils;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.DataProtectionSkillset;
import com.yoco.commons.modal.user.Skillset;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

@Slf4j
class UserPROUtilTest {
    @Test
    void validateAndExtractUserPRO_nullUserPRO_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(null);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Assertions.assertNull(UserPROUtil.validateAndExtractUserPRO("234", "123"));
        }
    }

    @Test
    void validateAndExtractUserPRO_inActiveUserPRO_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(true);
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Assertions.assertNull(UserPROUtil.validateAndExtractUserPRO("234", "123"));
        }
    }

    @Test
    void validateAndExtractUserPRO_ActiveUserPRO_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(false);
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Assertions.assertEquals(userPro,UserPROUtil.validateAndExtractUserPRO("234", "123"));
        }
    }

    @Test
    void validateAndExtractUserPROWithContact_nullUserPRO_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserProWithContact("234","123")).thenReturn(null);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserProActive(null)).thenCallRealMethod();
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPROWithContact("234","123")).thenCallRealMethod();
            Assertions.assertNull(UserPROUtil.validateAndExtractUserPRO("234", "123"));
        }
    }

    @Test
    void validateAndExtractUserPROWithContact_inActiveUserPRO_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(true);
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserProWithContact("234","123")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserProActive(userPro)).thenCallRealMethod();
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPROWithContact("234","123")).thenCallRealMethod();
            Assertions.assertNull(UserPROUtil.validateAndExtractUserPRO("234", "123"));
        }
    }

    @Test
    void validateAndExtractUserPROWithContact_ActiveUserPRO_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(false);
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserProWithContact("234","123")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserProActive(userPro)).thenCallRealMethod();
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPROWithContact("234","123")).thenCallRealMethod();
            Assertions.assertEquals(userPro,UserPROUtil.validateAndExtractUserPROWithContact("234", "123"));
        }
    }

    @Test
    void validateAndExtractDefaultUserPRO_inActiveUserPro_test()throws NoSuchAlgorithmException, IOException{
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(true);
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(userPro);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Assertions.assertNull(UserPROUtil.validateAndExtractDefaultUserPRO("123"));
        }
    }

    @Test
    void validateAndExtractDefaultUserPRO_ActiveUserPro_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(false);
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(userPro);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Assertions.assertEquals(userPro,UserPROUtil.validateAndExtractDefaultUserPRO("123"));
        }
    }

    @Test
    void setUserProAsDefault_NullUserPRO_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserPROUtil.setUserProAsDefault(null);
            userImplMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void setUserProAsDefault_InactiveUserPRO_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setDelete(true);
            UserPROUtil.setUserProAsDefault(userPRO);
            userImplMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void setUserProAsDefault_ValidUserPRO_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setDelete(false);
            userPRO.setDefault(false);
            UserPROUtil.setUserProAsDefault(userPRO);
            Mockito.verify(userImplMock).save(userPRO);
            Assertions.assertTrue(userPRO.isDefault());
        }
    }

    @Test
    void getEmailIdForUser_nullUserContact_test(){
        try(MockedStatic<ContactImpl> contactImplMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(null);
            contactImplMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);
            Assertions.assertEquals("",UserPROUtil.getEmailIdForUser("123"));
        }
    }

    @Test
    void getEmailIdForUser_nullUserContactEmail_test(){
        try(MockedStatic<ContactImpl> contactImplMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(new Contact());
            contactImplMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);
            Assertions.assertEquals("",UserPROUtil.getEmailIdForUser("123"));
        }
    }

    @Test
    void getEmailIdForUser_ValidUserContactEmail_test(){
        try(MockedStatic<ContactImpl> contactImplMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact();
            contact.setEmailID(" email@email.com ");
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(contact);
            contactImplMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);
            Assertions.assertEquals("email@email.com",UserPROUtil.getEmailIdForUser("123"));
        }
    }

    @Test
    void getDefaultUserPro_nullUserPRO_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(null);
            Mockito.when(userImplMock.getUserWithoutContact("accID","123")).thenReturn(null);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDForUser(anyString())).thenReturn("accID");
            Assertions.assertNull(UserPROUtil.getDefaultUserPro("123"));
        }
    }

    @Test
    void getDefaultUserPro_validUserPRO_test()throws NoSuchAlgorithmException, IOException{
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(false);
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(null);
            Mockito.when(userImplMock.getUserWithoutContact("accID","123")).thenReturn(userPro);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDForUser(anyString())).thenReturn("accID");
            Assertions.assertEquals(userPro,UserPROUtil.getDefaultUserPro("123"));
            Mockito.verify(userImplMock).save(userPro);
        }
    }

    @Test
    void getUserProWithContact_nullPro_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(null);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            PeopleRelationJDO expectedPro = UserPROUtil.getUserProWithContact("234", "123");
            Assertions.assertTrue(ObjUtils.isNull(expectedPro));
        }
    }

    @Test
    void getUserProWithContact_nullContact_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){

            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(false);
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);

            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(null);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);

            PeopleRelationJDO expectedPro = UserPROUtil.getUserProWithContact("234", "123");
            Assertions.assertTrue(ObjUtils.isNull(expectedPro.getContact()));
        }
    }

    @Test
    void getUserProWithContact_valid_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){

            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(false);
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);

            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact();
            contact.setId("123");
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);

            PeopleRelationJDO expectedPro = UserPROUtil.getUserProWithContact("234", "123");
            Assertions.assertEquals(contact,expectedPro.getContact());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void extractPROSkills_inValidJson_test(String testValue){
        PeopleRelationJDO userPRO = new PeopleRelationJDO();
        userPRO.setSkillsets(testValue);
        Assertions.assertNull(UserPROUtil.extractPROSkills(userPRO));
    }

    @Test
    void extractPROSkills_validJson_test(){
        String skills = JsonUtil.getJson(Skillset.generateSkillsetForStaff("accountId",false));
        PeopleRelationJDO userPRO = new PeopleRelationJDO();
        userPRO.setSkillsets(skills);
        Map<String,Object> resp = UserPROUtil.extractPROSkills(userPRO);
        Assertions.assertNotNull(resp);
        Assertions.assertEquals(JsonUtil.convertJsonToMap(skills),resp);
    }

    @ParameterizedTest
    @NullAndEmptySource
    void extractDataProtectionSkill_inValidJson_test(String testValue){
        PeopleRelationJDO userPRO = new PeopleRelationJDO();
        userPRO.setDataProtectionSkillset(testValue);
        Assertions.assertNull(UserPROUtil.extractDataProtectionSkill(userPRO));
    }

    @Test
    void extractDataProtectionSkill_validJson_test(){
        String skills = String.valueOf(DataProtectionSkillset.getDefaultUserDataProtectionSkillset().getValue());
        PeopleRelationJDO userPRO = new PeopleRelationJDO();
        userPRO.setDataProtectionSkillset(skills);
        Map<String,Object> resp = UserPROUtil.extractDataProtectionSkill(userPRO);
        Assertions.assertNotNull(resp);
        Assertions.assertEquals(JsonUtil.convertJsonToMap(skills),resp);
    }

    @Test
    void updatePROSkill_valid_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(false);
            userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForStaff("accountId",false)));

            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.savePro(any(PeopleRelationJDO.class))).thenReturn(null);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);

            Map<String,Object> skill = JsonUtil.convertJsonToMap(userPro.getSkillsets());
            Assertions.assertNotNull(UserPROUtil.updatePROSkill(userPro,skill));
            Mockito.verify(userImplMock).savePro(any(PeopleRelationJDO.class));
        }
    }

    @Test
    void getUserPermissions_nullAccessPolicy_test(){
        try(MockedConstruction<AccessManager> accessManagerMockedConstruction = Mockito.mockConstruction(AccessManager.class, (accessManagerMock, context) -> {
            Mockito.when(accessManagerMock.getPolicy("accID","123")).thenReturn(null);
        })){
        Assertions.assertTrue(UserPROUtil.getUserPermissions("accID","123").isEmpty());
        }
    }

    @Test
    void getUserPermissions_nullPermissions_test(){
        try(MockedConstruction<AccessManager> accessManagerMockedConstruction = Mockito.mockConstruction(AccessManager.class, (accessManagerMock, context) -> {
            Mockito.when(accessManagerMock.getPolicy("accID","123")).thenReturn(new AccessPolicy());
        })){
            Assertions.assertTrue(UserPROUtil.getUserPermissions("accID","123").isEmpty());
        }
    }

    @Test
    void getUserPermissions_validPermissions_test(){
        AccessPolicy accessPolicyMock = new AccessPolicy();
        Set<String> permissionsMock = new HashSet(){{add("permission1");}};
        accessPolicyMock.setPermissions(permissionsMock);
        try(MockedConstruction<AccessManager> accessManagerMockedConstruction = Mockito.mockConstruction(AccessManager.class, (accessManagerMock, context) -> {
            Mockito.when(accessManagerMock.getPolicy("accID","123")).thenReturn(accessPolicyMock);
        })){
            Assertions.assertEquals(permissionsMock,UserPROUtil.getUserPermissions("accID","123"));
        }
    }

    @Test
    void isUserAnAdminInAccount_nullAccountTest(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","123")).thenReturn(null);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount("accID","123")).thenCallRealMethod();
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(null)).thenCallRealMethod();
            Assertions.assertFalse(UserPROUtil.isUserAnAdminInAccount("accID","123"));
        }
    }

    @Test
    void isUserAnAdminInAccount_RoleNullAccountTest(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","123")).thenReturn(new PeopleRelationJDO());
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount("accID","123")).thenCallRealMethod();
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenCallRealMethod();
            Assertions.assertFalse(UserPROUtil.isUserAnAdminInAccount("accID","123"));
        }
    }

    @Test
    void isUserAnAdminInAccount_roleStaff_AccountTest(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setRole("staff");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","123")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount("accID","123")).thenCallRealMethod();
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(userPro)).thenCallRealMethod();
            Assertions.assertFalse(UserPROUtil.isUserAnAdminInAccount("accID","123"));
        }
    }

    @Test
    void isUserAnAdminInAccount_roleAdmin_AccountTest(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setRole("admin");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","123")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount("accID","123")).thenCallRealMethod();
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(userPro)).thenCallRealMethod();
            Assertions.assertTrue(UserPROUtil.isUserAnAdminInAccount("accID","123"));
        }
    }

    @Test
    void sortProBasedOnEmail_test(){
        PeopleRelationJDO pro1 = new PeopleRelationJDO();
        pro1.setEmailID("aemail");
        PeopleRelationJDO pro2= new PeopleRelationJDO();
        pro2.setEmailID("vemail");
        PeopleRelationJDO pro3 = new PeopleRelationJDO();
        pro3.setEmailID("aaemail");
        List<PeopleRelationJDO> proList = new ArrayList(){{add(pro1);add(pro2);add(pro3);}};
        UserPROUtil.sortProListBasedOnEmail(proList);
        Assertions.assertEquals(pro3,proList.get(0));
        Assertions.assertEquals(pro1,proList.get(1));
        Assertions.assertEquals(pro2,proList.get(2));
    }

    @Test
    void getAllUsersInCompany_nullUserResponsetest(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUsers("accID",null,5000,"")).thenReturn(new HashMap<>());
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Assertions.assertTrue(UserPROUtil.getAllUsersInCompany("accID",null).isEmpty());
        }
    }

    @Test
    void getAllUsersInCompany_noCursorResponsetest(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUsers("accID",null,5000,"")).thenReturn(new HashMap(){{put("users",new ArrayList(){{add(new PeopleRelationJDO());}});}});
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Assertions.assertEquals(1,UserPROUtil.getAllUsersInCompany("accID",null).size());
        }
    }

    @Test
    void getAllUsersInCompany_CursorResponsetest(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUsers("accID",null,5000,"")).thenReturn(new HashMap(){{put("users",new ArrayList(){{add(new PeopleRelationJDO());}});
            put("cursor","cur");}});
            Mockito.when(userImplMock.getAllUsers("accID",null,5000,"cur")).thenReturn(new HashMap(){{put("users",new ArrayList(){{add(new PeopleRelationJDO());}});}});
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Assertions.assertEquals(2,UserPROUtil.getAllUsersInCompany("accID",null).size());
        }
    }

    @Test
    void getPrimaryAdmin_fetchFromDB_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setMember(null);
            accessManagerMockedStatic.when(() -> AccessManager.getSuperAdmin(anyString())).thenReturn(accessPolicy);

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setContactId("123");
            userPro.setParentContactId("123");
            userPro.setDelete(false);
            userPro.setRole(Skillset.ROLE_ADMIN);

            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setContactId("113");
            userPro2.setParentContactId("123");
            userPro2.setDelete(false);
            userPro2.setRole(Skillset.ROLE_ADMIN);

            List<PeopleRelationJDO> userList = new ArrayList<>();
            userList.add(userPro);
            userList.add(userPro2);

            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getUsersByRole(anyString(),anyString())).thenReturn(userList);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);

            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact();
            contact.setId("123");
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);

            PeopleRelationJDO expectedPro = UserPROUtil.getPrimaryAdmin("accountId");
            Assertions.assertEquals(userPro,expectedPro);
        }
    }

    @Test
    void getPrimaryAdmin_fetchFromDB_noAdmin_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setMember(null);
            accessManagerMockedStatic.when(() -> AccessManager.getSuperAdmin(anyString())).thenReturn(accessPolicy);

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setContactId("123");
            userPro.setParentContactId("112");
            userPro.setDelete(false);
            userPro.setRole(Skillset.ROLE_ADMIN);

            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setContactId("113");
            userPro2.setParentContactId("112");
            userPro2.setDelete(false);
            userPro2.setRole(Skillset.ROLE_ADMIN);

            List<PeopleRelationJDO> userList = new ArrayList<>();
            userList.add(userPro);
            userList.add(userPro2);

            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getUsersByRole(anyString(),anyString())).thenReturn(userList);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);

            PeopleRelationJDO expectedPro = UserPROUtil.getPrimaryAdmin("accountId");
            Assertions.assertNull(expectedPro);
        }
    }

    @Test
    void getPrimaryAdmin_fetchFromDB_noContact_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            accessManagerMockedStatic.when(() -> AccessManager.getSuperAdmin(anyString())).thenThrow(new IllegalArgumentException("exception"));

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setContactId("123");
            userPro.setParentContactId("123");
            userPro.setDelete(false);
            userPro.setRole(Skillset.ROLE_ADMIN);

            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setContactId("113");
            userPro2.setParentContactId("123");
            userPro2.setDelete(false);
            userPro2.setRole(Skillset.ROLE_ADMIN);

            List<PeopleRelationJDO> userList = new ArrayList<>();
            userList.add(userPro);
            userList.add(userPro2);

            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getUsersByRole(anyString(),anyString())).thenReturn(userList);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);

            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(null);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);

            PeopleRelationJDO expectedPro = UserPROUtil.getPrimaryAdmin("accountId");
            Assertions.assertNull(expectedPro.getContact());
        }
    }

    @Test
    void getPrimaryAdmin_fetchFromDB_noUser_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            accessManagerMockedStatic.when(() -> AccessManager.getSuperAdmin(anyString())).thenReturn(null);

            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getUsersByRole(anyString(),anyString())).thenReturn(null);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);

            PeopleRelationJDO expectedPro = UserPROUtil.getPrimaryAdmin("accountId");
            Assertions.assertNull(expectedPro);
        }
    }

    @Test
    void getPrimaryAdmin_IAM_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setMember("123");
            accessManagerMockedStatic.when(() -> AccessManager.getSuperAdmin(anyString())).thenReturn(accessPolicy);

            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(false);
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);

            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact();
            contact.setId("123");
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);

            PeopleRelationJDO expectedPro = UserPROUtil.getPrimaryAdmin("accountId");
            Assertions.assertEquals(userPro,expectedPro);
        }
    }

    @Test
    void getUserProByEmailID_nullUser_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getUserByEmailID(anyString(),anyString())).thenReturn(null);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Assertions.assertNull(UserPROUtil.getUserProByEmailID("accountId","emailId"));
            Mockito.verify(userImplMock).getUserByEmailID("accountId","emailId");
        }
    }

    @Test
    void getUserProByEmailID_nullContact_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            PeopleRelationJDO user = new PeopleRelationJDO();
            user.setContactId("contactId");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getUserByEmailID(anyString(),anyString())).thenReturn(user);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);

            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(null);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);

            Assertions.assertNotNull(UserPROUtil.getUserProByEmailID("accountId","emailId"));
            Mockito.verify(userImplMock).getUserByEmailID("accountId","emailId");
            Mockito.verify(contactImplMock).getByID("contactId");
        }
    }

    @Test
    void getUserProByEmailID_validContact_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            PeopleRelationJDO user = new PeopleRelationJDO();
            user.setContactId("contactId");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getUserByEmailID(anyString(),anyString())).thenReturn(user);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);

            Contact contact = new Contact();
            contact.setId("contactId");
            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);

            user.setContact(contact);
            Assertions.assertEquals(user,UserPROUtil.getUserProByEmailID("accountId","emailId"));
            Mockito.verify(userImplMock).getUserByEmailID("accountId","emailId");
            Mockito.verify(contactImplMock).getByID("contactId");
        }
    }

    @Test
    void removeTeamIDFromProSkillset_nullUserPro_Test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","contactID")).thenReturn(null);
            userPROUtilMockedStatic.when(()->UserPROUtil.removeTeamIDFromProSkillset("accID","contactID","teamID")).thenCallRealMethod();
            UserPROUtil.removeTeamIDFromProSkillset("accID","contactID","teamID");
            userPROUtilMockedStatic.verify(()->UserPROUtil.removeTeamIDFromProSkillset(any(PeopleRelationJDO.class),anyString()), Mockito.times(0));
        }
    }

    @Test
    void removeTeamIDFromProSkillset_validUserPro_Test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","contactID")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()->UserPROUtil.removeTeamIDFromProSkillset("accID","contactID","teamID")).thenCallRealMethod();
            UserPROUtil.removeTeamIDFromProSkillset("accID","contactID","teamID");
            userPROUtilMockedStatic.verify(()->UserPROUtil.removeTeamIDFromProSkillset(userPro,"teamID"), Mockito.times(1));
        }
    }

    @Test
    void extractTeamSkillSetMap_nullUserPro_test(){
        Assertions.assertTrue(UserPROUtil.extractTeamSkillSetMap(new PeopleRelationJDO()).isEmpty());
    }

    @Test
    void extractTeamSkillSetMap_invalidTeamSkillsetString_test(){
        PeopleRelationJDO userPRO = new PeopleRelationJDO();
        userPRO.setSkillsets(JsonUtil.getJson(Map.of("teamSkillset","")));
        Assertions.assertTrue(UserPROUtil.extractTeamSkillSetMap(userPRO).isEmpty());
    }

    @Test
    void extractTeamSkillSetMap_validTeamSkillsetString_test(){
        PeopleRelationJDO userPRO = new PeopleRelationJDO();
        userPRO.setSkillsets(JsonUtil.getJson(Map.of("teamSkillset","{\"key\":\"value\"}")));
        Assertions.assertEquals(Map.of("key","value"),UserPROUtil.extractTeamSkillSetMap(userPRO));
    }

    @Test
    void removeTeamIDFromProSkillset_nullUserSkillSetMap_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()->UserPROUtil.extractPROSkills(userPro)).thenCallRealMethod();
            userPROUtilMockedStatic.when(()->UserPROUtil.extractTeamSkillSetMap(any(Map.class))).thenCallRealMethod();
            userPROUtilMockedStatic.when(()->UserPROUtil.removeTeamIDFromProSkillset(userPro,"teamID")).thenCallRealMethod();
            UserPROUtil.removeTeamIDFromProSkillset(userPro,"teamID");
            userPROUtilMockedStatic.verify(()->UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),any(Map.class)), Mockito.times(0));
        }
    }

    @Test
    void removeTeamIDFromProSkillset_emptyTeamSkillSetMap_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()->UserPROUtil.extractPROSkills(userPro)).thenCallRealMethod();
            userPROUtilMockedStatic.when(()->UserPROUtil.extractTeamSkillSetMap(any(Map.class))).thenCallRealMethod();
            userPro.setSkillsets(JsonUtil.getJson(Map.of("teamSkillset","")));
            userPROUtilMockedStatic.when(()->UserPROUtil.removeTeamIDFromProSkillset(userPro,"teamID")).thenCallRealMethod();
            UserPROUtil.removeTeamIDFromProSkillset(userPro,"teamID");
            userPROUtilMockedStatic.verify(()->UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),any(Map.class)), Mockito.times(0));
        }
    }

    @Test
    void removeTeamIDFromProSkillset_validTeamSkillSetMapContainsTeamID_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()->UserPROUtil.extractPROSkills(userPro)).thenCallRealMethod();
            userPROUtilMockedStatic.when(()->UserPROUtil.extractTeamSkillSetMap(any(Map.class))).thenCallRealMethod();
            userPro.setSkillsets(JsonUtil.getJson(Map.of("teamSkillset","{\"teamID\":\"\"}")));
            userPROUtilMockedStatic.when(()->UserPROUtil.removeTeamIDFromProSkillset(userPro,"teamID")).thenCallRealMethod();
            UserPROUtil.removeTeamIDFromProSkillset(userPro,"teamID");
            userPROUtilMockedStatic.verify(()->UserPROUtil.updatePROSkill(userPro,Map.of("teamSkillset","{}")), Mockito.times(1));
        }
    }

    @Test
    void isPrimaryAdmin_nullPro_test(){
        Assertions.assertFalse(UserPROUtil.isPrimaryAdmin(null));
    }

    @Test
    void isPrimaryAdmin_notPrimaryAdmin_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("123");
        userPro.setParentContactId("234");
        Assertions.assertFalse(UserPROUtil.isPrimaryAdmin(userPro));
    }

    @Test
    void isPrimaryAdmin_PrimaryAdmin_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("123");
        userPro.setParentContactId("123");
        Assertions.assertTrue(UserPROUtil.isPrimaryAdmin(userPro));
    }

    @Test
    void isUserAssociatedToYoco_noAssociation_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUserProsForUserByEmail("email",false)).thenReturn(List.of());
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Assertions.assertFalse(UserPROUtil.isUserAssociatedToYoco("email"));
        }
    }

    @Test
    void isUserAssociatedToYoco_validAssociation_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getAllUserProsForUserByEmail("email",false)).thenReturn(List.of(new PeopleRelationJDO()));
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Assertions.assertTrue(UserPROUtil.isUserAssociatedToYoco("email"));
        }
    }
}
