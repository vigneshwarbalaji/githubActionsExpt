package com.yoco.contact.helper.hook;

import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.Status;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.contact.helper.ContactHelper;
import com.yoco.user.helper.staff.UserStaffHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class CreateContactHookHelperTest {

    CreateContactHookHelper contactHookHelper = CreateContactHookHelper.getInstance();

    @Test
    void extractValidAccounts_null_dcm_linked_accounts_test(){
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(contactHookHelper.extractValidAccounts(List.of())));
    }

    @Test
    void extractValidAccounts_null_accounts_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl account = Mockito.mock(AccountImpl.class);
            Mockito.when(account.getAccounts(anyList())).thenReturn(null);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(account);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(contactHookHelper.extractValidAccounts(List.of("acc1,acc2"))));
        }
    }

    @Test
    void extractValidAccounts_valid_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl account = Mockito.mock(AccountImpl.class);
            Mockito.when(account.getAccounts(anyList())).thenReturn(List.of(new SettingsJDO()));
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(account);
            Assertions.assertFalse(ObjUtils.isNullOrEmpty(contactHookHelper.extractValidAccounts(List.of("acc1,acc2"))));
        }
    }

    @Test
    void extractProCreationList_noProsExists_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl user = Mockito.mock(UserImpl.class);
            Mockito.when(user.getAllUserProsForUser(anyString(),any())).thenReturn(null);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(user);
            List<SettingsJDO> accountsList = contactHookHelper.extractProCreationList(List.of(new SettingsJDO()),"contId");
            Assertions.assertEquals(1,accountsList.size());
        }
    }

    @Test
    void extractProCreationList_allProExists_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accId");
            UserImpl user = Mockito.mock(UserImpl.class);
            Mockito.when(user.getAllUserProsForUser(anyString(),any())).thenReturn(List.of(userPro));
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(user);
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accId");
            account.setStatus(Status.ACTIVE.toString());
            List<SettingsJDO> accountsList = contactHookHelper.extractProCreationList(List.of(account),"contId");
            Assertions.assertEquals(0,accountsList.size());
        }
    }


    @Test
    void extractProCreationList_noProMatch_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accId1");
            UserImpl user = Mockito.mock(UserImpl.class);
            Mockito.when(user.getAllUserProsForUser(anyString(),any())).thenReturn(List.of(userPro));
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(user);
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accId");
            account.setStatus(Status.ACTIVE.toString());
            List<SettingsJDO> accountsList = contactHookHelper.extractProCreationList(List.of(account),"contId");
            Assertions.assertEquals(1,accountsList.size());
        }
    }

    @Test
    void extractProCreationList_partialProMatch_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accId1");
            PeopleRelationJDO userPro1 = new PeopleRelationJDO();
            userPro1.setUniquepin("accId");
            UserImpl user = Mockito.mock(UserImpl.class);
            Mockito.when(user.getAllUserProsForUser(anyString(),any())).thenReturn(List.of(userPro,userPro1));
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(user);
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accId");
            account.setStatus(Status.ACTIVE.toString());
            SettingsJDO account1 = new SettingsJDO();
            account1.setPeopleUniquePin("accId2");
            account1.setStatus(Status.ACTIVE.toString());
            List<SettingsJDO> accountsList = contactHookHelper.extractProCreationList(List.of(account,account1),"contId");
            Assertions.assertEquals(1,accountsList.size());
            SettingsJDO expectedAccount = accountsList.get(0);
            Assertions.assertEquals("accId2",expectedAccount.getPeopleUniquePin());
        }
    }

    @Test
    void buildActivityMessage_basic_test(){
        Contact contact = new Contact();
        contact.setId("Id");
        contact.setEmailID("test@gmail.com");
        contact.setFirstName("fName");
        String actualActivityMsg = contactHookHelper.buildActivityMessage(contact);
        Assertions.assertEquals("Contact created for emailID :: test@gmail.com contactID : Id Details are :Firstname :: fName.",actualActivityMsg);
    }

    @Test
    void buildActivityMessage_valid_lastName_test(){
        Contact contact = new Contact();
        contact.setId("Id");
        contact.setEmailID("test@gmail.com");
        contact.setFirstName("fName");
        contact.setLastName("lName");
        String actualActivityMsg = contactHookHelper.buildActivityMessage(contact);
        Assertions.assertEquals("Contact created for emailID :: test@gmail.com contactID : Id Details are :Firstname :: fName, Lastname :: lName.",actualActivityMsg);
    }

    @Test
    void buildActivityMessage_valid_photoId_test(){
        Contact contact = new Contact();
        contact.setId("Id");
        contact.setEmailID("test@gmail.com");
        contact.setFirstName("fName");
        contact.setPhotoID("photo.png");
        String actualActivityMsg = contactHookHelper.buildActivityMessage(contact);
        Assertions.assertEquals("Contact created for emailID :: test@gmail.com contactID : Id Details are :Firstname :: fName, Photo exists.",actualActivityMsg);
    }

    @Test
    void buildActivityMessage_valid_test(){
        Contact contact = new Contact();
        contact.setId("Id");
        contact.setEmailID("test@gmail.com");
        contact.setFirstName("fName");
        contact.setLastName("lName");
        contact.setPhotoID("photo.png");
        String actualActivityMsg = contactHookHelper.buildActivityMessage(contact);
        Assertions.assertEquals("Contact created for emailID :: test@gmail.com contactID : Id Details are :Firstname :: fName, Lastname :: lName, Photo exists.",actualActivityMsg);
    }

    @Test
    void createContactFromHook_invalid_Default_domain_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenReturn("");
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("id");
            dcmContactDTO.setLogin("test@gmail.com");
            contactHookHelper.processContactCreation(dcmContactDTO);
            dcmUtilMockedStatic.verify(()-> DcmUtil.getDefaultAccountIDWithEmailID("test@gmail.com"),times(1));
        }
    }

    @Test
    void createContactFromHook_invalid_dcm_accounts_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenReturn("accId");
            AccountImpl accountMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountMock.getAccounts(anyList())).thenReturn(null);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountMock);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("id");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setLinkedAccounts(List.of("accId"));
            contactHookHelper.processContactCreation(dcmContactDTO);
            dcmUtilMockedStatic.verify(()-> DcmUtil.getDefaultAccountIDWithEmailID("test@gmail.com"),times(1));
            Mockito.verify(accountMock,times(1)).getAccounts(List.of("accId"));
        }
    }

    @Test
    void createContactFromHook_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<ContactHelper> contactHelperMockedStatic = Mockito.mockStatic(ContactHelper.class);
            MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class)){

            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenReturn("accId");
            SettingsJDO account1 = new SettingsJDO();
            account1.setPeopleUniquePin("accId");
            SettingsJDO account2 = new SettingsJDO();
            account2.setPeopleUniquePin("accId2");
            AccountImpl accountMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountMock.getAccounts(anyList())).thenReturn(List.of(account1,account2));
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountMock);

            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getAllUserProsForUser(anyString(),any())).thenReturn(null);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            Contact contact = new Contact("id","test@gmail.com","fName","lName","photo.png",0l);
            ContactHelper contactHelper = Mockito.mock(ContactHelper.class);
            Mockito.when(contactHelper.validateAndCreateContact(any(DcmContactDTO.class))).thenReturn(contact);
            contactHelperMockedStatic.when(ContactHelper::getInstance).thenReturn(contactHelper);

            UserStaffHelper userStaffHelper = Mockito.mock(UserStaffHelper.class);
            Mockito.when(userStaffHelper.createUserPRO(any(SettingsJDO.class),anyBoolean(),any(Contact.class),anyString())).thenReturn(new PeopleRelationJDO());
            Mockito.when(userStaffHelper.createUserEventsHandler(any(),any(SettingsJDO.class),anyBoolean(),anyString(),any(PeopleRelationJDO.class))).thenReturn(Map.of());
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelper);

            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("id");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setLinkedAccounts(List.of("accId,accId2"));
            contactHookHelper.processContactCreation(dcmContactDTO);

            dcmUtilMockedStatic.verify(()-> DcmUtil.getDefaultAccountIDWithEmailID("test@gmail.com"),times(1));
            Mockito.verify(accountMock,times(1)).getAccounts(List.of("accId,accId2"));
            Mockito.verify(userMock).getAllUserProsForUser("id",null);
            Mockito.verify(contactHelper).validateAndCreateContact(dcmContactDTO);
        }
    }


}