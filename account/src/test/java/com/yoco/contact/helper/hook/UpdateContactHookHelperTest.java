package com.yoco.contact.helper.hook;

import com.fullauth.api.manage.exception.FullAuthApiException;
import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.enums.Status;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.contact.helper.ProfileHelper;
import com.yoco.user.helper.staff.UserStaffHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.mockito.ArgumentMatchers.*;

class UpdateContactHookHelperTest {

    UpdateContactHookHelper updateContactHookHelper = UpdateContactHookHelper.getInstance();

    @Test
    void validateAndProcessProfileInfoUpdation_emailID_update_test(){
        UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        dcmContactDTO.setId("contId");
        dcmContactDTO.setLogin("test@gmail.com");
        dcmContactDTO.setFirstName("fName");
        dcmContactDTO.setLastName("lName");
        dcmContactDTO.setPhotoID("pic");
        Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
        Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        updateContactHookHelper.validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        Contact newContact = contact;
        newContact.setEmailID("test@gmail.com");
        Mockito.verify(updateContactHookHelper).processProfileInfoUpdation(dcmContactDTO,newContact,new ArrayList<>(),"Contact update from DCM. Details are : contact emailID : test1@gmail.com DCM emailID : test@gmail.com ",true);
    }

    @Test
    void validateAndProcessProfileInfoUpdation_firstName_update_test(){
        UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        dcmContactDTO.setId("contId");
        dcmContactDTO.setLogin("test@gmail.com");
        dcmContactDTO.setFirstName("fName");
        dcmContactDTO.setLastName("lName");
        dcmContactDTO.setPhotoID("pic");
        Contact contact = new Contact("contId","test@gmail.com","name","lName","pic",0L);
        Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        updateContactHookHelper.validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        Contact newContact = contact;
        newContact.setFirstName("fName");
        Mockito.verify(updateContactHookHelper).processProfileInfoUpdation(dcmContactDTO,newContact,new ArrayList<>(),"Contact update from DCM. Details are : contact FirstName : name DCM FirstName : fName ",false);
    }

    @Test
    void validateAndProcessProfileInfoUpdation_lastName_update_test(){
        UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        dcmContactDTO.setId("contId");
        dcmContactDTO.setLogin("test@gmail.com");
        dcmContactDTO.setFirstName("fName");
        dcmContactDTO.setLastName("lName");
        dcmContactDTO.setPhotoID("pic");
        Contact contact = new Contact("contId","test@gmail.com","fName","name","pic",0L);
        Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        updateContactHookHelper.validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        Contact newContact = contact;
        newContact.setLastName("lName");
        Mockito.verify(updateContactHookHelper).processProfileInfoUpdation(dcmContactDTO,newContact,new ArrayList<>(),"Contact update from DCM. Details are : contact LastName : name DCM LastName : lName ",false);
    }

    @Test
    void validateAndProcessProfileInfoUpdation_photoID_update_test(){
        UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        dcmContactDTO.setId("contId");
        dcmContactDTO.setLogin("test@gmail.com");
        dcmContactDTO.setFirstName("fName");
        dcmContactDTO.setLastName("lName");
        dcmContactDTO.setPhotoID("pic");
        Contact contact = new Contact("contId","test@gmail.com","fName","lName","pic.pic",0L);
        Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        updateContactHookHelper.validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        Contact newContact = contact;
        newContact.setPhotoID("pic");
        Mockito.verify(updateContactHookHelper).processProfileInfoUpdation(dcmContactDTO,newContact,new ArrayList<>(),"Contact update from DCM. Details are : contact photoID : pic.pic DCM photoID : pic",false);
    }

    @Test
    void validateAndProcessProfileInfoUpdation_null_photoID_emptyEmail_emptyFirstName_test(){
        UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        dcmContactDTO.setId("contId");
        dcmContactDTO.setLogin("");
        dcmContactDTO.setFirstName("");
        dcmContactDTO.setLastName("lName");
        dcmContactDTO.setPhotoID(null);
        Contact contact = new Contact("contId","test@gmail.com","fName","lName","pic.pic",0L);
        Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        updateContactHookHelper.validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        Contact newContact = contact;
        newContact.setPhotoID("pic");
        Mockito.verify(updateContactHookHelper,Mockito.times(0)).processProfileInfoUpdation(dcmContactDTO,newContact,new ArrayList<>(),"Contact update from DCM. Details are : contact photoID : pic.pic DCM photoID : pic",false);
    }

    @Test
    void validateAndProcessProfileInfoUpdation_emailID_And_firstName_update_test(){
        UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        dcmContactDTO.setId("contId");
        dcmContactDTO.setLogin("test@gmail.com");
        dcmContactDTO.setFirstName("fName");
        dcmContactDTO.setLastName("lName");
        dcmContactDTO.setPhotoID("pic");
        Contact contact = new Contact("contId","test1@gmail.com","name","lName","pic",0L);
        Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        updateContactHookHelper.validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        Contact newContact = contact;
        newContact.setFirstName("fName");
        newContact.setEmailID("test@gmail.com");
        Mockito.verify(updateContactHookHelper).processProfileInfoUpdation(dcmContactDTO,newContact,new ArrayList<>(),"Contact update from DCM. Details are : contact emailID : test1@gmail.com DCM emailID : test@gmail.com contact FirstName : name DCM FirstName : fName ",true);
    }

    @Test
    void validateAndProcessProfileInfoUpdation_emailID_And_lastName_update_test(){
        UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        dcmContactDTO.setId("contId");
        dcmContactDTO.setLogin("test@gmail.com");
        dcmContactDTO.setFirstName("fName");
        dcmContactDTO.setLastName("lName");
        dcmContactDTO.setPhotoID("pic");
        Contact contact = new Contact("contId","test1@gmail.com","fName","name","pic",0L);
        Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        updateContactHookHelper.validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        Contact newContact = contact;
        newContact.setLastName("lName");
        newContact.setEmailID("test@gmail.com");
        Mockito.verify(updateContactHookHelper).processProfileInfoUpdation(dcmContactDTO,newContact,new ArrayList<>(),"Contact update from DCM. Details are : contact emailID : test1@gmail.com DCM emailID : test@gmail.com contact LastName : name DCM LastName : lName ",true);
    }

    @Test
    void validateAndProcessProfileInfoUpdation_emailID_And_photoID_update_test(){
        UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        dcmContactDTO.setId("contId");
        dcmContactDTO.setLogin("test@gmail.com");
        dcmContactDTO.setFirstName("fName");
        dcmContactDTO.setLastName("lName");
        dcmContactDTO.setPhotoID("pic");
        Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic.pic",0L);
        Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        updateContactHookHelper.validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        Contact newContact = contact;
        newContact.setPhotoID("pic");
        newContact.setEmailID("test@gmail.com");
        Mockito.verify(updateContactHookHelper).processProfileInfoUpdation(dcmContactDTO,newContact,new ArrayList<>(),"Contact update from DCM. Details are : contact emailID : test1@gmail.com DCM emailID : test@gmail.com contact photoID : pic.pic DCM photoID : pic",true);
    }


    @Test
    void validateAndProcessProfileInfoUpdation_all_update_test(){
        UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        dcmContactDTO.setId("contId");
        dcmContactDTO.setLogin("test@gmail.com");
        dcmContactDTO.setFirstName("fName");
        dcmContactDTO.setLastName("lName");
        dcmContactDTO.setPhotoID("pic");
        Contact contact = new Contact("contId","test1@gmail.com","test","","pic.pic",0L);
        Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        updateContactHookHelper.validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        Contact newContact = contact;
        newContact.setFirstName("fName");
        newContact.setLastName("lName");
        newContact.setPhotoID("pic");
        newContact.setEmailID("test@gmail.com");
        Mockito.verify(updateContactHookHelper).processProfileInfoUpdation(dcmContactDTO,newContact,new ArrayList<>(),"Contact update from DCM. Details are : contact emailID : test1@gmail.com DCM emailID : test@gmail.com contact FirstName : test DCM FirstName : fName contact LastName :  DCM LastName : lName contact photoID : pic.pic DCM photoID : pic",true);
    }

    @Test
    void validateAndProcessProfileInfoUpdation_no_update_test(){
        UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        dcmContactDTO.setId("contId");
        dcmContactDTO.setLogin("test@gmail.com");
        dcmContactDTO.setFirstName("fName");
        dcmContactDTO.setLastName("lName");
        dcmContactDTO.setPhotoID("pic");
        Contact contact = new Contact("contId","test@gmail.com","fName","lName","pic",0L);
        Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        updateContactHookHelper.validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        Contact newContact = contact;
        newContact.setFirstName("fName");
        newContact.setLastName("lName");
        newContact.setPhotoID("pic");
        newContact.setEmailID("test@gmail.com");
        Mockito.verify(updateContactHookHelper,Mockito.times(0)).processProfileInfoUpdation(dcmContactDTO,newContact,new ArrayList<>(),"contact emailID : test1@gmail.com DCM emailID : test@gmail.com contact FirstName : test DCM FirstName : fName contact LastName :  DCM LastName : lName contact photoID : pic.pic DCM photoID : pic",true);
    }

    @Test
    void validateAndProcessProfileInfoUpdation_exception_test(){
        UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
        DcmContactDTO dcmContactDTO = new DcmContactDTO();
        dcmContactDTO.setId("contId");
        dcmContactDTO.setLogin("test@gmail.com");
        dcmContactDTO.setFirstName("fName");
        dcmContactDTO.setLastName("lName");
        dcmContactDTO.setPhotoID("pic");
        Contact contact = new Contact("contId",null,"fName","lName","pic",0L);
        Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        updateContactHookHelper.validateAndProcessProfileInfoUpdation(dcmContactDTO,contact,new ArrayList<>());
        Contact newContact = contact;
        newContact.setFirstName("fName");
        newContact.setLastName("lName");
        newContact.setPhotoID("pic");
        newContact.setEmailID("test@gmail.com");
        Mockito.verify(updateContactHookHelper,Mockito.times(0)).processProfileInfoUpdation(dcmContactDTO,newContact,new ArrayList<>(),"contact emailID : test1@gmail.com DCM emailID : test@gmail.com contact FirstName : test DCM FirstName : fName contact LastName :  DCM LastName : lName contact photoID : pic.pic DCM photoID : pic",true);
    }

    @Test
    void processProfileInfoUpdation_deleteUser_test(){
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.saveContact(any(Contact.class))).thenReturn(new Contact());
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            Contact contact = new Contact();
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(true);
            List<PeopleRelationJDO> proList = new ArrayList<>();
            proList.add(userPro);
            updateContactHookHelper.processProfileInfoUpdation(dcmContactDTO,contact,proList,"activity",false);
            Mockito.verify(contactMock,Mockito.times(1)).saveContact(any(Contact.class));
            profileHelperMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void processProfileInfoUpdation_deleteUser_emailUpdate_test(){
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.saveContact(any(Contact.class))).thenReturn(new Contact());
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.doNothing().when(userMock).savePros(anyList());
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(null);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            Contact contact = new Contact();
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(true);
            List<PeopleRelationJDO> proList = new ArrayList<>();
            proList.add(userPro);
            updateContactHookHelper.processProfileInfoUpdation(dcmContactDTO,contact,proList,"activity",true);
            Mockito.verify(contactMock,Mockito.times(1)).saveContact(any(Contact.class));
            profileHelperMockedStatic.verifyNoInteractions();
            Mockito.verify(userMock,Mockito.times(1)).savePros(anyList());
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"));
        }
    }

    @Test
    void processProfileInfoUpdation_activeUser_test(){
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.saveContact(any(Contact.class))).thenReturn(new Contact());
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            ProfileHelper profileHelperMock = Mockito.mock(ProfileHelper.class);
            Mockito.when(profileHelperMock.initiateProfileUpdateQueue(any(Contact.class),any(PeopleRelationJDO.class),anyString())).thenReturn(new UserDTO());
            profileHelperMockedStatic.when(ProfileHelper::getInstance).thenReturn(profileHelperMock);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            Contact contact = new Contact();
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(false);
            List<PeopleRelationJDO> proList = new ArrayList<>();
            proList.add(userPro);
            updateContactHookHelper.processProfileInfoUpdation(dcmContactDTO,contact,proList,"activity",false);
            Mockito.verify(contactMock,Mockito.times(1)).saveContact(any(Contact.class));
            Mockito.verify(profileHelperMock,Mockito.times(1)).initiateProfileUpdateQueue(any(Contact.class),any(PeopleRelationJDO.class),anyString());
        }
    }

    @Test
    void processProfileInfoUpdation_activeUser_emailUpdate_test(){
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.saveContact(any(Contact.class))).thenReturn(new Contact());
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.doNothing().when(userMock).savePros(anyList());
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(null);
            ProfileHelper profileHelperMock = Mockito.mock(ProfileHelper.class);
            Mockito.when(profileHelperMock.initiateProfileUpdateQueue(any(Contact.class),any(PeopleRelationJDO.class),anyString())).thenReturn(new UserDTO());
            profileHelperMockedStatic.when(ProfileHelper::getInstance).thenReturn(profileHelperMock);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            Contact contact = new Contact();
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setDelete(false);
            List<PeopleRelationJDO> proList = new ArrayList<>();
            proList.add(userPro);
            updateContactHookHelper.processProfileInfoUpdation(dcmContactDTO,contact,proList,"activity",true);
            Mockito.verify(contactMock,Mockito.times(1)).saveContact(any(Contact.class));
            Mockito.verify(userMock,Mockito.times(1)).savePros(anyList());
            Mockito.verify(profileHelperMock,Mockito.times(1)).initiateProfileUpdateQueue(any(Contact.class),any(PeopleRelationJDO.class),anyString());
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"));
        }
    }

    @Test
    void processProfileInfoUpdation_multipleUsers_test(){
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.saveContact(any(Contact.class))).thenReturn(new Contact());
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            ProfileHelper profileHelperMock = Mockito.mock(ProfileHelper.class);
            Mockito.when(profileHelperMock.initiateProfileUpdateQueue(any(Contact.class),any(PeopleRelationJDO.class),anyString())).thenReturn(new UserDTO());
            profileHelperMockedStatic.when(ProfileHelper::getInstance).thenReturn(profileHelperMock);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            Contact contact = new Contact();
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("111");
            userPro.setDelete(false);
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setUniquepin("112");
            userPro2.setDelete(true);
            PeopleRelationJDO userPro3 = new PeopleRelationJDO();
            userPro3.setUniquepin("113");
            userPro3.setDelete(true);
            PeopleRelationJDO userPro4 = new PeopleRelationJDO();
            userPro4.setUniquepin("114");
            userPro4.setDelete(false);
            List<PeopleRelationJDO> proList = new ArrayList<>();
            proList.add(userPro);
            proList.add(userPro2);
            proList.add(userPro3);
            proList.add(userPro4);
            updateContactHookHelper.processProfileInfoUpdation(dcmContactDTO,contact,proList,"activity",false);
            Mockito.verify(contactMock,Mockito.times(1)).saveContact(any(Contact.class));
            userMockedStatic.verifyNoInteractions();
            Mockito.verify(profileHelperMock,Mockito.times(2)).initiateProfileUpdateQueue(any(Contact.class),any(PeopleRelationJDO.class),anyString());
            accessManagerMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void processProfileInfoUpdation_multipleUsers_emailUpdate_test(){
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.saveContact(any(Contact.class))).thenReturn(new Contact());
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.doNothing().when(userMock).savePros(anyList());
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(null);
            ProfileHelper profileHelperMock = Mockito.mock(ProfileHelper.class);
            Mockito.when(profileHelperMock.initiateProfileUpdateQueue(any(Contact.class),any(PeopleRelationJDO.class),anyString())).thenReturn(new UserDTO());
            profileHelperMockedStatic.when(ProfileHelper::getInstance).thenReturn(profileHelperMock);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            Contact contact = new Contact();
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("111");
            userPro.setDelete(false);
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setUniquepin("112");
            userPro2.setDelete(true);
            PeopleRelationJDO userPro3 = new PeopleRelationJDO();
            userPro3.setUniquepin("113");
            userPro3.setDelete(true);
            PeopleRelationJDO userPro4 = new PeopleRelationJDO();
            userPro4.setUniquepin("114");
            userPro4.setDelete(false);
            List<PeopleRelationJDO> proList = new ArrayList<>();
            proList.add(userPro);
            proList.add(userPro2);
            proList.add(userPro3);
            proList.add(userPro4);
            updateContactHookHelper.processProfileInfoUpdation(dcmContactDTO,contact,proList,"activity",true);
            Mockito.verify(contactMock,Mockito.times(1)).saveContact(any(Contact.class));
            Mockito.verify(userMock,Mockito.times(1)).savePros(anyList());
            Mockito.verify(profileHelperMock,Mockito.times(2)).initiateProfileUpdateQueue(any(Contact.class),any(PeopleRelationJDO.class),anyString());
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"));
        }
    }

    @Test
    void updatePrimaryUserAccountsWithMailID_no_policies_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(null);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            updateContactHookHelper.updateSettingsJdoForPrimaryUsersWithMailID(dcmContactDTO);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"));
        }
    }

    @Test
    void updatePrimaryUserAccountsWithMailID_no_policies_test2(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(List.of());
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            updateContactHookHelper.updateSettingsJdoForPrimaryUsersWithMailID(dcmContactDTO);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"));
        }
    }

    @Test
    void updatePrimaryUserAccountsWithMailID_no_super_admins_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setPermissions(Set.of(IAMPermission.ACTIVITY.toString()));
            List<AccessPolicy> accessPolicyList = new ArrayList<>();
            accessPolicyList.add(accessPolicy);
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(accessPolicyList);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            updateContactHookHelper.updateSettingsJdoForPrimaryUsersWithMailID(dcmContactDTO);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"));
        }
    }

    @Test
    void updatePrimaryUserAccountsWithMailID_no_super_admin_accounts_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setPermissions(Set.of(IAMPermission.SUPER_ADMIN.toString()));
            accessPolicy.setResource("account/123");
            List<AccessPolicy> accessPolicyList = new ArrayList<>();
            accessPolicyList.add(accessPolicy);
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(accessPolicyList);
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getAccounts(anyList())).thenReturn(null);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            updateContactHookHelper.updateSettingsJdoForPrimaryUsersWithMailID(dcmContactDTO);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"));
            Mockito.verify(accountImplMock).getAccounts(List.of("123"));
        }
    }

    @Test
    void updatePrimaryUserAccountsWithMailID_single_super_admin_accounts_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setPermissions(Set.of(IAMPermission.SUPER_ADMIN.toString()));
            accessPolicy.setResource("account/123");
            List<AccessPolicy> accessPolicyList = new ArrayList<>();
            accessPolicyList.add(accessPolicy);
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(accessPolicyList);
            SettingsJDO account = new SettingsJDO();
            account.setSourceEmail("source@gmail.com");
            List<SettingsJDO> accountsList = new ArrayList<>();
            accountsList.add(account);
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getAccounts(anyList())).thenReturn(accountsList);
            Mockito.doNothing().when(accountImplMock).saveAccounts(anyList());
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            updateContactHookHelper.updateSettingsJdoForPrimaryUsersWithMailID(dcmContactDTO);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"));
            Mockito.verify(accountImplMock).getAccounts(List.of("123"));
            SettingsJDO savedAccount = new SettingsJDO();
            savedAccount.setSourceEmail("test@gmail.com");
            Mockito.verify(accountImplMock).saveAccounts(List.of(savedAccount));
        }
    }

    @Test
    void updatePrimaryUserAccountsWithMailID_multiple_super_admin_accounts_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setPermissions(Set.of(IAMPermission.SUPER_ADMIN.toString()));
            accessPolicy.setResource("account/123");
            AccessPolicy accessPolicy2 = new AccessPolicy();
            accessPolicy2.setPermissions(Set.of(IAMPermission.SUPER_ADMIN.toString()));
            accessPolicy2.setResource("account/1233");
            List<AccessPolicy> accessPolicyList = new ArrayList<>();
            accessPolicyList.add(accessPolicy);
            accessPolicyList.add(accessPolicy2);
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(accessPolicyList);
            SettingsJDO account = new SettingsJDO();
            account.setSourceEmail("source@gmail.com");
            List<SettingsJDO> accountsList = new ArrayList<>();
            accountsList.add(account);
            accountsList.add(account);
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getAccounts(anyList())).thenReturn(accountsList);
            Mockito.doNothing().when(accountImplMock).saveAccounts(anyList());
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            updateContactHookHelper.updateSettingsJdoForPrimaryUsersWithMailID(dcmContactDTO);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"));
            Mockito.verify(accountImplMock).getAccounts(List.of("123","1233"));
            SettingsJDO savedAccount = new SettingsJDO();
            savedAccount.setSourceEmail("test@gmail.com");
            Mockito.verify(accountImplMock).saveAccounts(List.of(savedAccount,savedAccount));
        }
    }

    @Test
    void updatePrimaryUserAccountsWithMailID_exception_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenThrow(new FullAuthApiException("exception"));
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            updateContactHookHelper.updateSettingsJdoForPrimaryUsersWithMailID(dcmContactDTO);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"));
        }
    }

    @Test
    void processContactUpdation_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getAllUserProsForUser(anyString(),any())).thenReturn(new ArrayList<>());
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);
            UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setFirstName("fName");
            dcmContactDTO.setLastName("lName");
            dcmContactDTO.setPhotoID("pic");
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            Mockito.doCallRealMethod().when(updateContactHookHelper).processContactUpdation(dcmContactDTO,contact);
            updateContactHookHelper.processContactUpdation(dcmContactDTO,contact);
            Contact newContact = contact;
            newContact.setEmailID("test@gmail.com");
            Mockito.verify(updateContactHookHelper).validateAndProcessProfileInfoUpdation(dcmContactDTO,newContact,new ArrayList<>());
            Mockito.verify(updateContactHookHelper).validateAndProcessProsAssociation(dcmContactDTO,newContact,new ArrayList<>());
        }
    }

    @Test
    void validateAndProcessProsAssociation_null_valid_Accounts_test(){
        try(MockedStatic<CreateContactHookHelper> userMockedStatic = Mockito.mockStatic(CreateContactHookHelper.class)){
            CreateContactHookHelper contactHookHelper = Mockito.mock(CreateContactHookHelper.class);
            Mockito.when(contactHookHelper.extractValidAccounts(anyList())).thenReturn(null);
            userMockedStatic.when(CreateContactHookHelper::getInstance).thenReturn(contactHookHelper);
            UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setFirstName("fName");
            dcmContactDTO.setLastName("lName");
            dcmContactDTO.setPhotoID("pic");
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProsAssociation(dcmContactDTO,contact,new ArrayList<>());
            updateContactHookHelper.validateAndProcessProsAssociation(dcmContactDTO,contact,new ArrayList<>());
            Contact newContact = contact;
            newContact.setEmailID("test@gmail.com");
            Mockito.verify(updateContactHookHelper,Mockito.times(0)).validateAndExtractProsToActivate(anyList(),anyList());
            Mockito.verify(updateContactHookHelper,Mockito.times(0)).processProsActivation(any(),anyList(),anyList());
        }
    }

    @Test
    void validateAndProcessProsAssociation_null_valid_Accounts_test2(){
        try(MockedStatic<CreateContactHookHelper> userMockedStatic = Mockito.mockStatic(CreateContactHookHelper.class)){
            CreateContactHookHelper contactHookHelper = Mockito.mock(CreateContactHookHelper.class);
            Mockito.when(contactHookHelper.extractValidAccounts(anyList())).thenReturn(List.of());
            userMockedStatic.when(CreateContactHookHelper::getInstance).thenReturn(contactHookHelper);
            UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setFirstName("fName");
            dcmContactDTO.setLastName("lName");
            dcmContactDTO.setPhotoID("pic");
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProsAssociation(dcmContactDTO,contact,new ArrayList<>());
            updateContactHookHelper.validateAndProcessProsAssociation(dcmContactDTO,contact,new ArrayList<>());
            Contact newContact = contact;
            newContact.setEmailID("test@gmail.com");
            Mockito.verify(updateContactHookHelper,Mockito.times(0)).validateAndExtractProsToActivate(anyList(),anyList());
            Mockito.verify(updateContactHookHelper,Mockito.times(0)).processProsActivation(any(),anyList(),anyList());
        }
    }

    @Test
    void validateAndProcessProsAssociation_no_valid_ActivatePros_test(){
        try(MockedStatic<CreateContactHookHelper> userMockedStatic = Mockito.mockStatic(CreateContactHookHelper.class)){
            CreateContactHookHelper contactHookHelper = Mockito.mock(CreateContactHookHelper.class);
            List<SettingsJDO> accounts = List.of(new SettingsJDO());
            Mockito.when(contactHookHelper.extractValidAccounts(anyList())).thenReturn(accounts);
            Mockito.when(contactHookHelper.validateAndExtractProsToCreate(anyList(),anyList())).thenReturn(null);
            userMockedStatic.when(CreateContactHookHelper::getInstance).thenReturn(contactHookHelper);
            UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setFirstName("fName");
            dcmContactDTO.setLastName("lName");
            dcmContactDTO.setPhotoID("pic");
            dcmContactDTO.setLinkedAccounts(new ArrayList<>());
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            Mockito.when(updateContactHookHelper.validateAndExtractProsToActivate(anyList(),anyList())).thenReturn(null);
            Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProsAssociation(dcmContactDTO,contact,new ArrayList<>());
            updateContactHookHelper.validateAndProcessProsAssociation(dcmContactDTO,contact,new ArrayList<>());
            Contact newContact = contact;
            newContact.setEmailID("test@gmail.com");
            Mockito.verify(updateContactHookHelper,Mockito.times(1)).validateAndExtractProsToActivate(new ArrayList<>(),accounts);
            Mockito.verify(updateContactHookHelper,Mockito.times(0)).processProsActivation(any(),anyList(),anyList());
        }
    }

    @Test
    void validateAndProcessProsAssociation_no_valid_ActivatePros_test2(){
        try(MockedStatic<CreateContactHookHelper> userMockedStatic = Mockito.mockStatic(CreateContactHookHelper.class)){
            CreateContactHookHelper contactHookHelper = Mockito.mock(CreateContactHookHelper.class);
            List<SettingsJDO> accounts = List.of(new SettingsJDO());
            Mockito.when(contactHookHelper.extractValidAccounts(anyList())).thenReturn(accounts);
            Mockito.when(contactHookHelper.validateAndExtractProsToCreate(anyList(),anyList())).thenReturn(List.of());
            userMockedStatic.when(CreateContactHookHelper::getInstance).thenReturn(contactHookHelper);
            UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setFirstName("fName");
            dcmContactDTO.setLastName("lName");
            dcmContactDTO.setPhotoID("pic");
            dcmContactDTO.setLinkedAccounts(new ArrayList<>());
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            Mockito.when(updateContactHookHelper.validateAndExtractProsToActivate(anyList(),anyList())).thenReturn(List.of());
            Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProsAssociation(dcmContactDTO,contact,new ArrayList<>());
            updateContactHookHelper.validateAndProcessProsAssociation(dcmContactDTO,contact,new ArrayList<>());
            Contact newContact = contact;
            newContact.setEmailID("test@gmail.com");
            Mockito.verify(updateContactHookHelper,Mockito.times(1)).validateAndExtractProsToActivate(new ArrayList<>(),accounts);
            Mockito.verify(updateContactHookHelper,Mockito.times(0)).processProsActivation(any(),anyList(),anyList());
        }
    }

    @Test
    void validateAndProcessProsAssociation_valid_test(){
        try(MockedStatic<CreateContactHookHelper> userMockedStatic = Mockito.mockStatic(CreateContactHookHelper.class)){
            CreateContactHookHelper contactHookHelper = Mockito.mock(CreateContactHookHelper.class);
            List<SettingsJDO> accounts = List.of(new SettingsJDO());
            Mockito.when(contactHookHelper.extractValidAccounts(anyList())).thenReturn(accounts);
            Mockito.when(contactHookHelper.validateAndExtractProsToCreate(anyList(),anyList())).thenReturn(null);
            userMockedStatic.when(CreateContactHookHelper::getInstance).thenReturn(contactHookHelper);
            UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setFirstName("fName");
            dcmContactDTO.setLastName("lName");
            dcmContactDTO.setPhotoID("pic");
            dcmContactDTO.setLinkedAccounts(new ArrayList<>());
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            List<PeopleRelationJDO> userPros = List.of(new PeopleRelationJDO());
            Mockito.when(updateContactHookHelper.validateAndExtractProsToActivate(anyList(),anyList())).thenReturn(userPros);
            Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProsAssociation(dcmContactDTO,contact,userPros);
            updateContactHookHelper.validateAndProcessProsAssociation(dcmContactDTO,contact,userPros);
            Mockito.verify(updateContactHookHelper,Mockito.times(1)).validateAndExtractProsToActivate(userPros,accounts);
            Mockito.verify(updateContactHookHelper,Mockito.times(1)).processProsActivation(contact,accounts,userPros);
        }
    }

    @Test
    void validateAndProcessProsAssociation_valid_test2(){
        try(MockedStatic<CreateContactHookHelper> userMockedStatic = Mockito.mockStatic(CreateContactHookHelper.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            CreateContactHookHelper contactHookHelper = Mockito.mock(CreateContactHookHelper.class);
            List<SettingsJDO> accounts = List.of(new SettingsJDO());
            Mockito.when(contactHookHelper.extractValidAccounts(anyList())).thenReturn(accounts);
            Mockito.when(contactHookHelper.validateAndExtractProsToCreate(anyList(),anyList())).thenReturn(accounts);
            userMockedStatic.when(CreateContactHookHelper::getInstance).thenReturn(contactHookHelper);
            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenReturn("account1");
            Mockito.doNothing().when(contactHookHelper).processPROCreation(anyList(),anyString(),any(Contact.class),anyString());
            UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setFirstName("fName");
            dcmContactDTO.setLastName("lName");
            dcmContactDTO.setPhotoID("pic");
            dcmContactDTO.setLinkedAccounts(new ArrayList<>());
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            List<PeopleRelationJDO> userPros = List.of(new PeopleRelationJDO());
            Mockito.when(updateContactHookHelper.validateAndExtractProsToActivate(anyList(),anyList())).thenReturn(userPros);
            Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProsAssociation(dcmContactDTO,contact,userPros);
            updateContactHookHelper.validateAndProcessProsAssociation(dcmContactDTO,contact,userPros);
            Mockito.verify(updateContactHookHelper,Mockito.times(1)).validateAndExtractProsToActivate(userPros,accounts);
            Mockito.verify(updateContactHookHelper,Mockito.times(1)).processProsActivation(contact,accounts,userPros);
        }
    }

    @Test
    void validateAndProcessProsAssociation_exception_test(){
        try(MockedStatic<CreateContactHookHelper> userMockedStatic = Mockito.mockStatic(CreateContactHookHelper.class)){
            userMockedStatic.when(CreateContactHookHelper::getInstance).thenReturn(null);
            UpdateContactHookHelper updateContactHookHelper = Mockito.mock(UpdateContactHookHelper.class);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setFirstName("fName");
            dcmContactDTO.setLastName("lName");
            dcmContactDTO.setPhotoID("pic");
            dcmContactDTO.setLinkedAccounts(null);
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            Mockito.when(updateContactHookHelper.validateAndExtractProsToActivate(anyList(),anyList())).thenReturn(null);
            Mockito.doCallRealMethod().when(updateContactHookHelper).validateAndProcessProsAssociation(dcmContactDTO,contact,null);
            updateContactHookHelper.validateAndProcessProsAssociation(dcmContactDTO,contact,null);
            Mockito.verify(updateContactHookHelper,Mockito.times(0)).validateAndExtractProsToActivate(any(),anyList());
            Mockito.verify(updateContactHookHelper,Mockito.times(0)).processProsActivation(any(),anyList(),any());
        }
    }

    @Test
    void validateAndExtractProsToActivate_single_pro_inactive_test(){
        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setDelete(true);
        userPro1.setUniquepin("account1");
        List<PeopleRelationJDO> userPros = new ArrayList<>();
        userPros.add(userPro1);
        SettingsJDO account1 = new SettingsJDO();
        account1.setPeopleUniquePin("account1");
        account1.setStatus(Status.ACTIVE.toString());
        List<SettingsJDO> accounts = new ArrayList<>();
        accounts.add(account1);
        List<PeopleRelationJDO> result = updateContactHookHelper.validateAndExtractProsToActivate(userPros,accounts);
        Assertions.assertEquals(1,result.size());
        Assertions.assertEquals(userPro1,result.get(0));
    }

    @Test
    void validateAndExtractProsToActivate_single_pro_active_test(){
        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setDelete(false);
        userPro1.setUniquepin("account1");
        List<PeopleRelationJDO> userPros = new ArrayList<>();
        userPros.add(userPro1);
        SettingsJDO account1 = new SettingsJDO();
        account1.setPeopleUniquePin("account1");
        account1.setStatus(Status.ACTIVE.toString());
        List<SettingsJDO> accounts = new ArrayList<>();
        accounts.add(account1);
        List<PeopleRelationJDO> result = updateContactHookHelper.validateAndExtractProsToActivate(userPros,accounts);
        Assertions.assertTrue(result.isEmpty());
    }

    @Test
    void validateAndExtractProsToActivate_single_pro_inActive_account_test(){
        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setDelete(true);
        userPro1.setUniquepin("account1");
        List<PeopleRelationJDO> userPros = new ArrayList<>();
        userPros.add(userPro1);
        SettingsJDO account1 = new SettingsJDO();
        account1.setPeopleUniquePin("account1");
        account1.setStatus(Status.INACTIVE.toString());
        List<SettingsJDO> accounts = new ArrayList<>();
        accounts.add(account1);
        List<PeopleRelationJDO> result = updateContactHookHelper.validateAndExtractProsToActivate(userPros,accounts);
        Assertions.assertTrue(result.isEmpty());
    }

    @Test
    void validateAndExtractProsToActivate_single_pro_invalid_account_test(){
        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setDelete(true);
        userPro1.setUniquepin("account11");
        List<PeopleRelationJDO> userPros = new ArrayList<>();
        userPros.add(userPro1);
        SettingsJDO account1 = new SettingsJDO();
        account1.setPeopleUniquePin("account1");
        account1.setStatus(Status.ACTIVE.toString());
        List<SettingsJDO> accounts = new ArrayList<>();
        accounts.add(account1);
        List<PeopleRelationJDO> result = updateContactHookHelper.validateAndExtractProsToActivate(userPros,accounts);
        Assertions.assertTrue(result.isEmpty());
    }

    @Test
    void validateAndExtractProsToActivate_multiple_pros_test(){
        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setDelete(true);
        userPro1.setUniquepin("account1");
        PeopleRelationJDO userPro2 = new PeopleRelationJDO();
        userPro2.setDelete(true);
        userPro2.setUniquepin("account2");
        List<PeopleRelationJDO> userPros = new ArrayList<>();
        userPros.add(userPro1);
        userPros.add(userPro2);
        SettingsJDO account1 = new SettingsJDO();
        account1.setPeopleUniquePin("account1");
        account1.setStatus(Status.ACTIVE.toString());
        SettingsJDO account2 = new SettingsJDO();
        account2.setPeopleUniquePin("account2");
        account2.setStatus(Status.ACTIVE.toString());
        List<SettingsJDO> accounts = new ArrayList<>();
        accounts.add(account1);
        accounts.add(account2);
        List<PeopleRelationJDO> result = updateContactHookHelper.validateAndExtractProsToActivate(userPros,accounts);
        Assertions.assertFalse(result.isEmpty());
        Assertions.assertEquals(2,result.size());
        Assertions.assertEquals(List.of(userPro1,userPro2),result);
    }

    @Test
    void validateAndExtractProsToActivate_multiple_pros_valid_test2(){
        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setDelete(true);
        userPro1.setUniquepin("account1");
        PeopleRelationJDO userPro2 = new PeopleRelationJDO();
        userPro2.setDelete(true);
        userPro2.setUniquepin("account2");
        List<PeopleRelationJDO> userPros = new ArrayList<>();
        userPros.add(userPro1);
        userPros.add(userPro2);
        SettingsJDO account1 = new SettingsJDO();
        account1.setPeopleUniquePin("account1");
        account1.setStatus(Status.ACTIVE.toString());
        SettingsJDO account2 = new SettingsJDO();
        account2.setPeopleUniquePin("account2");
        account2.setStatus(Status.INACTIVE.toString());
        List<SettingsJDO> accounts = new ArrayList<>();
        accounts.add(account1);
        accounts.add(account2);
        List<PeopleRelationJDO> result = updateContactHookHelper.validateAndExtractProsToActivate(userPros,accounts);
        Assertions.assertFalse(result.isEmpty());
        Assertions.assertEquals(1,result.size());
        Assertions.assertEquals(userPro1,result.get(0));
    }

    @Test
    void validateAndExtractProsToActivate_multiple_pros_invalid_test(){
        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setDelete(false);
        userPro1.setUniquepin("account1");
        PeopleRelationJDO userPro2 = new PeopleRelationJDO();
        userPro2.setDelete(true);
        userPro2.setUniquepin("account2");
        List<PeopleRelationJDO> userPros = new ArrayList<>();
        userPros.add(userPro1);
        userPros.add(userPro2);
        SettingsJDO account1 = new SettingsJDO();
        account1.setPeopleUniquePin("account1");
        account1.setStatus(Status.ACTIVE.toString());
        SettingsJDO account2 = new SettingsJDO();
        account2.setPeopleUniquePin("account2");
        account2.setStatus(Status.INACTIVE.toString());
        List<SettingsJDO> accounts = new ArrayList<>();
        accounts.add(account1);
        accounts.add(account2);
        List<PeopleRelationJDO> result = updateContactHookHelper.validateAndExtractProsToActivate(userPros,accounts);
        Assertions.assertTrue(result.isEmpty());
    }

    @Test
    void validateAndExtractProsToActivate_multiple_pros_invalid_test2(){
        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setDelete(false);
        userPro1.setUniquepin("account1");
        PeopleRelationJDO userPro2 = new PeopleRelationJDO();
        userPro2.setDelete(false);
        userPro2.setUniquepin("account2");
        List<PeopleRelationJDO> userPros = new ArrayList<>();
        userPros.add(userPro1);
        userPros.add(userPro2);
        SettingsJDO account1 = new SettingsJDO();
        account1.setPeopleUniquePin("account1");
        account1.setStatus(Status.ACTIVE.toString());
        SettingsJDO account2 = new SettingsJDO();
        account2.setPeopleUniquePin("account2");
        account2.setStatus(Status.ACTIVE.toString());
        List<SettingsJDO> accounts = new ArrayList<>();
        accounts.add(account1);
        accounts.add(account2);
        List<PeopleRelationJDO> result = updateContactHookHelper.validateAndExtractProsToActivate(userPros,accounts);
        Assertions.assertTrue(result.isEmpty());
    }

    @Test
    void validateAndExtractProsToActivate_multiple_pros_invalid_test3(){
        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setDelete(true);
        userPro1.setUniquepin("account1");
        PeopleRelationJDO userPro2 = new PeopleRelationJDO();
        userPro2.setDelete(true);
        userPro2.setUniquepin("account2");
        List<PeopleRelationJDO> userPros = new ArrayList<>();
        userPros.add(userPro1);
        userPros.add(userPro2);
        SettingsJDO account1 = new SettingsJDO();
        account1.setPeopleUniquePin("account1");
        account1.setStatus(Status.INACTIVE.toString());
        SettingsJDO account2 = new SettingsJDO();
        account2.setPeopleUniquePin("account2");
        account2.setStatus(Status.INACTIVE.toString());
        List<SettingsJDO> accounts = new ArrayList<>();
        accounts.add(account1);
        accounts.add(account2);
        List<PeopleRelationJDO> result = updateContactHookHelper.validateAndExtractProsToActivate(userPros,accounts);
        Assertions.assertTrue(result.isEmpty());
    }

    @Test
    void validateAndExtractProsToActivate_multiple_pros_invalid_test4(){
        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setDelete(true);
        userPro1.setUniquepin("account11");
        PeopleRelationJDO userPro2 = new PeopleRelationJDO();
        userPro2.setDelete(true);
        userPro2.setUniquepin("account2");
        List<PeopleRelationJDO> userPros = new ArrayList<>();
        userPros.add(userPro1);
        userPros.add(userPro2);
        SettingsJDO account1 = new SettingsJDO();
        account1.setPeopleUniquePin("account1");
        account1.setStatus(Status.ACTIVE.toString());
        SettingsJDO account2 = new SettingsJDO();
        account2.setPeopleUniquePin("account2");
        account2.setStatus(Status.INACTIVE.toString());
        List<SettingsJDO> accounts = new ArrayList<>();
        accounts.add(account1);
        accounts.add(account2);
        List<PeopleRelationJDO> result = updateContactHookHelper.validateAndExtractProsToActivate(userPros,accounts);
        Assertions.assertTrue(result.isEmpty());
    }

    @Test
    void processProsActivation_valid_test(){
        try(MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserStaffHelper userStaffHelperMock = Mockito.mock(UserStaffHelper.class);
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelperMock);
            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenReturn("account1");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO defaultPro = new PeopleRelationJDO();
            defaultPro.setUniquepin("default");
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(defaultPro);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            SettingsJDO account1 = new SettingsJDO();
            account1.setPeopleUniquePin("account1");
            account1.setStatus(Status.ACTIVE.toString());
            List<SettingsJDO> accounts = new ArrayList<>();
            accounts.add(account1);
            PeopleRelationJDO userPro1 = new PeopleRelationJDO();
            userPro1.setDelete(true);
            userPro1.setDefault(false);
            userPro1.setUniquepin("account1");
            userPro1.setEmailID("test1@gmail.com");
            List<PeopleRelationJDO> userPros = new ArrayList<>();
            userPros.add(userPro1);
            updateContactHookHelper.processProsActivation(contact,accounts,userPros);
            Mockito.verify(userStaffHelperMock).activateUserSkillHelper(userPro1);
            Mockito.verify(userStaffHelperMock).activateUserEventsHandler(account1,userPro1,"MailID test1@gmail.com is not active, so reactivated it back in this domain account1","hook");
        }
    }

    @Test
    void processProsActivation_invalid_account_test(){
        try(MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserStaffHelper userStaffHelperMock = Mockito.mock(UserStaffHelper.class);
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelperMock);
            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenReturn("account1");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO defaultPro = new PeopleRelationJDO();
            defaultPro.setUniquepin("default");
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(defaultPro);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            PeopleRelationJDO userPro1 = new PeopleRelationJDO();
            userPro1.setDelete(true);
            userPro1.setDefault(false);
            userPro1.setUniquepin("account1");
            userPro1.setEmailID("test1@gmail.com");
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setDelete(true);
            userPro2.setDefault(false);
            userPro2.setUniquepin("account2");
            userPro2.setEmailID("test1@gmail.com");
            List<PeopleRelationJDO> userPros = new ArrayList<>();
            userPros.add(userPro1);
            userPros.add(userPro2);
            SettingsJDO account1 = new SettingsJDO();
            account1.setPeopleUniquePin("account11111");
            account1.setStatus(Status.ACTIVE.toString());
            List<SettingsJDO> accounts = new ArrayList<>();
            accounts.add(account1);
            updateContactHookHelper.processProsActivation(contact,accounts,userPros);
            Mockito.verify(userStaffHelperMock,Mockito.times(2)).activateUserSkillHelper(any(PeopleRelationJDO.class));
            Mockito.verify(userStaffHelperMock,Mockito.times(0)).activateUserEventsHandler(any(SettingsJDO.class),any(PeopleRelationJDO.class),anyString(),anyString());
        }
    }

    @Test
    void processProsActivation_null_defaultPRO_test(){
        try(MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserStaffHelper userStaffHelperMock = Mockito.mock(UserStaffHelper.class);
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelperMock);
            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenReturn("account1");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(null);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            PeopleRelationJDO userPro1 = new PeopleRelationJDO();
            userPro1.setDelete(true);
            userPro1.setDefault(false);
            userPro1.setUniquepin("account1");
            userPro1.setEmailID("test1@gmail.com");
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setDelete(true);
            userPro2.setDefault(false);
            userPro2.setUniquepin("account2");
            userPro2.setEmailID("test1@gmail.com");
            List<PeopleRelationJDO> userPros = new ArrayList<>();
            userPros.add(userPro1);
            userPros.add(userPro2);
            SettingsJDO account1 = new SettingsJDO();
            account1.setPeopleUniquePin("account11111");
            account1.setStatus(Status.ACTIVE.toString());
            List<SettingsJDO> accounts = new ArrayList<>();
            accounts.add(account1);
            updateContactHookHelper.processProsActivation(contact,accounts,userPros);
            Mockito.verify(userStaffHelperMock,Mockito.times(2)).activateUserSkillHelper(any(PeopleRelationJDO.class));
            Mockito.verify(userStaffHelperMock,Mockito.times(0)).activateUserEventsHandler(any(SettingsJDO.class),any(PeopleRelationJDO.class),anyString(),anyString());
        }
    }

    @Test
    void processProsActivation_defaultPRO_test(){
        try(MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserStaffHelper userStaffHelperMock = Mockito.mock(UserStaffHelper.class);
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelperMock);
            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenReturn("account1");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO defaultPro = new PeopleRelationJDO();
            defaultPro.setUniquepin("account1");
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(defaultPro);
            Mockito.when(userImplMock.savePro(any(PeopleRelationJDO.class))).thenReturn(defaultPro);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            PeopleRelationJDO userPro1 = new PeopleRelationJDO();
            userPro1.setDelete(true);
            userPro1.setDefault(false);
            userPro1.setUniquepin("account1");
            userPro1.setEmailID("test1@gmail.com");
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setDelete(true);
            userPro2.setDefault(false);
            userPro2.setUniquepin("account2");
            userPro2.setEmailID("test1@gmail.com");
            List<PeopleRelationJDO> userPros = new ArrayList<>();
            userPros.add(userPro1);
            userPros.add(userPro2);
            SettingsJDO account1 = new SettingsJDO();
            account1.setPeopleUniquePin("account11111");
            account1.setStatus(Status.ACTIVE.toString());
            List<SettingsJDO> accounts = new ArrayList<>();
            accounts.add(account1);
            updateContactHookHelper.processProsActivation(contact,accounts,userPros);
            Mockito.verify(userStaffHelperMock,Mockito.times(2)).activateUserSkillHelper(any(PeopleRelationJDO.class));
            Mockito.verify(userStaffHelperMock,Mockito.times(0)).activateUserEventsHandler(any(SettingsJDO.class),any(PeopleRelationJDO.class),anyString(),anyString());
        }
    }

    @Test
    void processProsActivation_exception_test(){
        try(MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserStaffHelper userStaffHelperMock = Mockito.mock(UserStaffHelper.class);
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelperMock);
            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenReturn("account1");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO defaultPro = new PeopleRelationJDO();
            defaultPro.setUniquepin("account1");
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(defaultPro);
            Mockito.when(userImplMock.savePro(any(PeopleRelationJDO.class))).thenReturn(defaultPro);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            SettingsJDO account1 = new SettingsJDO();
            account1.setPeopleUniquePin("account11111");
            account1.setStatus(Status.ACTIVE.toString());
            List<SettingsJDO> accounts = new ArrayList<>();
            accounts.add(account1);
            updateContactHookHelper.processProsActivation(contact,accounts,null);
            Mockito.verify(userStaffHelperMock,Mockito.times(0)).activateUserSkillHelper(any(PeopleRelationJDO.class));
            Mockito.verify(userStaffHelperMock,Mockito.times(0)).activateUserEventsHandler(any(SettingsJDO.class),any(PeopleRelationJDO.class),anyString(),anyString());
        }
    }

    @Test
    void processProsCreation_invalid_accountId_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<CreateContactHookHelper> contactHookHelperMockedStatic = Mockito.mockStatic(CreateContactHookHelper.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            CreateContactHookHelper contactHookHelper = Mockito.mock(CreateContactHookHelper.class);
            contactHookHelperMockedStatic.when(CreateContactHookHelper::getInstance).thenReturn(contactHookHelper);
            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenReturn("");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO defaultPro = new PeopleRelationJDO();
            defaultPro.setUniquepin("default");
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(defaultPro);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            SettingsJDO account1 = new SettingsJDO();
            account1.setPeopleUniquePin("account11111");
            account1.setStatus(Status.ACTIVE.toString());
            List<SettingsJDO> accounts = new ArrayList<>();
            accounts.add(account1);
            updateContactHookHelper.processProsCreation(contact,accounts,contactHookHelper);
            dcmUtilMockedStatic.verify(()-> DcmUtil.getDefaultAccountIDWithEmailID("test1@gmail.com"));
            Mockito.verify(contactHookHelper,Mockito.times(0)).processPROCreation(anyList(),anyString(),any(Contact.class),anyString());
        }
    }

    @Test
    void processProsCreation_default_pro_test() throws NoSuchAlgorithmException, IOException {
        try (MockedStatic<CreateContactHookHelper> contactHookHelperMockedStatic = Mockito.mockStatic(CreateContactHookHelper.class);
             MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
             MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)) {
            CreateContactHookHelper contactHookHelper = Mockito.mock(CreateContactHookHelper.class);
            Mockito.doNothing().when(contactHookHelper).processPROCreation(anyList(), anyString(), any(Contact.class), anyString());
            contactHookHelperMockedStatic.when(CreateContactHookHelper::getInstance).thenReturn(contactHookHelper);
            dcmUtilMockedStatic.when(() -> DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenReturn("account1");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO defaultPro = new PeopleRelationJDO();
            defaultPro.setUniquepin("default");
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(defaultPro);
            Mockito.when(userImplMock.savePro(any(PeopleRelationJDO.class))).thenReturn(defaultPro);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Contact contact = new Contact("contId", "test1@gmail.com", "fName", "lName", "pic", 0L);
            SettingsJDO account1 = new SettingsJDO();
            account1.setPeopleUniquePin("account11111");
            account1.setStatus(Status.ACTIVE.toString());
            List<SettingsJDO> accounts = new ArrayList<>();
            accounts.add(account1);
            updateContactHookHelper.processProsCreation(contact, accounts, contactHookHelper);
            dcmUtilMockedStatic.verify(() -> DcmUtil.getDefaultAccountIDWithEmailID("test1@gmail.com"));
            Mockito.verify(contactHookHelper, Mockito.times(1)).processPROCreation(anyList(), anyString(), any(Contact.class), anyString());
        }
    }

    @Test
    void processProsCreation_non_default_pro_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<CreateContactHookHelper> contactHookHelperMockedStatic = Mockito.mockStatic(CreateContactHookHelper.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            CreateContactHookHelper contactHookHelper = Mockito.mock(CreateContactHookHelper.class);
            Mockito.doNothing().when(contactHookHelper).processPROCreation(anyList(),anyString(),any(Contact.class),anyString());
            contactHookHelperMockedStatic.when(CreateContactHookHelper::getInstance).thenReturn(contactHookHelper);
            dcmUtilMockedStatic.when(()-> DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenReturn("default");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO defaultPro = new PeopleRelationJDO();
            defaultPro.setUniquepin("default");
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(defaultPro);
            Mockito.when(userImplMock.savePro(any(PeopleRelationJDO.class))).thenReturn(defaultPro);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Contact contact = new Contact("contId","test1@gmail.com","fName","lName","pic",0L);
            SettingsJDO account1 = new SettingsJDO();
            account1.setPeopleUniquePin("account11111");
            account1.setStatus(Status.ACTIVE.toString());
            List<SettingsJDO> accounts = new ArrayList<>();
            accounts.add(account1);
            updateContactHookHelper.processProsCreation(contact,accounts,contactHookHelper);
            dcmUtilMockedStatic.verify(()-> DcmUtil.getDefaultAccountIDWithEmailID("test1@gmail.com"));
            Mockito.verify(contactHookHelper,Mockito.times(1)).processPROCreation(anyList(),anyString(),any(Contact.class),anyString());
        }
    }

}