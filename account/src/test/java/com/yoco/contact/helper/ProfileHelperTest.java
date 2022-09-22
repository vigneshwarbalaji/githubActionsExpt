package com.yoco.contact.helper;

import com.yoco.MockPRO;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.constants.CommonConstants;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;

class ProfileHelperTest {

    ProfileHelper profileHelper = ProfileHelper.getInstance();

    @Test
    void profileImageUpdateHandler_nullContact_test() throws IOException {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getByID(anyString())).thenReturn(null);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            Assertions.assertNull(profileHelper.profileImageUpdateHandler(MockPRO.getMockPRO(),"photo"));
        }
    }

    @Test
    void profileImageUpdateHandler_samePhotoId_test() throws IOException {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact();
            contact.setPhotoID("pic.png");
            Mockito.when(contactMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            Assertions.assertNull(profileHelper.profileImageUpdateHandler(MockPRO.getMockPRO(),"pic.png"));
        }
    }

    @Test
    void profileImageUpdateHandler_valid_test() throws IOException {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<ContactTaskInitiator> contactTaskInitiatorMockedStatic = Mockito.mockStatic(ContactTaskInitiator.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact();
            contact.setPhotoID("pic1.png");
            contact.setId("id1");
            contact.setEmailID("test@gmail.com");
            Mockito.when(contactMock.getByID(anyString())).thenReturn(contact);
            Contact contactNew = new Contact();
            contactNew.setPhotoID("pic.png");
            contactNew.setId("id1");
            contactNew.setEmailID("test@gmail.com");
            Mockito.when(contactMock.saveContact(any(Contact.class))).thenReturn(contactNew);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            contactTaskInitiatorMockedStatic.when(()-> ContactTaskInitiator.initiateContactUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            PeopleRelationJDO expectedPro = MockPRO.getMockPRO();
            expectedPro.setContact(contactNew);
            UserDTO userDTO = new UserDTO(expectedPro,null);
            Assertions.assertEquals(userDTO,profileHelper.profileImageUpdateHandler(MockPRO.getMockPRO(),"pic.png"));
            String activity = "Contact update from DCM :test@gmail.com contactID : id1. Details are : contact photoID : pic1.png DCM photoID : pic.png";
            contactTaskInitiatorMockedStatic.verify(()-> ContactTaskInitiator.initiateContactUpdateOperationsTaskQueue(
                    Map.of( CommonConstants.USER_KEY,userDTO,
                            CommonConstants.ACTION,"profileUpdate",
                            CommonConstants.ACTIVITY,activity)));
        }
    }

    @Test
    void profileImageUpdateHandler_exception_test() throws IOException {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<ContactTaskInitiator> contactTaskInitiatorMockedStatic = Mockito.mockStatic(ContactTaskInitiator.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact();
            contact.setPhotoID("pic1.png");
            contact.setId("id1");
            contact.setEmailID("test@gmail.com");
            Mockito.when(contactMock.getByID(anyString())).thenReturn(contact);
            Contact contactNew = new Contact();
            contactNew.setPhotoID("pic.png");
            contactNew.setId("id1");
            contactNew.setEmailID("test@gmail.com");
            Mockito.when(contactMock.saveContact(any(Contact.class))).thenReturn(contactNew);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            contactTaskInitiatorMockedStatic.when(()-> ContactTaskInitiator.initiateContactUpdateOperationsTaskQueue(anyMap())).thenThrow(new IOException("exception"));
            PeopleRelationJDO expectedPro = MockPRO.getMockPRO();
            expectedPro.setContact(contactNew);
            UserDTO userDTO = new UserDTO(expectedPro,null);
            Assertions.assertNull(profileHelper.profileImageUpdateHandler(MockPRO.getMockPRO(),"pic.png"));
            String activity = "Contact update from DCM :test@gmail.com contactID : id1. Details are : contact photoID : pic1.png DCM photoID : pic.png";
            contactTaskInitiatorMockedStatic.verify(()-> ContactTaskInitiator.initiateContactUpdateOperationsTaskQueue(
                    Map.of( CommonConstants.USER_KEY,userDTO,
                            CommonConstants.ACTION,"profileUpdate",
                            CommonConstants.ACTIVITY,activity)));
        }
    }

    @Test
    void updateProfileInDcm_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(() -> DcmUtil.updateContactInDcm(anyString(),anyMap())).thenReturn(Map.of());
            Assertions.assertNotNull(profileHelper.updateProfileInDcm(Map.of(ContactConstants.FIRST_NAME,"first",ContactConstants.LAST_NAME,"last"),"accId","id"));
            dcmUtilMockedStatic.verify(() -> DcmUtil.updateContactInDcm("accId",Map.of(ContactConstants.FIRST_NAME,"first",ContactConstants.LAST_NAME,"last",ContactConstants.ID,"id")));
        }
    }

    @Test
    void updateProfileInDcm_InValid_constraints_test(){
        try {
            profileHelper.updateProfileInDcm(Map.of(),"accId","id");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void profileUpdateHandler_valid_test() throws IOException {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<ContactTaskInitiator> contactTaskInitiatorMockedStatic = Mockito.mockStatic(ContactTaskInitiator.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact();
            contact.setFirstName("first");
            contact.setLastName("last");
            contact.setId("id1");
            contact.setEmailID("test@gmail.com");
            Mockito.when(contactMock.getByID(anyString())).thenReturn(contact);
            Contact contactNew = new Contact();
            contactNew.setFirstName("first1");
            contactNew.setLastName("last1");
            contactNew.setId("id1");
            contactNew.setEmailID("test@gmail.com");
            Mockito.when(contactMock.saveContact(any(Contact.class))).thenReturn(contactNew);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            contactTaskInitiatorMockedStatic.when(()-> ContactTaskInitiator.initiateContactUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            PeopleRelationJDO expectedPro = MockPRO.getMockPRO();
            expectedPro.setContact(contactNew);
            UserDTO userDTO = new UserDTO(expectedPro,null);
            Assertions.assertEquals(userDTO,profileHelper.profileUpdateHandler(MockPRO.getMockPRO(),Map.of(ContactConstants.FIRST_NAME,"first1",ContactConstants.LAST_NAME,"last1")));
            String activity = "contact FirstName : first DCM FirstName : first1 contact LastName : last DCM LastName : last1";
            contactTaskInitiatorMockedStatic.verify(()-> ContactTaskInitiator.initiateContactUpdateOperationsTaskQueue(
                    Map.of( CommonConstants.USER_KEY,userDTO,
                            CommonConstants.ACTION,"profileUpdate",
                            CommonConstants.ACTIVITY,activity)));
        }
    }


    @Test
    void profileUpdateHandler_already_profile_updated_test() throws IOException {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<ContactTaskInitiator> contactTaskInitiatorMockedStatic = Mockito.mockStatic(ContactTaskInitiator.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact();
            contact.setFirstName("first");
            contact.setLastName("last");
            contact.setId("id1");
            contact.setEmailID("test@gmail.com");
            Mockito.when(contactMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            Assertions.assertNull(profileHelper.profileUpdateHandler(MockPRO.getMockPRO(),Map.of(ContactConstants.FIRST_NAME,"first",ContactConstants.LAST_NAME,"last")));
            contactTaskInitiatorMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void profileUpdateHandler_null_contact_test() throws IOException {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<ContactTaskInitiator> contactTaskInitiatorMockedStatic = Mockito.mockStatic(ContactTaskInitiator.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getByID(anyString())).thenReturn(null);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            Assertions.assertNull(profileHelper.profileUpdateHandler(MockPRO.getMockPRO(),Map.of(ContactConstants.FIRST_NAME,"first",ContactConstants.LAST_NAME,"last")));
            contactTaskInitiatorMockedStatic.verifyNoInteractions();
        }
    }



}