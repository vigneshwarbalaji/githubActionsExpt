package com.yoco.contact.helper;

import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class ContactHelperTest {

    ContactHelper contactHelper = ContactHelper.getInstance();

    @Test
    void validateAndCreateContact_new_contact_test() {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){

            Contact contact = new Contact();
            contact.setId("id");

            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getByID(anyString())).thenReturn(null);
            Mockito.when(contactMock.saveContact(any(Contact.class))).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);

            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setFirstName("fName");
            dcmContactDTO.setLastName("lName");
            dcmContactDTO.setId("id");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setPhotoID("photo");

            Assertions.assertEquals(contact,contactHelper.validateAndCreateContact(dcmContactDTO));
            Mockito.verify(contactMock).saveContact(any(Contact.class));
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void validateAndCreateContact_optionalEmptyValues_test(String testValue) {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){

            Contact contact = new Contact();
            contact.setId("id");

            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getByID(anyString())).thenReturn(null);
            Mockito.when(contactMock.saveContact(any(Contact.class))).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);

            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setFirstName(testValue);
            dcmContactDTO.setLastName(testValue);
            dcmContactDTO.setId("id");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setPhotoID("photo");

            Assertions.assertEquals(contact,contactHelper.validateAndCreateContact(dcmContactDTO));
            Mockito.verify(contactMock).saveContact(any(Contact.class));
        }
    }

    @Test
    void validateAndCreateContact_contact_exists_test() {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){

            Contact contact = new Contact();
            contact.setId("id");

            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);

            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setFirstName("fName");
            dcmContactDTO.setLastName("lName");
            dcmContactDTO.setId("id");
            dcmContactDTO.setLogin("test@gmail.com");
            dcmContactDTO.setPhotoID("photo");

            Assertions.assertEquals(contact,contactHelper.validateAndCreateContact(dcmContactDTO));
            Mockito.verify(contactMock).getByID("id");
        }
    }

}