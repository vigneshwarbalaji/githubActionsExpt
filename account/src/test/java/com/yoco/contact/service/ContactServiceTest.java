package com.yoco.contact.service;

import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyList;

class ContactServiceTest {

    private ContactService contactService = new ContactService();

    @ParameterizedTest
    @NullAndEmptySource
    void getContacts_invalid_param_test(String testValue) {
        try{
            contactService.getContacts(testValue);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getContacts_no_contacts_test() {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getContacts(anyList())).thenReturn(null);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            Assertions.assertEquals(Map.of(),contactService.getContacts("contact1"));
        }
    }

    @Test
    void getContacts_valid_test() {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            List<Contact> contactList = List.of(new Contact("id", "test@gmail.com", "fName", "lName", "", 0L),
                    new Contact("id2", "test2@gmail.com", "fName", "lName", "", 0L));
            Mockito.when(contactMock.getContacts(anyList())).thenReturn(contactList);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            Assertions.assertEquals(Map.of(ContactConstants.CONTACTS,contactList),contactService.getContacts("id,,id2"));
        }
    }

}