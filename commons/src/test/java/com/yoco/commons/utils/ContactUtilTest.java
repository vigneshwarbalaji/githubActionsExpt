package com.yoco.commons.utils;

import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.utils.account.ContactUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

class ContactUtilTest {
    @Test
    void getEmailID_nullContact_test(){
        try(MockedConstruction<ContactImpl> contactMockedConstruction = Mockito.mockConstruction(ContactImpl.class,(contactImplMock,context)->{
            Mockito.when(contactImplMock.getByID("123")).thenReturn(null);
        })){
            Assertions.assertEquals("", ContactUtil.getEmailID("123"));
        }
    }

    @Test
    void getEmailID_nullEmailID_test(){
        try(MockedConstruction<ContactImpl> contactMockedConstruction = Mockito.mockConstruction(ContactImpl.class,(contactImplMock,context)->{
            Mockito.when(contactImplMock.getByID("123")).thenReturn(new Contact());
        })){
            Assertions.assertEquals("", ContactUtil.getEmailID("123"));
        }
    }

    @Test
    void getEmailID_validEmailID_test(){
        Contact contact = new Contact();
        contact.setEmailID("email");
        try(MockedConstruction<ContactImpl> contactMockedConstruction = Mockito.mockConstruction(ContactImpl.class,(contactImplMock,context)->{
            Mockito.when(contactImplMock.getByID("123")).thenReturn(contact);
        })){
            Assertions.assertEquals("email", ContactUtil.getEmailID("123"));
        }
    }
}
