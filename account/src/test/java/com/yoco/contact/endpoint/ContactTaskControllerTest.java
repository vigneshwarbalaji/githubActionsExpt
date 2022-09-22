package com.yoco.contact.endpoint;

import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.contact.helper.ContactTaskHandler;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyMap;

class ContactTaskControllerTest {

    @Test
    void contactTaskHandler() throws IOException, ClassNotFoundException {

        try(MockedStatic<ContactTaskHandler> contactTaskHelperMockedStatic = Mockito.mockStatic(ContactTaskHandler.class)) {

            ContactTaskHandler contactTaskHandler = Mockito.mock(ContactTaskHandler.class);
            Mockito.doNothing().when(contactTaskHandler).handleContactTaskHandler(anyMap());
            contactTaskHelperMockedStatic.when(ContactTaskHandler::getInstance).thenReturn(contactTaskHandler);

            Map<String,Object> payload = new HashMap<>();
            payload.put("key","data");

            new ContactTaskController().contactTaskHandler(CloudTaskUtil.convertObjectToByteArray(payload));

            Mockito.verify(contactTaskHandler).handleContactTaskHandler(payload);
        }
    }


}