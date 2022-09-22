package com.yoco.contact.endpoint;

import com.yoco.contact.service.ContactHookService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

import static org.mockito.ArgumentMatchers.anyString;

class FullHookApiTest {

   @Test
    void processFullHook_valid_test(){
       try (MockedConstruction<ContactHookService> mock = Mockito.mockConstruction(ContactHookService.class, (contactHookServiceMock, context) -> {
                Mockito.doNothing().when(contactHookServiceMock).processFullHook(anyString());
            })) {
           new FullHookApi().processFullHook("contactId");
           Assertions.assertEquals(1,mock.constructed().size());
       }
    }
}