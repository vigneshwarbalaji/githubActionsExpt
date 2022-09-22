package com.yoco.contact.endpoint;

import com.yoco.commons.modal.GenericResponse;
import com.yoco.contact.enums.CONTACT_ERROR_MESSAGE;
import com.yoco.contact.service.ContactService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.Map;

import static org.mockito.ArgumentMatchers.anyString;

class ContactApiTest {

    @ParameterizedTest
    @NullAndEmptySource
    void getContacts_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<ContactService> mock = Mockito.mockConstruction(ContactService.class, (contactServiceMock, context) -> {
            Mockito.when(contactServiceMock.getContacts(anyString())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new ContactApi().getContacts("id1,id2");
            var genericResponse = new GenericResponse(false, null, CONTACT_ERROR_MESSAGE.CONTACT_DOES_NOT_EXIST.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getContacts_validResponse_test(){
        Map<String, Object> data = Map.of("data", "value");
        try (MockedConstruction<ContactService> mock = Mockito.mockConstruction(ContactService.class, (contactServiceMock, context) -> {
            Mockito.when(contactServiceMock.getContacts(anyString())).thenReturn(data);
        })){
            ResponseEntity<GenericResponse> responseEntity = new ContactApi().getContacts("id1,id2");
            var genericResponse = new GenericResponse(true,null,null);
            genericResponse.setData(data);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getContacts_exception_test(){
        try (MockedConstruction<ContactService> mock = Mockito.mockConstruction(ContactService.class, (contactServiceMock, context) -> {
            Mockito.when(contactServiceMock.getContacts(anyString())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new ContactApi().getContacts("id1,id2");
            var genericResponse = new GenericResponse(false,null,"exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

}