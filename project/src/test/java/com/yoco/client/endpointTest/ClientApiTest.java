package com.yoco.client.endpointTest;

import com.yoco.client.endpoint.ClientApi;
import com.yoco.client.enums.CLIENT_ERROR_RESPONSE;
import com.yoco.client.service.ClientService;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.error.ApiErrorCode;
import com.yoco.commons.modal.GenericResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;

import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;

class ClientApiTest {

    @Test
    void createClient_empty_hashMap_test() throws Exception {

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            Contact objContact = new Contact();
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(objContact);

            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.createClient(anyString(), anyString(), anyString())).thenReturn(new HashMap<>());
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().createClient("accountId", "clientId", new MockHttpServletRequest());

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(CLIENT_ERROR_RESPONSE.UNABLE_TO_CREATE_CLIENT.value(), ((GenericResponse) responseEntity.getBody()).getErrorMessage());
        }
    }

    @Test
    void createClient_null_test() throws Exception {

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            Contact objContact = new Contact();
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(objContact);

            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.createClient(anyString(), anyString(), anyString())).thenReturn(null);
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().createClient("accountId", "clientId", new MockHttpServletRequest());

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(CLIENT_ERROR_RESPONSE.UNABLE_TO_CREATE_CLIENT.value(), ((GenericResponse) responseEntity.getBody()).getErrorMessage());
        }
    }

    @Test
    void createClient_exception_test() throws Exception {

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)) {

            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException());

            ResponseEntity responseEntity = new ClientApi().createClient("accountID", "payload", new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
        }
    }

    @Test
    void createClient_success_test() throws Exception {

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            Contact objContact = new Contact();
            objContact.setId("contactId");
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(objContact);

            HashMap<String, Object> responseMap = new HashMap<>();
            responseMap.put("client", "client info");

            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.createClient(anyString(), anyString(), anyString())).thenReturn(responseMap);
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().createClient("accountId", "clientId", new MockHttpServletRequest());

            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.setData(responseMap);
            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
        }
    }

    @Test
    void getAllClients_empty_hashMap_test() throws Exception {

        try (MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.getAll(anyString(), anyString(), anyString())).thenReturn(new HashMap<>());
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().getAllClients("accountId", "clientId", "");

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(CLIENT_ERROR_RESPONSE.NO_CLIENT_EXISTS.value(), ((GenericResponse) responseEntity.getBody()).getErrorMessage());
        }
    }

    @Test
    void getAllClients_null_test() throws Exception {

        try (MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.getAll(anyString(), anyString(), anyString())).thenReturn(null);
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().getAllClients("accountId", "clientId", "");

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(CLIENT_ERROR_RESPONSE.NO_CLIENT_EXISTS.value(), ((GenericResponse) responseEntity.getBody()).getErrorMessage());
        }
    }

    @Test
    void getAllClients_valid_test() throws Exception {

        try (MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            Map<String,Object> mockResponse = new HashMap<>();
            mockResponse.put("client","clientJdo");
            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.getAll(anyString(), anyString(), anyString())).thenReturn(mockResponse);
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().getAllClients("accountId", "clientId", "");

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
        }
    }

    @Test
    void getAllClients_exception_test() throws Exception {
        try (MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.getAll(anyString(), anyString(), anyString())).thenThrow(new IllegalArgumentException());
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().getAllClients("accountID", "payload", "");
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
        }
    }

    @Test
    void updateClient_empty_response_test(){
        try(MockedStatic<ClientService> mockedStatic = Mockito.mockStatic(ClientService.class)) {

            ClientService clientService = Mockito.mock(ClientService.class);
            Mockito.when(clientService.updateClient(anyString(),anyString(),anyString())).thenReturn(new HashMap<>());
            mockedStatic.when(ClientService::getClientService).thenReturn(clientService);

            ResponseEntity<GenericResponse> responseEntity = new ClientApi().updateClient( "accountId","clientId","payload");

            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            var genericResponse = new GenericResponse(false, null, CLIENT_ERROR_RESPONSE.INVALID_CLIENT_ID.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
        }
    }

    @Test
    void updateClient_valid_response_test(){
        try(MockedStatic<ClientService> mockedStatic = Mockito.mockStatic(ClientService.class)) {

            Map<String,Object> mockResponse = new HashMap<>();
            mockResponse.put("client","clientJDO");
            ClientService clientService = Mockito.mock(ClientService.class);
            Mockito.when(clientService.updateClient(anyString(),anyString(),anyString())).thenReturn(mockResponse);
            mockedStatic.when(ClientService::getClientService).thenReturn(clientService);

            ResponseEntity<GenericResponse> responseEntity = new ClientApi().updateClient( "accountId","clientId","payload");

            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
        }
    }

    @Test
    void updateClient_error_test(){
        try(MockedStatic<ClientService> mockedStatic = Mockito.mockStatic(ClientService.class)) {

            Map<String,Object> mockResponse = new HashMap<>();
            mockResponse.put("client","clientJDO");
            ClientService clientService = Mockito.mock(ClientService.class);
            Mockito.when(clientService.updateClient(anyString(),anyString(),anyString())).thenThrow(new IllegalArgumentException("exception"));
            mockedStatic.when(ClientService::getClientService).thenReturn(clientService);

            ResponseEntity<GenericResponse> responseEntity = new ClientApi().updateClient( "accountId","clientId","payload");

            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            var genericResponse = new GenericResponse(false, ApiErrorCode.BAD_REQUEST, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
        }
    }

    @Test
    void getClientByID_empty_hashMap_test() throws Exception {

        try (MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.getByID(anyString())).thenReturn(new HashMap<>());
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().getClientByID( "1234");

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(CLIENT_ERROR_RESPONSE.NO_CLIENT_EXISTS.value(), ((GenericResponse) responseEntity.getBody()).getErrorMessage());
        }
    }

    @Test
    void getClientByID_null_test() throws Exception {

        try (MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.getByID(anyString())).thenReturn(null);
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().getClientByID("124");

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(CLIENT_ERROR_RESPONSE.NO_CLIENT_EXISTS.value(), ((GenericResponse) responseEntity.getBody()).getErrorMessage());
        }
    }

    @Test
    void getClientByID_valid_test() throws Exception {

        try (MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            Map<String,Object> mockResponse = new HashMap<>();
            mockResponse.put("client","clientJdo");
            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.getByID(anyString())).thenReturn(mockResponse);
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().getClientByID("124");

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
        }
    }

    @Test
    void getClientByID_exception_test() throws Exception {
        try (MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.getByID(anyString())).thenThrow(new IllegalArgumentException());
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().getClientByID("1234");
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
        }
    }

    @Test
    void deleteClientByID_empty_hashMap_test() throws Exception {

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            Contact objContact = new Contact();
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(objContact);

            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.deleteClient(anyString(), anyString(), anyString())).thenReturn(new HashMap<>());
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().deleteClientByID("1234", "accountID",new MockHttpServletRequest());;

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(CLIENT_ERROR_RESPONSE.NO_CLIENT_EXISTS.value(), ((GenericResponse) responseEntity.getBody()).getErrorMessage());
        }
    }

    @Test
    void deleteClientByID_null_test() throws Exception {

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            Contact objContact = new Contact();
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(objContact);

            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.deleteClient(anyString(), anyString(), anyString())).thenReturn(null);
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().deleteClientByID("1234", "accountID",new MockHttpServletRequest());;

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(CLIENT_ERROR_RESPONSE.NO_CLIENT_EXISTS.value(), ((GenericResponse) responseEntity.getBody()).getErrorMessage());
        }
    }

    @Test
    void deleteClientByID_exception_test() throws Exception {

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)) {

            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException());

            ResponseEntity responseEntity = new ClientApi().deleteClientByID("1234", "accountID",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
        }
    }

    @Test
    void deleteClientByID_valid_test() throws Exception {

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedStatic<ClientService> clientServiceMockedStatic = mockStatic(ClientService.class)) {

            Contact objContact = new Contact();
            objContact.setId("contactId");
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(objContact);

            Map<String,Object> mockResponse = new HashMap<>();
            mockResponse.put("client","clientJdo");
            ClientService objClientservice = Mockito.mock(ClientService.class);
            Mockito.when(objClientservice.deleteClient(anyString(), anyString(), anyString())).thenReturn(mockResponse);
            clientServiceMockedStatic.when(ClientService::getClientService).thenReturn(objClientservice);

            ResponseEntity responseEntity = new ClientApi().deleteClientByID("1234", "accountID",new MockHttpServletRequest());;

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.setData(mockResponse);

            Assertions.assertEquals(genericResponse,responseEntity.getBody());
        }
    }


}
