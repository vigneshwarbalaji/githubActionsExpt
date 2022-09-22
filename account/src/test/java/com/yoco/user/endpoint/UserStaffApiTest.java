package com.yoco.user.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.constants.CommonConstants;
import com.yoco.user.service.UserStaffService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mockStatic;

class UserStaffApiTest {

    @ParameterizedTest
    @NullAndEmptySource
    void createUserPRO_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
            Mockito.when(userServiceMock.createUserPRO(anyString(),anyString())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().createUserPRO("accountId","contactId");
            var genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.FAILED_TO_PERSIST.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void createUserPRO_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
            Mockito.when(userServiceMock.createUserPRO(anyString(),anyString())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().createUserPRO("accountId","contactId");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void createUserPRO_Exception_test() {
        try (MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
            Mockito.when(userServiceMock.createUserPRO(anyString(), anyString())).thenThrow(new IllegalArgumentException("exception"));
        })) {
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().createUserPRO("accountID", "contactId");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse, responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
            Assertions.assertEquals(400, responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void createUser_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
            Mockito.when(userServiceMock.createUser(anyString(),any(Contact.class),anyString(),anyBoolean())).thenReturn(mockMap);
        })){
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().createUser("accountId","contactId",httpServletRequest);
            var genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.FAILED_TO_PERSIST.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void createUser_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        mockResponse.put(Commons.SUCCESS,true);
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
            Mockito.when(userServiceMock.createUser(anyString(),any(Contact.class),anyString(),anyBoolean())).thenReturn(mockResponse);
        })){
            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().createUser("accountId","contactId",httpServletRequest);
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.CREATED,responseEntity.getStatusCode());
            Assertions.assertEquals(201,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void createUser_success_false_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put(Commons.ERROR_RESPONSE,"dummy");
        mockResponse.put(Commons.SUCCESS,false);
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
                 Mockito.when(userServiceMock.createUser(anyString(),any(Contact.class),anyString(),anyBoolean())).thenReturn(mockResponse);
             })){
            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().createUser("accountId","contactId",httpServletRequest);
            var genericResponse = new GenericResponse(false, null, "dummy");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void createUser_success_false_user_key_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put(Commons.ERROR_RESPONSE,"dummy");
        mockResponse.put(Commons.SUCCESS,false);
        mockResponse.put(CommonConstants.USER_KEY,"userMap");
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
                 Mockito.when(userServiceMock.createUser(anyString(),any(Contact.class),anyString(),anyBoolean())).thenReturn(mockResponse);
             })){
            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().createUser("accountId","contactId",httpServletRequest);
            var genericResponse = new GenericResponse(false, null, "dummy");
            genericResponse.setData(new HashMap<>(){{put(CommonConstants.USER_KEY,"userMap");}});
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void createUser_Exception_test() {
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
             Mockito.when(userServiceMock.createUser(anyString(),any(Contact.class),anyString(),anyBoolean())).thenThrow(new IllegalArgumentException("exception"));
         })) {
            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().createUser("accountID", "contactId",httpServletRequest);
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse, responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
            Assertions.assertEquals(400, responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void importUsers_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
            Mockito.when(userServiceMock.importUser(anyString(),any(Contact.class),anyString())).thenReturn(mockMap);
        })){
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().importUsers("accountId","contactId",httpServletRequest);
            var genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.FAILED_TO_PERSIST.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void importUsers_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        mockResponse.put(Commons.SUCCESS,true);
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
                 Mockito.when(userServiceMock.importUser(anyString(),any(Contact.class),anyString())).thenReturn(mockResponse);
             })){
            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().importUsers("accountId","contactId",httpServletRequest);
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
        }
    }

    @Test
    void importUsers_Exception_test() {
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
                 Mockito.when(userServiceMock.importUser(anyString(),any(Contact.class),anyString())).thenThrow(new IllegalArgumentException("exception"));
             })) {
            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().importUsers("accountID", "contactId",httpServletRequest);
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse, responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
            Assertions.assertEquals(400, responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void activateUser_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
            Mockito.when(userServiceMock.activateUser(anyString(),anyString(),any(Contact.class))).thenReturn(mockMap);
        })){
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().activateUser("accountId","contactId",httpServletRequest);
            var genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void activateUser_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
                 Mockito.when(userServiceMock.activateUser(anyString(),anyString(),any(Contact.class))).thenReturn(mockResponse);
             })){
            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().activateUser("accountId","contactId",httpServletRequest);
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
        }
    }

    @Test
    void activateUser_Exception_test() {
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userServiceMock, context) -> {
                 Mockito.when(userServiceMock.activateUser(anyString(),anyString(),any(Contact.class))).thenThrow(new IllegalArgumentException("exception"));
             })) {
            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new UserStaffApi().activateUser("accountID", "contactId",httpServletRequest);
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse, responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
            Assertions.assertEquals(400, responseEntity.getStatusCode().value());
        }
    }

    @Test
    void deleteUser_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new UserStaffApi().deleteUser("accID","123",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void deleteUser_valid_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        HashMap<String,Object> mockResponse = new HashMap<>();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<UserStaffService> mock = Mockito.mockConstruction(UserStaffService.class, (userStaffServiceMock, context) -> {
                Mockito.when(userStaffServiceMock.deleteUser("accID","contactID","123")).thenReturn(mockResponse);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new UserStaffApi().deleteUser("accID","contactID",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }
}