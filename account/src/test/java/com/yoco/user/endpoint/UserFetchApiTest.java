package com.yoco.user.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.user.enums.USER_ERROR_MESSAGE;
import com.yoco.user.service.UserFetchService;
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

class UserFetchApiTest {

    @ParameterizedTest
    @NullAndEmptySource
    void getUser_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getUser(any(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getUser("accountId", "contactId");
            var genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.USER_NOT_FOUND.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getUser_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getUser(any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getUser("accountId", "contactId");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getUser_Exception_test(){
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getUser(any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getUser("accountId", "contactId");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getAccessPolicy_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getAccessPolicy(any(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAccessPolicy("accountId", "contactId");
            var genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.USER_NOT_FOUND.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getAccessPolicy_no_policy_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getAccessPolicy(any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAccessPolicy("accountId", "contactId");
            var genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.NO_POLICY_EXISTS.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getAccessPolicy_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put(AccessManager.ACCESS_POLICY,"dummy");
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getAccessPolicy(any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAccessPolicy("accountId", "contactId");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getAccessPolicy_Exception_test(){
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getAccessPolicy(any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAccessPolicy("accountId", "contactId");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getAllUsers_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getAllUsers(any(),anyBoolean(),anyInt(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAllUsers("accountId", false,"",10);
            var genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.NO_USERS_FOUND.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getAllUsers_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getAllUsers(any(),anyBoolean(),anyInt(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAllUsers("accountId", false,"",10);
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getAllUsers_Exception_test(){
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getAllUsers(any(),anyBoolean(),anyInt(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAllUsers("accountId", false,"",10);
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getAllUsersWithOutContact_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getAllUsersWithOutContact(any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAllUsersWithoutContact("accountId");
            var genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.NO_USERS_FOUND.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getAllUsersWithOutContact_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getAllUsersWithOutContact(any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAllUsersWithoutContact("accountId");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getAllUsersWithOutContact_Exception_test(){
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getAllUsersWithOutContact(any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAllUsersWithoutContact("accountId");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getAllRecentlyModifiedPROs_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getRecentlyUpdatedPROs(any(),anyLong())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAllRecentlyModifiedPROs("accountId",1646384551000L);
            var genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.NO_USERS_FOUND.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getAllRecentlyModifiedPROs_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getRecentlyUpdatedPROs(any(),anyLong())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAllRecentlyModifiedPROs("accountId",1646384551000L);
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getAllRecentlyModifiedPROs_Exception_test(){
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getRecentlyUpdatedPROs(any(),anyLong())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAllRecentlyModifiedPROs("accountID",1646384551000L);
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getMeInfo_exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("exceptionTest"));
            ResponseEntity<GenericResponse> expected = new UserFetchApi().getMeInfo("accountId",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "exceptionTest");
            Assertions.assertEquals(genericResponse,expected.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,expected.getStatusCode());
            Assertions.assertEquals(400,expected.getStatusCode().value());
        }
    }

    @Test
    void getMeInfo_valid_test() {
        Contact mockContact = new Contact();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock,context) -> {
                Mockito.when(userFetchServiceMock.getMeInfo("accountID",mockContact)).thenReturn(new HashMap(){{put("user","user");}});
            })){
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> expected = new UserFetchApi().getMeInfo("accountID",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(new HashMap(){{put("user","user");}});
            Assertions.assertEquals(genericResponse,expected.getBody());
            Assertions.assertEquals(HttpStatus.OK,expected.getStatusCode());
            Assertions.assertEquals(200,expected.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getAllUserInAccount_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock, context) -> {
            Mockito.when(userFetchServiceMock.getAllUsersInAccount(anyString(), anyString())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserFetchApi().getAllUserInAccount("accountId", "");
            var genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.NO_USERS_FOUND.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getAllUserInAccount_exception_test(){
        try(MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock,context) -> {
            Mockito.when(userFetchServiceMock.getAllUsersInAccount(anyString(), anyString())).thenThrow(new IllegalArgumentException("exceptionTest"));
        })){
            ResponseEntity<GenericResponse> expected = new UserFetchApi().getAllUserInAccount("accountId", "");
            var genericResponse = new GenericResponse(false, null, "exceptionTest");
            Assertions.assertEquals(genericResponse,expected.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,expected.getStatusCode());
            Assertions.assertEquals(400,expected.getStatusCode().value());
        }
    }

    @Test
    void getAllUserInAccount_valid_test() {
        Contact mockContact = new Contact();
        try(MockedConstruction<UserFetchService> mock = Mockito.mockConstruction(UserFetchService.class, (userFetchServiceMock,context) -> {
                Mockito.when(userFetchServiceMock.getAllUsersInAccount(anyString(), anyString())).thenReturn(new HashMap(){{put("user","user");}});
            })){
            ResponseEntity<GenericResponse> expected = new UserFetchApi().getAllUserInAccount("accountID", "");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(new HashMap(){{put("user","user");}});
            Assertions.assertEquals(genericResponse,expected.getBody());
            Assertions.assertEquals(HttpStatus.OK,expected.getStatusCode());
            Assertions.assertEquals(200,expected.getStatusCode().value());
        }
    }
}