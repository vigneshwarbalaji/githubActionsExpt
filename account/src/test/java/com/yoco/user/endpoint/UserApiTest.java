package com.yoco.user.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.user.service.UserService;
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

class UserApiTest {

    @ParameterizedTest
    @NullAndEmptySource
    void updateProProfile_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserService> mock = Mockito.mockConstruction(UserService.class, (userServiceMock, context) -> {
            Mockito.when(userServiceMock.updateProfilePRO(anyString(),anyString(),anyString())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserApi().updateProProfile("accountId","contactId","payload");
            var genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.FAILED_TO_PERSIST.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updateProProfile_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserService> mock = Mockito.mockConstruction(UserService.class, (userServiceMock, context) -> {
            Mockito.when(userServiceMock.updateProfilePRO(anyString(),anyString(),anyString())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserApi().updateProProfile("accountId","contactId","payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updateProProfile_Exception_test(){
        try (MockedConstruction<UserService> mock = Mockito.mockConstruction(UserService.class, (userServiceMock, context) -> {
            Mockito.when(userServiceMock.updateProfilePRO(anyString(),anyString(),anyString())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserApi().updateProProfile("accountID","contactId","payload");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void downloadStaffData_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new UserApi().downloadStaffData("accID","status","field",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void downloadStaffData_valid_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        HashMap<String,Object> mockResponse = new HashMap<>();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<UserService> mock = Mockito.mockConstruction(UserService.class, (userServiceMock, context) -> {
                Mockito.when(userServiceMock.downloadStaffData("123","accID","status","field")).thenReturn(mockResponse);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new UserApi().downloadStaffData("accID","status","field",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }
}