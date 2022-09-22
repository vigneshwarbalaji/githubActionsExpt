package com.yoco.contact.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.contact.service.ProfileService;
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
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class ProfileApiTest {

   @Test
    void getProfileImageUploadUrl_successFalse_test(){
        try (MockedConstruction<ProfileService> mock = Mockito.mockConstruction(ProfileService.class, (profileServiceMock, context) -> {
            Mockito.when(profileServiceMock.getProfileImageUploadUrl(anyString(),anyString())).thenReturn(Map.of(Commons.SUCCESS,false));
        })){
            ResponseEntity<GenericResponse> responseEntity = new ProfileApi().getProfileImageUploadUrl("id","fName");
            var genericResponse = new GenericResponse(false, null, null);
            genericResponse.setData(Map.of("upload_url",""));
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getProfileImageUploadUrl_successTrue_test(){
        try (MockedConstruction<ProfileService> mock = Mockito.mockConstruction(ProfileService.class, (profileServiceMock, context) -> {
            Mockito.when(profileServiceMock.getProfileImageUploadUrl(anyString(),anyString())).thenReturn(Map.of(Commons.SUCCESS,true,"upload_url","urlLink"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new ProfileApi().getProfileImageUploadUrl("id","fName");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(Map.of("upload_url","urlLink"));
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getProfileImageUploadUrl_exception_test(){
        try (MockedConstruction<ProfileService> mock = Mockito.mockConstruction(ProfileService.class, (profileServiceMock, context) -> {
            Mockito.when(profileServiceMock.getProfileImageUploadUrl(anyString(),anyString())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new ProfileApi().getProfileImageUploadUrl("id","fName");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void updateProfileImage_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<ProfileService> mock = Mockito.mockConstruction(ProfileService.class, (profileServiceMock, context) -> {
            Mockito.when(profileServiceMock.updateProfileImage(anyString())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new ProfileApi().updateProfileImage("payload");
            var genericResponse = new GenericResponse(false, null, null);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updateProfileImage_successTrue_test(){
        try (MockedConstruction<ProfileService> mock = Mockito.mockConstruction(ProfileService.class, (profileServiceMock, context) -> {
            Mockito.when(profileServiceMock.updateProfileImage(anyString())).thenReturn(Map.of("user","updatedUser"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new ProfileApi().updateProfileImage("payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(Map.of("user","updatedUser"));
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updateProfileImage_exception_test(){
        try (MockedConstruction<ProfileService> mock = Mockito.mockConstruction(ProfileService.class, (profileServiceMock, context) -> {
            Mockito.when(profileServiceMock.updateProfileImage(anyString())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new ProfileApi().updateProfileImage("payload");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void updateProfile_nullResponse_test(Map<String,Object> mockMap){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
             MockedConstruction<ProfileService> mock = Mockito.mockConstruction(ProfileService.class, (profileServiceMock, context) -> {
            Mockito.when(profileServiceMock.updateProfile(any(Contact.class),anyString())).thenReturn(mockMap);
        })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> responseEntity = new ProfileApi().updateProfile("payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updateProfile_successTrue_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
             MockedConstruction<ProfileService> mock = Mockito.mockConstruction(ProfileService.class, (profileServiceMock, context) -> {
            Mockito.when(profileServiceMock.updateProfile(any(Contact.class),anyString())).thenReturn(Map.of("user","updatedUser"));
        })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> responseEntity = new ProfileApi().updateProfile("payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(Map.of("user","updatedUser"));
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updateProfile_exception_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
             MockedConstruction<ProfileService> mock = Mockito.mockConstruction(ProfileService.class, (profileServiceMock, context) -> {
            Mockito.when(profileServiceMock.updateProfile(any(Contact.class),anyString())).thenThrow(new IllegalArgumentException("exception"));
        })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> responseEntity = new ProfileApi().updateProfile("payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

}