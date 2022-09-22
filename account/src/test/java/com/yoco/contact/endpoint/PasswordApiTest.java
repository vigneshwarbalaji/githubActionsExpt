package com.yoco.contact.endpoint;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.annotation.helper.AccessTokenCacheService;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.cloudservices.CommonTaskInitiator;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.enums.error.DCM_ERROR_RESPONSE;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.contact.enums.CONTACT_ERROR_MESSAGE;
import com.yoco.contact.helper.password.PasswordDCMHelper;
import com.yoco.contact.service.PasswordService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;


class PasswordApiTest {

    @ParameterizedTest
    @NullAndEmptySource
    void getVerificationId_nullResponse_test(Map<String,Object> mockMap){
        try (MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)){
            passwordHelperMockedStatic.when(()-> PasswordDCMHelper.getVerificationID(anyString())).thenReturn(mockMap);

            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().getVerificationId("contactId");

            var genericResponse = new GenericResponse(false, null, DCM_ERROR_RESPONSE.FAILED_TO_FETCH_FROM_DCM.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getVerificationId_success_true_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put(Commons.SUCCESS,true);
        mockResponse.put(ContactConstants.VERIFICATION_ID,"verId");
        try (MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)){
            passwordHelperMockedStatic.when(()-> PasswordDCMHelper.getVerificationID(anyString())).thenReturn(mockResponse);

            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().getVerificationId("contactId");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add(ContactConstants.VERIFICATION_ID,"verId");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
        }
    }

    @Test
    void getVerificationId_success_false_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put(Commons.SUCCESS,false);
        mockResponse.put("error","error from dcm");
        try (MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)){
            passwordHelperMockedStatic.when(()-> PasswordDCMHelper.getVerificationID(anyString())).thenReturn(mockResponse);

            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().getVerificationId("contactId");
            var genericResponse = new GenericResponse(false, null, "error from dcm");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
        }
    }

    @Test
    void getVerificationId_exception_test(){
        try (MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)){
            passwordHelperMockedStatic.when(()-> PasswordDCMHelper.getVerificationID(anyString())).thenThrow(new IOException("exception"));

            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().getVerificationId("contactId");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse, responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
            Assertions.assertEquals(400, responseEntity.getStatusCode().value());
        }
    }

    @Test
    void validateVerificationId_success_true_test(){
        try (MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)){
            passwordHelperMockedStatic.when(()-> PasswordDCMHelper.validateVerificationId(anyString())).thenReturn(true);
            ResponseEntity<Boolean> responseEntity = new PasswordApi().validateVerificationId("123");
            Assertions.assertTrue(responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
        }
    }

    @Test
    void validateVerificationId_success_false_test(){
        try (MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)){
            passwordHelperMockedStatic.when(()-> PasswordDCMHelper.validateVerificationId(anyString())).thenReturn(false);
            ResponseEntity<Boolean> responseEntity = new PasswordApi().validateVerificationId("123");
            Assertions.assertFalse(responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
        }
    }

    @Test
    void validateVerificationId_exception_test(){
        try (MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)){
            passwordHelperMockedStatic.when(()-> PasswordDCMHelper.validateVerificationId(anyString())).thenThrow(new IllegalArgumentException("exception"));
            ResponseEntity<Boolean> responseEntity = new PasswordApi().validateVerificationId("123");
            Assertions.assertFalse(responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
            Assertions.assertEquals(400, responseEntity.getStatusCode().value());
        }
    }


    @ParameterizedTest
    @NullAndEmptySource
    void updatePassword_nullResponse_test(Map<String,Object> mockMap){
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
             MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
            Mockito.when(passwordServiceMock.updatePassword(anyString(),anyString(),anyString(),anyString())).thenReturn(mockMap);
        })){
            Contact mockContact = new Contact();
            mockContact.setId("123");
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);

            OauthAccessToken accessToken = new OauthAccessToken();
            accessToken.setAccessToken("token");
            accessToken.setIssuedTo("id");

            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(accessToken);

            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().updatePassword("accountId","payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }


    @Test
    void updatePassword_success_true_test(){
        Map<String,Object> mockMap = new HashMap<>();
        mockMap.put(Commons.SUCCESS,true);
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
             MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = Mockito.mockStatic(AccessTokenCacheService.class);
             MockedStatic<CommonTaskInitiator> initiatorMockedStatic = Mockito.mockStatic(CommonTaskInitiator.class);
             MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
                 Mockito.when(passwordServiceMock.updatePassword(anyString(),anyString(),anyString(),anyString())).thenReturn(mockMap);
             })){
            Contact mockContact = new Contact();
            mockContact.setId("123");
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);

            initiatorMockedStatic.when(()-> CommonTaskInitiator.initiateClearCacheQueue(anyString(),anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            OauthAccessToken accessToken = new OauthAccessToken();
            accessToken.setAccessToken("token");
            accessToken.setIssuedTo("id");

            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(accessToken);

            accessTokenCacheServiceMockedStatic.when(()-> AccessTokenCacheService.clearAccessTokensForUser(any(OauthAccessToken.class),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().updatePassword("accountId","payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updatePassword_success_false_errorMessage_test(){
        Map<String,Object> mockMap = new HashMap<>();
        mockMap.put(Commons.SUCCESS,false);
        mockMap.put(Commons.ERROR_MESSAGE,"errorMsg");
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
             MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = Mockito.mockStatic(AccessTokenCacheService.class);
             MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
                 Mockito.when(passwordServiceMock.updatePassword(anyString(),anyString(),anyString(),anyString())).thenReturn(mockMap);
             })){
            Contact mockContact = new Contact();
            mockContact.setId("123");
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);

            OauthAccessToken accessToken = new OauthAccessToken();
            accessToken.setAccessToken("token");
            accessToken.setIssuedTo("id");

            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(accessToken);

            accessTokenCacheServiceMockedStatic.when(()-> AccessTokenCacheService.clearAccessTokensForUser(any(OauthAccessToken.class),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().updatePassword("accountId","payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "errorMsg");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updatePassword_success_false_error_test(){
        Map<String,Object> mockMap = new HashMap<>();
        mockMap.put(Commons.SUCCESS,false);
        mockMap.put(Commons.ERROR,"errorReason");
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
             MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = Mockito.mockStatic(AccessTokenCacheService.class);
             MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
                 Mockito.when(passwordServiceMock.updatePassword(anyString(),anyString(),anyString(),anyString())).thenReturn(mockMap);
             })){
            Contact mockContact = new Contact();
            mockContact.setId("123");
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);

            OauthAccessToken accessToken = new OauthAccessToken();
            accessToken.setAccessToken("token");
            accessToken.setIssuedTo("id");

            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(accessToken);

            accessTokenCacheServiceMockedStatic.when(()-> AccessTokenCacheService.clearAccessTokensForUser(any(OauthAccessToken.class),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().updatePassword("accountId","payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "errorReason");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updatePassword_exception_test(){
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
             MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = Mockito.mockStatic(AccessTokenCacheService.class);
             MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
                 Mockito.when(passwordServiceMock.updatePassword(anyString(),anyString(),anyString(),anyString())).thenThrow(new IllegalArgumentException("exception"));
             })){
            Contact mockContact = new Contact();
            mockContact.setId("123");
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);

            OauthAccessToken accessToken = new OauthAccessToken();
            accessToken.setAccessToken("token");
            accessToken.setIssuedTo("id");

            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(accessToken);

            accessTokenCacheServiceMockedStatic.when(()-> AccessTokenCacheService.clearAccessTokensForUser(any(OauthAccessToken.class),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().updatePassword("accountId","payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void forgotPassword_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
            Mockito.when(passwordServiceMock.forgotPassword(anyString())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().processForgotPasswordRequest("test@gmail.com");
            var genericResponse = new GenericResponse(false, null, DCM_ERROR_RESPONSE.FAILED_TO_FETCH_FROM_DCM.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void forgotPassword_success_True_Response_test(){
        Map<String,Object> mockMap = Map.of(Commons.SUCCESS,true,ContactConstants.VERIFICATION_ID,"123");
        try (MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
            Mockito.when(passwordServiceMock.forgotPassword(anyString())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().processForgotPasswordRequest("test@gmail.com");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add(ContactConstants.VERIFICATION_ID,"123");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void forgotPassword_success_False_Response_test(){
        Map<String,Object> mockMap = Map.of(Commons.SUCCESS,false,Commons.ERROR,"errorMsg");
        try (MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
            Mockito.when(passwordServiceMock.forgotPassword(anyString())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().processForgotPasswordRequest("test@gmail.com");
            var genericResponse = new GenericResponse(false, null, "errorMsg");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void forgotPassword_limit_reached_test(){
        try (MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
            Mockito.when(passwordServiceMock.forgotPassword(anyString())).thenThrow(new IllegalArgumentException(COMMON_ERROR_RESPONSE.REQUEST_LIMIT_REACHED.value()));
        })){
            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().processForgotPasswordRequest("test@gmail.com");
            var genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.REQUEST_LIMIT_REACHED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.TOO_MANY_REQUESTS,responseEntity.getStatusCode());
            Assertions.assertEquals(429,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void forgotPassword_exception_test(){
        try (MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
            Mockito.when(passwordServiceMock.forgotPassword(anyString())).thenThrow(new IllegalArgumentException(COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.value()));
        })){
            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().processForgotPasswordRequest("test@gmail.com");
            var genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }


    @ParameterizedTest
    @NullAndEmptySource
    void resetPassword_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
            Mockito.when(passwordServiceMock.resetPassword(anyString(),anyString())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().resetPassword("contactId","payload");
            var genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void resetPassword_success_True_Response_test(){
        Map<String,Object> mockMap = new HashMap<>();
        mockMap.put(Commons.SUCCESS,true);
        mockMap.put(ContactConstants.CONTACT,new HashMap<>(){{put(ContactConstants.IS_PASSWORD_PRESENT,true);}});
        try (MockedStatic<CommonTaskInitiator> initiatorMockedStatic = Mockito.mockStatic(CommonTaskInitiator.class);
             MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
             MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
            Mockito.when(passwordServiceMock.resetPassword(anyString(),anyString())).thenReturn(mockMap);
        })){
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            initiatorMockedStatic.when(()-> CommonTaskInitiator.initiateClearCacheQueue(anyString(),anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().resetPassword("contactId","payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add(ContactConstants.IS_PASSWORD_PRESENT,true);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void resetPassword_success_False_Response_test(){
        Map<String,Object> mockMap = Map.of(Commons.SUCCESS,false,Commons.ERROR,"errorMsg");
        try (MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
            Mockito.when(passwordServiceMock.resetPassword(anyString(),anyString())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().resetPassword("contactId","payload");
            var genericResponse = new GenericResponse(false, null, "errorMsg");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void resetPassword_exception_test(){
        try (MockedConstruction<PasswordService> mock = Mockito.mockConstruction(PasswordService.class, (passwordServiceMock, context) -> {
            Mockito.when(passwordServiceMock.resetPassword(anyString(),anyString())).thenThrow(new IllegalArgumentException(CONTACT_ERROR_MESSAGE.CONTACT_DOES_NOT_EXIST.value()));
        })){
            ResponseEntity<GenericResponse> responseEntity = new PasswordApi().resetPassword("contactId","payload");
            var genericResponse = new GenericResponse(false, null, CONTACT_ERROR_MESSAGE.CONTACT_DOES_NOT_EXIST.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

}