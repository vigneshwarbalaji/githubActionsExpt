package com.yoco.commons.annotation.implementation;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.GenericResponse;
import org.aspectj.lang.ProceedingJoinPoint;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

class ValidateAccessTokenTest {
    @Test
    void processRequestAuthentication_nullJwt_test() throws Throwable {
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(AnnotationHelper::extractServletRequest).thenReturn(new MockHttpServletRequest());
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractJwtFromRequest(any(MockHttpServletRequest.class))).thenReturn(null);
            annotationHelperMockedStatic.when(AnnotationHelper::generate401FailureResponse).thenCallRealMethod();
            ProceedingJoinPoint proceedingJoinPointMock = Mockito.mock(ProceedingJoinPoint.class);
            ResponseEntity<GenericResponse> result = new ValidateAccessToken().processRequestAuthentication(proceedingJoinPointMock);
            Assertions.assertEquals(HttpStatus.UNAUTHORIZED, result.getStatusCode());
            Assertions.assertFalse(result.getBody().isSuccess());
            Assertions.assertEquals("Unauthorized request.",result.getBody().getErrorMessage());
        }
    }

    @Test
    void processRequestAuthentication_nullToken_test() throws Throwable {
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(AnnotationHelper::extractServletRequest).thenReturn(new MockHttpServletRequest());
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractJwtFromRequest(any(MockHttpServletRequest.class))).thenReturn("jwt");
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractAccessTokenFromJwt("jwt")).thenReturn(null);
            annotationHelperMockedStatic.when(AnnotationHelper::generate401FailureResponse).thenCallRealMethod();
            ProceedingJoinPoint proceedingJoinPointMock = Mockito.mock(ProceedingJoinPoint.class);
            ResponseEntity<GenericResponse> result = new ValidateAccessToken().processRequestAuthentication(proceedingJoinPointMock);
            Assertions.assertEquals(HttpStatus.UNAUTHORIZED, result.getStatusCode());
            Assertions.assertFalse(result.getBody().isSuccess());
            Assertions.assertEquals("Unauthorized request.",result.getBody().getErrorMessage());
        }
    }

    @Test
    void processRequestAuthentication_TokenTypeServer_test() throws Throwable {
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            MockHttpServletRequest mockRequest = new MockHttpServletRequest();
            annotationHelperMockedStatic.when(AnnotationHelper::extractServletRequest).thenReturn(mockRequest);
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractJwtFromRequest(any(MockHttpServletRequest.class))).thenReturn("jwt");
            OauthAccessToken token = new OauthAccessToken();
            token.setType("server");
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractAccessTokenFromJwt("jwt")).thenReturn(token);
            ProceedingJoinPoint proceedingJoinPointMock = Mockito.mock(ProceedingJoinPoint.class);
            Mockito.when(proceedingJoinPointMock.proceed()).thenReturn(new ResponseEntity<>(
                    new GenericResponse(true, null, null),
                    HttpStatus.OK));
            ResponseEntity<GenericResponse> result = new ValidateAccessToken().processRequestAuthentication(proceedingJoinPointMock);
            Assertions.assertEquals(HttpStatus.OK, result.getStatusCode());
            Assertions.assertTrue(result.getBody().isSuccess());
            Assertions.assertNull(result.getBody().getErrorMessage());
            Assertions.assertEquals(token,mockRequest.getAttribute(AnnotationHelper.CURRENT_USER_ACCESSTOKEN));
        }
    }

    @Test
    void processRequestAuthentication_TokenTypeUser_NullContact_test() throws Throwable {
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            MockHttpServletRequest mockRequest = new MockHttpServletRequest();
            annotationHelperMockedStatic.when(AnnotationHelper::extractServletRequest).thenReturn(mockRequest);
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractJwtFromRequest(any(MockHttpServletRequest.class))).thenReturn("jwt");
            OauthAccessToken token = new OauthAccessToken();
            token.setType("user");
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractAccessTokenFromJwt("jwt")).thenReturn(token);
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractContactFromAccessToken(token)).thenReturn(null);
            annotationHelperMockedStatic.when(AnnotationHelper::generate401FailureResponse).thenCallRealMethod();
            ProceedingJoinPoint proceedingJoinPointMock = Mockito.mock(ProceedingJoinPoint.class);
            ResponseEntity<GenericResponse> result = new ValidateAccessToken().processRequestAuthentication(proceedingJoinPointMock);
            Assertions.assertEquals(HttpStatus.UNAUTHORIZED, result.getStatusCode());
            Assertions.assertFalse(result.getBody().isSuccess());
            Assertions.assertEquals("Unauthorized request.",result.getBody().getErrorMessage());
        }
    }

    @Test
    void processRequestAuthentication_TokenTypeUser_validContact_test() throws Throwable {
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            MockHttpServletRequest mockRequest = new MockHttpServletRequest();
            annotationHelperMockedStatic.when(AnnotationHelper::extractServletRequest).thenReturn(mockRequest);
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractJwtFromRequest(any(MockHttpServletRequest.class))).thenReturn("jwt");
            OauthAccessToken token = new OauthAccessToken();
            token.setType("user");
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractAccessTokenFromJwt("jwt")).thenReturn(token);
            Contact userInfo = new Contact();
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractContactFromAccessToken(token)).thenReturn(userInfo);
            ProceedingJoinPoint proceedingJoinPointMock = Mockito.mock(ProceedingJoinPoint.class);
            Mockito.when(proceedingJoinPointMock.proceed()).thenReturn(new ResponseEntity<>(
                    new GenericResponse(true, null, null),
                    HttpStatus.OK));
            ResponseEntity<GenericResponse> result = new ValidateAccessToken().processRequestAuthentication(proceedingJoinPointMock);
            Assertions.assertEquals(HttpStatus.OK, result.getStatusCode());
            Assertions.assertTrue(result.getBody().isSuccess());
            Assertions.assertNull(result.getBody().getErrorMessage());
            Assertions.assertEquals(token,mockRequest.getAttribute(AnnotationHelper.CURRENT_USER_ACCESSTOKEN));
            Assertions.assertEquals(userInfo,mockRequest.getAttribute(AnnotationHelper.CURRENT_USER_CONTACT));
        }
    }
}
