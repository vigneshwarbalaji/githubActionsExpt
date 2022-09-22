package com.yoco.commons.annotation.implementation;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.annotation.helper.AnnotationHelper;
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

class ValidateTokenScopesTest {
    @Test
    void validateTokenScopes_nullToken_test() throws Throwable {
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(AnnotationHelper::extractServletRequest).thenReturn(new MockHttpServletRequest());
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(MockHttpServletRequest.class))).thenReturn(null);
            annotationHelperMockedStatic.when(AnnotationHelper::generate401FailureResponse).thenCallRealMethod();
            ProceedingJoinPoint proceedingJoinPointMock = Mockito.mock(ProceedingJoinPoint.class);
            ResponseEntity<GenericResponse> result = (ResponseEntity<GenericResponse>) new ValidateTokenScopes().processTokenScopeValidation(proceedingJoinPointMock);
            Assertions.assertEquals(HttpStatus.UNAUTHORIZED, result.getStatusCode());
            Assertions.assertFalse(result.getBody().isSuccess());
            Assertions.assertEquals("Unauthorized request.",result.getBody().getErrorMessage());
        }
    }

    @Test
    void validateTokenScopes_containsAnyScopeFalse_test() throws Throwable {
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(AnnotationHelper::extractServletRequest).thenReturn(new MockHttpServletRequest());
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(MockHttpServletRequest.class))).thenReturn(new OauthAccessToken());
            annotationHelperMockedStatic.when(()->AnnotationHelper.hasValidContainsAnyScopes(null,null)).thenReturn(false);
            annotationHelperMockedStatic.when(AnnotationHelper::generate403FailureResponse).thenCallRealMethod();
            ProceedingJoinPoint proceedingJoinPointMock = Mockito.mock(ProceedingJoinPoint.class);
            ResponseEntity<GenericResponse> result = (ResponseEntity<GenericResponse>) new ValidateTokenScopes().processTokenScopeValidation(proceedingJoinPointMock);
            Assertions.assertEquals(HttpStatus.FORBIDDEN, result.getStatusCode());
            Assertions.assertFalse(result.getBody().isSuccess());
            Assertions.assertEquals("You are not authorized to access this resource.",result.getBody().getErrorMessage());
        }
    }

    @Test
    void validateTokenScopes_RequiredScopeFalse_test() throws Throwable {
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(AnnotationHelper::extractServletRequest).thenReturn(new MockHttpServletRequest());
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(MockHttpServletRequest.class))).thenReturn(new OauthAccessToken());
            annotationHelperMockedStatic.when(()->AnnotationHelper.hasValidContainsAnyScopes(null,null)).thenReturn(true);
            annotationHelperMockedStatic.when(()->AnnotationHelper.hasValidRequiredScopes(null,null)).thenReturn(false);
            annotationHelperMockedStatic.when(AnnotationHelper::generate403FailureResponse).thenCallRealMethod();
            ProceedingJoinPoint proceedingJoinPointMock = Mockito.mock(ProceedingJoinPoint.class);
            ResponseEntity<GenericResponse> result = (ResponseEntity<GenericResponse>) new ValidateTokenScopes().processTokenScopeValidation(proceedingJoinPointMock);
            Assertions.assertEquals(HttpStatus.FORBIDDEN, result.getStatusCode());
            Assertions.assertFalse(result.getBody().isSuccess());
            Assertions.assertEquals("You are not authorized to access this resource.",result.getBody().getErrorMessage());
        }
    }

    @Test
    void validateTokenScopes_validScopes_test() throws Throwable {
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(AnnotationHelper::extractServletRequest).thenReturn(new MockHttpServletRequest());
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(MockHttpServletRequest.class))).thenReturn(new OauthAccessToken());
            annotationHelperMockedStatic.when(()->AnnotationHelper.hasValidContainsAnyScopes(null,null)).thenReturn(true);
            annotationHelperMockedStatic.when(()->AnnotationHelper.hasValidRequiredScopes(null,null)).thenReturn(true);
            ProceedingJoinPoint proceedingJoinPointMock = Mockito.mock(ProceedingJoinPoint.class);
            Mockito.when(proceedingJoinPointMock.proceed()).thenReturn(new ResponseEntity<>(
                    new GenericResponse(true, null, null),
                    HttpStatus.OK));
            ResponseEntity<GenericResponse> result = (ResponseEntity<GenericResponse>) new ValidateTokenScopes().processTokenScopeValidation(proceedingJoinPointMock);
            Assertions.assertEquals(HttpStatus.OK, result.getStatusCode());
            Assertions.assertTrue(result.getBody().isSuccess());
            Assertions.assertNull(result.getBody().getErrorMessage());
        }
    }
}
