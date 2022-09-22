package com.yoco.commons.annotation.helper;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.modal.GenericResponse;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.reflect.MethodSignature;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.HashSet;
import static org.mockito.Mockito.mockStatic;


class AnnotationHelperTest{
    @Test
    void extractJwtFromRequestNullRequestTest(){
        Assertions.assertNull(AnnotationHelper.extractJwtFromRequest(null));
    }

    @Test
    void extractJwtFromRequestNoAuthorizationHeaderTest(){
        Assertions.assertNull(AnnotationHelper.extractJwtFromRequest(new MockHttpServletRequest()));
    }

    @Test
    void extractJwtFromRequestEmptyAuthorizationHeaderTest(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader("Authorization","");
        Assertions.assertNull(AnnotationHelper.extractJwtFromRequest(request));
    }

    @Test
    void extractJwtFromRequestValidTest(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader("Authorization","Bearer authToken");
        Assertions.assertEquals("authToken",AnnotationHelper.extractJwtFromRequest(request));
    }

    @Test
    void extractServletRequestExceptionTest(){
        try(MockedStatic<RequestContextHolder> requestContextHolderMockedStatic = mockStatic(RequestContextHolder.class)){
            requestContextHolderMockedStatic.when(RequestContextHolder::currentRequestAttributes).thenReturn(null);
            Assertions.assertNull(AnnotationHelper.extractServletRequest());
        }
    }

    @Test
    void extractServletRequestValidTest(){
        try(MockedStatic<RequestContextHolder> requestContextHolderMockedStatic = mockStatic(RequestContextHolder.class)){
            MockHttpServletRequest request = new MockHttpServletRequest();
            ServletRequestAttributes servletRequestAttributesMock = Mockito.mock(ServletRequestAttributes.class);
            Mockito.when(servletRequestAttributesMock.getRequest()).thenReturn(request);
            requestContextHolderMockedStatic.when(RequestContextHolder::currentRequestAttributes).thenReturn(servletRequestAttributesMock);
            Assertions.assertEquals(request,AnnotationHelper.extractServletRequest());
        }
    }

    @Test
    void extractAccessTokenFromJwtExceptionTest(){
        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class)){
          accessTokenCacheServiceMockedStatic.when(() -> AccessTokenCacheService.getAccessToken("jwt")).thenThrow(new IllegalArgumentException());
          Assertions.assertNull(AnnotationHelper.extractAccessTokenFromJwt("jwt"));
        }
    }

    @Test
    void extractAccessTokenFromJwtInvalidJwtTest(){
        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class);
            MockedStatic<FullAuthService> fullAuthServiceMockedStatic = mockStatic(FullAuthService.class);){
            accessTokenCacheServiceMockedStatic.when(() -> AccessTokenCacheService.getAccessToken("jwt")).thenReturn(null);
            fullAuthServiceMockedStatic.when(() -> FullAuthService.getAccessTokenInfo("jwt")).thenReturn(null);
            Assertions.assertNull(AnnotationHelper.extractAccessTokenFromJwt("jwt"));
        }
    }

    @Test
    void extractAccessTokenFromJwtValidJwtPresentInCacheTest(){
        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class);){
            OauthAccessToken token = new OauthAccessToken();
            accessTokenCacheServiceMockedStatic.when(() -> AccessTokenCacheService.getAccessToken("jwt")).thenReturn(token);
            Assertions.assertEquals(token,AnnotationHelper.extractAccessTokenFromJwt("jwt"));
        }
    }

    @Test
    void extractAccessTokenFromJwtValidJwtFromFullAuthTest(){
        try(MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = mockStatic(AccessTokenCacheService.class);
            MockedStatic<FullAuthService> fullAuthServiceMockedStatic = mockStatic(FullAuthService.class);){
            OauthAccessToken token = new OauthAccessToken();
            accessTokenCacheServiceMockedStatic.when(() -> AccessTokenCacheService.getAccessToken("jwt")).thenReturn(null);
            fullAuthServiceMockedStatic.when(() -> FullAuthService.getAccessTokenInfo("jwt")).thenReturn(token);
            Assertions.assertEquals(token,AnnotationHelper.extractAccessTokenFromJwt("jwt"));
            accessTokenCacheServiceMockedStatic.verify(() -> AccessTokenCacheService.putAccessToken(token));
        }
    }

    @Test
    void extractContactFromAccessTokenNullTokenTest(){
        Assertions.assertNull(AnnotationHelper.extractContactFromAccessToken(null));
    }

    @Test
    void extractContactFromAccessTokenNullContactIdTest(){
        Assertions.assertNull(AnnotationHelper.extractContactFromAccessToken(new OauthAccessToken()));
    }

    @Test
    void extractContactFromAccessTokenValidTest(){
        try(MockedStatic<ContactImpl> contactMockedStatic = mockStatic(ContactImpl.class)){
            OauthAccessToken token = new OauthAccessToken();
            token.setUserId("userID");
            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Contact contact= new Contact();
            Mockito.when(contactImplMock.getByID("userID")).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);
            Assertions.assertEquals(contact,AnnotationHelper.extractContactFromAccessToken(token));
        }
    }

    @Test
    void extractContainsAnyScopes_nullJoinPoint_test(){
        ApiScope[] result = AnnotationHelper.extractContainsAnyScopes(null);
        Assertions.assertEquals(0, result.length);
    }

    @Test
    void extractRequiredScopes_nullJoinPoint_test(){
        ApiScope[] result = AnnotationHelper.extractRequiredScopes(null);
        Assertions.assertEquals(0, result.length);
    }

    @Test
    void hasValidContainsAnyScopes_nullExpectedScopes_test(){
        Assertions.assertTrue(AnnotationHelper.hasValidContainsAnyScopes(new HashSet<>(),null));
    }

    @Test
    void hasValidContainsAnyScopes_EmptyExpectedScopes_test(){
        Assertions.assertTrue(AnnotationHelper.hasValidContainsAnyScopes(new HashSet<>(),new ApiScope[]{}));
    }

    @Test
    void hasValidContainsAnyScopes_nullActualScopes_test(){
        Assertions.assertFalse(AnnotationHelper.hasValidContainsAnyScopes(null,new ApiScope[]{ApiScope.YOCOAPIS_FULLACCESS}));
    }

    @Test
    void hasValidContainsAnyScopes_emptyActualScopes_test(){
        Assertions.assertFalse(AnnotationHelper.hasValidContainsAnyScopes(new HashSet<>(),new ApiScope[]{ApiScope.YOCOAPIS_FULLACCESS}));
    }

    @Test
    void hasValidContainsAnyScopes_NoContainsAnyScopes_test(){
        Assertions.assertFalse(AnnotationHelper.hasValidContainsAnyScopes(new HashSet(){{add("yoco-api.read");}},new ApiScope[]{ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_CLIENT_READ}));
    }

    @Test
    void hasValidContainsAnyScopes_ValidContainsAnyScopes_test(){
        Assertions.assertTrue(AnnotationHelper.hasValidContainsAnyScopes(new HashSet(){{add("yoco-api.fullaccess");}},new ApiScope[]{ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_CLIENT_READ}));
    }

    @Test
    void hasValidRequiredScopes_nullExpectedScopes_test(){
        Assertions.assertTrue(AnnotationHelper.hasValidRequiredScopes(new HashSet<>(),null));
    }

    @Test
    void hasValidRequiredScopes_EmptyExpectedScopes_test(){
        Assertions.assertTrue(AnnotationHelper.hasValidRequiredScopes(new HashSet<>(),new ApiScope[]{}));
    }

    @Test
    void hasValidRequiredScopes_nullActualScopes_test(){
        Assertions.assertFalse(AnnotationHelper.hasValidRequiredScopes(null,new ApiScope[]{ApiScope.YOCOAPIS_FULLACCESS}));
    }

    @Test
    void hasValidRequiredScopes_emptyActualScopes_test(){
        Assertions.assertFalse(AnnotationHelper.hasValidRequiredScopes(new HashSet<>(),new ApiScope[]{ApiScope.YOCOAPIS_FULLACCESS}));
    }

    @Test
    void hasValidRequiredScopes_NoRequiredScopes_test(){
        Assertions.assertFalse(AnnotationHelper.hasValidRequiredScopes(new HashSet(){{add("yoco-api.read");}},new ApiScope[]{ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_CLIENT_READ}));
    }

    @Test
    void hasValidRequiredScopes_partialRequiredScopesScopes_test(){
        Assertions.assertFalse(AnnotationHelper.hasValidRequiredScopes(new HashSet(){{add("yoco-api.fullaccess");}},new ApiScope[]{ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_CLIENT_READ}));
    }

    @Test
    void hasValidRequiredScopes_ValidRequiredScopesScopes_test(){
        Assertions.assertTrue(AnnotationHelper.hasValidRequiredScopes(new HashSet(){{add("yoco-api.fullaccess");add("yoco-api.account.client.read");}},new ApiScope[]{ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_CLIENT_READ}));
    }
//
    @Test
    void generate401FailureResponse_test(){
        ResponseEntity<GenericResponse> result = AnnotationHelper.generate401FailureResponse();
        Assertions.assertEquals(HttpStatus.UNAUTHORIZED, result.getStatusCode());
        Assertions.assertFalse(result.getBody().isSuccess());
        Assertions.assertEquals("Unauthorized request.",result.getBody().getErrorMessage());
    }

    @Test
    void generate403FailureResponse_test(){
        ResponseEntity<GenericResponse> result = AnnotationHelper.generate403FailureResponse();
        Assertions.assertEquals(HttpStatus.FORBIDDEN, result.getStatusCode());
        Assertions.assertFalse(result.getBody().isSuccess());
        Assertions.assertEquals("You are not authorized to access this resource.",result.getBody().getErrorMessage());
    }

    @Test
    void extractCurrentUserContactFromRequest_nullRequestTest(){
        Assertions.assertNull(AnnotationHelper.extractCurrentUserContactFromRequest(null));
    }

    @Test
    void extractCurrentUserContactFromRequest_ValidRequestTest(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        Contact contact = new Contact();
        request.setAttribute("currentUserContact",contact);
        Assertions.assertEquals(contact,AnnotationHelper.extractCurrentUserContactFromRequest(request));
    }

    @Test
    void extractCurrentUserAccessTokenFromRequest_nullRequestTest(){
        Assertions.assertNull(AnnotationHelper.extractCurrentUserAccessTokenFromRequest(null));
    }

    @Test
    void extractCurrentUserAccessTokenFromRequest_ValidRequestTest(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        OauthAccessToken token = new OauthAccessToken();
        request.setAttribute("currentUserAccessToken",token);
        Assertions.assertEquals(token,AnnotationHelper.extractCurrentUserAccessTokenFromRequest(request));
    }

    @ValidateTokenScopes()
    public void annotationScopeNull(){}

    @ValidateTokenScopes(required = ApiScope.YOCOAPIS_FULLACCESS,containsAny = ApiScope.YOCOAPIS_FULLACCESS)
    public void annotationScopeValid(){}



    @Test
    void extractRequiredScopes_nullScopes_test() throws NoSuchMethodException {
        ProceedingJoinPoint proceedingJoinPoint = Mockito.mock(ProceedingJoinPoint.class);
        MethodSignature signature = Mockito.mock(MethodSignature.class);
        Method method =AnnotationHelperTest.class.getMethod("annotationScopeNull");
        Mockito.when(signature.getMethod()).thenReturn(method);
        Mockito.when(proceedingJoinPoint.getSignature()).thenReturn(signature);
        ApiScope[] result = AnnotationHelper.extractRequiredScopes(proceedingJoinPoint);
        Assertions.assertEquals(0, result.length);
    }

    @Test
    void extractRequiredScopes_ValidScopes_test() throws NoSuchMethodException {
        ProceedingJoinPoint proceedingJoinPoint = Mockito.mock(ProceedingJoinPoint.class);
        MethodSignature signature = Mockito.mock(MethodSignature.class);
        Method method =AnnotationHelperTest.class.getMethod("annotationScopeValid");
        Mockito.when(signature.getMethod()).thenReturn(method);
        Mockito.when(proceedingJoinPoint.getSignature()).thenReturn(signature);
        ApiScope[] result = AnnotationHelper.extractRequiredScopes(proceedingJoinPoint);
        Assertions.assertEquals(1, result.length);
        Assertions.assertEquals(ApiScope.YOCOAPIS_FULLACCESS, result[0]);
    }

    @Test
    void extractContainsAnyScopes_nullScopes_test() throws NoSuchMethodException {
        ProceedingJoinPoint proceedingJoinPoint = Mockito.mock(ProceedingJoinPoint.class);
        MethodSignature signature = Mockito.mock(MethodSignature.class);
        Method method =AnnotationHelperTest.class.getMethod("annotationScopeNull");
        Mockito.when(signature.getMethod()).thenReturn(method);
        Mockito.when(proceedingJoinPoint.getSignature()).thenReturn(signature);
        ApiScope[] result = AnnotationHelper.extractContainsAnyScopes(proceedingJoinPoint);
        Assertions.assertEquals(0, result.length);
    }

    @Test
    void extractContainsAnyScopes_ValidScopes_test() throws NoSuchMethodException {
        ProceedingJoinPoint proceedingJoinPoint = Mockito.mock(ProceedingJoinPoint.class);
        MethodSignature signature = Mockito.mock(MethodSignature.class);
        Method method =AnnotationHelperTest.class.getMethod("annotationScopeValid");
        Mockito.when(signature.getMethod()).thenReturn(method);
        Mockito.when(proceedingJoinPoint.getSignature()).thenReturn(signature);
        ApiScope[] result = AnnotationHelper.extractContainsAnyScopes(proceedingJoinPoint);
        Assertions.assertEquals(1, result.length);
        Assertions.assertEquals(ApiScope.YOCOAPIS_FULLACCESS, result[0]);
    }

    private ValidateTokenScopes getInstance(ApiScope[] containsAnyScopes, ApiScope[] requiredScopes)
    {
        return new ValidateTokenScopes()
        {
            @Override
            public ApiScope[] containsAny()
            {
                return containsAnyScopes;
            }
            @Override
            public ApiScope[] required()
            {
                return requiredScopes;
            }
            @Override
            public Class<? extends Annotation> annotationType()
            {
                return ValidateTokenScopes.class;
            }
        };
    }

    @Test
    void isAccessTokenTypeUser_nullToken_test(){
        Assertions.assertFalse(AnnotationHelper.isAccessTokenTypeUser(null));
    }

    @Test
    void isAccessTokenTypeServer_nullToken_test(){
        Assertions.assertFalse(AnnotationHelper.isAccessTokenTypeServer(null));
    }

    @Test
    void isAccessTokenTypeUser_tokenTypeNotUser_test(){
        OauthAccessToken token = new OauthAccessToken();
        token.setType("server");
        Assertions.assertFalse(AnnotationHelper.isAccessTokenTypeUser(token));
    }

    @Test
    void isAccessTokenTypeUser_validToken_test(){
        OauthAccessToken token = new OauthAccessToken();
        token.setType("user");
        Assertions.assertTrue(AnnotationHelper.isAccessTokenTypeUser(token));
    }

    @Test
    void isAccessTokenTypeServer_tokenTypeNotServer_test(){
        OauthAccessToken token = new OauthAccessToken();
        token.setType("user");
        Assertions.assertFalse(AnnotationHelper.isAccessTokenTypeServer(token));
    }

    @Test
    void isAccessTokenTypeServer_validToken_test(){
        OauthAccessToken token = new OauthAccessToken();
        token.setType("server");
        Assertions.assertTrue(AnnotationHelper.isAccessTokenTypeServer(token));
    }
}