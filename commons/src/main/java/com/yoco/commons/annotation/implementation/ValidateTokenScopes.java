package com.yoco.commons.annotation.implementation;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

@Aspect
@Component
@Order(2)
@Slf4j
public class ValidateTokenScopes {
    @Around("@annotation(com.yoco.commons.annotation.declaration.ValidateTokenScopes)")
    public Object processTokenScopeValidation(ProceedingJoinPoint proceedingJoinPoint) throws Throwable {
        OauthAccessToken accessToken = AnnotationHelper.extractCurrentUserAccessTokenFromRequest(AnnotationHelper.extractServletRequest());
        if(accessToken == null){
            return AnnotationHelper.generate401FailureResponse();
        }
        ApiScope[] containsAnyScopes = AnnotationHelper.extractContainsAnyScopes(proceedingJoinPoint);
        if(!AnnotationHelper.hasValidContainsAnyScopes(accessToken.getScopes(),containsAnyScopes)){
            return AnnotationHelper.generate403FailureResponse();
        }
        ApiScope[] requiredScopes = AnnotationHelper.extractRequiredScopes(proceedingJoinPoint);
        if(!AnnotationHelper.hasValidRequiredScopes(accessToken.getScopes(),requiredScopes)){
            return AnnotationHelper.generate403FailureResponse();
        }
        return proceedingJoinPoint.proceed();
    }
}
