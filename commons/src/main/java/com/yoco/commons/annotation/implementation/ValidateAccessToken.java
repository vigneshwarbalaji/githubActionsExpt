package com.yoco.commons.annotation.implementation;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.modal.GenericResponse;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.core.annotation.Order;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import javax.servlet.http.HttpServletRequest;

@Slf4j
@Aspect
@Component
@Order(1)
public class ValidateAccessToken {
    @Around("@annotation(com.yoco.commons.annotation.declaration.ValidateAccessToken)")
    public ResponseEntity<GenericResponse> processRequestAuthentication(ProceedingJoinPoint proceedingJoinPoint) throws Throwable {
        HttpServletRequest request = AnnotationHelper.extractServletRequest();
        String jwt = AnnotationHelper.extractJwtFromRequest(request);
        if(jwt == null){
            return AnnotationHelper.generate401FailureResponse();
        }
        OauthAccessToken accessToken = AnnotationHelper.extractAccessTokenFromJwt(jwt);
        if(accessToken == null){
            return AnnotationHelper.generate401FailureResponse();
        }
        if("user".equalsIgnoreCase(accessToken.getType().toString())){
            var userContactInfo = AnnotationHelper.extractContactFromAccessToken(accessToken);
            if(userContactInfo == null){
                return AnnotationHelper.generate401FailureResponse();
            }
            request.setAttribute(AnnotationHelper.CURRENT_USER_CONTACT,userContactInfo);
        }
        request.setAttribute(AnnotationHelper.CURRENT_USER_ACCESSTOKEN, accessToken);
        return (ResponseEntity<GenericResponse>) proceedingJoinPoint.proceed();
    }
}
