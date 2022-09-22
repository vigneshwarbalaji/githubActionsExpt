package com.yoco.commons.annotation.helper;

import com.fullauth.api.enums.OauthTokenType;
import com.fullauth.api.model.oauth.OauthAccessToken;
import com.fullauth.api.utils.TokenUtil;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.error.ApiErrorCode;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import javax.servlet.http.HttpServletRequest;
import java.util.Set;

@Slf4j
public class AnnotationHelper {
    private AnnotationHelper(){}

    public static final String CURRENT_USER_CONTACT = "currentUserContact";
    public static final String CURRENT_USER_ACCESSTOKEN = "currentUserAccessToken";

    public static String extractJwtFromRequest(HttpServletRequest request){
        if(request == null){
            return null;
        }
        String jwtHeader = request.getHeader("Authorization");
        return ObjUtils.isNullOrEmpty(jwtHeader) ? null : TokenUtil.extractAuthToken(jwtHeader, OauthTokenType.BEARER.toString());
    }

    public static HttpServletRequest extractServletRequest(){
        HttpServletRequest request = null;
        try{
             request = ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes()).getRequest();
        }catch (Exception e){
            log.info(e.getMessage());
        }
        return request;
    }

    public static OauthAccessToken extractAccessTokenFromJwt(String jwt) {
        OauthAccessToken accessToken = null;
        try{
            accessToken = AccessTokenCacheService.getAccessToken(jwt);
            if(accessToken == null){
                accessToken = FullAuthService.getAccessTokenInfo(jwt);
                if(accessToken != null){
                    AccessTokenCacheService.putAccessToken(accessToken);
                }
            }
        }catch (Exception e){
            log.info(e.getMessage());
        }
        return accessToken;
    }

    public static Contact extractContactFromAccessToken(OauthAccessToken accessToken){
        if(accessToken == null || accessToken.getUserId() == null){
            return null;
        }
        return ContactImpl.getContactImplInstance().getByID(accessToken.getUserId());
    }

    public static ApiScope[] extractContainsAnyScopes(ProceedingJoinPoint proceedingJoinPoint){
        if(proceedingJoinPoint == null){
            return new ApiScope[]{};
        }
        MethodSignature signature = (MethodSignature) proceedingJoinPoint.getSignature();
        ApiScope[] containsAnyScopes =  signature.getMethod().getAnnotation(ValidateTokenScopes.class).containsAny();
        return containsAnyScopes == null ? new ApiScope[]{} : containsAnyScopes;
    }

    public static ApiScope[] extractRequiredScopes(ProceedingJoinPoint proceedingJoinPoint){
        if(proceedingJoinPoint == null){
            return new ApiScope[]{};
        }
        MethodSignature signature = (MethodSignature) proceedingJoinPoint.getSignature();
        ApiScope[] requiredScopes =  signature.getMethod().getAnnotation(ValidateTokenScopes.class).required();
        return requiredScopes == null ? new ApiScope[]{} : requiredScopes;
    }

    public static boolean hasValidContainsAnyScopes(Set<String> actualScopes, ApiScope[] expectedScopes){
        if(ObjUtils.isNullOrEmpty(expectedScopes)){
            return true;
        }
        if(ObjUtils.isNullOrEmpty(actualScopes)){
            return false;
        }
        for (ApiScope scope : expectedScopes){
            if(actualScopes.contains(scope.toString())){
                return true;
            }
        }
        return false;
    }

    public static boolean hasValidRequiredScopes(Set<String> actualScopes, ApiScope[] expectedScopes){
        if(ObjUtils.isNullOrEmpty(expectedScopes)){
            return true;
        }
        if(ObjUtils.isNullOrEmpty(actualScopes)){
            return false;
        }
        for (ApiScope scope : expectedScopes){
            if(!actualScopes.contains(scope.toString())){
                return false;
            }
        }
        return true;
    }

    public static ResponseEntity<GenericResponse> generate401FailureResponse(){
        return new ResponseEntity<>(
                new GenericResponse(false, ApiErrorCode.UNAUTHORIZED_REQUEST, "Unauthorized request."),
                HttpStatus.UNAUTHORIZED);
    }

    public static ResponseEntity<GenericResponse> generate403FailureResponse(){
        log.info("inside");
        return new ResponseEntity<>(
                new GenericResponse(false, ApiErrorCode.FORBIDDEN_REQUEST, "You are not authorized to access this resource."),
                HttpStatus.FORBIDDEN);
    }

    public static Contact extractCurrentUserContactFromRequest(HttpServletRequest request){
        if(request == null){
            return null;
        }
        return (Contact) request.getAttribute(AnnotationHelper.CURRENT_USER_CONTACT);
    }

    public static OauthAccessToken extractCurrentUserAccessTokenFromRequest(HttpServletRequest request){
        if(request == null){
            return null;
        }
        return (OauthAccessToken) request.getAttribute(AnnotationHelper.CURRENT_USER_ACCESSTOKEN);
    }

    public static boolean isAccessTokenTypeUser(OauthAccessToken token){
        if(token == null){
            return false;
        }
        return "user".equalsIgnoreCase(token.getType().toString());
    }

    public static boolean isAccessTokenTypeServer(OauthAccessToken token){
        if(token == null){
            return false;
        }
        return "server".equalsIgnoreCase(token.getType().toString());
    }
}
