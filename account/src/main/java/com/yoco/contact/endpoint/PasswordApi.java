package com.yoco.contact.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AccessTokenCacheService;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.cloudservices.CommonTaskInitiator;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.enums.error.DCM_ERROR_RESPONSE;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.contact.helper.password.PasswordDCMHelper;
import com.yoco.contact.service.PasswordService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class PasswordApi {

    private final PasswordService passwordService = new PasswordService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/contact/{contactID}/verificationId")
    public ResponseEntity<GenericResponse> getVerificationId(@PathVariable("contactID") String contactID){
        var genericResponse = new GenericResponse();
        try{
            Map<String,Object> verificationRespMap = PasswordDCMHelper.getVerificationID(contactID);

            if(ObjUtils.isNullOrEmpty(verificationRespMap)){
                genericResponse = new GenericResponse(false, null, DCM_ERROR_RESPONSE.FAILED_TO_FETCH_FROM_DCM.value());

            }else if(Boolean.TRUE.equals(verificationRespMap.get(Commons.SUCCESS))) {
                genericResponse.setSuccess(true);
                genericResponse.add(ContactConstants.VERIFICATION_ID, verificationRespMap.get(ContactConstants.VERIFICATION_ID));

            } else{
                genericResponse = new GenericResponse(false, null, (String) verificationRespMap.get(Commons.ERROR));
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch(Exception e){
            log.error(" error on getVerificationId : "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }


    @GetMapping("/contact/{verificationId}/validate")
    public ResponseEntity<Boolean> validateVerificationId(@PathVariable("verificationId") String verificationId){
        try{
            boolean isValid = PasswordDCMHelper.validateVerificationId(verificationId);
            return ResponseEntity.status(HttpStatus.OK).body(isValid);
        }catch(Exception e){
            log.error(" error on validateVerificationId : "+e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(false);
        }
    }


    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_IDENTITY})
    @PutMapping("/account/{accountID}/contact/password")
    public ResponseEntity<GenericResponse> updatePassword(@PathVariable("accountID") String accountID,
                                                          @RequestBody String payload,
                                                          HttpServletRequest request){
        var genericResponse = new GenericResponse();
        try{
            var requesterContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            var token = AnnotationHelper.extractCurrentUserAccessTokenFromRequest(request);

            String contactID = requesterContact.getId();

            Map<String,Object> updatePasswordResp = passwordService.updatePassword(accountID,contactID,payload,token.getAccessToken());

            if(ObjUtils.isNullOrEmpty(updatePasswordResp)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());

            }else if(Boolean.TRUE.equals(updatePasswordResp.get(Commons.SUCCESS))) {
                genericResponse.setSuccess(true);
                AccessTokenCacheService.clearAccessTokensForUser(token,contactID);
                CommonTaskInitiator.initiateClearCacheQueue(token.getAccessToken(),Map.of()); // this is temporary patch, once the mem cache dependency is removed, this code snippet should be removed.
            } else{
                genericResponse = new GenericResponse(false, null, updatePasswordResp.containsKey(Commons.ERROR_MESSAGE)
                        ? (String) updatePasswordResp.get(Commons.ERROR_MESSAGE): (String) updatePasswordResp.get(Commons.ERROR));
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch(Exception e){
            log.error(" error on updatePassword : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }


    @GetMapping("/contact/password/processForgotPasswordRequest")
    public ResponseEntity<GenericResponse> processForgotPasswordRequest(@RequestParam String email){
        var genericResponse = new GenericResponse();
        try{
            Map<String,Object> forgotPassRespMap = passwordService.forgotPassword(email);

            if(ObjUtils.isNullOrEmpty(forgotPassRespMap)){
                genericResponse = new GenericResponse(false, null, DCM_ERROR_RESPONSE.FAILED_TO_FETCH_FROM_DCM.value());

            }else if(Boolean.TRUE.equals(forgotPassRespMap.get(Commons.SUCCESS))) {
                genericResponse.setSuccess(true);
                genericResponse.add(ContactConstants.VERIFICATION_ID, forgotPassRespMap.get(ContactConstants.VERIFICATION_ID));

            } else{
                genericResponse = new GenericResponse(false, null, (String) forgotPassRespMap.get(Commons.ERROR));
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch(Exception e){
            log.error(" error on forgotPassword : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());

            HttpStatus httpStatus = COMMON_ERROR_RESPONSE.REQUEST_LIMIT_REACHED.value().equalsIgnoreCase(e.getMessage())
                    ? HttpStatus.TOO_MANY_REQUESTS : HttpStatus.BAD_REQUEST;

            return ResponseEntity.status(httpStatus).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping("/contact/{contactID}/password/reset")
    public ResponseEntity<GenericResponse> resetPassword(@PathVariable("contactID") String contactID,
                                                         @RequestBody String payload){
        var genericResponse = new GenericResponse();
        try{
            Map<String,Object> resetPassRespMap = passwordService.resetPassword(contactID,payload);

            if(ObjUtils.isNullOrEmpty(resetPassRespMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());

            }else if(Boolean.TRUE.equals(resetPassRespMap.get(Commons.SUCCESS))) {
                genericResponse.setSuccess(true);
                HashMap<String, Object> dcmContact = (HashMap<String, Object>) resetPassRespMap.get(ContactConstants.CONTACT);
                genericResponse.add(ContactConstants.IS_PASSWORD_PRESENT, dcmContact.get(ContactConstants.IS_PASSWORD_PRESENT));
                CommonTaskInitiator.initiateClearCacheQueue(FullAuthService.getServerAccessToken(),Map.of("contactID",contactID));
            } else{
                genericResponse = new GenericResponse(false, null, (String) resetPassRespMap.get(Commons.ERROR));
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch(Exception e){
            log.error(" error on reset Password : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

}
