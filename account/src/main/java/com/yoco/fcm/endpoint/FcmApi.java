package com.yoco.fcm.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.enums.error.ApiErrorCode;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.fcm.helper.FcmHelper;
import com.yoco.fcm.service.FcmService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class FcmApi {

    FcmService fcmService = new FcmService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/fcm/device/syncInfo")
    public ResponseEntity<GenericResponse> getSyncInfo(@PathVariable("accountID") String accountID,
                                                       @RequestParam(value = "deviceID", required = false) String deviceID,
                                                       HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            Map<String, Object> responseMap = fcmService.getFcmSyncInfo(accountID, loggedInContact.getId(), deviceID);
            if((boolean) responseMap.get(FcmHelper.IS_DEVICE_INFO_AVAILABLE)){
                responseMap.remove(FcmHelper.IS_DEVICE_INFO_AVAILABLE);
                response.setSuccess(true);
                response.add(FcmHelper.SYNC_OBJECT_INFO,responseMap);
                return ResponseEntity.status(HttpStatus.OK).body(response);
            }else{
                response.setErrorMessage(responseMap.get(FcmHelper.ERROR_MESSAGE).toString());
                response.setErrorCode(ApiErrorCode.NOT_FOUND);
                response.setSuccess(false);
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(response);
            }
        }catch(Exception e){
            log.info("Exception in syncInfo api :: " + e.getMessage());
            response.setErrorCode(ApiErrorCode.BAD_REQUEST);
            response.setErrorMessage(e.getMessage());
            response.setSuccess(false);
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS})
    @DeleteMapping("/account/{accountID}/fcm/device/{deviceID}")
    public ResponseEntity<GenericResponse> unRegisterDeviceInfo(@PathVariable("accountID") String accountID,
                                                                @PathVariable("deviceID") String deviceID,
                                                                HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            Map<String, Object> responseMap = fcmService.deleteDeviceInfo(accountID, loggedInContact.getId(), deviceID);
            if((boolean) responseMap.get(FcmHelper.IS_DEVICE_UN_REGISTERED)){
                response.setSuccess(true);
                response.add(FcmHelper.UNREGISTER_DEVICE_INFO, responseMap);
                return ResponseEntity.status(HttpStatus.OK).body(response);
            }else{
                response.setSuccess(false);
                response.setErrorMessage(responseMap.get(FcmHelper.ERROR_MESSAGE).toString());
                response.setErrorCode(ApiErrorCode.NOT_FOUND);
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(response);
            }
        }catch(Exception e){
            log.info("Exception in unRegisterDeviceInfo api :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorCode(ApiErrorCode.BAD_REQUEST);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS})
    @PostMapping("/account/{accountID}/fcm/device")
    public ResponseEntity<GenericResponse> registerDevice(@PathVariable("accountID") String accountID,
                                                          @RequestBody String payload,
                                                          HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            Map<String, Object> responseMap = fcmService.registerDeviceInfo(accountID, loggedInContact, payload);
            response.setSuccess(true);
            response.setData(responseMap);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch(Exception e){
            log.info("Exception in registerDevice api :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            response.setErrorCode(ApiErrorCode.BAD_REQUEST);
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

}
