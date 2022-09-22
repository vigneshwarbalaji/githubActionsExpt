package com.yoco.user.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.user.service.UserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class UserApi {

    private final UserService userService = new UserService();

    /**
     * Updates role,NFC,Employee Id for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping(value = "/account/{accountID}/user/{contactID}")
    public ResponseEntity<GenericResponse> updateProProfile (@PathVariable("accountID") String accountID,
                                                             @PathVariable("contactID") String contactID,
                                                             @RequestBody String payload){
        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> updateProProfileMap = userService.updateProfilePRO(accountID,contactID,payload);

            if(ObjUtils.isNullOrEmpty(updateProProfileMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.FAILED_TO_PERSIST.value());
            }else{
                genericResponse.setSuccess(true);
                genericResponse.setData(updateProProfileMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error(" error on updating pro : "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/staff/download")
    public ResponseEntity<GenericResponse> downloadStaffData(@PathVariable("accountID") String accountID,
                                                             @RequestParam(required = false) String status,
                                                             @RequestParam(required = false) String outputFields,
                                                             HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var requesterContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            response.setData(userService.downloadStaffData(requesterContact.getId(),accountID,status,outputFields));
            response.setSuccess(true);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch(Exception e){
            log.info("Exception while downloading staff info :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }
}
