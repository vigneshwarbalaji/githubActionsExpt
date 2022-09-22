package com.yoco.user.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.constants.CommonConstants;
import com.yoco.user.service.UserStaffService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class UserStaffApi {

    private final UserStaffService userStaffService = UserStaffService.getInstance();

    /**
     * This endpoint adds user in an account
     * @param accountID
     * @requestBody payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PostMapping(value = "/account/{accountID}/user/create")
    public ResponseEntity<GenericResponse> createUser (@PathVariable("accountID") String accountID,
                                                       @RequestBody String payload,
                                                       HttpServletRequest request){
        var genericResponse  = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);

            Map<String,Object> createdUserMap = userStaffService.createUser(accountID,loggedInContact,payload,true);

            if(ObjUtils.isNullOrEmpty(createdUserMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.FAILED_TO_PERSIST.value());
            }else if(Boolean.TRUE.equals(createdUserMap.get(Commons.SUCCESS))){
                genericResponse.setSuccess(true);
                genericResponse.setData(createdUserMap);
                return ResponseEntity.status(HttpStatus.CREATED).body(genericResponse);
            }else{
                genericResponse = new GenericResponse(false, null,(String) createdUserMap.get(Commons.ERROR_RESPONSE));
                if(createdUserMap.containsKey(CommonConstants.USER_KEY)){
                    genericResponse.add(CommonConstants.USER_KEY, createdUserMap.get(CommonConstants.USER_KEY));
                }
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error(" error on creating user : "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }


    /**
     * Creates a pro entry if given contact exists
     * @param accountID
     * @param contactID
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PostMapping(value = "/account/{accountID}/contact/{contactID}/createPro")
    public ResponseEntity<GenericResponse> createUserPRO (@PathVariable("accountID") String accountID,
                                                          @PathVariable("contactID") String contactID){
        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> proMap = userStaffService.createUserPRO(accountID,contactID);

            if(ObjUtils.isNullOrEmpty(proMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.FAILED_TO_PERSIST.value());
            }else{
                genericResponse.setSuccess(true);
                genericResponse.setData(proMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error(" error on creating pro : "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    /**
     * This endpoint imports users in an account
     * @param accountID
     * @requestBody payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PostMapping(value = "/account/{accountID}/users/import")
    public ResponseEntity<GenericResponse> importUsers (@PathVariable("accountID") String accountID,
                                                       @RequestBody String payload,
                                                       HttpServletRequest request){
        var genericResponse  = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);

            Map<String,Object> importedUserMap = userStaffService.importUser(accountID,loggedInContact,payload);

            if(ObjUtils.isNullOrEmpty(importedUserMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.FAILED_TO_PERSIST.value());
            }else {
                genericResponse.setSuccess(true);
                genericResponse.setData(importedUserMap);
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error(" error on importing users : "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    /**
     * This endpoint activates user in an account
     * @param accountID
     * @requestBody payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping(value = "/account/{accountID}/contact/{contactID}/user/activate")
    public ResponseEntity<GenericResponse> activateUser (@PathVariable("accountID") String accountID,
                                                         @PathVariable("contactID") String contactID,
                                                        HttpServletRequest request){
        var genericResponse  = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);

            Map<String,Object> activateUserMap = userStaffService.activateUser(accountID,contactID,loggedInContact);

            if(ObjUtils.isNullOrEmpty(activateUserMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());
            }else {
                genericResponse.setSuccess(true);
                genericResponse.setData(activateUserMap);
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error(" error on activate user : "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @DeleteMapping(value = "/account/{accountID}/contact/{contactID}")
    public ResponseEntity<GenericResponse> deleteUser (@PathVariable("accountID") String accountID,
                                                       @PathVariable("contactID") String contactID,
                                                       HttpServletRequest request){
        var genericResponse  = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            genericResponse.setSuccess(true);
            genericResponse.setData(userStaffService.deleteUser(accountID,contactID,loggedInContact.getId()));
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        }catch ( Exception e ){
            log.error(" error on deleting user : "+e.getMessage());
            genericResponse.setSuccess(false);
            genericResponse.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

}
