package com.yoco.user.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.user.enums.USER_ERROR_MESSAGE;
import com.yoco.user.service.UserFetchService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class UserFetchApi {

    private final UserFetchService userFetchService = new UserFetchService();

    /**
     * Gives the user information for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping(value = "/account/{accountID}/user/contact/{contactID}")
    public ResponseEntity<GenericResponse> getUser (@PathVariable("accountID") String accountID,
                                                    @PathVariable("contactID") String contactID){
        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> getUserMap = userFetchService.getUser(accountID,contactID);

            if(ObjUtils.isNullOrEmpty(getUserMap)){
                genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.USER_NOT_FOUND.value());
            }else{
                genericResponse.setSuccess(true);
                genericResponse.setData(getUserMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error(" error on getting user: "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    /**
     * Gives the access policy information for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping(value = "/account/{accountID}/contact/{contactID}/accessPolicy")
    public ResponseEntity<GenericResponse> getAccessPolicy (@PathVariable("accountID") String accountID,
                                                            @PathVariable("contactID") String contactID){
        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> getUserAccessPolicyMap = userFetchService.getAccessPolicy(accountID,contactID);

            if(ObjUtils.isNullOrEmpty(getUserAccessPolicyMap)){
                genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.USER_NOT_FOUND.value());

            }else if(getUserAccessPolicyMap.containsKey(AccessManager.ACCESS_POLICY)){
                genericResponse.setSuccess(true);
                genericResponse.setData(getUserAccessPolicyMap);
            }else{
                genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.NO_POLICY_EXISTS.value());
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error(" error on getting user accessPolicy information: " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    /**
     * Gives all users(default active) of an account with contact information
     * @param accountID
     * @QueryParam isDeleted
     * @QueryParam cursor
     * @QueryParam limit
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping(value = "/account/{accountID}/user")
    public ResponseEntity<GenericResponse> getAllUsers (@PathVariable("accountID") String accountID,
                                                        @RequestParam(value = "isDeleted", required = false) Boolean isDeleted,
                                                        @RequestParam(value = "cursor", required = false) String cursor,
                                                        @RequestParam(defaultValue = "500", required = false) int limit){
        var genericResponse  = new GenericResponse();
        try{
            log.info(" isDeleted " + isDeleted + " cursor " + cursor + " limit " + limit);
            Map<String,Object> getUsersMap = userFetchService.getAllUsers(accountID,isDeleted,limit,cursor);

            if(ObjUtils.isNullOrEmpty(getUsersMap)){
                genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.NO_USERS_FOUND.value());
            }else{
                genericResponse.setSuccess(true);
                genericResponse.setData(getUsersMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error(" error on getting users : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }


    /**
     * Gives all active users of an account without contact information
     * @param accountID
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping(value = "/account/{accountID}/users")
    public ResponseEntity<GenericResponse> getAllUsersWithoutContact (@PathVariable("accountID") String accountID){
        var genericResponse  = new GenericResponse();
        try{

            Map<String,Object> getAllUsersMap = userFetchService.getAllUsersWithOutContact(accountID);

            if(ObjUtils.isNullOrEmpty(getAllUsersMap)){
                genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.NO_USERS_FOUND.value());
            }else{
                genericResponse.setSuccess(true);
                genericResponse.setData(getAllUsersMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error(" error on getting all users info : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    /**
     * Gives all recently modified pros
     * @param accountID
     * @QueryParam lastModified - mandatory
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping(value = "/account/{accountID}/recentlyModified")
    public ResponseEntity<GenericResponse> getAllRecentlyModifiedPROs (@PathVariable("accountID") String accountID,
                                                                       @RequestParam(value = "lastModified") long lastModified){
        var genericResponse  = new GenericResponse();
        try{

            Map<String,Object> getRecentUpdatedUsersMap = userFetchService.getRecentlyUpdatedPROs(accountID,lastModified);

            if(ObjUtils.isNullOrEmpty(getRecentUpdatedUsersMap)){
                genericResponse = new GenericResponse(false, null, USER_ERROR_MESSAGE.NO_USERS_FOUND.value());
            }else{
                genericResponse.setSuccess(true);
                genericResponse.setData(getRecentUpdatedUsersMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error(" error on getting all recently modified pros info : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }


    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_IDENTITY, ApiScope.AWAPIS_FULLACCESS})
    @GetMapping(value = "/account/{accountID}/me")
    public ResponseEntity<GenericResponse> getMeInfo (@PathVariable("accountID") String accountID, HttpServletRequest request){
        var genericResponse  = new GenericResponse();
        try{
            var userContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            Map<String,Object> meInfo = userFetchService.getMeInfo(accountID, userContact);
            genericResponse.setSuccess(true);
            genericResponse.setData(meInfo);
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        }catch ( Exception e ){
            log.error(" error on getting me info : " + e.getMessage());
            genericResponse.setSuccess(false);
            genericResponse.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/download/users")
    public ResponseEntity<GenericResponse> getAllUserInAccount(@PathVariable("accountID") String accountID,
                                                               @RequestParam(value = "cursor",required = false, defaultValue = "") String cursor){
        var response = new GenericResponse();
        try{

            Map<String,Object> responseMap = userFetchService.getAllUsersInAccount(accountID, cursor);

            if(ObjUtils.isNullOrEmpty(responseMap)){
                response = new GenericResponse(false, null, USER_ERROR_MESSAGE.NO_USERS_FOUND.value());
            }else{
                response.setSuccess(true);
                response.setData(responseMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(response);

        }catch(Exception e){
            log.info("Exception while downloading staff info :: " + e.getMessage());
            response = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }
}