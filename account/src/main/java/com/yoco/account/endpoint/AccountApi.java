package com.yoco.account.endpoint;

import com.yoco.account.service.AccountService;
import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.constants.CommonConstants;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;

@Slf4j
@RestController
@RequestMapping("/v2")
public class AccountApi {
    AccountService accountService = new AccountService();
    public static final String ACCOUNT_KEY = "account";

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny={ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_ACC_READ})
    @GetMapping("/account/{accountID}")
    public ResponseEntity<GenericResponse> getAccount(@PathVariable String accountID, HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var requesterContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            response.add(ACCOUNT_KEY,accountService.getAccount(accountID,requesterContact.getId()));
            response.setSuccess(true);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch (Exception e){
            return handleExceptionAndReturnResponse(response, e);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny={ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_ACC_READ})
    @GetMapping("/contact/{contactID}/account")
    public ResponseEntity<GenericResponse> getAllAccountsForUser(@PathVariable String contactID, HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var requesterContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            response.add(ACCOUNT_KEY + "s", accountService.getAllAccountsForUser(contactID,requesterContact.getId()));
            response.setSuccess(true);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch (Exception e){
            return handleExceptionAndReturnResponse(response, e);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny={ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_ACC_READ})
    @GetMapping("/contact/{contactID}/account/default/user")
    public ResponseEntity<GenericResponse> getDefaultAccountProForUser(@PathVariable String contactID, HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var accessToken = AnnotationHelper.extractCurrentUserAccessTokenFromRequest(request);
            response.add(CommonConstants.USER_KEY, accountService.getDefaultAccountProForUser(contactID,accessToken));
            response.setSuccess(true);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch (Exception e){
            return handleExceptionAndReturnResponse(response, e);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny={ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_ACC_READ})
    @PutMapping("/contact/{contactID}/account/default/{accountID}")
    public ResponseEntity<GenericResponse> setDefaultAccountForUser(@PathVariable String contactID,@PathVariable String accountID, HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var requesterContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            response.add(CommonConstants.USER_KEY, accountService.setDefaultAccountForUser(contactID,requesterContact.getId(),accountID));
            response.setSuccess(true);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch (Exception e){
            return handleExceptionAndReturnResponse(response, e);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny={ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_ACC_READ})
    @DeleteMapping("/account/{accountID}")
    public ResponseEntity<GenericResponse> deleteAccount(@PathVariable String accountID, HttpServletRequest request, @RequestBody String payload){
        var response = new GenericResponse();
        try{
            var requesterContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            response.add(CommonConstants.ACCOUNT_KEY, accountService.deleteAccount(requesterContact,accountID,payload));
            response.setSuccess(true);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch (Exception e){
            return handleExceptionAndReturnResponse(response, e);
        }
    }

    @PostMapping("/account")
    public ResponseEntity<GenericResponse> createAccount(HttpServletRequest request, @RequestBody String payload){
        var response = new GenericResponse();
        try{
            response.setData(accountService.createAccount(request,payload));
            response.setSuccess(true);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch (Exception e){
            return handleExceptionAndReturnResponse(response, e);
        }
    }

    @PostMapping("/account/one_tap")
    public ResponseEntity<GenericResponse> processOneTapRequest(HttpServletRequest request, @RequestBody String payload){
        var response = new GenericResponse();
        try{
            response.setData(accountService.processOneTapRequest(request,payload));
            response.setSuccess(true);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch (Exception e){
            return handleExceptionAndReturnResponse(response, e);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny={ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_ACC_READ_WRITE})
    @PutMapping("/account/{accountID}")
    public ResponseEntity<GenericResponse> updateAccount(HttpServletRequest request,@PathVariable String accountID, @RequestBody String payload){
        var response = new GenericResponse();
        try{
            response.setData(accountService.updateAccount(AnnotationHelper.extractCurrentUserContactFromRequest(request),accountID,payload));
            response.setSuccess(true);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch (Exception e){
            return handleExceptionAndReturnResponse(response, e);
        }
    }

    private ResponseEntity<GenericResponse> handleExceptionAndReturnResponse(GenericResponse response, Exception e) {
        log.info("Exception :: " + e.getMessage());
        response.setSuccess(false);
        response.setErrorMessage(e.getMessage());
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
    }
}
