package com.yoco.payroll.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import com.yoco.payroll.service.PayrollService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class PayrollApi {

    private PayrollService payrollService = new PayrollService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS})
    @GetMapping("/account/{accountID}/entries/user-confirmed")
    public ResponseEntity<GenericResponse> getUserConfirmedEntries(@PathVariable("accountID") String accountID,
                                                                   @RequestParam(defaultValue = "") String range,
                                                                   @RequestParam(defaultValue = "") String from,
                                                                   @RequestParam(defaultValue = "") String to,
                                                                   @RequestParam(value = "cursor", required = false, defaultValue = "") String cursor,
                                                                   HttpServletRequest req){
        var genericResponse =  new GenericResponse();
        try{
            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            Map<String, Object>responseMap = payrollService.getUserAcknowledgedEntries(accountID, loggedInUserContact, range, from, to, cursor);

            if(ObjUtils.isNullOrEmpty(responseMap)){
                genericResponse = new GenericResponse(Boolean.FALSE, null, EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value());
            }else{
                genericResponse.setData(responseMap);
                genericResponse.setSuccess(Boolean.TRUE);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch(Exception exception){
            genericResponse = new GenericResponse(false, null, exception.getMessage());
            log.info("Exception in get user confirmed entries Api :"+ exception.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }


    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS})
    @GetMapping("/account/{accountID}/entries/admin-updated")
    public ResponseEntity<GenericResponse> getAdminUpdatedEntries(@PathVariable("accountID") String accountID,
                                                                   @RequestParam(defaultValue = "") String range,
                                                                   @RequestParam(defaultValue = "") String from,
                                                                   @RequestParam(defaultValue = "") String to,
                                                                   HttpServletRequest req){
        var genericResponse =  new GenericResponse();
        try{
            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            Map<String, Object>responseMap = payrollService.getAdminUpdatedEntries(accountID, loggedInUserContact, range, from, to);

            if(ObjUtils.isNullOrEmpty(responseMap)){
                genericResponse = new GenericResponse(Boolean.FALSE, null, EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value());
            }else{
                genericResponse.setSuccess(Boolean.TRUE);
                genericResponse.setData(responseMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        }catch (Exception exception){
            log.info("Exception in get admin updated entries api :"+ exception.getMessage());
            genericResponse = new GenericResponse(false, null, exception.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS})
    @PutMapping("/account/{accountID}/contact/{contactID}/payroll/approve")
    public ResponseEntity<GenericResponse> approvePayroll (@PathVariable("accountID") String accountID,
                                                           @PathVariable("contactID") String contactID,
                                                           @RequestBody String payload,
                                                           HttpServletRequest req) {
        var response = new GenericResponse();
        try{
            var currentUserContactFromRequest = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            Map<String, Object> approvedEventsResp = payrollService.approvePayrollEvents(accountID,contactID,currentUserContactFromRequest,payload);
            if(ObjUtils.isNullOrEmpty(approvedEventsResp)) {
                response.setSuccess(Boolean.FALSE);
                response.setErrorMessage("No entries were modified");
            }else{
                response.setSuccess(Boolean.TRUE);
                response.setData(approvedEventsResp);
            }
            return ResponseEntity.status(HttpStatus.OK).body(response);
        } catch (Exception e){
            log.error(" error on updating entry : " + e.getMessage());
            response = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS})
    @PutMapping("/account/{accountID}/contact/{contactID}/entry/{entryID}/user-verify")
    public ResponseEntity<GenericResponse> updateUserVerifiedEntries(@PathVariable("accountID") String accountID,
                                                                     @PathVariable("contactID") String contactID,
                                                                     @PathVariable("entryID") String entryID,
                                                                     HttpServletRequest req){
        var genericResponse =  new GenericResponse();
        try{
            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            Map<String, Object>responseMap = payrollService.updateUserVerifiedEntries(accountID, contactID, entryID, loggedInUserContact);

            if(ObjUtils.isNullOrEmpty(responseMap)){
                genericResponse.setSuccess(Boolean.FALSE);
                genericResponse.setErrorMessage(EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value());
            }else{
                genericResponse.setSuccess(Boolean.TRUE);
                genericResponse.setData(responseMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        }catch (Exception exception){
            log.info("Exception in update user verified entries api :"+ exception.getMessage());
            genericResponse = new GenericResponse(false, null, exception.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS,ApiScope.YOCOAPIS_REPORTS_READ_WRITE})
    @PutMapping("/account/{accountID}/contact/{contactID}/entry/{entryID}/admin-update")
    public ResponseEntity<GenericResponse> adminUpdateEntry(@PathVariable("accountID") String accountID,
                                                            @PathVariable("contactID") String contactID,
                                                            @PathVariable("entryID") String entryID,
                                                            @RequestBody String payload,
                                                            HttpServletRequest request){
        var response = new GenericResponse();
        try{
            response = payrollService.adminUpdateEntry(accountID,contactID,entryID,payload,AnnotationHelper.extractCurrentUserContactFromRequest(request));
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch (Exception e){
            log.info("Exception while admin updating entry :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }
}
