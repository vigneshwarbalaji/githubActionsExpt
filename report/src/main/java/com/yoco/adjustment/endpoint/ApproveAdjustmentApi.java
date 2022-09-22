package com.yoco.adjustment.endpoint;

import com.yoco.adjustment.service.ApproveAdjustmentService;
import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class ApproveAdjustmentApi {

    private ApproveAdjustmentService approveAdjustmentService = new ApproveAdjustmentService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS, ApiScope.YOCOAPIS_REPORTS_READ_WRITE})
    @PutMapping("/account/{accountID}/entry/{entryID}/approve")
    public ResponseEntity<GenericResponse> approveAdjustment(@PathVariable("accountID") String accountID,
                                                              @PathVariable("entryID") String entryID,
                                                              HttpServletRequest req) {
        var genericResponse =  new GenericResponse();
        try {
            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            genericResponse.setSuccess(approveAdjustmentService.approveAdjustment(accountID,entryID,loggedInUserContact));
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info(" exception in approve adjustment " + e.getMessage());
            genericResponse = new GenericResponse(Boolean.FALSE, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS, ApiScope.YOCOAPIS_REPORTS_READ_WRITE})
    @PutMapping("/account/{accountID}/entry/{entryID}/edit-approve")
    public ResponseEntity<GenericResponse> editApproveAdjustment(@PathVariable("accountID") String accountID,
                                                                 @PathVariable("entryID") String entryID,
                                                                 @RequestBody String payload,
                                                                 HttpServletRequest req) {
        var genericResponse =  new GenericResponse();
        try {
            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            Map<String,Object> response = approveAdjustmentService.editApproveAdjustment(accountID,entryID,payload,loggedInUserContact);
            if(ObjUtils.isNullOrEmpty(response)){
                genericResponse.setSuccess(false);
                genericResponse.setErrorMessage(COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                genericResponse.setSuccess(!response.containsKey(Commons.MESSAGE));
                genericResponse.setData(response);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info(" exception in edit approve adjustment " + e.getMessage());
            genericResponse = new GenericResponse(Boolean.FALSE, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

}
