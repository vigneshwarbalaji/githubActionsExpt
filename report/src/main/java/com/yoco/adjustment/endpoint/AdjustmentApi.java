package com.yoco.adjustment.endpoint;

import com.yoco.adjustment.modal.GetAdjustmentPayloadDTO;
import com.yoco.adjustment.service.AdjustmentService;
import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class AdjustmentApi {

    private AdjustmentService adjustmentService = new AdjustmentService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS})
    @GetMapping("/account/{accountID}/adjustments")
    public ResponseEntity<GenericResponse> getAdjustments(@PathVariable("accountID") String accountID,
                                                             @RequestParam(defaultValue = "") String contactID,
                                                             @RequestParam(defaultValue = "") String range,
                                                             @RequestParam(defaultValue = "") String from,
                                                             @RequestParam(defaultValue = "") String to,
                                                             @RequestParam("type") String status,
                                                             @RequestParam(defaultValue = "") String timeZone,
                                                             @RequestParam(value = "isThisRequestFromHoursPage", required = false, defaultValue = "") String isThisRequestFromHoursPage,
                                                             @RequestParam(value = "cursor", required = false, defaultValue = "") String cursor,
                                                             HttpServletRequest req) {
        var genericResponse =  new GenericResponse();
        try{
            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            var getAdjustmentPayloadDTO = new GetAdjustmentPayloadDTO(contactID, range, from, to, timeZone, loggedInUserContact);
            Map<String, Object> responseMap = adjustmentService.getAdjustments(accountID, status, getAdjustmentPayloadDTO, cursor, isThisRequestFromHoursPage);
            if(ObjUtils.isNullOrEmpty(responseMap)){
                genericResponse = new GenericResponse(Boolean.FALSE, null, EVENTS_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                genericResponse.setSuccess(Boolean.TRUE);
                genericResponse.setData(responseMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        }catch (Exception exception){
            genericResponse = new GenericResponse(false, null, exception.getMessage());
            log.info("Exception in getAdjustments Api :"+ exception.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS})
    @DeleteMapping("/account/{accountID}/adjustment/{entryID}")
    public ResponseEntity<GenericResponse> deleteAdjustment(@PathVariable String accountID,
                                                            @PathVariable String entryID,
                                                            @RequestParam(defaultValue = "false") boolean deleteSubsets,
                                                            @RequestParam(defaultValue = "") String timezone,
                                                            @RequestParam(defaultValue = "dd-MMM-yyyy hh:mm:ss a") String dateFormat,
                                                            HttpServletRequest request){
        var response = new GenericResponse();
        try{
            response = adjustmentService.deleteAdjustment(accountID,entryID,deleteSubsets,AnnotationHelper.extractCurrentUserContactFromRequest(request),timezone,dateFormat);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch (Exception e){
            log.info("Exception while deleting adjustment :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS})
    @PutMapping("/account/{accountID}/adjustment/{entryID}/reject")
    public ResponseEntity<GenericResponse> rejectAdjustment(@PathVariable String accountID,
                                                            @PathVariable String entryID,
                                                            @RequestParam(defaultValue = "false") boolean rejectSubsets,
                                                            @RequestParam(defaultValue = "") String timezone,
                                                            @RequestParam(defaultValue = "dd-MMM-yyyy hh:mm:ss a") String dateFormat,
                                                            @RequestBody String payload,
                                                            HttpServletRequest request){
        var response = new GenericResponse();
        try{
            response = adjustmentService.rejectAdjustment(accountID,entryID,rejectSubsets,AnnotationHelper.extractCurrentUserContactFromRequest(request),timezone,dateFormat,payload);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch (Exception e){
            log.info("Exception while rejecting adjustment :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }
}

