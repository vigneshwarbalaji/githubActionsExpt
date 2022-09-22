package com.yoco.downloads.adjustment.endpoint;

import com.yoco.adjustment.modal.GetAdjustmentPayloadDTO;
import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.downloads.adjustment.service.AdjustmentDownloadService;
import com.yoco.downloads.util.DownloadUtil;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class AdjustmentDownloadApi {

    private AdjustmentDownloadService adjustmentDownloadService = new AdjustmentDownloadService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS})
    @GetMapping("/account/{accountID}/adjustments/pdf")
    public ResponseEntity<GenericResponse> downloadPdf(@PathVariable("accountID") String accountID,
                                                       @RequestParam(defaultValue = "") String range,
                                                       @RequestParam(defaultValue = "") String from,
                                                       @RequestParam(defaultValue = "") String to,
                                                       @RequestParam("type") String status,
                                                       HttpServletResponse res,
                                                       HttpServletRequest req) {
        var genericResponse =  new GenericResponse();
        try{
            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            var getAdjustmentPayloadDTO = new GetAdjustmentPayloadDTO("", range, from, to, "", loggedInUserContact);
            Map<String, Object> responseMap = adjustmentDownloadService.downloadAdjustmentsPdf(accountID, status, getAdjustmentPayloadDTO);
            if(ObjUtils.isNullOrEmpty(responseMap)){
                genericResponse = new GenericResponse(Boolean.FALSE, null, EVENTS_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                genericResponse.setSuccess(Boolean.TRUE);
                DownloadUtil.setHeadersAndContentTypeForThePdfResponse(responseMap, res, req);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        }catch (Exception exception){
            genericResponse = new GenericResponse(false, null, exception.getMessage());
            log.info("Exception in download pdf Api :"+ exception.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }
}
