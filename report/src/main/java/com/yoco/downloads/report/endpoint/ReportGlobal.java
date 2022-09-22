package com.yoco.downloads.report.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.downloads.report.service.ReportDownloadService;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
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
public class ReportGlobal {

    ReportDownloadService objReportDownload = new ReportDownloadService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_IDENTITY})
    @GetMapping(value = "/account/{accountID}/report/download")
    public ResponseEntity<GenericResponse> downloadGlobalreports (@PathVariable("accountID") String accountID,
                                                                  @RequestParam(value = "range") String range,
                                                                  @RequestParam(value = "from", required = false, defaultValue = "") String from,
                                                                  @RequestParam(value = "to", required = false, defaultValue = "") String to,
                                                                  @RequestParam(value = "cursor", required = false, defaultValue = "") String cursor,
                                                                  HttpServletRequest req) {

        Map<String, Object> payload = new HashMap<>();
        payload.put("range", range);
        payload.put("from", from);
        payload.put("to", to);

        var genericResponse  = new GenericResponse();

        try {
            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            var  responseMap = objReportDownload.downloadGlobal(accountID, loggedInUserContact, payload, cursor);

            if(ObjUtils.isNullOrEmpty(responseMap)){
                genericResponse = new GenericResponse(Boolean.FALSE, null, EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value());
            }else {
                genericResponse.setSuccess(true);
                genericResponse.setData(responseMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        } catch (Exception e){
            log.error(" error on downloadReportEvents : " + e.getMessage());
            genericResponse.setSuccess(false);
            genericResponse.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }



}
