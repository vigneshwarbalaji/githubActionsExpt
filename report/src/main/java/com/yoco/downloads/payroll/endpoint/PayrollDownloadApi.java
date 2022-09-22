package com.yoco.downloads.payroll.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.downloads.payroll.service.PayrollDownloadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Slf4j
@RestController
@RequestMapping("/v2")
public class PayrollDownloadApi {

    private PayrollDownloadService payrollDownloadService = new PayrollDownloadService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_IDENTITY})
    @GetMapping(value = "/account/{accountID}/payroll/download")
    public ResponseEntity<GenericResponse> downloadPayrollEvents (@PathVariable("accountID") String accountID,
                                                                  @RequestParam String status,
                                                                  @RequestParam String zone,
                                                                  @RequestParam(value = "from", required = false, defaultValue = "") String from,
                                                                  @RequestParam(value = "to", required = false, defaultValue = "") String to,
                                                                  @RequestParam(value = "cursor", required = false, defaultValue = "") String cursor){
        var genericResponse  = new GenericResponse();
        try{
            genericResponse.setSuccess(true);
            genericResponse.setData(payrollDownloadService.downloadPayrollEvents(accountID,zone,status,from,to,cursor));
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        }catch (Exception e){
            log.error(" error on downloadPayrollEvents : " + e.getMessage());
            genericResponse.setSuccess(false);
            genericResponse.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

}
