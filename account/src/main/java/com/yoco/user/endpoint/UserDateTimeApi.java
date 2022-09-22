package com.yoco.user.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.user.service.UserDateTimeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;

@Slf4j
@RestController
@RequestMapping("/v2")
public class UserDateTimeApi {

    private final UserDateTimeService userDateTimeService = new UserDateTimeService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/contact/{contactID}/range/{range}")
    public ResponseEntity<GenericResponse> getRangeInfo(@PathVariable("accountID") String accountID,
                                                        @PathVariable("contactID") String contactID,
                                                        @PathVariable("range") String range,
                                                        @RequestParam(required = false) String fromDate,
                                                        @RequestParam(required = false) String toDate){
        var genericResponse  = new GenericResponse();
        try{
            genericResponse.setSuccess(true);
            genericResponse.add("range",userDateTimeService.getRangeInfo(accountID,contactID,range,fromDate,toDate));
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        }catch(Exception e){
            log.error(" error on getting range info : " + e.getMessage());
            genericResponse.setSuccess(false);
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/timezone/dst")
    public ResponseEntity<GenericResponse> isDSTorNot (@PathVariable("accountID") String accountID,
                                                       HttpServletRequest req){
        var genericResponse  = new GenericResponse();

        try{

            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);

            var resp = userDateTimeService.checkDSTorNot(accountID, loggedInContact.getId());

            genericResponse.setData(resp);
            genericResponse.setSuccess(true);

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch(Exception e){
            log.error(" error on checking dst or not : " + e.getMessage());
            genericResponse.setSuccess(false);
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

}
