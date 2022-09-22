package com.yoco.activity.endpoint;

import com.yoco.activity.enums.ACTIVITY_ERROR_RESPONSE;
import com.yoco.activity.service.ActivityService;
import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
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
public class ActivityApi {

    private ActivityService objActivityService = new ActivityService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PostMapping("/account/{accountID}/activity")
    public ResponseEntity<GenericResponse> createActivity (HttpServletRequest req, @PathVariable("accountID") String accountID, @RequestBody String payload){

        var genericResponse  = new GenericResponse();

        try{

            var contact = AnnotationHelper.extractCurrentUserContactFromRequest(req);

             Map<String, Object> responseMap = objActivityService.createActivity(accountID, contact, payload);

            if(ObjUtils.isNullOrEmpty(responseMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.UNABLE_TO_CREATE.value());
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
            } else {
                genericResponse.setSuccess(true);
                genericResponse.setData(responseMap);
                return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
            }

        }catch(Exception e){
            log.error(" error on creating activity : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/contact/{contactID}/activity")
    public ResponseEntity<GenericResponse> getActivities (@PathVariable("accountID") String accountID,
                                                          @PathVariable("contactID") String contactID,
                                                          @RequestParam String from,
                                                          @RequestParam(required = false) String to,
                                                          @RequestParam String timeZoneID,
                                                          @RequestParam(required = false) String cursor,
                                                          @RequestParam(required = false) String isPrevious){

        var genericResponse  = new GenericResponse();

        try{

            Map<String, Object> responseMap = objActivityService.getActivities(accountID, contactID, from, to, timeZoneID, cursor, isPrevious);

            if(ObjUtils.isNullOrEmpty(responseMap)){
                genericResponse = new GenericResponse(false, null, ACTIVITY_ERROR_RESPONSE.ACTIVITIES_NOT_FOUND.value());
            } else {
                genericResponse.setSuccess(true);
                genericResponse.add("activities", responseMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch(Exception e){
            log.error(" error on getting activity : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }
}
