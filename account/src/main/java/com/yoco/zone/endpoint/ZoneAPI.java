package com.yoco.zone.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.error.ApiErrorCode;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.TimeZoneUtil;
import com.yoco.zone.enums.ZONE_ERROR_MESSAGE;
import com.yoco.zone.service.ZoneService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class ZoneAPI {

    private final  ZoneService objZoneService = new ZoneService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/zones")
    public ResponseEntity<GenericResponse> getTimeZoneList (){

        var genericResponse  = new GenericResponse();

        try{

            Map<String, Object> responseMap = TimeZoneUtil.getAllZoneIds();

            genericResponse.setSuccess(true);
            genericResponse.add("zones", responseMap);

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch(Exception e){
            log.error(" error on getting timezone list : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/autoDetect")
    public ResponseEntity<GenericResponse> autoDetect (@RequestParam String sysOffset){

        var genericResponse  = new GenericResponse();

        try{

            String response = objZoneService.getAutoDetectedZoneSevice( sysOffset );

            genericResponse.setSuccess(true);
            genericResponse.add("zone", response);

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch(Exception e){
            log.error(" error on auto detect : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/dates")
    public ResponseEntity<GenericResponse> getDates (@RequestParam(value = "timeZoneID", required = false) String timeZoneID,
                                                     @RequestParam(value = "range", required = false) String range,
                                                     @RequestParam(value = "fromDate", required = false) String fromDate,
                                                     @RequestParam(value = "toDate", required = false) String toDate,
                                                     @RequestParam(value = "order", required = false) String order,
                                                     @RequestParam(value = "weekStartDay", required = false) String weekStartDay){

        var genericResponse  = new GenericResponse();

        try{

            var responseMap = objZoneService.getDates(timeZoneID, range, fromDate, toDate, order, weekStartDay);

            if ((boolean)responseMap.get("success")) {
                genericResponse.setSuccess(true);
                genericResponse.add("dates",responseMap.get("datesList"));
                return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
            } else {
                genericResponse = new GenericResponse(false, ApiErrorCode.BAD_REQUEST, ZONE_ERROR_MESSAGE.ERROR_ON_GETTING_DATES.value());
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
            }

        }catch(Exception e){
            log.error(" error on getting dates : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping("/account/{accountID}/user/timezone")
    public ResponseEntity<GenericResponse> updateTimezone (@PathVariable("accountID") String  accountID, @RequestBody String payload, HttpServletRequest req){

        var genericResponse  = new GenericResponse();

        try{
            var requesterContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            var srcClientId = HeaderUtil.extractClientIdFromRequest(req);

            var responseMap = objZoneService.updateTimezone(accountID, requesterContact.getId(), payload, srcClientId);

            if (!ObjUtils.isNullOrEmpty(responseMap)) {
                genericResponse.setSuccess(true);
                genericResponse.add("startTimeOfDay",responseMap.get("startTimeOfDay"));
                genericResponse.add("user", responseMap.get("user"));
                return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
            } else {
                genericResponse = new GenericResponse(false, ApiErrorCode.BAD_REQUEST, "Time-zone update failed");
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
            }

        }catch(Exception e){
            log.error(" error on updating timezone : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

}
