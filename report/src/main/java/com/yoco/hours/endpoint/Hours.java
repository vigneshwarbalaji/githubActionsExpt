package com.yoco.hours.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import com.yoco.hours.service.HourService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class Hours {

    private HourService objHourService = new HourService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_CLOCK_READ, ApiScope.AWAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/clock/active")
    public ResponseEntity<GenericResponse> getActiveEntry(@PathVariable("accountID") String accountID,
                                          HttpServletRequest req, @RequestParam(defaultValue = "") String contactID){
        var genericResponse  = new GenericResponse();
        try{
            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);

            Map<String, Object> responseMap = objHourService.getActiveEntry(accountID, loggedInUserContact, contactID);
            if( !ObjUtils.isNullOrEmpty( responseMap ) ){
                genericResponse.setSuccess( Boolean.TRUE );
                genericResponse.setData(responseMap);
            }else{
                genericResponse = new GenericResponse( Boolean.FALSE, null, EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value());
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        }catch(Exception e){
            genericResponse = new GenericResponse(false, null, e.getMessage());
            log.error(" error on getting active entry : " + e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_CLOCK_READ, ApiScope.AWAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/timer")
    public ResponseEntity<GenericResponse> timerInfo(@PathVariable("accountID") String accountID,
                                                          HttpServletRequest req){
        var genericResponse  = new GenericResponse();
        try{
            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            var userAgent = HeaderUtil.isMobileUserAgent(req);

            Map<String, Object> responseMap = objHourService.getTimerInfo(accountID, loggedInUserContact, userAgent);
            if (!ObjUtils.isNullOrEmpty(responseMap)) {

                genericResponse.setSuccess( Boolean.TRUE );
                if(ObjUtils.isNull(responseMap.get(SchedulingKeys.ENTRIES))) {
                    genericResponse.add(SchedulingKeys.ENTRIES, new ArrayList<>());
                } else {
                    genericResponse.add(SchedulingKeys.ENTRIES, responseMap.get(SchedulingKeys.ENTRIES));
                }
                genericResponse.add("recentlyClockedInProjectInfo", responseMap.get("last_ClockedIn_Prj_Info"));

            }else{
                genericResponse.setSuccess( Boolean.TRUE );
                genericResponse.add(SchedulingKeys.ENTRIES,new ArrayList<>());
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        }catch(Exception e){
            log.error(" error on getting timer : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS, ApiScope.YOCOAPIS_CLOCK_READ, ApiScope.AWAPIS_FULLACCESS})
    @PutMapping("/account/{accountID}/entry/{id}")
    public ResponseEntity<GenericResponse> updateEntry (@PathVariable("accountID") String accountID,
                                                        @PathVariable("id") String entryID,
                                                        HttpServletRequest req,
                                                        @RequestBody String payload) {

       var genericResponse = new GenericResponse();

        try{
            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            var clientID = HeaderUtil.extractClientIdFromRequest(req);

            log.info("This is here");
            Map<String, Object> responseMap = objHourService.updateEntry(accountID, entryID, loggedInUserContact, JsonUtil.convertJsonToMap(payload), clientID);

            if( ObjUtils.isNullOrEmpty( responseMap ) ){
                genericResponse = new GenericResponse( Boolean.FALSE, null, EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value());
            }else{
                genericResponse.setSuccess( Boolean.TRUE );
                genericResponse.setData(responseMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        }catch(Exception e){
            log.error(" error on updating entry : " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS, ApiScope.YOCOAPIS_CLOCK_READ, ApiScope.AWAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/dates")
    public ResponseEntity<GenericResponse> datesInfo (@PathVariable("accountID") String accountID,
                                                      @RequestParam String range,
                                                      @RequestParam(value = "from", required = false, defaultValue = "") String from,
                                                      @RequestParam(value = "to", required = false, defaultValue = "") String to,
                                                      HttpServletRequest req
                                                    ) {

        var genericResponse = new GenericResponse();

        try{

            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);

            Map<String, Object> responseMap = objHourService.getDatesForHoursPage(accountID, loggedInUserContact, range, from, to);

            genericResponse.setSuccess( Boolean.TRUE );
            genericResponse.setData(responseMap);
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch(Exception e){

            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS, ApiScope.YOCOAPIS_CLOCK_READ, ApiScope.AWAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/reports/all")
    public ResponseEntity<GenericResponse> getAll (@PathVariable("accountID") String accountID,
                                                   HttpServletRequest req,
                                                   @RequestParam String range,
                                                   @RequestParam(defaultValue = "") String zone,
                                                   @RequestParam(defaultValue = "") String contactID,
                                                   @RequestParam(defaultValue = "") String projectID,
                                                   @RequestParam(defaultValue = "") String from,
                                                   @RequestParam(defaultValue = "") String to) {

        var genericResponse = new GenericResponse();

        try{

            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);

            Map<String, String> payload = new HashMap<>();
            payload.put("contactID", contactID);
            payload.put("projectID", projectID);
            payload.put("zone", zone);
            payload.put("from", from);
            payload.put("to", to);
            payload.put("range", range);

            Map<String, Object> responseMap = objHourService.getAll(accountID, loggedInUserContact, payload);

            if( ObjUtils.isNullOrEmpty( responseMap ) ){
                genericResponse = new GenericResponse(Boolean.FALSE, null, EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value());
            }else{
                genericResponse.setSuccess( Boolean.TRUE );
                genericResponse.setData(responseMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch(Exception e){

            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULL_ACCESS, ApiScope.YOCOAPIS_CLOCK_READ_WRITE})
    @DeleteMapping("/account/{accountID}/entry/{entryID}")
    public ResponseEntity<GenericResponse> deleteEntry(@PathVariable String accountID,
                                                       @PathVariable String entryID,
                                                       @RequestParam(defaultValue = "") String timeZone,
                                                       HttpServletRequest request){
        var response = new GenericResponse();
        try{
            response.setData(objHourService.deleteEntry(accountID,entryID,AnnotationHelper.extractCurrentUserContactFromRequest(request), timeZone));
            response.setSuccess(true);
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch (Exception e){
            log.info("Exception while deleting entry :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

}
