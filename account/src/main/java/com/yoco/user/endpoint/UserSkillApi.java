package com.yoco.user.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.user.service.UserSkillService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class UserSkillApi {

    private UserSkillService userSkillService = new UserSkillService();

    /**
     * Updates the user weeklySkill information for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @param payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping(value = "/account/{accountID}/contact/{contactID}/skillset/weeklyMail")
    public ResponseEntity<GenericResponse> modifyWeeklySkill (@PathVariable("accountID") String accountID,
                                                              @PathVariable("contactID") String contactID,
                                                              @RequestBody String payload){
        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> modifyWeeklySkillMap = userSkillService.modifyWeeklySkill(accountID,contactID,payload);

            if(ObjUtils.isNullOrEmpty(modifyWeeklySkillMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                if(Boolean.FALSE.equals(modifyWeeklySkillMap.get(Commons.SUCCESS))){
                    genericResponse = new GenericResponse(false, null, modifyWeeklySkillMap.get(Commons.MESSAGE).toString());
                }else{
                    genericResponse.setSuccess(true);
                    genericResponse.setData(modifyWeeklySkillMap);
                }
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error("error on modifying weekly report mail skillSet :"+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    /**
     * Updates the user clock skill information for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @param payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping(value = "/account/{accountID}/contact/{contactID}/skillset/clock")
    public ResponseEntity<GenericResponse> modifyClockSkill (@PathVariable("accountID") String accountID,
                                                             @PathVariable("contactID") String contactID,
                                                             @RequestBody String payload){

        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> modifyClockSkillMap = userSkillService.modifyClockSkill(accountID,contactID,payload);

            if(ObjUtils.isNullOrEmpty(modifyClockSkillMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                if(Boolean.FALSE.equals(modifyClockSkillMap.get(Commons.SUCCESS))){
                    genericResponse = new GenericResponse(false, null, modifyClockSkillMap.get(Commons.MESSAGE).toString());
                }else{
                    genericResponse.setSuccess(true);
                    genericResponse.setData(modifyClockSkillMap);
                }
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error("error on modifying clock skillSet :"+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }

    }

    /**
     * Updates the force clock-out skill information for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @param payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping(value = "/account/{accountID}/contact/{contactID}/skillset/force-clockout")
    public ResponseEntity<GenericResponse> modifyForceClockOutSkill (@PathVariable("accountID") String accountID,
                                                                     @PathVariable("contactID") String contactID,
                                                                     @RequestBody String payload){

        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> modifyForceClockOutSkillMap = userSkillService.modifyForceClockOutSkill(accountID,contactID,payload);

            if(ObjUtils.isNullOrEmpty(modifyForceClockOutSkillMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                if(Boolean.FALSE.equals(modifyForceClockOutSkillMap.get(Commons.SUCCESS))){
                    genericResponse = new GenericResponse(false, null, modifyForceClockOutSkillMap.get(Commons.MESSAGE).toString());
                }else{
                    genericResponse.setSuccess(true);
                    genericResponse.setData(modifyForceClockOutSkillMap);
                }
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error("error on modifying force clockout skillSet :"+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }

    }

    /**
     * Updates the report skill information for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @param payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping(value = "/account/{accountID}/contact/{contactID}/skillset/report")
    public ResponseEntity<GenericResponse> modifyReportSkill (@PathVariable("accountID") String accountID,
                                                              @PathVariable("contactID") String contactID,
                                                              @RequestBody String payload){

        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> modifyReportSkillMap = userSkillService.modifyReportSkill(accountID,contactID,payload);

            if(ObjUtils.isNullOrEmpty(modifyReportSkillMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                if(Boolean.FALSE.equals(modifyReportSkillMap.get(Commons.SUCCESS))){
                    genericResponse = new GenericResponse(false, null, modifyReportSkillMap.get(Commons.MESSAGE).toString());
                }else{
                    genericResponse.setSuccess(true);
                    genericResponse.setData(modifyReportSkillMap);
                }
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error("error on modifying report skillSet :"+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }

    }

    /**
     * Updates the adjustment skill information for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @param payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping(value = "/account/{accountID}/contact/{contactID}/skillset/adjustment")
    public ResponseEntity<GenericResponse> modifyAdjustmentSkill (@PathVariable("accountID") String accountID,
                                                                  @PathVariable("contactID") String contactID,
                                                                  @RequestBody String payload){

        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> modifyAdjustmentSkillMap = userSkillService.modifyAdjustmentSkill(accountID,contactID,payload);

            if(ObjUtils.isNullOrEmpty(modifyAdjustmentSkillMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                if(Boolean.FALSE.equals(modifyAdjustmentSkillMap.get(Commons.SUCCESS))){
                    genericResponse = new GenericResponse(false, null, modifyAdjustmentSkillMap.get(Commons.MESSAGE).toString());
                }else{
                    genericResponse.setSuccess(true);
                    genericResponse.setData(modifyAdjustmentSkillMap);
                }
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error("error on modifying adjustment skillSet :"+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }

    }

    /**
     * Updates the activity skill information for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @param payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping(value = "/account/{accountID}/contact/{contactID}/skillset/activity")
    public ResponseEntity<GenericResponse> modifyActivitySkill (@PathVariable("accountID") String accountID,
                                                                @PathVariable("contactID") String contactID,
                                                                @RequestBody String payload){

        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> modifyActivitySkillMap = userSkillService.modifyActivitySkill(accountID,contactID,payload);

            if(ObjUtils.isNullOrEmpty(modifyActivitySkillMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                if(Boolean.FALSE.equals(modifyActivitySkillMap.get(Commons.SUCCESS))){
                    genericResponse = new GenericResponse(false, null, modifyActivitySkillMap.get(Commons.MESSAGE).toString());
                }else{
                    genericResponse.setSuccess(true);
                    genericResponse.setData(modifyActivitySkillMap);
                }
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error("error on modifying activity skillSet :"+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }

    }

    /**
     * Updates the hours-confirmation skill information for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @param payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping(value = "/account/{accountID}/contact/{contactID}/skillset/hours-confirmation")
    public ResponseEntity<GenericResponse> modifyConfirmHoursSkill (@PathVariable("accountID") String accountID,
                                                                    @PathVariable("contactID") String contactID,
                                                                    @RequestBody String payload){

        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> modifyConfirmHoursSkillMap = userSkillService.modifyConfirmHoursSkill(accountID,contactID,payload);

            if(ObjUtils.isNullOrEmpty(modifyConfirmHoursSkillMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                if(Boolean.FALSE.equals(modifyConfirmHoursSkillMap.get(Commons.SUCCESS))){
                    genericResponse = new GenericResponse(false, null, modifyConfirmHoursSkillMap.get(Commons.MESSAGE).toString());
                }else{
                    genericResponse.setSuccess(true);
                    genericResponse.setData(modifyConfirmHoursSkillMap);
                }
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error("error on modifying confirm-hours skillSet :"+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }

    }

    /**
     * Updates the team reports skill information for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @param payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping(value = "/account/{accountID}/contact/{contactID}/skillset/team-report")
    public ResponseEntity<GenericResponse> modifyTeamReportSkill (@PathVariable("accountID") String accountID,
                                                                  @PathVariable("contactID") String contactID,
                                                                  @RequestBody String payload){

        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> modifyTeamReportSkillMap = userSkillService.modifyTeamReportSkill(accountID,contactID,payload);

            if(ObjUtils.isNullOrEmpty(modifyTeamReportSkillMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                if(Boolean.FALSE.equals(modifyTeamReportSkillMap.get(Commons.SUCCESS))){
                    genericResponse = new GenericResponse(false, null, modifyTeamReportSkillMap.get(Commons.MESSAGE).toString());
                }else{
                    genericResponse.setSuccess(true);
                    genericResponse.setData(modifyTeamReportSkillMap);
                }
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error("error on modifying team-report skillSet :"+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }

    }

    /**
     * Updates remainder skill for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @param payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping(value = "/account/{accountID}/contact/{contactID}/skillset/reminder")
    public ResponseEntity<GenericResponse> modifyReminderSkill(@PathVariable("accountID") String accountID,
                                                               @PathVariable("contactID") String contactID,
                                                               @RequestBody String payload){

        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> modifyReminderSkillMap = userSkillService.modifyReminderSkill(accountID,contactID,payload);

            if(ObjUtils.isNullOrEmpty(modifyReminderSkillMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                if(Boolean.FALSE.equals(modifyReminderSkillMap.get(Commons.SUCCESS))){
                    genericResponse = new GenericResponse(false, null, modifyReminderSkillMap.get(Commons.MESSAGE).toString());
                }else{
                    genericResponse.setSuccess(true);
                    genericResponse.setData(modifyReminderSkillMap);
                }
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error("error on modifying reminder skill :"+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }

    }

    /**
     * Updates remainder time information for the given contactID and accountID
     * @param accountID
     * @param contactID
     * @param payload
     * @return
     */
    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping(value = "/account/{accountID}/contact/{contactID}/reminder/time")
    public ResponseEntity<GenericResponse> updateReminderTime (@PathVariable("accountID") String accountID,
                                                               @PathVariable("contactID") String contactID,
                                                               @RequestBody String payload){

        var genericResponse  = new GenericResponse();
        try{
            Map<String,Object> updateReminderTimeMap = userSkillService.updateReminderTime(accountID,contactID,payload);

            if(ObjUtils.isNullOrEmpty(updateReminderTimeMap)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                if(Boolean.FALSE.equals(updateReminderTimeMap.get(Commons.SUCCESS))){
                    genericResponse = new GenericResponse(false, null, updateReminderTimeMap.get(Commons.MESSAGE).toString());
                }else{
                    genericResponse.setSuccess(true);
                    genericResponse.setData(updateReminderTimeMap);
                }
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch ( Exception e ){
            log.error("error on updating remainder time :"+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }

    }

}