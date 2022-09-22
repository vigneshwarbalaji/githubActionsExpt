package com.yoco.user.helper.skill;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.*;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.CommonConstants;
import com.yoco.user.enums.USER_ERROR_MESSAGE;
import com.yoco.user.helper.UserTaskInitiator;
import com.yoco.user.service.UserSkillService;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

@Slf4j
public class UserSkillHelper {

    public static final String SKILLSET_OPERATION_KEY = "SKILLSET_OPERATION";
    public static final String SKILLSETS_OPERATION = "Skillsets_Operation";
    public static final String ALL = "all";
    public static final String STAFF = "staff";
    public static final String EDIT = "edit";
    public static final String VIEW = "view";
    public static final String ADJUSTMENTS_KEY = "adjustments";
    public static final String ADJUSTMENT = "adjustment";
    public static final String PERMISSIONS_KEY = "permissions";
    public static final String REPORT_KEY = "report";
    public static final String UPDATED_PRO = "updatedPRO";
    public static final String ACCESS_POLICY_KEY = "accessPolicy";
    public static final String SKILL_MAP_KEY = "skillMap";
    public static final String TEAM_SKILL_SET = "teamSkillset";
    public static final String TEAM_SKILL_SET_MAP = "teamSkillSetMap";

    public static UserSkillHelper getInstance(){
        return new UserSkillHelper();
    }

    public Map<String,Object> validatePayloadAndExtractSkillDetails(String accountID, String contactID, String payload,
                                                                    boolean shouldPerformAccessCheck){
        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(contactID), COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value());
        Validator.checkArgument( ObjUtils.isNullOrEmpty(payload) || !JsonUtil.isValidJson(payload),
                COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());

        Map<String, Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payloadMap), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());

        PeopleRelationJDO userPRO = UserPROUtil.getUserProWithContact(accountID, contactID);
        Validator.checkArgument(ObjUtils.isNull(userPRO), USER_ERROR_MESSAGE.USER_NOT_FOUND.value());

        Map<String, Object> skillMap = new HashMap<>();

        if(shouldPerformAccessCheck){
            skillMap = validateAndExtractPayload(payloadMap);
        }

        skillMap.put(CommonConstants.USER_KEY,userPRO);
        return skillMap;
    }

    public Map<String, Object> validateAndExtractPayload(Map<String, Object> payloadMap){

        Validator.checkArgument(!(payloadMap.get(UserSkillService.ACCESS_KEY) instanceof Boolean),
                USER_ERROR_MESSAGE.ACCESS_VALUE_BOOLEAN_ERROR_MESSAGE.value());

        Map<String, Object>requestMap = new HashMap<>();
        requestMap.put(UserSkillService.ACCESS_KEY, payloadMap.get(UserSkillService.ACCESS_KEY));
        if(payloadMap.containsKey(UserSkillService.TYPE))
            requestMap.put(UserSkillService.TYPE, payloadMap.get(UserSkillService.TYPE));

        if(payloadMap.containsKey(UserSkillService.SCOPE))
            requestMap.put(UserSkillService.SCOPE, payloadMap.get(UserSkillService.SCOPE));

        if(payloadMap.containsKey(UserSkillService.TEAM_ID))
            requestMap.put(UserSkillService.TEAM_ID, payloadMap.get(UserSkillService.TEAM_ID));

        return requestMap;
    }

    public Map<String,Object> validateEnterpriseAccountCheck(String accountID){
        Map<String, Object> responseMap = new HashMap<>();
        if(AccountUtil.isActiveEnterpriseAccount(accountID)){
            responseMap.put(Commons.SUCCESS,true);
        }else{
            log.info(" this account is not under enterprise plan ");
            responseMap.put(Commons.SUCCESS,false);
            responseMap.put(Commons.MESSAGE,COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        }
        return responseMap;
    }

    public Map<String,Object> generateWeeklySkillTaskQueueMap(UserDTO userDTO, AccessPolicy accessPolicy,boolean weeklyMail){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put(CommonConstants.ACTION,"weeklySkill");
        queueMap.put(CommonConstants.POLICY,accessPolicy);
        queueMap.put("weeklyMail",weeklyMail);
        return queueMap;
    }

    public UserDTO modifyWeeklySkillHelper(PeopleRelationJDO userPRO,AccessPolicy accessPolicy,boolean weeklyMail,String activity) throws IOException {

        Map<String,Object> skillMap = UserPROUtil.extractPROSkills(userPRO);

        if(!ObjUtils.isNullOrEmpty(skillMap)){

            skillMap.put(Skillset.SKILL_SET_KEY_WEEKLY_MAIL, weeklyMail);

            var userDTO = this.updateSkillHelper(userPRO,skillMap,activity,accessPolicy);

            UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateWeeklySkillTaskQueueMap(userDTO,accessPolicy,weeklyMail));

            return userDTO;
        }
        return null;
    }

    public Map<String,Object> generateClockSkillTaskQueueMap(UserDTO userDTO, AccessPolicy accessPolicy,boolean clockSkill){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"clockSkill");
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put("clockSkill", clockSkill);
        queueMap.put(CommonConstants.POLICY,accessPolicy);
        return queueMap;
    }

    public UserDTO modifyClockSkillHelper(PeopleRelationJDO userPRO,AccessPolicy accessPolicy, boolean clockSkill,String activity) throws IOException {

        Map<String,Object> skillMap = UserPROUtil.extractPROSkills(userPRO);

        if(!ObjUtils.isNullOrEmpty(skillMap)){

            if(Boolean.TRUE.equals(clockSkill))
                skillMap.put(Skillset.SKILL_SET_KEY_WEB_CLOCK_IN, Skillset.SKILLSET_EDIT);
            else
                skillMap.put(Skillset.SKILL_SET_KEY_WEB_CLOCK_IN, "");

            var userDTO = this.updateSkillHelper(userPRO,skillMap,activity,accessPolicy);

            UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateClockSkillTaskQueueMap(userDTO,accessPolicy,clockSkill));

            return userDTO;
        }
        return null;
    }

    public Map<String,Object> generateForceClockOutSkillTaskQueueMap(UserDTO userDTO, AccessPolicy accessPolicy,boolean forceClockOutSkill){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"forceClockOutSkill");
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put("forceClockOutSkill", forceClockOutSkill);
        queueMap.put(CommonConstants.POLICY,accessPolicy);
        return queueMap;
    }

    public UserDTO modifyForceClockOutSkillHelper(PeopleRelationJDO userPRO,AccessPolicy accessPolicy, boolean forceClockOutSkill,String activity) throws IOException {

        Map<String,Object> skillMap = UserPROUtil.extractPROSkills(userPRO);

        if(!ObjUtils.isNullOrEmpty(skillMap)){

            if(Boolean.TRUE.equals(forceClockOutSkill))
                skillMap.put(Skillset.SKILL_SET_KEY_FORCE_CLOCK_OUT, Skillset.SKILLSET_EDIT);
            else
                skillMap.put(Skillset.SKILL_SET_KEY_FORCE_CLOCK_OUT, "");

            var userDTO = this.updateSkillHelper(userPRO,skillMap,activity,accessPolicy);

            UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateForceClockOutSkillTaskQueueMap(userDTO,accessPolicy,forceClockOutSkill));

            return userDTO;
        }
        return null;
    }

    public UserDTO updateSkillHelper(PeopleRelationJDO userPRO,Map<String,Object> skill,String activity,AccessPolicy accessPolicy){

        userPRO = UserPROUtil.updatePROSkill(userPRO,skill);

        log.info(" Updated UserPRO: " + userPRO);

        ActivityUtil.saveActivity(userPRO.getUniquepin(),userPRO.getContactId(),SKILLSET_OPERATION_KEY, userPRO.getEmailID(),activity,
                ActivityUtil.ACTIVITIES.DUMMY.value(), DateUtil.getCurrentTime());

        Set<String> permissions = new HashSet<>();
        if(!ObjUtils.isNull(accessPolicy) && !ObjUtils.isNull(accessPolicy.getPermissions())){
            permissions = accessPolicy.getPermissions();
        }
        return new UserDTO(userPRO,permissions);
    }

    public Map<String, Object> modifyReportSkillHelper(PeopleRelationJDO userPRO,  boolean reportSkill, String type, String scope) throws IOException {

        Map<String,Object> skillMap = UserPROUtil.extractPROSkills(userPRO);
        Map<String, Object>responseMap = new HashMap<>();

        if(!ObjUtils.isNullOrEmpty(skillMap)){

            if(Boolean.TRUE.equals(reportSkill))
                responseMap = enableReportAccessHelper(userPRO, type, scope, skillMap);
            else
                responseMap = disableReportAccessHelper(userPRO, skillMap);

        }

        return responseMap;
    }

    public Map<String, Object> enableReportAccessHelper(PeopleRelationJDO userPRO, String type, String scope, Map<String,Object> skillMap) throws IOException {

        Map<String, Object> responseMap;

        if(EDIT.equalsIgnoreCase(type)){
            responseMap = enableReportEditAccessHelper(userPRO, scope, skillMap);
        }else{
            responseMap = enableReportViewAccessHelper(userPRO, scope, skillMap);
        }

        return responseMap;
    }

    public Map<String, Object> enableReportEditAccessHelper(PeopleRelationJDO userPRO, String scope, Map<String,Object> skillMap) throws IOException {
        Validator.checkArgument(STAFF.equalsIgnoreCase(userPRO.getRole()),"User should be an admin to obtain edit access");
        Map<String, Object>permissionType = new HashMap<>();
        AccessPolicy accessPolicy;
        var activity = "";
        Map<String, Object>responseMap = new HashMap<>();

        if(ALL.equalsIgnoreCase(scope)) {
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_EDITALL);
            activity += "Reports - edit access for all has been enabled.";
            accessPolicy = AccessManager.addEditAllPermissionToReports(userPRO.getUniquepin(),userPRO.getContactId());
            permissionType.put(REPORT_KEY, IAMPermission.REPORTS_EDIT_ALL.toString());
        }else {
            activity += "Reports - edit access for team has been enabled.";
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_EDIT);
            accessPolicy = AccessManager.addEditPermissionToReports(userPRO.getUniquepin(),userPRO.getContactId());
            permissionType.put(REPORT_KEY, IAMPermission.REPORTS_EDIT.toString());
        }

        var userDTO = this.updateSkillHelper(userPRO,skillMap,activity,accessPolicy);
        UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateReportSkillTaskQueueMap(userDTO,accessPolicy,permissionType));

        if(!ObjUtils.isNull(accessPolicy)){
            responseMap.put(Commons.SUCCESS,true);
            responseMap.put(UPDATED_PRO, userDTO);
            responseMap.put(PERMISSIONS_KEY, permissionType);
        }


        return responseMap;
    }

    public Map<String,Object> generateReportSkillTaskQueueMap(UserDTO userDTO, AccessPolicy accessPolicy,Map<String,Object> reportSkillMap){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"reportSkill");
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put("reportSkill", reportSkillMap);
        queueMap.put(CommonConstants.POLICY,accessPolicy);
        return queueMap;
    }

    public Map<String, Object> enableReportViewAccessHelper(PeopleRelationJDO userPRO, String scope, Map<String, Object>skillMap) throws IOException {

        Map<String, Object>permissionType = new HashMap<>();
        AccessPolicy accessPolicy;
        var activity = "";
        Map<String, Object>responseMap = new HashMap<>();
        if(ALL.equalsIgnoreCase(scope)) {
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_VIEWALL);
            activity += "Reports - view access for all has been enabled.";
            accessPolicy = AccessManager.addViewAllPermissionToReports(userPRO.getUniquepin(),userPRO.getContactId());
            permissionType.put(REPORT_KEY, IAMPermission.REPORTS_VIEW_ALL.toString());
        }else {
            activity += "Reports - view access for team has been enabled.";
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_VIEW);
            accessPolicy = AccessManager.addViewPermissionToReports(userPRO.getUniquepin(),userPRO.getContactId());
            permissionType.put(REPORT_KEY, IAMPermission.REPORTS_VIEW.toString());
        }

        var userDTO = this.updateSkillHelper(userPRO,skillMap,activity,accessPolicy);
        UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateReportSkillTaskQueueMap(userDTO,accessPolicy,permissionType));

        if(!ObjUtils.isNull(accessPolicy)){
            responseMap.put(Commons.SUCCESS,true);
            responseMap.put(UPDATED_PRO, userDTO);
            responseMap.put(PERMISSIONS_KEY, permissionType);
        }


        return responseMap;
    }

    public Map<String, Object> disableReportAccessHelper(PeopleRelationJDO userPRO, Map<String, Object>skillMap) throws IOException {

        Map<String, Object>permissionType = new HashMap<>();
        AccessPolicy accessPolicy;
        var activity = "";
        Map<String, Object>responseMap = new HashMap<>();

        skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, "");
        activity += "Reports access has been disabled.";
        accessPolicy = AccessManager.removeReportsPermission(userPRO.getUniquepin(),userPRO.getContactId());
        permissionType.put(REPORT_KEY, "");
        var userDTO = this.updateSkillHelper(userPRO,skillMap,activity,accessPolicy);
        UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateReportSkillTaskQueueMap(userDTO,accessPolicy,permissionType));

        if(!ObjUtils.isNull(accessPolicy)){
            responseMap.put(Commons.SUCCESS,true);
            responseMap.put(UPDATED_PRO, userDTO);
            responseMap.put(PERMISSIONS_KEY, permissionType);
        }

        return responseMap;

    }

    public Map<String,Object> generateAdjustmentSkillTaskQueueMap(UserDTO userDTO, AccessPolicy accessPolicy,Map<String,Object> adjustmentSkillMap){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"adjustmentSkill");
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put("adjustmentSkill", adjustmentSkillMap);
        queueMap.put(CommonConstants.POLICY,accessPolicy);
        return queueMap;
    }

    public Map<String, Object> modifyAdjustmentSkillHelper(PeopleRelationJDO userPRO, boolean adjustmentSkill, String type) throws IOException {
        Map<String,Object> skillMap = UserPROUtil.extractPROSkills(userPRO);
        Map<String, Object>adjustmentSkillMap;
        Map<String, Object>responseMap = new HashMap<>();

        if(Boolean.TRUE.equals(adjustmentSkill)){
            adjustmentSkillMap = this.addAdjustmentPermissionSkillHelper(userPRO, type, skillMap);
        }else{
            adjustmentSkillMap = this.removeAdjustmentPermissionSkillHelper(userPRO, skillMap);
        }

        String activity = (String) adjustmentSkillMap.get(CommonConstants.ACTIVITY);
        AccessPolicy accessPolicy = (AccessPolicy) adjustmentSkillMap.get(ACCESS_POLICY_KEY);
        skillMap = (Map<String, Object>) adjustmentSkillMap.get(SKILL_MAP_KEY);
        Map<String, Object>permissionMap = (Map<String, Object>) adjustmentSkillMap.get(PERMISSIONS_KEY);

        var userDTO = this.updateSkillHelper(userPRO,skillMap,activity,accessPolicy);
        UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateAdjustmentSkillTaskQueueMap(userDTO,accessPolicy, permissionMap));

        if(!ObjUtils.isNull(accessPolicy)){
            responseMap.put(Commons.SUCCESS, true);
            responseMap.put(PERMISSIONS_KEY, permissionMap);
            responseMap.put(UPDATED_PRO, userDTO);
        }

        return responseMap;

    }

    public Map<String, Object> addAdjustmentPermissionSkillHelper(PeopleRelationJDO userPRO, String type, Map<String,Object> skillMap) {
        var activity = "";
        AccessPolicy accessPolicy;
        Map<String, Object>permissionType = new HashMap<>();
        Map<String, Object>responseMap = new HashMap<>();
        if(EDIT.equalsIgnoreCase(type)){
            Validator.checkArgument(userPRO.getRole().equalsIgnoreCase(STAFF),"User should be an admin to obtain edit access");
            activity += "Adjustments - edit access has been enabled.";
            accessPolicy = AccessManager.addAdjustmentEditPermission(userPRO.getUniquepin(), userPRO.getContactId());
            skillMap.put(ADJUSTMENTS_KEY, EDIT);
            permissionType.put(ADJUSTMENT, IAMPermission.ADJUSTMENTS_EDIT.toString());
        }else{
            activity += "Adjustments - view access has been enabled.";
            accessPolicy = AccessManager.addAdjustmentViewPermission(userPRO.getUniquepin(), userPRO.getContactId());
            skillMap.put(ADJUSTMENTS_KEY, VIEW);
            permissionType.put(ADJUSTMENT, IAMPermission.ADJUSTMENTS_VIEW.toString());
        }

        responseMap.put(ACCESS_POLICY_KEY, accessPolicy);
        responseMap.put(CommonConstants.ACTIVITY, activity);
        responseMap.put(SKILL_MAP_KEY, skillMap);
        responseMap.put(PERMISSIONS_KEY, permissionType);

        return responseMap;
    }

    public Map<String, Object> removeAdjustmentPermissionSkillHelper(PeopleRelationJDO userPRO, Map<String,Object> skillMap) {
        var activity = "";
        Map<String, Object>permissionType = new HashMap<>();
        Map<String, Object>responseMap = new HashMap<>();
        activity += "Adjustments access has been disabled.";
        AccessPolicy accessPolicy = AccessManager.removeAdjustmentPermission(userPRO.getUniquepin(), userPRO.getContactId());
        skillMap.put(ADJUSTMENTS_KEY, "");
        permissionType.put(ADJUSTMENT, "");

        responseMap.put(ACCESS_POLICY_KEY, accessPolicy);
        responseMap.put(CommonConstants.ACTIVITY, activity);
        responseMap.put(SKILL_MAP_KEY, skillMap);
        responseMap.put(PERMISSIONS_KEY, permissionType);

        return responseMap;
    }

    public Map<String,Object> generateActivitySkillTaskQueueMap(UserDTO userDTO, AccessPolicy accessPolicy,boolean activitySkill){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"activitySkill");
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put("activitySkill", activitySkill);
        queueMap.put(CommonConstants.POLICY,accessPolicy);
        return queueMap;
    }

    public UserDTO modifyActivitySkillHelper(PeopleRelationJDO userPRO,AccessPolicy accessPolicy, boolean activitySkill,String activity) throws IOException {

        Map<String,Object> skillMap = UserPROUtil.extractPROSkills(userPRO);

        if(!ObjUtils.isNullOrEmpty(skillMap)){

            if(Boolean.TRUE.equals(activitySkill))
                skillMap.put(Skillset.SKILL_SET_KEY_SUB_STATUS, Skillset.SKILLSET_VIEW);
            else
                skillMap.put(Skillset.SKILL_SET_KEY_SUB_STATUS, "");

            var userDTO = this.updateSkillHelper(userPRO,skillMap,activity,accessPolicy);

            UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateActivitySkillTaskQueueMap(userDTO,accessPolicy,activitySkill));

            return userDTO;
        }
        return null;
    }

    public Map<String,Object> generateConfirmHoursSkillTaskQueueMap(UserDTO userDTO, AccessPolicy accessPolicy,boolean confirmHoursSkill){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"confirmHoursSkill");
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put("confirmHoursSkill", confirmHoursSkill);
        queueMap.put(CommonConstants.POLICY,accessPolicy);
        return queueMap;
    }

    public UserDTO modifyConfirmHoursSkillHelper(PeopleRelationJDO userPRO,AccessPolicy accessPolicy, boolean confirmHoursSkill,String activity) throws IOException {

        Map<String,Object> skillMap = UserPROUtil.extractPROSkills(userPRO);
        Map<String, Object>payrollMap = new HashMap<>();

        if(!ObjUtils.isNullOrEmpty(skillMap)){
            payrollMap.put(Skillset.SKILL_SET_KEY_CAN_CONFIRMHOURS, confirmHoursSkill);
            skillMap.put(Skillset.SKILL_SET_KEY_PAYROLL, JsonUtil.getJson(payrollMap));
            var userDTO = this.updateSkillHelper(userPRO,skillMap,activity,accessPolicy);
            UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateConfirmHoursSkillTaskQueueMap(userDTO,accessPolicy,confirmHoursSkill));
            return userDTO;
        }
        return null;
    }

    public Map<String,Object> generateTeamReportSkillTaskQueueMap(UserDTO userPRO){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"teamReportSkill");
        queueMap.put(CommonConstants.USER_KEY,userPRO);
        return queueMap;
    }

    public Map<String, Object> modifyTeamReportSkillHelper(PeopleRelationJDO userPRO, boolean teamReportSkill, String type, String teamID) throws IOException {

        Map<String,Object> skillMap = UserPROUtil.extractPROSkills(userPRO);
        Map<String, Object>teamReportSkillSetMap;
        Map<String, Object>responseMap = new HashMap<>();
        var activity = "";

        if(!ObjUtils.isNullOrEmpty(skillMap)){
            String teamSkillSetString = (String) skillMap.get(TEAM_SKILL_SET);
            Map<String, Object> teamSkillSetMap = ObjUtils.isNullOrEmpty(teamSkillSetString) ? new HashMap<>() : JsonUtil.convertJsonToMap(teamSkillSetString);
            if(Boolean.TRUE.equals(teamReportSkill)){
                teamReportSkillSetMap = this.addTeamReportPermissionSkillHelper(teamSkillSetMap, type, teamID);
            }else{
                teamReportSkillSetMap = this.removeTeamReportPermissionSkillHelper(teamSkillSetMap, teamID);
            }

            skillMap.put(TEAM_SKILL_SET, JsonUtil.getJson(teamReportSkillSetMap.get(TEAM_SKILL_SET_MAP)));
            activity = (String) teamReportSkillSetMap.get(CommonConstants.ACTIVITY);

        }

        var userDTO = this.updateSkillHelper(userPRO,skillMap,activity,null);
        UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateTeamReportSkillTaskQueueMap(userDTO));

        responseMap.put(Commons.SUCCESS, true);
        responseMap.put(UPDATED_PRO, userDTO);

        return responseMap;

    }

    public Map<String, Object> addTeamReportPermissionSkillHelper(Map<String, Object>teamSkillSetMap, String type, String teamID) {

        var activity = "";
        Map<String, Object> responseMap = new HashMap<>();

        if(EDIT.equalsIgnoreCase(type)){
            teamSkillSetMap.put(teamID, EDIT);
            activity += "Edit access has been enabled for team :"+ teamID;
        }else{
            teamSkillSetMap.put(teamID, VIEW);
            activity += "View access has been enabled for team :"+ teamID;
        }

        responseMap.put(CommonConstants.ACTIVITY, activity);
        responseMap.put(TEAM_SKILL_SET_MAP, teamSkillSetMap);

        return responseMap;

    }

    public Map<String, Object> removeTeamReportPermissionSkillHelper(Map<String, Object>teamSkillSetMap ,String teamID) {

        var activity = "";
        Map<String, Object> responseMap = new HashMap<>();

        teamSkillSetMap.remove(teamID);
        activity += "Report access has been revoked for team :"+ teamID;

        responseMap.put(CommonConstants.ACTIVITY, activity);
        responseMap.put(TEAM_SKILL_SET_MAP, teamSkillSetMap);

        return responseMap;

    }

    public Map<String,Object> generateReminderSkillTaskQueueMap(UserDTO userDTO, AccessPolicy accessPolicy,boolean reminderSkill, Map<String, Object>skillMap){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"reminderSkill");
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put("reminderSkill", reminderSkill);
        queueMap.put(SKILL_MAP_KEY, skillMap);
        queueMap.put(CommonConstants.POLICY,accessPolicy);
        return queueMap;
    }

    public UserDTO modifyReminderSkillHelper(PeopleRelationJDO userPRO,AccessPolicy accessPolicy, boolean reminderSkill,String activity) throws IOException {

        Map<String,Object> skillMap = UserPROUtil.extractPROSkills(userPRO);

        if(!ObjUtils.isNullOrEmpty(skillMap)){

            skillMap.put(Skillset.SKILL_SET_KEY_REMINDER, reminderSkill);
            var userDTO = this.updateSkillHelper(userPRO,skillMap,activity,accessPolicy);

            UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateReminderSkillTaskQueueMap(userDTO,accessPolicy,reminderSkill,skillMap));

            return userDTO;
        }
        return null;
    }

    public boolean isReminderTimeWithinTheLimit(int reminderTime) {
        return (reminderTime > 0 && reminderTime <= 24);
    }

    public Map<String,Object> generateReminderTimeTaskQueueMap(UserDTO userPRO, Map<String, Object>skillMap){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION, "modifyReminderTime");
        queueMap.put(SKILL_MAP_KEY, skillMap);
        queueMap.put(CommonConstants.USER_KEY,userPRO);
        return queueMap;
    }

    public Map<String, Object> modifyReminderTimeHelper(PeopleRelationJDO userPRO, int reminderTime) throws IOException {

        Map<String,Object> skillMap = UserPROUtil.extractPROSkills(userPRO);
        Map<String, Object>responseMap = new HashMap<>();
        var activity = "";

        if(!ObjUtils.isNullOrEmpty(skillMap) && isReminderTimeWithinTheLimit(reminderTime)){
            String hourString = (reminderTime > 1) ? (reminderTime + " hours") : (reminderTime + " hour");
            skillMap.put(UserSkillService.REMINDER_TIME, reminderTime);
            activity += "reminder time has been changed to " + hourString;
        }

        var userDTO = this.updateSkillHelper(userPRO,skillMap,activity,null);
        UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateReminderTimeTaskQueueMap(userDTO, skillMap));

        responseMap.put(Commons.SUCCESS, true);
        responseMap.put(UPDATED_PRO, userDTO);

        return responseMap;
    }
}