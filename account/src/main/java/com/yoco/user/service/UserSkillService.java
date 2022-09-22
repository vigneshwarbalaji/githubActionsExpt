package com.yoco.user.service;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.constants.CommonConstants;
import com.yoco.user.helper.skill.UserSkillHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@Service
public class UserSkillService {

    public static final String ACCESS_KEY = "access";
    public static final String UPDATED_PRO = "updatedPRO";
    public static final String PERMISSIONS_KEY = "permissions";
    public static final String TYPE = "type";
    public static final String SCOPE = "scope";
    public static final String TEAM_ID = "teamID";
    public static final String CLOCK_KEY = "clock";
    public static final String FORCE_CLOCKOUT_KEY = "forceClockOut";
    public static final String CONFIRM_HOURS_KEY = "confirmHours";
    public static final String REMINDER_TIME = "reminderTime";
    public static final String CLOCK_REMINDER = "clockReminder";


    public Map<String, Object> modifyWeeklySkill(String accountID, String contactID, String payload) throws IOException {

        var userSkillHelper = UserSkillHelper.getInstance();

        Map<String,Object> payloadSkillMap = userSkillHelper.validatePayloadAndExtractSkillDetails(accountID,contactID,payload,true);
        PeopleRelationJDO userPRO = (PeopleRelationJDO) payloadSkillMap.get(CommonConstants.USER_KEY);
        Boolean weeklyMail = (Boolean) payloadSkillMap.get(ACCESS_KEY);

        Map<String, Object> responseMap = userSkillHelper.validateEnterpriseAccountCheck(userPRO.getUniquepin());

        if(Boolean.TRUE.equals(responseMap.get(Commons.SUCCESS))){
            responseMap.clear();

            AccessPolicy accessPolicy;
            var activity = "";

            if (Boolean.TRUE.equals(weeklyMail)) {
                activity += "weekly reports mail skillset has been enabled";
                accessPolicy = AccessManager.addPermission(accountID,contactID,IAMPermission.WEEKLY_DIGEST.toString());
            }else{
                activity += "weekly reports mail skillset has been disabled";
                accessPolicy = AccessManager.removePermission(accountID,contactID,IAMPermission.WEEKLY_DIGEST.toString());
            }

            if(!ObjUtils.isNull(accessPolicy)){
                var userDTO = userSkillHelper.modifyWeeklySkillHelper(userPRO,accessPolicy,weeklyMail,activity);
                responseMap.put(Commons.SUCCESS,true);
                responseMap.put(UPDATED_PRO,userDTO);
            }
        }
        return  responseMap;
    }

    public Map<String, Object> modifyClockSkill(String accountID, String contactID, String payload) throws IOException {

        var userSkillHelper = UserSkillHelper.getInstance();

        Map<String, Object>responseMap = new HashMap<>();
        Map<String,Object> payloadSkillMap = userSkillHelper.validatePayloadAndExtractSkillDetails(accountID,contactID,payload,true);
        PeopleRelationJDO userPRO = (PeopleRelationJDO) payloadSkillMap.get(CommonConstants.USER_KEY);
        Boolean clockSkill = (Boolean) payloadSkillMap.get(ACCESS_KEY);
        Map<String,Object> permissionType = new HashMap<>();
        AccessPolicy accessPolicy;
        var activity = "";

        if (Boolean.TRUE.equals(clockSkill)) {
            activity += "Clock skillset has been enabled.";
            accessPolicy = AccessManager.addPermission(accountID,contactID,IAMPermission.CLOCK.toString());
            permissionType.put(CLOCK_KEY, clockSkill);
        }else{
            activity += "Clock skillset has been disabled.";
            accessPolicy = AccessManager.removePermission(accountID,contactID,IAMPermission.CLOCK.toString());
            permissionType.put(CLOCK_KEY, clockSkill);
        }

        if(!ObjUtils.isNull(accessPolicy)){
            var userDTO = userSkillHelper.modifyClockSkillHelper(userPRO,accessPolicy,clockSkill,activity);
            responseMap.put(Commons.SUCCESS,true);
            responseMap.put(PERMISSIONS_KEY, permissionType);
            responseMap.put(UPDATED_PRO,userDTO);
        }

        return responseMap;

    }

    public Map<String, Object> modifyForceClockOutSkill(String accountID, String contactID, String payload) throws IOException {

        var userSkillHelper = UserSkillHelper.getInstance();
        Map<String,Object> payloadSkillMap = userSkillHelper.validatePayloadAndExtractSkillDetails(accountID,contactID,payload,true);
        PeopleRelationJDO userPRO = (PeopleRelationJDO) payloadSkillMap.get(CommonConstants.USER_KEY);
        Map<String,Object> permissionType = new HashMap<>();
        Map<String, Object>responseMap = new HashMap<>();
        AccessPolicy accessPolicy;
        var activity = "";
        Boolean forceClockOutSkill = (Boolean) payloadSkillMap.get(ACCESS_KEY);

        if (Boolean.TRUE.equals(forceClockOutSkill)) {
            activity += "Force clockout skillset has been enabled";
            accessPolicy = AccessManager.addPermission(accountID,contactID,IAMPermission.FORCE_CLOCK_OUT.toString());
            permissionType.put(FORCE_CLOCKOUT_KEY, forceClockOutSkill);
        }else{
            activity += "Force clockout skillset has been disabled";
            accessPolicy = AccessManager.removePermission(accountID,contactID,IAMPermission.FORCE_CLOCK_OUT.toString());
            permissionType.put(FORCE_CLOCKOUT_KEY, forceClockOutSkill);
        }

        if(!ObjUtils.isNull(accessPolicy)){
            var userDTO = userSkillHelper.modifyForceClockOutSkillHelper(userPRO,accessPolicy,forceClockOutSkill,activity);
            responseMap.put(Commons.SUCCESS,true);
            responseMap.put(PERMISSIONS_KEY,permissionType);
            responseMap.put(UPDATED_PRO,userDTO);
        }

        return responseMap;

    }

    public Map<String, Object> modifyReportSkill(String accountID, String contactID, String payload) throws IOException {

        var userSkillHelper = UserSkillHelper.getInstance();
        Map<String,Object> payloadSkillMap = userSkillHelper.validatePayloadAndExtractSkillDetails(accountID,contactID,payload,true);
        PeopleRelationJDO userPRO = (PeopleRelationJDO) payloadSkillMap.get(CommonConstants.USER_KEY);
        String type = (String) payloadSkillMap.get(TYPE);
        String scope = (String) payloadSkillMap.get(SCOPE);
        Boolean reportSkill = (Boolean) payloadSkillMap.get(ACCESS_KEY);

        return userSkillHelper.modifyReportSkillHelper(userPRO, reportSkill, type, scope);

    }

    public Map<String, Object> modifyAdjustmentSkill(String accountID, String contactID, String payload) throws IOException {

        var userSkillHelper = UserSkillHelper.getInstance();
        Map<String,Object> payloadSkillMap = userSkillHelper.validatePayloadAndExtractSkillDetails(accountID,contactID,payload,true);
        String type = (String) payloadSkillMap.get(TYPE);
        PeopleRelationJDO userPRO = (PeopleRelationJDO) payloadSkillMap.get(CommonConstants.USER_KEY);
        Boolean adjustmentSkill = (Boolean) payloadSkillMap.get(ACCESS_KEY);

        return userSkillHelper.modifyAdjustmentSkillHelper(userPRO, adjustmentSkill, type);

    }

    public Map<String, Object> modifyActivitySkill(String accountID, String contactID, String payload) throws IOException {

        var userSkillHelper = UserSkillHelper.getInstance();

        Map<String, Object>responseMap = new HashMap<>();
        Map<String,Object> payloadSkillMap = userSkillHelper.validatePayloadAndExtractSkillDetails(accountID,contactID,payload,true);
        PeopleRelationJDO userPRO = (PeopleRelationJDO) payloadSkillMap.get(CommonConstants.USER_KEY);
        Boolean activitySkill = (Boolean) payloadSkillMap.get(ACCESS_KEY);
        Map<String,Object> permissionType = new HashMap<>();
        AccessPolicy accessPolicy;
        var activity = "";

        if (Boolean.TRUE.equals(activitySkill)) {
            activity += "Activities access has been enabled.";
            accessPolicy = AccessManager.addPermission(accountID,contactID,IAMPermission.ACTIVITY.toString());
            permissionType.put(CommonConstants.ACTIVITY, activitySkill);
        }else{
            activity += "Activities access has been disabled.";
            accessPolicy = AccessManager.removePermission(accountID,contactID,IAMPermission.ACTIVITY.toString());
            permissionType.put(CommonConstants.ACTIVITY, activitySkill);
        }

        if(!ObjUtils.isNull(accessPolicy)){
            var userDTO = userSkillHelper.modifyActivitySkillHelper(userPRO,accessPolicy,activitySkill,activity);
            responseMap.put(Commons.SUCCESS,true);
            responseMap.put(PERMISSIONS_KEY, permissionType);
            responseMap.put(UPDATED_PRO,userDTO);
        }

        return responseMap;

    }

    public Map<String, Object> modifyConfirmHoursSkill(String accountID, String contactID, String payload) throws IOException {

        var userSkillHelper = UserSkillHelper.getInstance();

        Map<String, Object>responseMap = new HashMap<>();
        Map<String,Object> payloadSkillMap = userSkillHelper.validatePayloadAndExtractSkillDetails(accountID,contactID,payload,true);
        PeopleRelationJDO userPRO = (PeopleRelationJDO) payloadSkillMap.get(CommonConstants.USER_KEY);
        Boolean confirmHoursSkill = (Boolean) payloadSkillMap.get(ACCESS_KEY);
        Map<String,Object> permissionType = new HashMap<>();
        AccessPolicy accessPolicy;
        var activity = "";

        if (Boolean.TRUE.equals(confirmHoursSkill)) {
            activity += "Confirm hours skillset has been enabled.";
            accessPolicy = AccessManager.addPermission(accountID,contactID,IAMPermission.CONFIRM_HOURS.toString());
            permissionType.put(CONFIRM_HOURS_KEY, confirmHoursSkill);
        }else{
            activity += "Confirm hours skillset has been disabled.";
            accessPolicy = AccessManager.removePermission(accountID,contactID,IAMPermission.CONFIRM_HOURS.toString());
            permissionType.put(CONFIRM_HOURS_KEY, confirmHoursSkill);
        }

        if(!ObjUtils.isNull(accessPolicy)){
            var userDTO = userSkillHelper.modifyConfirmHoursSkillHelper(userPRO,accessPolicy,confirmHoursSkill,activity);
            responseMap.put(Commons.SUCCESS,true);
            responseMap.put(PERMISSIONS_KEY, permissionType);
            responseMap.put(UPDATED_PRO,userDTO);
        }

        return responseMap;

    }

    public Map<String, Object> modifyTeamReportSkill(String accountID, String contactID, String payload) throws IOException {

        var userSkillHelper = UserSkillHelper.getInstance();

        Map<String,Object> payloadSkillMap = userSkillHelper.validatePayloadAndExtractSkillDetails(accountID,contactID,payload,true);
        PeopleRelationJDO userPRO = (PeopleRelationJDO) payloadSkillMap.get(CommonConstants.USER_KEY);
        Boolean teamReportSkill = (Boolean) payloadSkillMap.get(ACCESS_KEY);
        String type = (String) payloadSkillMap.get(TYPE);
        String teamID = (String) payloadSkillMap.get(TEAM_ID);

        return userSkillHelper.modifyTeamReportSkillHelper(userPRO, teamReportSkill, type, teamID);

    }

    public Map<String, Object> modifyReminderSkill(String accountID, String contactID, String payload) throws IOException {

        var userSkillHelper = UserSkillHelper.getInstance();
        Map<String,Object> payloadSkillMap = userSkillHelper.validatePayloadAndExtractSkillDetails(accountID,contactID,payload,true);
        PeopleRelationJDO userPRO = (PeopleRelationJDO) payloadSkillMap.get(CommonConstants.USER_KEY);
        Map<String,Object> permissionType = new HashMap<>();
        Map<String, Object>responseMap = new HashMap<>();
        AccessPolicy accessPolicy;
        var activity = "";
        Boolean reminderSkill = (Boolean) payloadSkillMap.get(ACCESS_KEY);

        if (Boolean.TRUE.equals(reminderSkill)) {
            activity += "reminder skillset has been enabled";
            accessPolicy = AccessManager.addPermission(accountID,contactID,IAMPermission.REMINDER.toString());
            permissionType.put(CLOCK_REMINDER, reminderSkill);
        }else{
            activity += "reminder skillset has been disabled";
            accessPolicy = AccessManager.removePermission(accountID,contactID,IAMPermission.REMINDER.toString());
            permissionType.put(CLOCK_REMINDER, reminderSkill);
        }

        if(!ObjUtils.isNull(accessPolicy)){
            var userDTO = userSkillHelper.modifyReminderSkillHelper(userPRO,accessPolicy,reminderSkill,activity);
            responseMap.put(Commons.SUCCESS,true);
            responseMap.put(PERMISSIONS_KEY,permissionType);
            responseMap.put(UPDATED_PRO,userDTO);
        }

        return responseMap;

    }

    public Map<String, Object> updateReminderTime(String accountID, String contactID, String payload) throws IOException {

        var userSkillHelper = UserSkillHelper.getInstance();

        Map<String, Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        Map<String,Object> payloadSkillMap = userSkillHelper.validatePayloadAndExtractSkillDetails(accountID,contactID,payload,false);
        PeopleRelationJDO userPRO = (PeopleRelationJDO) payloadSkillMap.get(CommonConstants.USER_KEY);
        int reminderTime = 0;
        if(payloadMap.containsKey(REMINDER_TIME)){
            reminderTime = (int) payloadMap.get(REMINDER_TIME);
        }
        return userSkillHelper.modifyReminderTimeHelper(userPRO, reminderTime);

    }

}