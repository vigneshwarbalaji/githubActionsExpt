package com.yoco.user.helper.skill;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.fullservices.FullReminders;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.events.InAppReminderUtil;
import com.yoco.constants.CommonConstants;
import com.yoco.user.helper.UserProProfileHelper;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;

@Slf4j
@NoArgsConstructor
public class UserSkillTaskHelper {

    public static UserSkillTaskHelper getInstance(){
        return new UserSkillTaskHelper();
    }

    private static final String UPDATE_PRO_KEY = "Update_NFCRFID";
    private static final String CLOCK_KEY = "clock";
    private static final String FORCE_CLOCKOUT_KEY = "forceClockOut";
    private static final String CONFIRM_HOURS_KEY = "confirmHours";
    private static final String CLOCK_REMINDER_KEY = "clockReminder";

    public void updateProTaskHelper(Map<String,Object> taskMap){
        var actionType = taskMap.get("action").toString();
        log.info(" actionType " + actionType);
        switch(actionType){
            case "weeklySkill" :  this.weeklySkillTaskHelper(taskMap);
                break;
            case "clockSkill" : this.clockSkillTaskHelper(taskMap);
                break;
            case "forceClockOutSkill" : this.forceClockOutSkillTaskHelper(taskMap);
                break;
            case "reportSkill" : this.reportSkillTaskHelper(taskMap);
                break;
            case "activitySkill" : this.activitySkillTaskHelper(taskMap);
                break;
            case "confirmHoursSkill" : this.confirmHoursSkillTaskHelper(taskMap);
                break;
            case "adjustmentSkill" : this.adjustmentSkillTaskHelper(taskMap);
                break;
            case "teamReportSkill" : this.teamReportSkillTaskHelper(taskMap);
                break;
            case "reminderSkill" : this.reminderSkillTaskHelper(taskMap);
                break;
            case "modifyReminderTime" : this.modifyReminderTimeTaskHelper(taskMap);
                break;
            case "updateProProfile": this.updateProProfileHelper(taskMap);
                break;
            default: log.info(" invalid actionType " + actionType);
        }
    }

    public void clockSkillTaskHelper(Map<String,Object> taskMap){

        UserDTO userPRO = (UserDTO) taskMap.get(CommonConstants.USER_KEY);
        var accessPolicy = (AccessPolicy) taskMap.get(CommonConstants.POLICY);
        boolean isClockSkillEnabled = Boolean.TRUE.equals(taskMap.get("clockSkill"));
        Map<String,Object> permissionType = new HashMap<>();
        permissionType.put(CLOCK_KEY,isClockSkillEnabled);

        UserSkillChannelPublishHelper.getInstance().publishToChannelWithUpdatedPolicy(accessPolicy,userPRO,permissionType);

    }

    public void reportSkillTaskHelper(Map<String,Object> taskMap){

        UserDTO userPRO = (UserDTO) taskMap.get(CommonConstants.USER_KEY);
        var accessPolicy = (AccessPolicy) taskMap.get(CommonConstants.POLICY);
        Map<String,Object> permissionType = (Map<String, Object>) taskMap.get("reportSkill");

        UserSkillChannelPublishHelper.getInstance().publishToChannelWithUpdatedPolicy(accessPolicy,userPRO,permissionType);

    }

    public void activitySkillTaskHelper(Map<String,Object> taskMap){

        UserDTO userPRO = (UserDTO) taskMap.get(CommonConstants.USER_KEY);
        var accessPolicy = (AccessPolicy) taskMap.get(CommonConstants.POLICY);
        boolean isActivitySkillEnabled = Boolean.TRUE.equals(taskMap.get("activitySkill"));
        Map<String,Object> permissionType = new HashMap<>();
        permissionType.put(CommonConstants.ACTIVITY,isActivitySkillEnabled);

        UserSkillChannelPublishHelper.getInstance().publishToChannelWithUpdatedPolicy(accessPolicy,userPRO,permissionType);

    }

    public void weeklySkillTaskHelper(Map<String,Object> taskMap){

        UserDTO userPRO = (UserDTO) taskMap.get(CommonConstants.USER_KEY);
        var accessPolicy = (AccessPolicy) taskMap.get(CommonConstants.POLICY);

        String accountID = userPRO.getAccountID();
        String contactID = userPRO.getContactID();
        String timeZone = userPRO.getZoneId();

        boolean isWeeklyDigestEnabled = Boolean.TRUE.equals(taskMap.get("weeklyMail"));

        Map<String,Object> permissionType = new HashMap<>();
        permissionType.put("weeklyDigest",isWeeklyDigestEnabled);

        UserSkillChannelPublishHelper.getInstance().publishToChannelWithUpdatedPolicy(accessPolicy,userPRO,permissionType);

        log.info(" isWeeklyDigestEnabled " + isWeeklyDigestEnabled);

        if (isWeeklyDigestEnabled) {
            log.info(" came to schedule weekly report mail ");
            UserSkillFullMetricHelper.getInstance().weeklyDigestEnabledMetric(accountID);
            FullReminders.scheduleWeeklyReportMailJob(accountID,contactID,timeZone);
        }else{
            UserSkillFullMetricHelper.getInstance().weeklyDigestDisabledMetric(accountID);
            FullReminders.deleteJobs(accountID,contactID,Skillset.SKILL_SET_KEY_WEEKLY_MAIL);
        }
    }

    public void adjustmentSkillTaskHelper(Map<String,Object> taskMap){

        UserDTO userPRO = (UserDTO) taskMap.get(CommonConstants.USER_KEY);
        var accessPolicy = (AccessPolicy) taskMap.get(CommonConstants.POLICY);
        Map<String,Object> permissionType = (Map<String, Object>) taskMap.get("adjustmentSkill");

        UserSkillChannelPublishHelper.getInstance().publishToChannelWithUpdatedPolicy(accessPolicy,userPRO,permissionType);

    }

    public void teamReportSkillTaskHelper(Map<String,Object> taskMap){
        UserDTO userPRO = (UserDTO) taskMap.get(CommonConstants.USER_KEY);
        UserSkillChannelPublishHelper.getInstance().publishToChannelOnSkillSetOperation(userPRO);
    }

    public void confirmHoursSkillTaskHelper(Map<String,Object> taskMap){

        UserDTO userPRO = (UserDTO) taskMap.get(CommonConstants.USER_KEY);
        var accessPolicy = (AccessPolicy) taskMap.get(CommonConstants.POLICY);
        boolean isConfirmHoursSkillEnabled = Boolean.TRUE.equals(taskMap.get("confirmHoursSkill"));
        Map<String,Object> permissionType = new HashMap<>();
        permissionType.put(CONFIRM_HOURS_KEY,isConfirmHoursSkillEnabled);

        UserSkillChannelPublishHelper.getInstance().publishToChannelWithUpdatedPolicy(accessPolicy,userPRO,permissionType);

    }

    public void forceClockOutSkillTaskHelper(Map<String,Object> taskMap){

        boolean isForceClockOutSkillEnabled = Boolean.TRUE.equals(taskMap.get("forceClockOutSkill"));
        Map<String,Object> permissionType = new HashMap<>();
        permissionType.put(FORCE_CLOCKOUT_KEY,isForceClockOutSkillEnabled);
        UserDTO userPRO = (UserDTO) taskMap.get(CommonConstants.USER_KEY);
        var accessPolicy = (AccessPolicy) taskMap.get(CommonConstants.POLICY);

        UserSkillChannelPublishHelper.getInstance().publishToChannelWithUpdatedPolicy(accessPolicy,userPRO,permissionType);

    }

    public void reminderSkillTaskHelper(Map<String,Object> taskMap){

        boolean isReminderSkillEnabled = Boolean.TRUE.equals(taskMap.get("reminderSkill"));
        Map<String,Object> permissionType = new HashMap<>();
        permissionType.put(CLOCK_REMINDER_KEY,isReminderSkillEnabled);
        UserDTO userPRO = (UserDTO) taskMap.get(CommonConstants.USER_KEY);
        var accessPolicy = (AccessPolicy) taskMap.get(CommonConstants.POLICY);
        Map<String, Object>skillMap = (Map<String, Object>) taskMap.get(UserSkillHelper.SKILL_MAP_KEY);
        new InAppReminderUtil().initiateInAppReminderForReminderPermission(userPRO, isReminderSkillEnabled, skillMap);

        UserSkillChannelPublishHelper.getInstance().publishToChannelWithUpdatedPolicy(accessPolicy,userPRO,permissionType);

    }

    public void modifyReminderTimeTaskHelper(Map<String,Object> taskMap){
        UserDTO userPRO = (UserDTO) taskMap.get(CommonConstants.USER_KEY);
        Map<String, Object>skillMap = (Map<String, Object>) taskMap.get(UserSkillHelper.SKILL_MAP_KEY);
        new InAppReminderUtil().handleInAppNotificationForReminderTimeChange(userPRO, skillMap);
        UserSkillChannelPublishHelper.getInstance().publishToChannelOnSkillSetOperation(userPRO);
    }

    public void updateProProfileHelper(Map<String,Object> taskMap){
        UserDTO userPRO = (UserDTO) taskMap.get(CommonConstants.USER_KEY);
        Map<String,Object> updatedProMap = (Map<String, Object>) taskMap.get("updatedProMap");
        boolean isRoleUpdated = (boolean) updatedProMap.get("isRoleUpdated");
        log.info(" isRoleUpdated " + isRoleUpdated);
        if(isRoleUpdated){
            var accessPolicy = (AccessPolicy) updatedProMap.get(CommonConstants.POLICY);
            String role = (String) updatedProMap.get(UserProProfileHelper.ROLE);
            this.publishAndSaveActivityOnUpdateRole(userPRO,accessPolicy,role);
        }else{
            String activity = (String) updatedProMap.get(CommonConstants.ACTIVITY);
            this.publishAndSaveActivityOnUpdateProProfile(userPRO,activity);
        }
    }


    private void publishAndSaveActivityOnUpdateRole(UserDTO userPRO,AccessPolicy accessPolicy, String role){
        log.info(" came to publish and save activity on role update ");
        Map<String,Object> permissionType = new HashMap<>();

        if(role.equalsIgnoreCase(Skillset.ROLE_ADMIN)){
            permissionType.put(UserProProfileHelper.ROLE,IAMPermission.ADMIN.toString());
        }else{
            permissionType.put(UserProProfileHelper.ROLE,"");
        }

        String activityMessage = userPRO.getContactID() + ": Role has been changed to " + role;

        ActivityUtil.saveActivity(userPRO.getAccountID(), userPRO.getContactID(),UPDATE_PRO_KEY,userPRO.getEmailID(),
                activityMessage, ActivityUtil.ACTIVITIES.DUMMY.value(), DateUtil.getCurrentTime());

        UserSkillChannelPublishHelper.getInstance().publishToChannelWithUpdatedPolicy(accessPolicy,userPRO,permissionType);
    }

    private void publishAndSaveActivityOnUpdateProProfile(UserDTO userPRO,String activity){
        log.info(" came to publish and save activity on role update ");
        ActivityUtil.saveActivity(userPRO.getAccountID(), userPRO.getContactID(),UPDATE_PRO_KEY,userPRO.getEmailID(),
                activity, com.yoco.commons.utils.ActivityUtil.ACTIVITIES.DUMMY.value(), DateUtil.getCurrentTime());
        RTMService.publishToChannel(userPRO.getAccountID(),"PRO_Update",CommonConstants.USER_KEY,userPRO);
    }

}