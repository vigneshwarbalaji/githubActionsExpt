package com.yoco.user.helper.skill;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.fullservices.FullReminders;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.constants.CommonConstants;
import com.yoco.user.helper.UserProProfileHelper;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class UserSkillTaskHelperTest {

    UserSkillTaskHelper userSkillTaskHelper = UserSkillTaskHelper.getInstance();


    @Test
    void updateProTaskHelper_weeklySkill_disabled_test(){
        try(MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class);
            MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
            MockedStatic<UserSkillFullMetricHelper> userFullMetricHelperMockedStatic = Mockito.mockStatic(UserSkillFullMetricHelper.class)){

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelWithUpdatedPolicy(any(AccessPolicy.class),any(UserDTO.class),anyMap());
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            fullRemindersMockedStatic.when(() -> FullReminders.deleteJobs(anyString(),anyString(),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            UserSkillFullMetricHelper userSkillFullMetricHelper = Mockito.mock(UserSkillFullMetricHelper.class);
            Mockito.doNothing().when(userSkillFullMetricHelper).weeklyDigestDisabledMetric(anyString());
            userFullMetricHelperMockedStatic.when(UserSkillFullMetricHelper::getInstance).thenReturn(userSkillFullMetricHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setTimezone(DateConstants.ZONE_ID_IST);

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put("action","weeklySkill");
            taskMap.put("user",userDTO);
            taskMap.put("policy",accessPolicy);
            taskMap.put("weeklyMail",false);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs("accountId","contactId", Skillset.SKILL_SET_KEY_WEEKLY_MAIL),times(1));

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("weeklyDigest",false);
            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelWithUpdatedPolicy(accessPolicy,userDTO,permissionType);
            Mockito.verify(userSkillFullMetricHelper,times(1)).weeklyDigestDisabledMetric("accountId");
        }
    }

    @Test
    void updateProTaskHelper_weeklySkill_enabled_test(){
        try(MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class);
            MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
            MockedStatic<UserSkillFullMetricHelper> userFullMetricHelperMockedStatic = Mockito.mockStatic(UserSkillFullMetricHelper.class)){

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelWithUpdatedPolicy(any(AccessPolicy.class),any(UserDTO.class),anyMap());
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            fullRemindersMockedStatic.when(() -> FullReminders.scheduleWeeklyReportMailJob(anyString(),anyString(),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            UserSkillFullMetricHelper userSkillFullMetricHelper = Mockito.mock(UserSkillFullMetricHelper.class);
            Mockito.doNothing().when(userSkillFullMetricHelper).weeklyDigestEnabledMetric(anyString());
            userFullMetricHelperMockedStatic.when(UserSkillFullMetricHelper::getInstance).thenReturn(userSkillFullMetricHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put("action","weeklySkill");
            taskMap.put("user",userDTO);
            taskMap.put("policy",accessPolicy);
            taskMap.put("weeklyMail",true);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            fullRemindersMockedStatic.verify(()-> FullReminders.scheduleWeeklyReportMailJob("accountId","contactId", DateConstants.ZONE_ID_IST),times(1));

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("weeklyDigest",true);
            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelWithUpdatedPolicy(accessPolicy,userDTO,permissionType);
            Mockito.verify(userSkillFullMetricHelper,times(1)).weeklyDigestEnabledMetric("accountId");
        }
    }

    @Test
    void updateProTaskHelper_clockSkill_test(){
        try(MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class)){

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelWithUpdatedPolicy(any(AccessPolicy.class),any(UserDTO.class),anyMap());
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put("action","clockSkill");
            taskMap.put("user",userDTO);
            taskMap.put("policy",accessPolicy);
            taskMap.put("clockSkill",true);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("clock",true);
            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelWithUpdatedPolicy(accessPolicy,userDTO,permissionType);
        }
    }

    @Test
    void updateProTaskHelper_forceClockOutSkill_test(){
        try(MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class)){

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelWithUpdatedPolicy(any(AccessPolicy.class),any(UserDTO.class),anyMap());
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put("action","forceClockOutSkill");
            taskMap.put("user",userDTO);
            taskMap.put("policy",accessPolicy);
            taskMap.put("forceClockOutSkill",true);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("forceClockOut",true);
            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelWithUpdatedPolicy(accessPolicy,userDTO,permissionType);
        }
    }

    @Test
    void updateProTaskHelper_reportSkill_test(){
        try(MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class)){

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelWithUpdatedPolicy(any(AccessPolicy.class),any(UserDTO.class),anyMap());
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("reportSkill",true);

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put("action","reportSkill");
            taskMap.put("user",userDTO);
            taskMap.put("policy",accessPolicy);
            taskMap.put("reportSkill",permissionType);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelWithUpdatedPolicy(accessPolicy,userDTO,permissionType);
        }
    }

    @Test
    void updateProTaskHelper_adjustmentSkill_test(){
        try(MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class)){

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelWithUpdatedPolicy(any(AccessPolicy.class),any(UserDTO.class),anyMap());
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("adjustmentSkill",true);

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put("action","adjustmentSkill");
            taskMap.put("user",userDTO);
            taskMap.put("policy",accessPolicy);
            taskMap.put("adjustmentSkill",permissionType);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelWithUpdatedPolicy(accessPolicy,userDTO,permissionType);
        }
    }

    @Test
    void updateProTaskHelper_activitySkill_test(){
        try(MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class)){

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelWithUpdatedPolicy(any(AccessPolicy.class),any(UserDTO.class),anyMap());
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put("action","activitySkill");
            taskMap.put("user",userDTO);
            taskMap.put("policy",accessPolicy);
            taskMap.put("activitySkill",true);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("activity",true);
            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelWithUpdatedPolicy(accessPolicy,userDTO,permissionType);
        }
    }

    @Test
    void updateProTaskHelper_confirmHoursSkill_test(){
        try(MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class)){

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelWithUpdatedPolicy(any(AccessPolicy.class),any(UserDTO.class),anyMap());
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put("action","confirmHoursSkill");
            taskMap.put("user",userDTO);
            taskMap.put("policy",accessPolicy);
            taskMap.put("confirmHoursSkill",true);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("confirmHours",true);
            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelWithUpdatedPolicy(accessPolicy,userDTO,permissionType);
        }
    }

    @Test
    void updateProTaskHelper_teamReportSkill_test(){
        try(MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class)){

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelOnSkillSetOperation(any(UserDTO.class));
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);

            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"teamReportSkill");
            taskMap.put(CommonConstants.USER_KEY,userDTO);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelOnSkillSetOperation(userDTO);
        }
    }

    @Test
    void updateProTaskHelper_reminderSkill_test(){
        try(MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class)){

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelWithUpdatedPolicy(any(),any(UserDTO.class),anyMap());
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            AccessPolicy accessPolicy = new AccessPolicy();

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);

            Map<String,Object> skillMap = new HashMap<>();

            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"reminderSkill");
            taskMap.put(CommonConstants.USER_KEY,userDTO);
            taskMap.put("reminderSkill", true);
            taskMap.put(UserSkillHelper.SKILL_MAP_KEY, skillMap);
            taskMap.put(CommonConstants.POLICY,accessPolicy);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("clockReminder",true);
            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelWithUpdatedPolicy(accessPolicy, userDTO, permissionType);
        }
    }

    @Test
    void updateProTaskHelper_modifyReminderTime_test(){
        try(MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class)){

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelOnSkillSetOperation(any(UserDTO.class));
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);

            Map<String,Object> skillMap = new HashMap<>();

            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION, "modifyReminderTime");
            taskMap.put(UserSkillHelper.SKILL_MAP_KEY, skillMap);
            taskMap.put(CommonConstants.USER_KEY,userDTO);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelOnSkillSetOperation(userDTO);
        }
    }

    @Test
    void updateProTaskHelper_defaultCase_test(){
        try( MockedStatic<UserSkillFullMetricHelper> userFullMetricHelperMockedStatic = Mockito.mockStatic(UserSkillFullMetricHelper.class)){
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put("action","dummy");
            userSkillTaskHelper.updateProTaskHelper(taskMap);
            userFullMetricHelperMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void updateRole_staffRole_test(){
        try( MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class)){

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>)invocation -> null);

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelWithUpdatedPolicy(any(AccessPolicy.class),any(UserDTO.class),anyMap());
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setEmailID("test@gmail.com");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);

            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put("action","updateProProfile");
            taskMap.put("user",userDTO);

            Map<String,Object> updatedMap = new HashMap<>();
            updatedMap.put("policy",null);
            updatedMap.put("isRoleUpdated",true);
            updatedMap.put(UserProProfileHelper.ROLE,Skillset.ROLE_STAFF);

            taskMap.put("updatedProMap",updatedMap);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put(UserProProfileHelper.ROLE,"");
            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelWithUpdatedPolicy(null,userDTO,permissionType);
            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),anyString(),anyString(),anyLong()));
        }
    }


    @Test
    void updateRole_admin_test(){
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserSkillChannelPublishHelper> userChannelPublishHelperMockedStatic = Mockito.mockStatic(UserSkillChannelPublishHelper.class)){

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>)invocation -> null);

            UserSkillChannelPublishHelper userSkillChannelPublishHelper = Mockito.mock(UserSkillChannelPublishHelper.class);
            Mockito.doNothing().when(userSkillChannelPublishHelper).publishToChannelWithUpdatedPolicy(any(AccessPolicy.class),any(UserDTO.class),anyMap());
            userChannelPublishHelperMockedStatic.when(UserSkillChannelPublishHelper::getInstance).thenReturn(userSkillChannelPublishHelper);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setEmailID("test@gmail.com");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);

            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put("action","updateProProfile");
            taskMap.put("user",userDTO);

            Map<String,Object> updatedMap = new HashMap<>();
            updatedMap.put("policy",accessPolicy);
            updatedMap.put("isRoleUpdated",true);
            updatedMap.put(UserProProfileHelper.ROLE,Skillset.ROLE_ADMIN);
            taskMap.put("updatedProMap",updatedMap);

            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put(UserProProfileHelper.ROLE,IAMPermission.ADMIN.toString());
            Mockito.verify(userSkillChannelPublishHelper,times(1)).publishToChannelWithUpdatedPolicy(accessPolicy,userDTO,permissionType);
            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),anyString(),anyString(),anyLong()));
        }
    }

    @Test
    void publishAndSaveActivityOnUpdateProProfile_test(){
        try( MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>)invocation -> null);

            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(),anyString(),anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");
            userDTO.setTimezone("(GMT+05:30) Asia/Kolkata (India Time)");
            userDTO.setZoneId(DateConstants.ZONE_ID_IST);
            userDTO.setEmailID("test@gmail.com");

            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put("action","updateProProfile");
            taskMap.put("user",userDTO);
            Map<String,Object> updatedMap = new HashMap<>();
            updatedMap.put("isRoleUpdated",false);
            updatedMap.put("activity","skill updated");
            taskMap.put("updatedProMap",updatedMap);
            new UserSkillTaskHelper().updateProTaskHelper(taskMap);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("accountId", "PRO_Update",
                    CommonConstants.USER_KEY,userDTO));

            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),anyString(),anyString(),anyLong()));
        }
    }

}