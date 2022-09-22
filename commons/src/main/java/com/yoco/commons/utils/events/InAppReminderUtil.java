package com.yoco.commons.utils.events;

import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.fullservices.FullReminders;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.utils.AccountUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.modal.user.UserDTO;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@NoArgsConstructor
public class InAppReminderUtil {

    public static InAppReminderUtil getInstance(){
        return new InAppReminderUtil();
    }

    private static final String EXCESS_REMINDER_META_QUERY_KEY = "clock";
    private static final String REMINDER_TIME = "reminderTime";
    private static final int DEFAULT_REMINDER_TIME = 8;

    public void handleInAppNotificationForClockOut(SettingsJDO settingsJDO, PeopleRelationJDO userPRO, String pomodoroStatus){
        String accountID = userPRO.getUniquepin();
        String contactID = userPRO.getContactId();

        if(AccountUtil.isActiveEnterpriseAccount(settingsJDO) && UserPROUtil.isUserProActive(userPRO)
                && new AccessManager().hasReminderPermission(userPRO)){
            FullReminders.deleteJobs(accountID,contactID,EXCESS_REMINDER_META_QUERY_KEY);
        }

        if(ReportsUtil.POMODORO.equalsIgnoreCase(pomodoroStatus)){
            FullReminders.deleteJobs(accountID,contactID,ReportsUtil.POMODORO);
        }
    }

    public void handleInAppNotificationForReminderTimeChange(UserDTO userPRO , Map<String, Object> skillMap){

        try{
        if(skillMap.containsKey(IAMPermission.REMINDER.toString())){
            List<Map<String, Object>> activeEntriesList = ReportImpl.getReportImplInstance().getActiveEntry(userPRO.getAccountID(), userPRO.getContactID());
            if (!ObjUtils.isNullOrEmpty(activeEntriesList) && !ObjUtils.isNullOrEmpty(activeEntriesList.get(0))) {
                var activeEntryMap= activeEntriesList.get(0);
                FullReminders.deleteJobs(userPRO.getAccountID(), userPRO.getContactID(), EXCESS_REMINDER_META_QUERY_KEY);
                long startTimeMillis = (Long)activeEntryMap.get(SchedulingKeys.START_TIME);
                calculateAndSetReminder(userPRO, startTimeMillis, skillMap);
            }
        }

        }catch (Exception exception){
            log.info("exception in handleInAppNotificationForReminderTimeChange : "+ exception.getMessage());
        }

    }

    public void initiateInAppReminderForReminderPermission(UserDTO userPro, Boolean isReminderSkillEnabled, Map<String, Object>skillMap){
     try{
        if(Boolean.TRUE.equals(isReminderSkillEnabled)){
            List<Map<String, Object>> activeEntriesList = ReportImpl.getReportImplInstance().getActiveEntry(userPro.getAccountID(), userPro.getContactID());
            if (!ObjUtils.isNullOrEmpty(activeEntriesList)) {
                var activeEntryMap = ObjUtils.isNullOrEmpty(activeEntriesList.get(0)) ? new HashMap<String, Object>() :
                        activeEntriesList.get(0);
                long startTimeMillis = (Long)activeEntryMap.get(SchedulingKeys.START_TIME);
                calculateAndSetReminder(userPro, startTimeMillis, skillMap);
            }
        }else{
            FullReminders.deleteJobs(userPro.getAccountID(),userPro.getContactID(),EXCESS_REMINDER_META_QUERY_KEY);
        }
        }catch (Exception exception){
            log.info("exception in handleInAppNotificationForReminderTimeChange : "+ exception.getMessage());
        }
    }


    public void calculateAndSetReminder(UserDTO userPro, Long startTimeMillis, Map<String, Object> skillMap) throws IOException, NoSuchAlgorithmException {
        long reminderTimeDuration = getReminderTimeInMillis(skillMap);
        long userTotalDuration = ClockUtil.getUserTotalDurationForCurrentDay(userPro, startTimeMillis);
        if(userTotalDuration < reminderTimeDuration){
            long reminderSettingTimeMillis = DateUtil.getCurrentTime() + (reminderTimeDuration - userTotalDuration);
            String scheduledDateTime = DateUtil.convertMillisToDateTimeText(DateFormats.YYYY_MM_DD_T_HH_MM_SS,reminderSettingTimeMillis, userPro.getZoneId());
            String callBackURL = CommonAppProperties.getYoCoDefaultApiUrl() + "/api/v2/account/" + userPro.getAccountID() + "/contact/" + userPro.getContactID() + "/clock/remind";
            FullReminders.createJob(userPro.getAccountID(), scheduledDateTime, userPro.getTimezone(), userPro.getContactID(), callBackURL, EXCESS_REMINDER_META_QUERY_KEY, 0);
        }
    }

    public long getReminderTimeInMillis(Map<String, Object> skillMap){
        Integer reminderTimeHours = (Integer)skillMap.get(REMINDER_TIME);
        if(ObjUtils.isNull(reminderTimeHours) || reminderTimeHours < 1){
            reminderTimeHours = DEFAULT_REMINDER_TIME;
        }
        return reminderTimeHours * 60 * 60 * 1000L;
    }


    private boolean doesAdjustmentAffectCurrentDayTotal(Long originalClockInLongTimeMillis, Long adjustedClockInLongTimeMillis, String timezone){
        return DateUtil.isCurrentDay(originalClockInLongTimeMillis,timezone) || DateUtil.isCurrentDay(adjustedClockInLongTimeMillis, timezone);
    }

    public void handleInAppNotificationForAdjustment(AdjustmentDTO adjustmentDTO, PeopleRelationJDO userPRO, String zone){
        if(this.doesAdjustmentAffectCurrentDayTotal(adjustmentDTO.getOriginalInTime(),adjustmentDTO.getAdjustedInTime(), zone)){
            this.handleInAppNotificationForReminderTimeChange(new UserDTO(userPRO,null), UserPROUtil.extractPROSkills(userPRO));
        }
    }

}
