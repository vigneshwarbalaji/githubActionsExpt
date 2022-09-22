package com.yoco.commons.fullservices;

import com.fullreminders.api.client.exception.ApiException;
import com.fullreminders.api.client.model.action.HookAction;
import com.fullreminders.api.client.model.job.JobFetchParams;
import com.fullreminders.api.client.model.job.JobRequest;
import com.fullreminders.api.client.model.response.JobsDeleteApiResponse;
import com.fullreminders.api.client.service.FullRemindersApi;
import com.fullreminders.api.client.service.JobsApi;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.GaeUtils;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;
import java.time.DayOfWeek;
import java.time.LocalDateTime;

@Slf4j
public class FullReminders {

    private FullReminders(){}

    private static final String TYPE = "type";
    private static final String HOOK_ACTION_BUILDER_ID = "hact1";
    public static final int REMINDER_HOUR = 9;

    private static JobsApi getJobReminderInstance(){
        return new FullRemindersApi(CommonAppProperties.getFullReminderApiKey(),GaeUtils.isAppModeLive()).jobsApi();
    }

    public static void createJob(String accountID, String jobTime, String timeZone, String userContactID, String hookUrl, String type,
                          int bufferTime) {
        JobRequest request = JobRequest.builder().acctId(accountID).userId(userContactID)
                .jobTime(jobTime).jobTimeZone(timeZone).bufferSecs(bufferTime)
                .metaData(TYPE, type).queryableMetaKey(TYPE)
                .hookAction(HookAction.builder(HOOK_ACTION_BUILDER_ID).url(hookUrl).build())
                .build();
        try {
            log.info(" request " + request);
            long jobId = getJobReminderInstance().createJob(request);
            log.info(" jobId created ... " + jobId);
        } catch (ApiException e) {
            log.info("Exception in create Job" + e.getMessage());
        }
    }

    public static void deleteJobs(String accountID, String userContactID, String type) {
        JobFetchParams params = JobFetchParams.builder().acctId(accountID).userId(userContactID)
                                .addMetaQuery(TYPE, type).build();
        try {
            JobsDeleteApiResponse result = getJobReminderInstance().deleteJobs(params);
            log.info(" deleted ? " + result.getDeletedJobs());
        } catch (ApiException e) {
            log.info("Exception in deleteJobs " + e.getMessage());
        }
    }

    public static void scheduleWeeklyReportMailJob(String accountID, String contactID, String timeZone){
        var nextMonday = getJobTimeStringForWeeklyMail(timeZone);
        log.info("nextMonday is : " + nextMonday + " in timeZone : " + timeZone);
        String callBackURL = CommonAppProperties.getYoCoDefaultApiUrl() + "/api/v2/account/" + accountID + "/contact/" + contactID + "/sendWeeklyReportMail";
        createJob(accountID,nextMonday,timeZone,contactID,callBackURL,Skillset.SKILL_SET_KEY_WEEKLY_MAIL, 0);
    }

    public static String getJobTimeStringForWeeklyMail(String timeZone){
        LocalDateTime nearestMonday = DateUtil.getNextOrSameDayOfWeek(timeZone, DayOfWeek.MONDAY);
        if(nearestMonday.getHour() >= REMINDER_HOUR) {
            nearestMonday = DateUtil.getNextDayOfWeek(timeZone, DayOfWeek.MONDAY);
        }
        nearestMonday = nearestMonday.withHour(9).minusMinutes(0).minusSeconds(0);
        return String.valueOf(nearestMonday).split("\\." )[0];
    }

    public static int deleteAllJobsForAccount(String accountID){
        JobsApi jobsInstance = getJobReminderInstance();
        var cursor = "";
        var batchDeletionIncomplete = true;
        var totalDeletedJobs = 0;
        try {
            while(batchDeletionIncomplete){
                JobFetchParams params = JobFetchParams.builder().acctId(accountID).build();
                if(!ObjUtils.isNullOrEmpty(cursor)){
                    params.setCursor(cursor);
                }
                JobsDeleteApiResponse result = jobsInstance.deleteJobs(params);
                totalDeletedJobs += result.getDeletedJobs();
                cursor = result.getCursor();
                if(ObjUtils.isNullOrEmpty(cursor)){
                    batchDeletionIncomplete = false;
                }
            }
            return totalDeletedJobs;
        } catch (Exception e) {
            log.error("error while deleting jobs for account :: " + accountID + " :: " + e.getMessage());
            return totalDeletedJobs;
        }
    }

}
