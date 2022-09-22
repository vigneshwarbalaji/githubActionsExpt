package com.yoco.commons.utils.events;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.services.FCMService;
import com.yoco.commons.services.RTMService;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.security.NoSuchAlgorithmException;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

@Slf4j
public class ClockTaskUtil {

    private ClockTaskUtil(){}

    public static ClockTaskUtil getClockTaskUtil() {
        return new ClockTaskUtil();
    }

    @NoArgsConstructor
    private enum CLOCK_TASK_KEYS {

        CLOCK_OPERATIONS_QUEUE("clockoperation"),
        CLOCK_OUT,
        FORCE_CLOCKOUT,
        CLOCK_OUT_WEB,
        ENTRY("entry"),
        FORCE_KEY("force");

        private String value;

        CLOCK_TASK_KEYS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    public static String getClockTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/common/clockTaskHandler";
    }

    public void initiateClockOperationsTaskQueue(Map<String,Object> clockObj) throws IOException {

        var byteOut = new ByteArrayOutputStream();
        var out = new ObjectOutputStream(byteOut);
        out.writeObject(clockObj);

        TaskCreator.createPostTask(CLOCK_TASK_KEYS.CLOCK_OPERATIONS_QUEUE.value(), ClockTaskUtil.getClockTaskCallBackUrl(),byteOut.toByteArray());
    }

    public void clockOperationsTaskHandler(Map<String,Object> payloadMap){

        String action = (String) payloadMap.get("action");
        log.info(" action " + action);

        if("clockOut".equalsIgnoreCase(action)){
            this.clockOutTaskHandler(payloadMap);
        }
    }

    public void clockOutTaskHandler(Map<String,Object> payloadMap) {

        try {
            String source = (String) payloadMap.get("source");
            String srcClientID = (String) payloadMap.get("sourceClientID");
            String outSourceActivity = (String) payloadMap.get("outSourceActivity");
            var receivedLongTime = (long) payloadMap.get("receivedLongTime");
            PeopleRelationJDO userObject = (PeopleRelationJDO) payloadMap.get("userPRO");
            SettingsJDO accountObject = (SettingsJDO) payloadMap.get("settingsJDO");
            ReportsDTO reportObject = (ReportsDTO) payloadMap.get("clockOutObject");
            String sourceValue = (String) payloadMap.get("sourceValue");

            var currentUser = userObject.getContact();

            String action = CLOCK_TASK_KEYS.FORCE_KEY.value().equalsIgnoreCase(source) ?
                    CLOCK_TASK_KEYS.FORCE_CLOCKOUT.toString() : CLOCK_TASK_KEYS.CLOCK_OUT.toString();

            ClockPublishUtil.publishClockToChannel(reportObject, userObject, outSourceActivity, receivedLongTime);

            ClockPublishUtil.publishMessageToSlack(CLOCK_TASK_KEYS.CLOCK_OUT_WEB.toString(), reportObject, accountObject.getDisplayTimeFormat(), currentUser);

            Set<String> contactIDSet = new HashSet<>();
            contactIDSet.add(reportObject.getContactID());

            FCMService.getFCMService().notifyFCM(reportObject.getAccountID(), contactIDSet, "clockInOut", true, null, srcClientID);

            if (userObject.isDefault())
                RTMService.publishToAW(userObject.getUniquepin(), userObject.getContactId(), action, CLOCK_TASK_KEYS.ENTRY.value(), reportObject);

            ClockMetricUtil.clockOutMetric(userObject.getUniquepin(),userObject.getContactId() , source, reportObject);

            new InAppReminderUtil().handleInAppNotificationForClockOut(accountObject, userObject, sourceValue);

        } catch (IOException | NoSuchAlgorithmException e) {
        log.info(" exception in clockOutTaskHandler" + e.getMessage());
        }
    }

}
