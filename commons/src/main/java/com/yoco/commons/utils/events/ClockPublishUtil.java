package com.yoco.commons.utils.events;

import com.yoco.commons.dataservices.impl.IntegrationsImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.IntegrationsJDO;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

@Slf4j
public class ClockPublishUtil {

    private ClockPublishUtil(){}

    @NoArgsConstructor
    private enum CLOCK_PUBLISH_KEYS {

        CLOCK_OPERATIONS_QUEUE("clock-operation"),
        PHOTO_ID("photoID"),
        CLOCK_OUT_WEB,
        DECIMAL_DURATION_FORMAT("#0.00"),
        SLACK_INTEGRATION("slack"),
        YOCO_CHANNEL_PATH("public/yoco");

        private String value;

        CLOCK_PUBLISH_KEYS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    public static void publishClockToChannel(ReportsDTO reportObject, PeopleRelationJDO userPRO, String activity, long receivedlongTime){

        Map<String, Object> mapGDPR = JsonUtil.convertJsonToMap(userPRO.getDataProtectionSkillset());
        Map<String, Object>profileMap = (Map<String, Object>)mapGDPR.get("profile");

        String clockInDate = DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY, reportObject.getInTime(),userPRO.getTimeZone());
        long startTimeOfToday = DateUtil.convertDateTimeTextToMillis(clockInDate + " 00:00:00 AM",
                DateFormats.DD_MMM_YYYY_HH_MM_SS_A,userPRO.getTimeZone());

        Map<String, String>channelPublishPayload = new HashMap<>();
        channelPublishPayload.put("status", reportObject.getStatus());
        channelPublishPayload.put("email", userPRO.getEmailID());
        channelPublishPayload.put("clockInLongTime", String.valueOf(reportObject.getInTime()));
        channelPublishPayload.put("contactId", userPRO.getContactId());
        channelPublishPayload.put("projectId", reportObject.getProjectID());
        channelPublishPayload.put("firstName", userPRO.getContact().getFirstName());
        channelPublishPayload.put("lastName", userPRO.getContact().getLastName());

        if((boolean)profileMap.get(CLOCK_PUBLISH_KEYS.PHOTO_ID.value())){
            channelPublishPayload.put(CLOCK_PUBLISH_KEYS.PHOTO_ID.value(), userPRO.getContact().getPhotoID());
        } else {
            channelPublishPayload.put(CLOCK_PUBLISH_KEYS.PHOTO_ID.value(), "");
        }

        channelPublishPayload.put("uniquepin", userPRO.getUniquepin());
        channelPublishPayload.put("userClockInId", reportObject.getId());
        channelPublishPayload.put("userClockInJDOJson", JsonUtil.getJson(reportObject));

        channelPublishPayload.put("startTimeOfToday", String.valueOf(startTimeOfToday));
        var userObject = new UserDTO(userPRO,new HashSet<>());
        var dataViewPermission = userObject.getData_view_permission();
        dataViewPermission.put("value",JsonUtil.getJson(dataViewPermission));
        userObject.setData_view_permission(dataViewPermission);
        channelPublishPayload.put("objPeopleRelationJDO", JsonUtil.getJson(userObject));

        String channelName = CLOCK_PUBLISH_KEYS.YOCO_CHANNEL_PATH.value() + "/" + userPRO.getUniquepin();
        RTMService.publishToRTMService(channelName, channelPublishPayload);

        if(!ObjUtils.isNullOrEmpty(activity) && !"null_in".equalsIgnoreCase(activity))
            ActivityUtil.saveActivity(reportObject.getAccountID(), reportObject.getContactID(), reportObject.getProjectID(), reportObject.getEmailID(), activity, ActivityUtil.ACTIVITIES.DUMMY.value(), receivedlongTime);
    }

    public static void publishMessageToSlack(String action, ReportsDTO reportObject, String timeFormat, Contact currentUser){

        try{
            String accountID = reportObject.getAccountID();
            String contactID = reportObject.getContactID();
            var message = "";

           if(action.equalsIgnoreCase(CLOCK_PUBLISH_KEYS.CLOCK_OUT_WEB.toString())){
                long outTime = reportObject.getOutTime();
                long inTime = reportObject.getInTime();
                message = getClockOutMessageFormatForSlack(outTime,inTime,currentUser,timeFormat);
            }

            if(!ObjUtils.isNullOrEmpty(message)) {
                HashMap<String, Object> payloadMap = new HashMap<>();
                payloadMap.put("text", message);
                String webHookUrl = getSlackWebHookUrl(accountID,contactID);
                var headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON};
                UrlFetcher.sendPostRequest(webHookUrl, payloadMap, headers, UrlFetcher.getHttpClientInstance());

            }

        }catch (Exception e){
            log.error(" Exception in publishMessageToSlack , " + e.getMessage());
        }
    }

    public static String getClockOutMessageFormatForSlack(long outTime, long inTime, Contact currentUser, String timeFormat){

        String clockOutMessageFormat = "*"+ currentUser.getFirstName() + "* has *Clocked Out* \n";
        var duration = "";
        long totalDurationInMilli = (DateUtil.truncateMillis(outTime) - DateUtil.truncateMillis(inTime));

        if(timeFormat.toLowerCase().equalsIgnoreCase(CLOCK_PUBLISH_KEYS.DECIMAL_DURATION_FORMAT.value())){
            try {
                var decimalFormat = new DecimalFormat("#0.00");
                duration = decimalFormat.format(totalDurationInMilli / (3600000D));
            } catch (Exception e) {
                log.info( "Not able to convert to decimal format" + e.getMessage());
                duration = "0.00";
            }
        }else{
            duration =  DateUtil.getTimeFormatAsPerCompany(totalDurationInMilli, timeFormat);
        }
        duration = duration + "";
        clockOutMessageFormat += "*Session Duration :* "+ duration;

        return clockOutMessageFormat;
    }

    public static String getSlackWebHookUrl(String accountID, String contactID){

        var webHookUrl = "";

        try{

            IntegrationsJDO integrationObject = IntegrationsImpl.getIntegrationsImplInstance().getIntegrationByType(accountID, contactID, "slack");

            if (!ObjUtils.isNull(integrationObject)) {

                String integrationDetails = integrationObject.getIntegrationDetails();

                if (!ObjUtils.isNullOrEmpty(integrationDetails)) {
                    Map<String, Object> integrationDetailsMap = JsonUtil.convertJsonToMap(integrationDetails);
                    webHookUrl = (ObjUtils.isNullOrEmpty(integrationDetailsMap) || !(integrationDetailsMap.containsKey("url"))) ? "" : (String) integrationDetailsMap.get("url");
                }

            }

        }catch (Exception e){
            log.error(" Exception in fetching slack integration details , " + e.getMessage());
        }
        return webHookUrl;
    }


}
