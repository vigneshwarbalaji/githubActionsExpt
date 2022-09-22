package com.yoco.commons.utils.events;

import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.modal.user.UserDTO;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class ClockUtil {

    private ClockUtil(){}

    @NoArgsConstructor
    private enum CLOCK_UTIL_KEYS {

        OUT_SOURCE_ACTIVITY("outSourceActivity"),
        SCHEDULER("scheduler"),
        OUT_SOURCE_USER_DELETE("force-del-web"),
        DELETE_USER("deleteUser"),
        OUT_SOURCE_USER_HOOK_DELETE("force-del-hook");
        private String value;

        CLOCK_UTIL_KEYS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    public static ReportsDTO clockOutHandler(SettingsJDO accountObject, PeopleRelationJDO userObject, Map<String, Object>eventMap, Map<String, Object>headersMap) throws IOException, NoSuchAlgorithmException {

        String srcClientID = (String) headersMap.get("srcClientID");
        String fromWhere = (String) headersMap.get("fromWhere");

        long receivedLongTime = DateUtil.getCurrentTime();
        long clockOutLongTime = eventMap.containsKey("clockOutLongTime") ? (Long)eventMap.get("clockOutLongTime") : 0L;

        if(clockOutLongTime == 0)
            clockOutLongTime = receivedLongTime;

        var reportObject =  new ReportsDTO(eventMap, userObject.getEmailID(), userObject.getTimeZone());

        String sourceValue = reportObject.getOutSource();

        Map<String, Object>outSourceMap = getClockOutSource(srcClientID);
        String outSource = (String) outSourceMap.get("outSource");
        String outSourceActivity = (String) outSourceMap.get(CLOCK_UTIL_KEYS.OUT_SOURCE_ACTIVITY.value());

        generateClockOutPayload(eventMap, clockOutLongTime, outSource);

        Map<String, Object>clockOutPayload = updateEvent(eventMap);

        var updatedReportObject = new ReportsDTO(clockOutPayload, userObject.getEmailID(), userObject.getTimeZone());

        Map<String, Object> clockOutOperationsPayload = new HashMap<>();
        clockOutOperationsPayload.put("source", fromWhere);
        clockOutOperationsPayload.put("sourceClientID", srcClientID);
        clockOutOperationsPayload.put(CLOCK_UTIL_KEYS.OUT_SOURCE_ACTIVITY.value(), outSourceActivity);
        clockOutOperationsPayload.put("action", "clockOut");
        clockOutOperationsPayload.put("settingsJDO", accountObject);
        clockOutOperationsPayload.put("userPRO", userObject);
        clockOutOperationsPayload.put("clockOutObject", updatedReportObject);
        clockOutOperationsPayload.put("receivedLongTime", receivedLongTime);
        clockOutOperationsPayload.put("sourceValue", sourceValue);

        if(!CLOCK_UTIL_KEYS.SCHEDULER.value().equalsIgnoreCase(outSource))
            ClockTaskUtil.getClockTaskUtil().initiateClockOperationsTaskQueue(clockOutOperationsPayload);

        return updatedReportObject;
    }

    public static Map<String, Object> getClockOutSource(String sourceClientID){
        Map<String,Object> outSourceMap = new HashMap<>();

        var outSource = "";
        outSource = ObjUtils.isNullOrEmpty(sourceClientID) ? "" : ClientSource.getClientIdMap().get(sourceClientID);

        if(ObjUtils.isNullOrEmpty(outSource)){
            outSource = "web";
        }

        outSourceMap.put("outSource", outSource);
        outSourceMap.put(CLOCK_UTIL_KEYS.OUT_SOURCE_ACTIVITY.value(), outSource + "_out");

        return outSourceMap;
    }

    public static Map<String, Object> generateClockOutPayload(Map<String, Object>eventMap, long clockOutLongTime, String outSource){

        HashMap<String, Object>metaMap = (HashMap<String, Object>) eventMap.get(SchedulingKeys.METADATA);
        metaMap.put(SchedulingKeys.OUTSOURCE, outSource);
        metaMap.put(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT);

        HashMap<String, Object> queryableMeta = (HashMap<String, Object>) eventMap.get(SchedulingKeys.QUERYABLE_META);
        queryableMeta.put(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT);

        HashMap<String, Object> locationMap = (HashMap<String, Object>) eventMap.get(SchedulingKeys.LOCATION);

        if(!ObjUtils.isNullOrEmpty(locationMap) && locationMap.containsKey(SchedulingKeys.CLOCK_IN_MESSAGE)) {
            HashMap<String, Object> clockInMap =  (HashMap<String,Object>) locationMap.get(SchedulingKeys.CLOCK_IN_MESSAGE);
            locationMap = (HashMap<String, Object>) generateLocationInfo(clockInMap.get(SchedulingKeys.IP).toString(), JsonUtil.getJson(clockInMap.get(SchedulingKeys.INFO)),
                                                                        clockInMap.get(SchedulingKeys.IP).toString(), JsonUtil.getJson(clockInMap.get(SchedulingKeys.INFO)));
        }

        eventMap.put(SchedulingKeys.METADATA , metaMap);
        eventMap.put(SchedulingKeys.QUERYABLE_META , queryableMeta);
        eventMap.put(SchedulingKeys.LOCATION, locationMap);
        eventMap.put(SchedulingKeys.END_TIME, clockOutLongTime);

        return eventMap;
    }

    public static Map<String, Object> generateLocationInfo(String inIp, String clockInMessage, String outIp, String clockOutMessage) {

        HashMap<String, Object> locationMap = new HashMap<>();

        HashMap<String, Object> clockInMsgMap = new HashMap<>();
        HashMap<String, Object> clockOutMsgMap = new HashMap<>();

        if(!ObjUtils.isNullOrEmpty(inIp)){
            clockInMsgMap.put(SchedulingKeys.IP, inIp);
        }

        if(!ObjUtils.isNullOrEmpty(clockInMessage)){
            HashMap<String, Object>clockInfoMap = (HashMap<String, Object>) JsonUtil.convertJsonToMap(clockInMessage);
            clockInMsgMap.put(SchedulingKeys.INFO, clockInfoMap);
        }

        if(!ObjUtils.isNullOrEmpty(clockInMsgMap))
            locationMap.put(SchedulingKeys.CLOCK_IN_MESSAGE, clockInMsgMap);

        if(!ObjUtils.isNullOrEmpty(outIp)){
            clockOutMsgMap.put(SchedulingKeys.IP, outIp);
        }

        if(!ObjUtils.isNullOrEmpty(clockOutMessage)){
            HashMap<String, Object>clockInfoMap = (HashMap<String, Object>) JsonUtil.convertJsonToMap(clockOutMessage);
            clockOutMsgMap.put(SchedulingKeys.INFO, clockInfoMap);
        }

        if(!ObjUtils.isNullOrEmpty(clockOutMsgMap))
            locationMap.put(SchedulingKeys.CLOCK_OUT_MESSAGE, clockOutMsgMap);

        return locationMap;
    }

    public static Map<String, Object> updateEvent (Map<String, Object> eventMap) throws IOException, NoSuchAlgorithmException {

        HashMap<String, Object> dataMap = new HashMap<>();

        dataMap.put(SchedulingKeys.BRAND, eventMap.get(SchedulingKeys.BRAND));
        dataMap.put(SchedulingKeys.MERCHANT, eventMap.get(SchedulingKeys.MERCHANT));
        dataMap.put(SchedulingKeys.ACTION, "EVENT_UPDATE");

        dataMap.put(SchedulingKeys.ID, eventMap.get(SchedulingKeys.ID));

        if(eventMap.containsKey(SchedulingKeys.CALENDAR) && ObjUtils.isNullOrEmpty(eventMap.get(SchedulingKeys.CALENDAR).toString()))
            dataMap.put(SchedulingKeys.CALENDAR, eventMap.get(SchedulingKeys.MERCHANT));
        else
            dataMap.put(SchedulingKeys.CALENDAR, eventMap.get(SchedulingKeys.CALENDAR));

        if(eventMap.containsKey(SchedulingKeys.START_TIME) && (long)eventMap.get(SchedulingKeys.START_TIME)>0l){
            String startDateTime = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, (long)eventMap.get(SchedulingKeys.START_TIME), "UTC");
            dataMap.put(SchedulingKeys.START_DATE_TIME, startDateTime);
        }

        if(eventMap.containsKey(SchedulingKeys.END_TIME) && (long)eventMap.get(SchedulingKeys.END_TIME)>0l){
            String endDateTime = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, (long)eventMap.get(SchedulingKeys.END_TIME), "UTC");
            dataMap.put(SchedulingKeys.END_DATE_TIME, endDateTime);
        }

        dataMap.put(SchedulingKeys.PROVIDER, eventMap.get(SchedulingKeys.PROVIDER));

        if(eventMap.containsKey(SchedulingKeys.PARENT_ID))
            dataMap.put(SchedulingKeys.PARENT_ID, eventMap.get(SchedulingKeys.PARENT_ID));
        else
            dataMap.put(SchedulingKeys.PARENT_ID, "");

        if(eventMap.containsKey(SchedulingKeys.SOURCE))
            dataMap.put(SchedulingKeys.SOURCE, eventMap.get(SchedulingKeys.SOURCE));
        else
            dataMap.put(SchedulingKeys.SOURCE, "web");

        dataMap.put(SchedulingKeys.TYPE, SchedulingKeys.EVENT);

        if(eventMap.containsKey(SchedulingKeys.PAYMENT_STATUS))
            dataMap.put(SchedulingKeys.PAYMENT_STATUS, eventMap.get(SchedulingKeys.PAYMENT_STATUS));

        if(eventMap.containsKey(SchedulingKeys.LOCATION))
            dataMap.put(SchedulingKeys.LOCATION, eventMap.get(SchedulingKeys.LOCATION));

        if(eventMap.containsKey(SchedulingKeys.METADATA))
            dataMap.put(SchedulingKeys.METADATA, eventMap.get(SchedulingKeys.METADATA));

        if(eventMap.containsKey(SchedulingKeys.QUERYABLE_META))
            dataMap.put(SchedulingKeys.QUERYABLE_META, eventMap.get(SchedulingKeys.QUERYABLE_META));

        Map<String, Object> response = SchedulingEngineUtil.updateEventReq(dataMap);

        log.info("response Map :"+ response);

        return eventMap;
    }

    public static void stopRunningEntryForDeletedUser(PeopleRelationJDO userPRO, SettingsJDO account,String source) throws IOException, NoSuchAlgorithmException {
       List<Map<String,Object>> runningEntriesList = ReportImpl.getReportImplInstance().getActiveEntry(userPRO.getUniquepin(),userPRO.getContactId());
       if(!ObjUtils.isNullOrEmpty(runningEntriesList)){
           source = source.equalsIgnoreCase("web") ? CLOCK_UTIL_KEYS.OUT_SOURCE_USER_DELETE.value() : CLOCK_UTIL_KEYS.OUT_SOURCE_USER_HOOK_DELETE.value();
           String currentOutsource = getOutSourceFromSchEvent(runningEntriesList.get(0));
           Map<String,Object> clockOutPayload = generateClockOutPayload(runningEntriesList.get(0),userPRO.getDateModified(),source);
           Map<String,Object> clockOutResponse = updateEvent(clockOutPayload);
           Map<String,Object> clockOutHandlerPayload = new HashMap<>();
           clockOutHandlerPayload.put("source","deleteUser");
           clockOutHandlerPayload.put("outSourceActivity", source);
           clockOutHandlerPayload.put("receivedLongTime",userPRO.getDateModified());
           clockOutHandlerPayload.put("sourceValue",currentOutsource);
           clockOutHandlerPayload.put("userPRO",userPRO);
           clockOutHandlerPayload.put("settingsJDO",account);
           clockOutHandlerPayload.put("clockOutObject",new ReportsDTO(clockOutResponse,userPRO.getEmailID(), userPRO.getTimeZone()));
           ClockTaskUtil.getClockTaskUtil().clockOutTaskHandler(clockOutHandlerPayload);
       }
    }

    public static void stopRunningEntryForAccountDeletion(Map<String,Object> entry, Long accountDeletedTime) throws IOException, NoSuchAlgorithmException {
        Map<String,Object> clockOutPayload = generateClockOutPayload(entry,accountDeletedTime,CLOCK_UTIL_KEYS.OUT_SOURCE_USER_DELETE.value());
        Map<String,Object> clockOutResponse = updateEvent(clockOutPayload);
        var reportsDTO = new ReportsDTO(clockOutResponse,"","");
        ClockMetricUtil.clockOutMetric(reportsDTO.getAccountID(),reportsDTO.getContactID(), CLOCK_UTIL_KEYS.DELETE_USER.value(), reportsDTO);
    }

    public static String getOutSourceFromSchEvent(Map<String,Object> eventMap){
        var sourceValue = "";
        if(!ObjUtils.isNullOrEmpty(eventMap) && eventMap.containsKey(SchedulingKeys.METADATA)){
            Map<String, Object> metaMap = (Map<String, Object>) eventMap.get(SchedulingKeys.METADATA);
            if(!ObjUtils.isNullOrEmpty(metaMap) && metaMap.containsKey(SchedulingKeys.OUTSOURCE)
                    && !ObjUtils.isNullOrEmpty((String) metaMap.get(SchedulingKeys.OUTSOURCE))){
                sourceValue = (String) metaMap.get(SchedulingKeys.OUTSOURCE);
            }
        }
        return sourceValue;
    }

    public static long calculateTotalDurationMillisForEntry(Map<String,Object> entry){
        var duration = 0L;
        if(!ObjUtils.isNullOrEmpty(entry)){
            Long startTimeMillis = (Long)entry.get(SchedulingKeys.START_TIME);
            Long endTimeMillis;
            if(isClockedInEntry(entry)){
                endTimeMillis = DateUtil.getCurrentTime();
            }else{
                endTimeMillis = (Long)entry.get(SchedulingKeys.END_TIME);
            }
            duration = endTimeMillis - startTimeMillis;
        }
        return duration < 0 ? 0 : duration;
    }

    public static long getUserTotalDurationForCurrentDay(UserDTO userPro, Long startTimeMillis) throws IOException, NoSuchAlgorithmException {
        var totalDurationForDay = 0L;
        if(startTimeMillis > 0 ){
            List<Map<String,Object>> entrieslist = new ReportImpl().getEntriesForCurrentDay(userPro);
            for(Map<String,Object> entry : entrieslist){
                totalDurationForDay += calculateTotalDurationMillisForEntry(entry);
            }
        }
        return totalDurationForDay;
    }

    public static boolean isClockedInEntry(Map<String, Object> entry){
        var isEntryRunning = false;
        if(!ObjUtils.isNullOrEmpty(entry)){
            Map<String,Object> metaMap = (Map<String, Object>) entry.get(SchedulingKeys.METADATA);
            if(!ObjUtils.isNullOrEmpty(metaMap)){
                isEntryRunning = REPORT_STATUS.CLOCKED_IN.toString().equalsIgnoreCase((String)metaMap.get(SchedulingKeys.STATUS));
            }
        }
        return isEntryRunning;
    }
}
