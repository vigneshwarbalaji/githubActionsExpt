package com.yoco.commons.utils.events;

import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.PayrollStatus;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import static com.yoco.commons.constants.SchedulingKeys.*;

public class ReportsUtil {
    private ReportsUtil(){}

    public static final String POMODORO = "pomodoro";

    public static boolean isRunningEntry(Map<String,Object> eventMap){
        return isEventStatusSameAsGivenStatus(eventMap, REPORT_STATUS.CLOCKED_IN.toString());
    }

    public static String getPayrollStatus(Map<String,Object> eventMap){
        if(ObjUtils.isNullOrEmpty(eventMap) ){
            return "";
        }
        String payrollStatus = (String)eventMap.get(SchedulingKeys.PAYMENT_STATUS);
        return ObjUtils.isNullOrEmpty(payrollStatus) ? "" : payrollStatus.trim();
    }

    public static String getEventStatus(Map<String,Object> eventMap){
        if(ObjUtils.isNullOrEmpty(eventMap) ){
            return "";
        }
        Map<String, Object> metaMap = (Map<String, Object>) eventMap.get(SchedulingKeys.METADATA);
        if(ObjUtils.isNullOrEmpty(metaMap) ){
            return "";
        }
        String status = (String)metaMap.get(SchedulingKeys.STATUS);
        return ObjUtils.isNullOrEmpty(status) ? "" : status.trim();
    }

    public static boolean isStatusManualClockedOut(Map<String,Object> eventMap){
        return isEventStatusSameAsGivenStatus(eventMap, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString());
    }

    public static boolean isStatusPending(Map<String,Object> eventMap){
        return isEventStatusSameAsGivenStatus(eventMap, REPORT_STATUS.PENDING.toString());
    }

    public static boolean isEventStatusSameAsGivenStatus(Map<String,Object> eventMap, String status){
        if(ObjUtils.isNullOrEmpty(status)){
            return false;
        }
        return status.equalsIgnoreCase(getEventStatus(eventMap));
    }

    public static boolean isPayrollStatusSameAsGivenStatus(Map<String,Object> eventMap, String payrollStatus){
        if(ObjUtils.isNullOrEmpty(payrollStatus)){
            return false;
        }
        return payrollStatus.equalsIgnoreCase(getPayrollStatus(eventMap));
    }

    public static boolean isPayrollStatusDefaultPayroll(Map<String,Object> eventMap){
        return isPayrollStatusSameAsGivenStatus(eventMap, PayrollStatus.DEFAULT_PAYROLL.toString());
    }

    public static boolean isPayrollStatusUserAcknowledged(Map<String,Object> eventMap){
        return isPayrollStatusSameAsGivenStatus(eventMap, PayrollStatus.USER_ACKNOWLEDGED.toString());
    }

    public static boolean isPayrollStatusAdminUpdated(Map<String,Object> eventMap){
        return isPayrollStatusSameAsGivenStatus(eventMap, PayrollStatus.ADMIN_UPDATED.toString());
    }

    public static String getContactIDFromEvent(Map<String, Object> event){
        var contactID = "";
        if(!ObjUtils.isNullOrEmpty(event) && !ObjUtils.isNullOrEmpty((List<String>) event.get(PROVIDER))){
            contactID = ((List<String>) event.get(PROVIDER)).get(0);
        }
        return contactID;
    }

    public static boolean isEventDeleted(Map<String,Object> eventMap){
        if(ObjUtils.isNullOrEmpty(eventMap)){
            return true;
        }
        return Boolean.TRUE.equals(eventMap.get(SchedulingKeys.IS_DELETED));
    }

    public static String getAccountIDFromEvent(Map<String, Object> eventMap) {
        var accountID = "";
        if(!ObjUtils.isNullOrEmpty(eventMap) && !ObjUtils.isNullOrEmpty((String)eventMap.get(MERCHANT))){
            accountID = (String)eventMap.get(MERCHANT);
        }
        return accountID;
    }

    public static String getIDFromEvent(Map<String, Object> eventMap) {
        var id = "";
        if(!ObjUtils.isNullOrEmpty(eventMap) && !ObjUtils.isNullOrEmpty((String)eventMap.get(ID))){
            id = (String)eventMap.get(ID);
        }
        return id;
    }

    public static long getUpdatedTimeFromEvent(Map<String, Object> eventMap) {
        var updatedTime = 0L;
        if(!ObjUtils.isNullOrEmpty(eventMap) && !ObjUtils.isNull(eventMap.get(UPDATED_TIME))){
            updatedTime = (Long)eventMap.get(UPDATED_TIME);
        }
        return updatedTime;
    }

    public static List<String> getParentIdsListFromEvents(List<Map<String,Object>> eventsList){
        if(ObjUtils.isNullOrEmpty(eventsList)){
            return new ArrayList<>();
        }
        return eventsList.stream().map(ReportsUtil::getParentIDFromEvent).filter(parentID -> !ObjUtils.isNullOrEmpty(parentID)).collect(Collectors.toList());
    }

    public static List<String> getIdsListFromEvents(List<Map<String,Object>> eventsList){
        if(ObjUtils.isNullOrEmpty(eventsList)){
            return new ArrayList<>();
        }
        return eventsList.stream().map(ReportsUtil::getIDFromEvent).filter(id -> !ObjUtils.isNullOrEmpty(id)).collect(Collectors.toList());
    }

    public static String getParentIDFromEvent(Map<String,Object> event){
        String parentID = "";
        if(!ObjUtils.isNullOrEmpty(event) && !ObjUtils.isNull(event.get(PARENT_ID))){
            parentID = (String)event.get(PARENT_ID);
        }
        return parentID;
    }

    public static boolean isAdjustmentNew(Map<String, Object> event){
        return ObjUtils.isNullOrEmpty(getParentIDFromEvent(event));
    }

    public static String getZuluStartDateTimeOfEvent(Map<String,Object> event){
        String startDateTime = "";
        if(!ObjUtils.isNullOrEmpty(event) && !ObjUtils.isNull(event.get(START_DATE_TIME))){
            startDateTime = (String)event.get(START_DATE_TIME);
        }
        return startDateTime;
    }

    public static String getZuluEndDateTimeOfEvent(Map<String,Object> event){
        String endDateTime = "";
        if(!ObjUtils.isNullOrEmpty(event) && !ObjUtils.isNull(event.get(END_DATE_TIME))){
            endDateTime = (String)event.get(END_DATE_TIME);
        }
        return endDateTime;
    }

    public static Long getStartMillisOfEvent(Map<String,Object> event){
        Long startMillis = 0l;
        if(!ObjUtils.isNullOrEmpty(event) && !ObjUtils.isNull(event.get(START_TIME))){
            startMillis = (Long) event.get(START_TIME);
        }
        return startMillis;
    }

    public static Long getEndMillisOfEvent(Map<String,Object> event){
        Long endMillis = 0l;
        if(!ObjUtils.isNullOrEmpty(event) && !ObjUtils.isNull(event.get(END_TIME))){
            endMillis = (Long) event.get(END_TIME);
        }
        return endMillis;
    }

    public static List<Map<String,Object>> getOverlappingEntriesForEvent(Map<String,Object> originalEvent) throws IOException, NoSuchAlgorithmException {
        if(ObjUtils.isNullOrEmpty(originalEvent)){
            return List.of();
        }
        List<Map<String,Object>> overlappingEvents = ReportImpl.getReportImplInstance().getEntries(ReportsUtil.getAccountIDFromEvent(originalEvent),ReportsUtil.getContactIDFromEvent(originalEvent),
                ReportsUtil.getZuluStartDateTimeOfEvent(originalEvent),ReportsUtil.getZuluEndDateTimeOfEvent(originalEvent), SchedulingKeys.DESC,false);
        String originalEventID = ReportsUtil.getIDFromEvent(originalEvent);
        return overlappingEvents.stream().filter(overlappingEvent -> !ReportsUtil.getIDFromEvent(overlappingEvent).equals(originalEventID)).collect(Collectors.toList());
    }


    public static Map<String,Object> getParentEventFromEvent(Map<String, Object> eventMap) throws IOException, NoSuchAlgorithmException {
        String parentID = getParentIDFromEvent(eventMap);
        if(!ObjUtils.isNullOrEmpty(parentID)) {
            Map<String,Object> resp =  SchedulingEngineUtil.getEventByID(parentID);
            if(!ObjUtils.isNullOrEmpty(resp) && Boolean.TRUE.equals(resp.get(SchedulingKeys.RESPONSE))){
                return resp.containsKey(SchedulingKeys.DATA) ? (Map<String, Object>) resp.get(SchedulingKeys.DATA) : Map.of();
            }
        }
        return Map.of();
    }

    public static Map<String,Object> getMetaDataMap(Map<String, Object> event){
        Map<String,Object> metaData = new HashMap<>();
        if(!ObjUtils.isNullOrEmpty(event) && !ObjUtils.isNull(event.get(METADATA))){
            metaData = (Map<String,Object>)event.get(METADATA);
        }
        return metaData;
    }

    public static Map<String,Object> getQueryableMetaMap(Map<String, Object> event){
        Map<String,Object> metaData = new HashMap<>();
        if(!ObjUtils.isNullOrEmpty(event) && !ObjUtils.isNull(event.get(QUERYABLE_META))){
            metaData = (Map<String,Object>)event.get(QUERYABLE_META);
        }
        return metaData;
    }

    public static PeopleRelationJDO extractUserProFromEvent(Map<String,Object> event){
        String userContactID = getContactIDFromEvent(event);
        String accountID = getAccountIDFromEvent(event);
        return UserPROUtil.getUserProWithContact(accountID,userContactID);
    }
}
