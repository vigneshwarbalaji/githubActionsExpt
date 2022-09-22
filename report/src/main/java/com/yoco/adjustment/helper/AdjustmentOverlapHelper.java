package com.yoco.adjustment.helper;

import com.google.common.collect.Lists;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.AdjustmentImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.ProjectUtil;
import com.yoco.enums.ADJUSTMENTS_ERROR_RESPONSE;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class AdjustmentOverlapHelper {

    private AdjustmentOverlapHelper(){}

    private static final String OVERLAPPING_ENTRY = "overlappingEntry";
    private static final String OVERLAPPING_ENTRY_OBJ = "overlappingEntryObj";
    private static final String OVERLAPPING_ENTRY_ID = "overlappingEntryID";
    private static final String OVERLAPPING_INFO = "overlappingInfo";
    private static final String OVERLAPPING_ENTRY_TYPE = "overlappingEntryType";


    private static List<String> getCalenderIds(String accountID, String contactID){
        List<String> calendarIds = ProjectUtil.getAllProjectIdsAssociatedToUser(accountID, contactID);
        calendarIds.add(accountID); // this is for entries which are not tagged with any project
        return calendarIds;
    }

    public static Map<String,Object> validateAndExtractOverlapEntry(String accountID,String contactID,Long startTime,Long endTime,String timezone,String entryIdToExclude) throws IOException, NoSuchAlgorithmException {
        Map<String,Object> responseMap = validateAndExtractOverlapEntryFromRunningEntry(accountID,contactID,endTime);
        if(ObjUtils.isNullOrEmpty(responseMap)){
            return validateAndExtractOverlapEntryFromClockedOutEntries(accountID,contactID,startTime,endTime,entryIdToExclude,timezone);
        }
        return responseMap;
    }

    public static Map<String,Object> validateAndExtractOverlapEntryFromRunningEntry(String accountID, String contactID, Long endTime) throws IOException, NoSuchAlgorithmException {
        List<Map<String,Object>> runningEntriesList = ReportImpl.getReportImplInstance().getActiveEntry(accountID,contactID);
        if(!ObjUtils.isNullOrEmpty(runningEntriesList)){
            Map<String,Object> clockedInMap =  runningEntriesList.get(0);
            if(!ObjUtils.isNullOrEmpty(clockedInMap) && endTime > (long) clockedInMap.get(SchedulingKeys.START_TIME)) {
                return clockedInMap;
            }
        }
        return new HashMap<>();
    }

    public static Map<String,Object> validateAndExtractOverlapEntryFromClockedOutEntries(String accountID, String contactID, Long startTimeMillis,
                                                                                         Long endTimeMillis, String entryIdToExclude, String timezone) throws IOException, NoSuchAlgorithmException {
        List<Map<String,Object>> entriesList = new ArrayList<>();

        int limit = 100;
        List<String> projectIds = getCalenderIds(accountID,contactID);
        List<List<String>> partitionedList = Lists.partition(projectIds, limit);

        String startTime = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU,startTimeMillis,timezone);
        String endTime = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU,endTimeMillis,timezone);

        AdjustmentImpl adjustmentImpl = AdjustmentImpl.getAdjustmentImplInstance();

        for(var calendarIds : partitionedList){
            Map<String,Object> schResp = adjustmentImpl.getOverLapEntries(accountID,calendarIds,contactID,startTime,endTime,entryIdToExclude);
            Map<String, Object> overlapEntry = extractOverLapEntryFromResponse(schResp);
            if(!ObjUtils.isNullOrEmpty(overlapEntry)){
                entriesList.add(overlapEntry);
            }
        }

        return extractSortedOverlappedEntry(entriesList);
    }

    private static Map<String,Object> extractSortedOverlappedEntry(List<Map<String,Object>> entriesList){
        if(ObjUtils.isNullOrEmpty(entriesList)){
            return new HashMap<>();
        }else{
            entriesList.sort( (entry1, entry2) -> (int) ((Long) entry1.get(SchedulingKeys.START_TIME) - (Long) entry2.get(SchedulingKeys.START_TIME)));
            return entriesList.get(0);
        }
    }

    public static Map<String,Object> extractOverLapEntryFromResponse(Map<String,Object> schResp){
        Map<String,Object> overlapEntry = new HashMap<>();
        if(!ObjUtils.isNullOrEmpty(schResp) && schResp.containsKey(SchedulingKeys.DATA)) {
            schResp = (Map<String, Object>) schResp.get(SchedulingKeys.DATA);
            if(!ObjUtils.isNullOrEmpty(schResp) && schResp.containsKey(SchedulingKeys.EVENTS)) {
                List<Map<String, Object>>  overLappingEntries = (ArrayList) schResp.get(SchedulingKeys.EVENTS);
                return overLappingEntries.isEmpty() ? overlapEntry : overLappingEntries.get(0);
            }
        }
        return overlapEntry;
    }


    public static Map<String,Object> extractOverLapEntryDetails(Map<String, Object> entry, String timeZone){

        Map<String, Object> responseMap = new HashMap<>();
        Map<String, Object> metaMap = (HashMap<String, Object>) entry.get(SchedulingKeys.METADATA);

        String status = metaMap.get(SchedulingKeys.STATUS).toString();
        long startMillis = (long)entry.get(SchedulingKeys.START_TIME);
        long endMillis = (long)entry.get(SchedulingKeys.END_TIME);

        String startDate = DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY, startMillis, timeZone);
        String startTime = DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A, startMillis, timeZone);

        StringBuilder overLappingEntry = new StringBuilder();
        boolean isActiveEntry = REPORT_STATUS.CLOCKED_IN.toString().equalsIgnoreCase(status);

        if(isActiveEntry){
            constructOverlapEntryForOngoing(overLappingEntry,startDate,startTime);
        }else{
            constructOverlapEntryForClockedOuts(overLappingEntry,startDate,startTime,endMillis,timeZone);
        }

        responseMap.put(Commons.MESSAGE, ADJUSTMENTS_ERROR_RESPONSE.ADJUSTMENT_OVERLAP.value());
        responseMap.put(OVERLAPPING_ENTRY, overLappingEntry.toString());
        return responseMap;
    }

    public static Map<String,Object> extractAllOverLapEntryDetails(Map<String, Object> entry, String timeZone, String emailId){

        Map<String, Object> responseMap = new HashMap<>();
        Map<String, Object> metaMap = (HashMap<String, Object>) entry.get(SchedulingKeys.METADATA);

        String status = metaMap.get(SchedulingKeys.STATUS).toString();
        long startMillis = (long)entry.get(SchedulingKeys.START_TIME);
        long endMillis = (long)entry.get(SchedulingKeys.END_TIME);

        String startDate = DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY, startMillis, timeZone);
        String startTime = DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A, startMillis, timeZone);

        StringBuilder overLappingEntry = new StringBuilder();
        boolean isActiveEntry = REPORT_STATUS.CLOCKED_IN.toString().equalsIgnoreCase(status);

        if(isActiveEntry){
            responseMap.put(OVERLAPPING_ENTRY_TYPE, "current_running");
            constructOverlapEntryForOngoing(overLappingEntry,startDate,startTime);
        }else{
            responseMap.put(OVERLAPPING_ENTRY_TYPE, REPORT_STATUS.PENDING.toString().equalsIgnoreCase(status) ? "pending" : "original");
            constructOverlapEntryForClockedOuts(overLappingEntry,startDate,startTime,endMillis,timeZone);
        }

        ReportsDTO reportsDTO = new ReportsDTO(entry,emailId,timeZone);
        responseMap.put(OVERLAPPING_ENTRY, overLappingEntry.toString());
        responseMap.put(OVERLAPPING_ENTRY_OBJ, reportsDTO);
        responseMap.put(OVERLAPPING_ENTRY_ID, reportsDTO.getId());
        responseMap.put(OVERLAPPING_INFO, ADJUSTMENTS_ERROR_RESPONSE.ADJUSTMENT_OVERLAP.value());
        responseMap.put(Commons.MESSAGE, ADJUSTMENTS_ERROR_RESPONSE.ADJUSTMENT_OVERLAP.value());

        return responseMap;
    }


    private static void constructOverlapEntryStartDateTime(StringBuilder overLappingEntry,String startDate,String startTime){
        overLappingEntry.append(startDate);
        overLappingEntry.append(" ");
        overLappingEntry.append(startTime);
    }

    private static void constructOverlapEntryForOngoing(StringBuilder overLappingEntry,String startDate,String startTime){
        constructOverlapEntryStartDateTime(overLappingEntry,startDate,startTime);
        overLappingEntry.append(" - Ongoing ");
    }

    private static void constructOverlapEntryForClockedOuts(StringBuilder overLappingEntry,String startDate,String startTime,
                                                            long endMillis, String timeZone){
        String endDate = DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY, endMillis, timeZone);
        String endTime = DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A, endMillis, timeZone);
        constructOverlapEntryStartDateTime(overLappingEntry,startDate,startTime);
        overLappingEntry.append(" - ");
        overLappingEntry.append(endDate);
        overLappingEntry.append(" ");
        overLappingEntry.append(endTime);
    }
}