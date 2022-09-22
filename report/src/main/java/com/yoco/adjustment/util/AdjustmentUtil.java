package com.yoco.adjustment.util;

import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;

import com.yoco.commons.modal.report.AdjustmentDTO;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class AdjustmentUtil {

    private AdjustmentUtil(){}

    public static String extractRefinedAdjustmentMessage(String message){
        return message.replace("Others -","").trim();
    }

    public static Map<String,Object> setAdjustmentInfo(String by, String reason){
        Map<String, Object> info = new HashMap<>();
        info.put(SchedulingKeys.BY, by);
        info.put(SchedulingKeys.WHEN, DateUtil.getCurrentTime());
        info.put(SchedulingKeys.NOTES, reason);
        return info;
    }

    public static Map<String,Object> extractAdditionalInfo(Map<String,Object> metaData){
        return  (!ObjUtils.isNullOrEmpty(metaData) && metaData.containsKey(SchedulingKeys.ADDITIONAL_INFO))
                ? (HashMap<String, Object>) metaData.get(SchedulingKeys.ADDITIONAL_INFO)
                : new HashMap<>();
    }

    public static Map<String,Object> extractAdjustmentInfo(Map<String, Object> additionalInfoMap){
        return (!ObjUtils.isNullOrEmpty(additionalInfoMap) && additionalInfoMap.containsKey(SchedulingKeys.ADJUSTMENT_INFO))
                ? (Map<String, Object>) additionalInfoMap.get(SchedulingKeys.ADJUSTMENT_INFO)
                : new HashMap<>();
    }

    public static Long adjustInTimeMillisForBreakSessionOverlapping(String accountID, String contactID, Long currentEntryInMillis, String timeZone) throws IOException, NoSuchAlgorithmException {
        String endDateTime = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU,currentEntryInMillis,timeZone);
        Map<String,Object> previousClockedOutEntry = ReportImpl.getReportImplInstance().getPreviousClockedOutEntryFromGivenTime(accountID,contactID,endDateTime);
        if(!ObjUtils.isNullOrEmpty(previousClockedOutEntry)){
            Long previousEntryOutMillis = (long)previousClockedOutEntry.get(SchedulingKeys.END_TIME);
            String previousEntryOutDateTime = DateUtil.convertMillisToDateTimeText(DateFormats.YYYY_MM_DD_HH_MM_SS_A,previousEntryOutMillis,timeZone);
            String currentEntryInDateTime = DateUtil.convertMillisToDateTimeText(DateFormats.YYYY_MM_DD_HH_MM_SS_A,currentEntryInMillis,timeZone);
            if(previousEntryOutMillis > currentEntryInMillis && previousEntryOutDateTime.equals(currentEntryInDateTime)){
                return previousEntryOutMillis + 1;
            }
        }
        return currentEntryInMillis;
    }

    public static boolean isAdjustmentMadeForToday(List<AdjustmentDTO> adjustments, String timezone){
        try{
            long currentDayStartMillis = DateUtil.getCurrentDayStartMillis(timezone);
            for(AdjustmentDTO adjustmentDTO : adjustments){
                if(adjustmentDTO.getAdjustedInTime() >= currentDayStartMillis || adjustmentDTO.getOriginalInTime() >= currentDayStartMillis){
                    return true;
                }
            }
            return false;
        }catch (Exception e){
            return false;
        }
    }

}
