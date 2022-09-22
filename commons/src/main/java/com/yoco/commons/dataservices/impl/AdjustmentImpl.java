package com.yoco.commons.dataservices.impl;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.dao.AdjustmentDao;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

@Slf4j
public class AdjustmentImpl implements AdjustmentDao {

    public static AdjustmentImpl getAdjustmentImplInstance(){
        return new AdjustmentImpl();
    }

    private static final String EVENT_LIST = "eventList";

    @Override
    public Map<String, Object> getAllAdjustments(String accountID, String contactID, String startDate, String endDate, String status, String cursor) throws IOException, NoSuchAlgorithmException {

        Map<String, Object>responseMap = new HashMap<>();
        HashMap<String, Object> queryMap = new HashMap<>();

        if (!ObjUtils.isNullOrEmpty(contactID)) {
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{contactID});
        }

        queryMap.put(SchedulingKeys.MERCHANT_ID, accountID);
        queryMap.put(SchedulingKeys.START_DATE_TIME, startDate);
        queryMap.put(SchedulingKeys.END_DATE_TIME, endDate);

        if(!ObjUtils.isNullOrEmpty(cursor)){
            queryMap.put(SchedulingKeys.CURSOR_STR,cursor);
        }

        if (!ObjUtils.isNullOrEmpty(status)) {
            queryMap.put(SchedulingKeys.STATUS, status);
            if (status.contains("REJECTED")) {
                queryMap.put(SchedulingKeys.IS_DELETED, true);
            }

            if(status.equalsIgnoreCase(REPORT_STATUS.APPROVED.toString())) {
                queryMap.put(SchedulingKeys.LIMIT,1000);
            }
        }
        Map<String, Object> schResp = SchedulingEngineUtil.getEventByQueryString(queryMap);

        Map<String, Object> schRespData = (HashMap<String, Object>) schResp.get(SchedulingKeys.DATA);

        if (ObjUtils.isNullOrEmpty(schRespData)) {
            return responseMap;
        }

        List<Map<String, Object>> adjustmentList = (ArrayList) schRespData.get(SchedulingKeys.EVENTS);
        responseMap.put(EVENT_LIST, adjustmentList);

        if (schRespData.containsKey(Commons.CURSOR)) {
            responseMap.put(Commons.CURSOR, schRespData.get(Commons.CURSOR));
        }
        return responseMap;
    }

    @Override
    public Map<String, Object> getOverLapEntries(String accountID, List<String> calendarIds, String contactID,
                                                 String startTime, String endTime, String entryIdToExclude) throws IOException, NoSuchAlgorithmException {
        Map<String, Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.CALENDAR_IDS, calendarIds);
        queryMap.put(SchedulingKeys.MERCHANT_ID, accountID);
        queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{contactID});
        queryMap.put(SchedulingKeys.START_DATE_TIME, startTime);
        queryMap.put(SchedulingKeys.END_DATE_TIME, endTime);
        queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);
        queryMap.put(SchedulingKeys.SORT_ORDER,SchedulingKeys.ASC);
        queryMap.put(SchedulingKeys.IS_GET_ADJUSTMENT,true);
        queryMap.put(SchedulingKeys.EVENT_IDS, new String[]{entryIdToExclude});
        return SchedulingEngineUtil.getEventByQueryString(queryMap);
    }

}
