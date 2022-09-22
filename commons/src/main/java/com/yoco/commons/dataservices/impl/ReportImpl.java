package com.yoco.commons.dataservices.impl;

import com.yoco.commons.constants.SchedulingEngineUrlConstants;
import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.dao.ReportDao;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.modal.date.RangeInfoDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

@Slf4j
public class ReportImpl implements ReportDao {

    public static ReportImpl getReportImplInstance(){
        return new ReportImpl();
    }

    @Override
    public Map<String, Object> getEntryByID(String entryID) throws IOException, NoSuchAlgorithmException {

        Map<String, Object> responseMap = new HashMap<>();
        if(ObjUtils.isNullOrEmpty(entryID)) {
            return responseMap;
        }

        Map<String, Object> schResp = SchedulingEngineUtil.getEventByID(entryID);
        Map<String, Object> schRespData = (HashMap<String, Object>) schResp.get(SchedulingKeys.DATA);

        if(ObjUtils.isNullOrEmpty(schRespData)){
            return responseMap;
        }

        return schRespData;
    }

    @Override
    public List<Map<String, Object>> getUserEntriesByTaskID(String accountID, String contactID, String taskID) throws IOException, TimeoutException, NoSuchAlgorithmException {
        List<Map<String,Object>> entriesList = new ArrayList<>();
        if(ObjUtils.isNullOrEmpty(accountID) || ObjUtils.isNullOrEmpty(contactID) || ObjUtils.isNullOrEmpty(taskID)){
            return entriesList;
        }
        HashMap<String, Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.MERCHANT_ID, accountID);
        queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{contactID});
        queryMap.put(SchedulingKeys.STATUS, taskID);
        queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);
        Map<String, Object> schResp = SchedulingEngineUtil.getEventByQueryString(queryMap);
        Map<String, Object> schRespData = (HashMap<String, Object>) schResp.get(SchedulingKeys.DATA);
        if(ObjUtils.isNullOrEmpty(schRespData)){
            return entriesList;
        }
        entriesList = (ArrayList) schRespData.get(SchedulingKeys.EVENTS);
        return ObjUtils.isNull(entriesList) ? new ArrayList<>() : entriesList;
    }

    @Override
    public List<Map<String, Object>> getActiveEntry(String accountID, String contactID) throws IOException, NoSuchAlgorithmException {
        List<Map<String, Object>> entriesList = new ArrayList<>();

        if(ObjUtils.isNullOrEmpty(accountID) || ObjUtils.isNullOrEmpty(contactID)){
            return entriesList;
        }
        HashMap<String, Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.MERCHANT_ID, accountID);
        queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{contactID});
        queryMap.put(SchedulingKeys.STATUS, REPORT_STATUS.CLOCKED_IN);
        queryMap.put(SchedulingKeys.SORT_ORDER, SchedulingKeys.DESC);
        queryMap.put(SchedulingKeys.TYPE, SchedulingKeys.EVENT);

        Map<String, Object> schResp = SchedulingEngineUtil.getEventByQueryString(queryMap);

        Map<String, Object> schRespData = (HashMap<String, Object>) schResp.get(SchedulingKeys.DATA);
        if(ObjUtils.isNullOrEmpty(schRespData)){
            return entriesList;
        }

        entriesList = (ArrayList) schRespData.get(SchedulingKeys.EVENTS);
        return ObjUtils.isNull(entriesList) ? new ArrayList<>() : entriesList;
    }

    @Override
    public List<Map<String, Object>> getEntries(String accountID, String contactID, String start, String end, String order, boolean includeDeleted) throws IOException, NoSuchAlgorithmException {
        List<Map<String, Object>> entriesList = new ArrayList<>();

        if(ObjUtils.isNullOrEmpty(accountID) || ObjUtils.isNullOrEmpty(contactID)){
            return entriesList;
        }
        HashMap<String, Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.MERCHANT_ID, accountID);
        queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{contactID});
        queryMap.put(SchedulingKeys.START_DATE_TIME, start);
        queryMap.put(SchedulingKeys.END_DATE_TIME, end);
        queryMap.put(SchedulingKeys.SORT_ORDER, order);
        queryMap.put(SchedulingKeys.TYPE, SchedulingKeys.EVENT);
        if(includeDeleted) {
            queryMap.put(SchedulingKeys.IS_EXPORT_SCHEDULE ,true);
        }
        Map<String, Object> schResp = SchedulingEngineUtil.getEventByQueryString(queryMap);
        Map<String, Object> schRespData = (HashMap<String, Object>) schResp.get(SchedulingKeys.DATA);
        if(ObjUtils.isNullOrEmpty(schRespData)){
            return entriesList;
        }

        entriesList = (ArrayList) schRespData.get(SchedulingKeys.EVENTS);
        return ObjUtils.isNull(entriesList) ? new ArrayList<>() : entriesList;

    }

    @Override
    public Map<String, Object> getLastClockedOutEntry(String accountID, String contactID) throws IOException, NoSuchAlgorithmException {

        Map<String, Object> responseMap = new HashMap<>();

        Map<String, Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.MERCHANT_ID, accountID);
        queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{contactID});
        queryMap.put(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT);
        queryMap.put(SchedulingKeys.LIMIT, 1);
        queryMap.put(SchedulingKeys.SORT_ORDER, SchedulingKeys.DESC);
        queryMap.put(SchedulingKeys.TYPE, SchedulingKeys.EVENT);

        Map<String, Object> schResp = SchedulingEngineUtil.getEventByQueryString(queryMap);

        Map<String, Object> schRespData = (HashMap<String, Object>) schResp.get(SchedulingKeys.DATA);

        if (!ObjUtils.isNullOrEmpty(schRespData)) {
            List<Map<String, Object>> entriesList = (ArrayList) schRespData.get(SchedulingKeys.EVENTS);
            if (!entriesList.isEmpty()) {
                return entriesList.get(0);
            }
        }

        return responseMap;
    }

    @Override
    public List<Map<String, Object>> getEntriesForCurrentDay(UserDTO userPro) throws IOException, NoSuchAlgorithmException {
        List<Map<String, Object>> entriesList = new ArrayList<>();
        RangeInfoDTO rangeInfoForCurrentDay = DateUtil.getRangeMillisForToday(userPro.getTimezone());
        Long startTimeMillis = rangeInfoForCurrentDay.getFromDateEpochMilliseconds();
        Long endTimeMillis = rangeInfoForCurrentDay.getToDateEpochMilliseconds();
        String startDateTime = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU,startTimeMillis, userPro.getZoneId());
        String endDateTime = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU,endTimeMillis, userPro.getZoneId());
        Map<String, Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.START_DATE_TIME, startDateTime);
        queryMap.put(SchedulingKeys.END_DATE_TIME, endDateTime);
        var provider = new String[]{userPro.getContactID()};
        queryMap.put(SchedulingKeys.MERCHANT_ID, userPro.getAccountID());
        queryMap.put(SchedulingKeys.REFERRERS_KEY, provider);
        queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);
        Map<String, Object> schResp = SchedulingEngineUtil.getEventByQueryString(queryMap);
        Map<String, Object> schRespData = (HashMap<String, Object>) schResp.get(SchedulingKeys.DATA);
        if(ObjUtils.isNullOrEmpty(schRespData)){
            return entriesList;
        }
        entriesList = (ArrayList) schRespData.get(SchedulingKeys.EVENTS);
        return ObjUtils.isNull(entriesList) ? new ArrayList<>() : entriesList;
    }

    @Override
    public List<Map<String, Object>> getAllClockedInEntriesForAccount(String accountID) throws IOException, NoSuchAlgorithmException {
        List<Map<String, Object>> entriesList = new ArrayList<>();

        HashMap<String, Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.MERCHANT_ID, accountID);
        queryMap.put(SchedulingKeys.STATUS, REPORT_STATUS.CLOCKED_IN);
        queryMap.put(SchedulingKeys.SORT_ORDER, SchedulingKeys.DESC);
        queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);
        Map<String, Object> schResp = SchedulingEngineUtil.getEventByQueryString(queryMap);
        Map<String, Object> schRespData = (Map<String, Object>) schResp.get(SchedulingKeys.DATA);

        if(ObjUtils.isNullOrEmpty(schRespData)){
            return entriesList;
        }
        entriesList = (ArrayList) schRespData.get(SchedulingKeys.EVENTS);
        return ObjUtils.isNull(entriesList) ? new ArrayList<>() : entriesList;
    }

    @Override
    public List<Map<String,Object>> deleteEvents(List<String> eventsIDs) throws IOException, NoSuchAlgorithmException{
        if(ObjUtils.isNullOrEmpty(eventsIDs)){
            return new ArrayList<>();
        }
        String queryParam = eventsIDs.stream().filter(id->!ObjUtils.isNullOrEmpty(id)).map("&ids="::concat).collect(Collectors.joining());
        String url = SchedulingEngineUrlConstants.getSchedulingEngineEventUrl() + "?" + queryParam;
        Map<String,Object> response = UrlFetcher.sendDeleteRequest(url,HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(), UrlFetcher.getHttpClientInstance());
        if(!ObjUtils.isNullOrEmpty(response) && Boolean.TRUE.equals(response.get(SchedulingKeys.RESPONSE))){
            return (List<Map<String, Object>>) response.get(SchedulingKeys.DATA);
        }
        return new ArrayList<>();
    }

    @Override
    public Map<String,Object> getAllEntriesOfAccount(String accountID,String startDateTime,String endDateTime,int limit,String cursor) throws IOException, NoSuchAlgorithmException {

        limit = limit == 0 ? 900 : limit;

        Map<String, Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.MERCHANT_ID, accountID);
        queryMap.put(SchedulingKeys.BRAND, DcmConstants.YOCO_BRAND_ID);
        queryMap.put(SchedulingKeys.START_DATE_TIME, startDateTime);
        queryMap.put(SchedulingKeys.END_DATE_TIME, endDateTime);
        queryMap.put(SchedulingKeys.LIMIT,limit);

        if(!ObjUtils.isNullOrEmpty(cursor)){
            queryMap.put(SchedulingKeys.CURSOR_STR,cursor);
        }

        return SchedulingEngineUtil.getEventByQueryString(queryMap);
    }

    @Override
    public Map<String, Object> getEventsByIds(List<String> eventIDs) throws IOException, NoSuchAlgorithmException {
        Map<String, Object> responseMap = new HashMap<>();
        if(ObjUtils.isNullOrEmpty(eventIDs)) {
            return responseMap;
        }
        Map<String, Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.EVENT_IDS, eventIDs);
        Map<String, Object> schResp = SchedulingEngineUtil.getEventByQueryString(queryMap);
        Map<String, Object> schRespData = (HashMap<String, Object>) schResp.get(SchedulingKeys.DATA);
        if(ObjUtils.isNullOrEmpty(schRespData)){
            return responseMap;
        }
        return schRespData;
    }

    @Override
    public Map<String, Object> updateEntries(String accountID, List<Map<String, Object>> eventsList) throws IOException, NoSuchAlgorithmException {
        if(ObjUtils.isNullOrEmpty(eventsList)) {
            return new HashMap<>();
        }
        Map<String,Object> requestMap = new HashMap<>();
        requestMap.put(SchedulingKeys.BRAND,SchedulingKeys.YOCOBOARD_BRAND);
        requestMap.put(SchedulingKeys.MERCHANT,accountID);
        requestMap.put(SchedulingKeys.DATA,eventsList);
        return SchedulingEngineUtil.updateBatchEventsReq(requestMap);
    }

    @Override
    public Map<String, Object> revertEvent(String entryID) throws IOException, NoSuchAlgorithmException {
        if(ObjUtils.isNullOrEmpty(entryID)){
            return Map.of();
        }
        List<Map<String,Object>> patchPayload = List.of(Map.of(SchedulingKeys.OP,SchedulingKeys.REPLACE,SchedulingKeys.PATH,SchedulingKeys.STATUS,SchedulingKeys.VALUE,SchedulingKeys.ACTIVE));
        String url = SchedulingEngineUrlConstants.getSchedulingEngineEventUrl() + "/" + entryID;
        Map<String,Object> response = UrlFetcher.sendPatchRequest(url, JsonUtil.getJson(patchPayload),HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(), UrlFetcher.getHttpClientInstance());
        if(!ObjUtils.isNullOrEmpty(response) && Boolean.TRUE.equals(response.get(SchedulingKeys.RESPONSE))){
            return (Map<String, Object>) response.get(SchedulingKeys.DATA);
        }
        return Map.of();
    }

    @Override
    public Map<String, Object> getPreviousClockedOutEntryFromGivenTime(String accountID, String contactID, String endDateTime) throws IOException, NoSuchAlgorithmException {
        Map<String, Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.MERCHANT_ID, accountID);
        queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{contactID});
        queryMap.put(SchedulingKeys.END_DATE_TIME,endDateTime);
        queryMap.put(SchedulingKeys.LIMIT, 2);
        queryMap.put(SchedulingKeys.SORT_ORDER, SchedulingKeys.DESC);
        queryMap.put(SchedulingKeys.TYPE, SchedulingKeys.EVENT);

        Map<String, Object> schResp = SchedulingEngineUtil.getEventByQueryString(queryMap);

        if(!ObjUtils.isNullOrEmpty(schResp)) {
            Map<String, Object> schRespData = (HashMap<String, Object>) schResp.get(SchedulingKeys.DATA);
            if (!ObjUtils.isNullOrEmpty(schRespData)) {
                List<Map<String, Object>> entriesList = (ArrayList) schRespData.get(SchedulingKeys.EVENTS);
                return this.extractPreviousEntry(entriesList);
            }
        }
        return new HashMap<>();
    }

    private Map<String,Object> extractPreviousEntry(List<Map<String, Object>> entriesList){
        Map<String, Object> responseMap = new HashMap<>();
        if (!entriesList.isEmpty()) {
            Map<String, Object> entry = entriesList.get(0);
            if (ReportsUtil.isRunningEntry(entry)) {
                return entriesList.size() > 1 ? entriesList.get(1) : responseMap;
            } else {
                return entry;
            }
        }
        return responseMap;
    }
}
