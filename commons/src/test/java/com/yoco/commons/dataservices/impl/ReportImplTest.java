package com.yoco.commons.dataservices.impl;

import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.constants.SchedulingEngineUrlConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeoutException;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class ReportImplTest {
    @Test
    void getReportImplInstance_test(){
        ReportImpl reportImpl = ReportImpl.getReportImplInstance();
        Assertions.assertEquals("class com.yoco.commons.dataservices.impl.ReportImpl",reportImpl.getClass().toString());
    }

    @Test
    void getUserEntriesByTaskID_nullAccountID_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        List<Map<String,Object>> actual = new ReportImpl().getUserEntriesByTaskID(null,"123","task");
        Assertions.assertTrue(actual.isEmpty());
    }

    @Test
    void getUserEntriesByTaskID_emptyAccountID_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        List<Map<String,Object>> actual = new ReportImpl().getUserEntriesByTaskID("","123","task");
        Assertions.assertTrue(actual.isEmpty());
    }

    @Test
    void getUserEntriesByTaskID_nullContactID_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        List<Map<String,Object>> actual = new ReportImpl().getUserEntriesByTaskID("acc",null,"task");
        Assertions.assertTrue(actual.isEmpty());
    }

    @Test
    void getUserEntriesByTaskID_emptyContactID_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        List<Map<String,Object>> actual = new ReportImpl().getUserEntriesByTaskID("acc","","task");
        Assertions.assertTrue(actual.isEmpty());
    }

    @Test
    void getUserEntriesByTaskID_nullTaskID_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        List<Map<String,Object>> actual = new ReportImpl().getUserEntriesByTaskID("acc","con",null);
        Assertions.assertTrue(actual.isEmpty());
    }

    @Test
    void getUserEntriesByTaskID_emptyTaskID_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        List<Map<String,Object>> actual = new ReportImpl().getUserEntriesByTaskID("acc","con","");
        Assertions.assertTrue(actual.isEmpty());
    }

    @Test
    void getUserEntriesByTaskID_nullData_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "acc");
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{"con"});
            queryMap.put(SchedulingKeys.STATUS, "task");
            queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(new HashMap<>());
            List<Map<String,Object>> actual = new ReportImpl().getUserEntriesByTaskID("acc","con","task");
            Assertions.assertTrue(actual.isEmpty());
        }
    }

    @Test
    void getUserEntriesByTaskID_nullEvents_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "acc");
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{"con"});
            queryMap.put(SchedulingKeys.STATUS, "task");
            queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(new HashMap(){{put("data",new HashMap());}});
            List<Map<String,Object>> actual = new ReportImpl().getUserEntriesByTaskID("acc","con","task");
            Assertions.assertTrue(actual.isEmpty());
        }
    }

    @Test
    void getUserEntriesByTaskID_valid_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "acc");
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{"con"});
            queryMap.put(SchedulingKeys.STATUS, "task");
            queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);
            List<Map<String,Object>> entriesList = new ArrayList<>();
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(new HashMap(){{
                put("data",new HashMap(){{
                    put("events",entriesList);
                }});
            }});
            List<Map<String,Object>> actual = new ReportImpl().getUserEntriesByTaskID("acc","con","task");
            Assertions.assertEquals(actual,entriesList);
        }
    }

    @Test
    void getActiveEntry_ifAccountIdIsEmpty_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try (MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)) {

            List<Map<String,Object>> entriesList = new ArrayList<>();

            List<Map<String,Object>> actual = new ReportImpl().getActiveEntry("", "123");
            Assertions.assertEquals(actual,entriesList);
        }
    }

    @Test
    void getActiveEntry_ifContactIdIsEmpty_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try (MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)) {

            List<Map<String,Object>> entriesList = new ArrayList<>();

            List<Map<String,Object>> actual = new ReportImpl().getActiveEntry("123", "");
            Assertions.assertEquals(actual,entriesList);
        }
    }

    @Test
    void getActiveEntry_valid_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try (MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)) {

            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "acc");
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{"con"});
            queryMap.put(SchedulingKeys.STATUS, "task");
            queryMap.put(SchedulingKeys.SORT_ORDER, SchedulingKeys.DESC);
            queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);

            List<Map<String,Object>> entriesList = new ArrayList<>();
            entriesList.add(queryMap);

            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(new HashMap(){{
                put("data",new HashMap(){{
                    put("events",entriesList);
                }});
            }});

            List<Map<String,Object>> actual = new ReportImpl().getActiveEntry("123", "123");
            Assertions.assertEquals("acc",entriesList.get(0).get("merchantId"));
        }
    }

    @Test
    void getEntriesForCurrentDay_valid_test() throws IOException, NoSuchAlgorithmException {
        try (MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)) {

            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "acc");
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{"con"});
            queryMap.put(SchedulingKeys.STATUS, "task");
            queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);

            List<Map<String,Object>> entriesList = new ArrayList<>();
            entriesList.add(queryMap);

            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(new HashMap(){{
                put("data",new HashMap(){{
                    put("events",entriesList);
                }});
            }});

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountID");
            userDTO.setContactID("contactID");
            userDTO.setZoneId("23");
            userDTO.setTimezone("IST");

            List<Map<String,Object>> actual = new ReportImpl().getEntriesForCurrentDay(userDTO);
            Assertions.assertEquals("acc",entriesList.get(0).get("merchantId"));
        }
    }

    @Test
    void getEntriesForCurrentDay_schedulingResponseDataIsNull_test() throws IOException, NoSuchAlgorithmException {
        try (MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)) {

            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "acc");
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{"con"});
            queryMap.put(SchedulingKeys.STATUS, "task");
            queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);

            List<Map<String,Object>> entriesList = new ArrayList<>();
            entriesList.add(queryMap);

            Map<String, Object>dataMap = new HashMap<>();
            dataMap.put(SchedulingKeys.EVENTS, entriesList);

            Map<String, Object>schedulingResponse = new HashMap<>();
            schedulingResponse.put(SchedulingKeys.DATA, dataMap);

            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(schedulingResponse);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountID");
            userDTO.setContactID("contactID");
            userDTO.setZoneId("23");
            userDTO.setTimezone("IST");

            Assertions.assertEquals(new ArrayList<>(),new ReportImpl().getEntriesForCurrentDay(userDTO));
        }
    }

    @Test
    void getAllClockedInEntriesForAccount_events_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            Map<String, Object> expected = new HashMap<>();
            Map<String, Object> events = new HashMap<>();
            events.put("entries", new ArrayList<>());
            expected.put("data", events);
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(any(Map.class))).thenReturn(expected);
            ArrayList respList = (ArrayList) new ReportImpl().getAllClockedInEntriesForAccount("accID");
            Assertions.assertTrue(ObjUtils.isEmptyList(respList));
        }
    }

    @Test
    void getEntryByID_nullentryID_test() throws IOException, NoSuchAlgorithmException {
       Map<String,Object> resp = new ReportImpl().getEntryByID(null);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
    }

    @Test
    void getEntryByID_emptyentryID_test() throws IOException, NoSuchAlgorithmException {
        Map<String,Object> resp = new ReportImpl().getEntryByID("");
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
    }

    @Test
    void getEntryByID_nullData_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByID(anyString())).thenReturn(new HashMap<>());
            Map<String,Object> actual = new ReportImpl().getEntryByID("entryId");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(actual));
        }
    }

    @Test
    void getEntryByID_nullEvents_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByID(anyString())).thenReturn(new HashMap(){{put("data",null);}});
            Map<String,Object> actual = new ReportImpl().getEntryByID("entryId");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(actual));
        }
    }

    @Test
    void getEntryByID_valid_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        HashMap<String, Object> resp = new HashMap<>();
        resp.put("entry", "entry");
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByID(anyString())).thenReturn(new HashMap(){{put("data", resp);}});
            Map<String,Object> actual = new ReportImpl().getEntryByID("entryId");
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(actual));
        }
    }

    @Test
    void getEntries_null_accountID_test() throws IOException, NoSuchAlgorithmException {
        List<Map<String,Object>> resp = new ReportImpl().getEntries(null, "contactID", "start", "end", "order", false);
        Assertions.assertTrue(ObjUtils.isEmptyList(resp));
    }

    @Test
    void getEntries_empty_accountID_test() throws IOException, NoSuchAlgorithmException {
        List<Map<String,Object>> resp = new ReportImpl().getEntries("", "contactID", "start", "end", "order", false);
        Assertions.assertTrue(ObjUtils.isEmptyList(resp));
    }

    @Test
    void getEntries_null_ContactID_test() throws IOException, NoSuchAlgorithmException {
        List<Map<String,Object>> resp = new ReportImpl().getEntries("acc", null, "start", "end", "order", false);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
    }

    @Test
    void getEntries_empty_ContactID_test() throws IOException, NoSuchAlgorithmException {
        List<Map<String,Object>> resp = new ReportImpl().getEntries("acc", "", "start", "end", "order", false);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
    }

    @Test
    void getEntries_nullData_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{"contactID"});
            queryMap.put(SchedulingKeys.START_DATE_TIME, "start");
            queryMap.put(SchedulingKeys.END_DATE_TIME, "end");
            queryMap.put(SchedulingKeys.SORT_ORDER, "order");
            queryMap.put(SchedulingKeys.TYPE, SchedulingKeys.EVENT);
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(new HashMap<>());
            List<Map<String,Object>> resp = new ReportImpl().getEntries("accountID", "contactID", "start", "end", "order", false);
            Assertions.assertTrue(resp.isEmpty());
        }
    }

    @Test
    void getEntries_nullEvents_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{"contactID"});
            queryMap.put(SchedulingKeys.START_DATE_TIME, "start");
            queryMap.put(SchedulingKeys.END_DATE_TIME, "end");
            queryMap.put(SchedulingKeys.SORT_ORDER, "order");
            queryMap.put(SchedulingKeys.TYPE, SchedulingKeys.EVENT);
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(new HashMap(){{put("data",new HashMap());}});
            List<Map<String,Object>> resp = new ReportImpl().getEntries("accountID", "contactID", "start", "end", "order", false);
            Assertions.assertTrue(resp.isEmpty());
        }
    }

    @Test
    void getEntries_valid_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{"contactID"});
            queryMap.put(SchedulingKeys.START_DATE_TIME, "start");
            queryMap.put(SchedulingKeys.END_DATE_TIME, "end");
            queryMap.put(SchedulingKeys.SORT_ORDER, "order");
            queryMap.put(SchedulingKeys.TYPE, SchedulingKeys.EVENT);
            List<Map<String,Object>> entriesList = new ArrayList<>();
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(new HashMap(){{
                put("data",new HashMap(){{
                    put("events",entriesList);
                }});
            }});
            List<Map<String,Object>> resp = new ReportImpl().getEntries("accountID", "contactID", "start", "end", "order", false);
            Assertions.assertEquals(resp,entriesList);
        }
    }

    @Test
    void getLastClockedOutEntry_nullData_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(any(Map.class))).thenReturn(new HashMap<>());
            Map<String,Object> resp = new ReportImpl().getLastClockedOutEntry("accountID", "contactID");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
        }
    }

    @Test
    void getLastClockedOutEntry_nullEvents_test() throws IOException, TimeoutException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(any(Map.class))).thenReturn(new HashMap(){{put("data",new HashMap());}});
            Map<String,Object> resp = new ReportImpl().getLastClockedOutEntry("accountID", "contactID");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
        }
    }

    @Test
    void getLastClockedOutEntry_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            List<Map<String,Object>> entriesList = new ArrayList<>();
            entriesList.add(new HashMap<>(){{put("entry", "entry");}});
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(any(Map.class))).thenReturn(new HashMap(){{
                put("data",new HashMap(){{
                    put("events",entriesList);
                }});
            }});
            Map<String,Object> resp = new ReportImpl().getLastClockedOutEntry("accountID", "contactID");
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));
        }
    }

    @Test
    void getAllEntriesOfAccount_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put("data",Map.of("events",List.of()));

            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(mockResp);
            Map<String,Object> resp =  new ReportImpl().getAllEntriesOfAccount("accountID", "from","to",0,"");
            Assertions.assertEquals(resp,mockResp);

            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.BRAND, DcmConstants.YOCO_BRAND_ID);
            queryMap.put(SchedulingKeys.START_DATE_TIME, "from");
            queryMap.put(SchedulingKeys.END_DATE_TIME, "to");
            queryMap.put(SchedulingKeys.LIMIT, 900);

            schedulingEngineUtilMockedStatic.verify(()->SchedulingEngineUtil.getEventByQueryString(queryMap),times(1));
        }
    }

    @Test
    void getAllEntriesOfAccount_valid_cursor_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put("data",Map.of("events",List.of()));

            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(mockResp);
            Map<String,Object> resp =  new ReportImpl().getAllEntriesOfAccount("accountID", "from","to",1000,"cursor");
            Assertions.assertEquals(resp,mockResp);

            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.BRAND, DcmConstants.YOCO_BRAND_ID);
            queryMap.put(SchedulingKeys.START_DATE_TIME, "from");
            queryMap.put(SchedulingKeys.END_DATE_TIME, "to");
            queryMap.put(SchedulingKeys.LIMIT, 1000);
            queryMap.put(SchedulingKeys.CURSOR_STR,"cursor");

            schedulingEngineUtilMockedStatic.verify(()->SchedulingEngineUtil.getEventByQueryString(queryMap),times(1));
        }
    }

    @Test
    void getEventsByIds_ifEventIdsAreNull_test() throws IOException, NoSuchAlgorithmException {
        List<String> eventIds = new ArrayList<>();
        Map<String, Object> responseMap = new ReportImpl().getEventsByIds(eventIds);
        Assertions.assertEquals(new HashMap<>(),responseMap);
    }

    @Test
    void getEventsByIds_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            Map<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{"contactID"});
            queryMap.put(SchedulingKeys.STATUS, "PENDING");
            queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);
            Map<String, Object>entryMap = new HashMap<>();
            entryMap.put("entry", "entry");
            List<Map<String,Object>> entriesList = new ArrayList<>();
            entriesList.add(entryMap);
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(new HashMap(){{
                put("data",new HashMap(){{
                    put("events",entriesList);
                }});
            }});
            List<String> eventIds = new ArrayList<>();
            eventIds.add("123");
            Map<String, Object> responseMap = new ReportImpl().getEventsByIds(eventIds);
            Assertions.assertNotNull(responseMap);
        }
    }

    @Test
    void updateEntries_nullEventsList_test() throws IOException, NoSuchAlgorithmException {
        Assertions.assertEquals(new HashMap<>(),new ReportImpl().updateEntries("accId",null));
    }

    @Test
    void updateEntries_emptyEventsList_test() throws IOException, NoSuchAlgorithmException {
        Assertions.assertEquals(new HashMap<>(),new ReportImpl().updateEntries("accId",List.of()));
    }

    @Test
    void updateEntries_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.updateBatchEventsReq(anyMap())).thenReturn(Map.of("key","data"));
            Map<String,Object> event1 = new HashMap<>();
            event1.put("id","1");
            Map<String,Object> event2 = new HashMap<>();
            event2.put("id","2");
            Assertions.assertEquals(Map.of("key","data"),new ReportImpl().updateEntries("accId",List.of(event1,event2)));
            Map<String,Object> requestMap = new HashMap<>();
            requestMap.put(SchedulingKeys.BRAND,SchedulingKeys.YOCOBOARD_BRAND);
            requestMap.put(SchedulingKeys.MERCHANT,"accId");
            requestMap.put(SchedulingKeys.DATA,List.of(event1,event2));
            schedulingEngineUtilMockedStatic.verify(()-> SchedulingEngineUtil.updateBatchEventsReq(requestMap));
        }
    }

    @Test
    void deleteEvents_nullEventsTest() throws IOException, NoSuchAlgorithmException {
        Assertions.assertEquals(List.of(),new ReportImpl().deleteEvents(null));
    }

    @Test
    void deleteEvents_emptyEventsTest() throws IOException, NoSuchAlgorithmException {
        Assertions.assertEquals(List.of(),new ReportImpl().deleteEvents(List.of()));
    }

    @Test
    void deleteEvents_nullResponseTest() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUrlConstants> schedulingEngineUrlConstantsMockedStatic = Mockito.mockStatic(SchedulingEngineUrlConstants.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            schedulingEngineUrlConstantsMockedStatic.when(SchedulingEngineUrlConstants::getSchedulingEngineEventUrl).thenReturn("url");
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            urlFetcherMockedStatic.when(()->UrlFetcher.sendDeleteRequest("url?&ids=123&ids=234",new String[]{},null)).thenReturn(null);
            Assertions.assertEquals(List.of(),new ReportImpl().deleteEvents(List.of("123","","234")));
        }
    }

    @Test
    void deleteEvents_SuccessFalseResponseTest() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUrlConstants> SchedulingEngineUrlConstantsMockedStatic = Mockito.mockStatic(SchedulingEngineUrlConstants.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            SchedulingEngineUrlConstantsMockedStatic.when(SchedulingEngineUrlConstants::getSchedulingEngineEventUrl).thenReturn("url");
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            urlFetcherMockedStatic.when(()->UrlFetcher.sendDeleteRequest("url?&ids=123&ids=234",new String[]{},null)).thenReturn(Map.of("response",false));
            Assertions.assertEquals(List.of(),new ReportImpl().deleteEvents(List.of("123","","234")));
        }
    }

    @Test
    void deleteEvents_SuccessTrueResponseTest() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUrlConstants> SchedulingEngineUrlConstantsMockedStatic = Mockito.mockStatic(SchedulingEngineUrlConstants.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            SchedulingEngineUrlConstantsMockedStatic.when(SchedulingEngineUrlConstants::getSchedulingEngineEventUrl).thenReturn("url");
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            urlFetcherMockedStatic.when(()->UrlFetcher.sendDeleteRequest("url?&ids=123&ids=234",new String[]{},null)).thenReturn(Map.of("response",true,"data",List.of(Map.of("1","2"),Map.of("3","4"))));
            Assertions.assertEquals(List.of(Map.of("1","2"),Map.of("3","4")),new ReportImpl().deleteEvents(List.of("123","","234")));
        }
    }

    @Test
    void revertEvent_nullEntryID_test() throws IOException, NoSuchAlgorithmException {
        ReportImpl report = new ReportImpl();
        Assertions.assertEquals(Map.of(),report.revertEvent(null));
    }

    @Test
    void revertEvent_emptyEntryID_test() throws IOException, NoSuchAlgorithmException {
        ReportImpl report = new ReportImpl();
        Assertions.assertEquals(Map.of(),report.revertEvent(""));
    }

    @Test
    void revertEvent_nullResponse_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUrlConstants> schedulingEngineUrlConstantsMockedStatic = Mockito.mockStatic(SchedulingEngineUrlConstants.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            ReportImpl report = new ReportImpl();
            schedulingEngineUrlConstantsMockedStatic.when(SchedulingEngineUrlConstants::getSchedulingEngineEventUrl).thenReturn("url");
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPatchRequest("url/123", JsonUtil.getJson(List.of(Map.of("op","replace","path","status","value","ACTIVE"))),new String[]{},null)).thenReturn(null);
            Assertions.assertEquals(Map.of(),report.revertEvent("123"));
        }
    }

    @Test
    void revertEvent_emptyResponse_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUrlConstants> schedulingEngineUrlConstantsMockedStatic = Mockito.mockStatic(SchedulingEngineUrlConstants.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            ReportImpl report = new ReportImpl();
            schedulingEngineUrlConstantsMockedStatic.when(SchedulingEngineUrlConstants::getSchedulingEngineEventUrl).thenReturn("url");
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPatchRequest("url/123", JsonUtil.getJson(List.of(Map.of("op","replace","path","status","value","ACTIVE"))),new String[]{},null)).thenReturn(Map.of());
            Assertions.assertEquals(Map.of(),report.revertEvent("123"));
        }
    }

    @Test
    void revertEvent_successFalse_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUrlConstants> schedulingEngineUrlConstantsMockedStatic = Mockito.mockStatic(SchedulingEngineUrlConstants.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            ReportImpl report = new ReportImpl();
            schedulingEngineUrlConstantsMockedStatic.when(SchedulingEngineUrlConstants::getSchedulingEngineEventUrl).thenReturn("url");
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPatchRequest("url/123", JsonUtil.getJson(List.of(Map.of("op","replace","path","status","value","ACTIVE"))),new String[]{},null)).thenReturn(Map.of("response",false));
            Assertions.assertEquals(Map.of(),report.revertEvent("123"));
        }
    }

    @Test
    void revertEvent_validResponse_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUrlConstants> schedulingEngineUrlConstantsMockedStatic = Mockito.mockStatic(SchedulingEngineUrlConstants.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            ReportImpl report = new ReportImpl();
            schedulingEngineUrlConstantsMockedStatic.when(SchedulingEngineUrlConstants::getSchedulingEngineEventUrl).thenReturn("url");
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPatchRequest("url/123", JsonUtil.getJson(List.of(Map.of("op","replace","path","status","value","ACTIVE"))),new String[]{},null)).thenReturn(Map.of("response",true,"data",Map.of("1","2")));
            Assertions.assertEquals(Map.of("1","2"),report.revertEvent("123"));
        }
    }

    @Test
    void getPreviousEntryFromGivenTime_nullResp_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(null);
            Assertions.assertEquals(new HashMap<>(),new ReportImpl().getPreviousClockedOutEntryFromGivenTime("accId","contId",""));
        }
    }

    @Test
    void getPreviousEntryFromGivenTime_noData_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(Map.of("key","value"));
            Assertions.assertEquals(new HashMap<>(),new ReportImpl().getPreviousClockedOutEntryFromGivenTime("accId","contId",""));
        }
    }

    @Test
    void getPreviousEntryFromGivenTime_emptyData_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(Map.of("data",new HashMap()));
            Assertions.assertEquals(new HashMap<>(),new ReportImpl().getPreviousClockedOutEntryFromGivenTime("accId","contId",""));
        }
    }

    @Test
    void getPreviousEntryFromGivenTime_noEvents_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            Map<String,Object> responseMap = new HashMap<>();
            Map<String,Object> event = new HashMap<>();
            event.put("events",new ArrayList<>());
            responseMap.put("data",event);
            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(responseMap);
            Assertions.assertEquals(new HashMap<>(),new ReportImpl().getPreviousClockedOutEntryFromGivenTime("accId","contId",""));
        }
    }

    @Test
    void getPreviousEntryFromGivenTime_runningEntry_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            reportsUtilMockedStatic.when(()-> ReportsUtil.isRunningEntry(anyMap())).thenReturn(true);
            Map<String,Object> responseMap = new HashMap<>();
            Map<String,Object> event = new HashMap<>();
            event.put("events",new ArrayList<>(){{add(Map.of("startTime","123l","status","running"));add(Map.of("startTime","456l","status","Pending"));}});
            responseMap.put("data",event);
            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(responseMap);
            Assertions.assertEquals(Map.of("startTime","456l","status","Pending"),new ReportImpl().getPreviousClockedOutEntryFromGivenTime("accId","contId",""));
        }
    }

    @Test
    void getPreviousEntryFromGivenTime_runningEntry_test2() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            reportsUtilMockedStatic.when(()-> ReportsUtil.isRunningEntry(anyMap())).thenReturn(true);
            Map<String,Object> responseMap = new HashMap<>();
            Map<String,Object> event = new HashMap<>();
            event.put("events",new ArrayList<>(){{add(Map.of("startTime","123l","status","running"));}});
            responseMap.put("data",event);
            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(responseMap);
            Assertions.assertEquals(new HashMap<>(),new ReportImpl().getPreviousClockedOutEntryFromGivenTime("accId","contId",""));
        }
    }

    @Test
    void getPreviousEntryFromGivenTime_not_runningEntry_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            reportsUtilMockedStatic.when(()-> ReportsUtil.isRunningEntry(anyMap())).thenReturn(false);
            Map<String,Object> responseMap = new HashMap<>();
            Map<String,Object> event = new HashMap<>();
            event.put("events",new ArrayList<>(){{add(Map.of("startTime","123l","status","Manual_Clocked_Out"));}});
            responseMap.put("data",event);
            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(responseMap);
            Assertions.assertEquals(Map.of("startTime","123l","status","Manual_Clocked_Out"),new ReportImpl().getPreviousClockedOutEntryFromGivenTime("accId","contId",""));
        }
    }


}
