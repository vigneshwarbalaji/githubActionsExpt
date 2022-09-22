package com.yoco.commons.utils;

import com.fullreminders.api.client.model.action.HookAction;
import com.fullreminders.api.client.model.job.JobRequest;
import com.fullreminders.api.client.service.JobsApi;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.fullservices.FullReminders;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.events.ClockMetricUtil;
import com.yoco.commons.utils.events.ClockTaskUtil;
import com.yoco.commons.utils.events.ClockUtil;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import com.yoco.commons.modal.user.UserDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mock;

class ClockUtilTest {

    @Test
    void clockOutHandler_valid_test() throws IOException, NoSuchAlgorithmException {

        try(MockedConstruction<ClockTaskUtil> clockMock = Mockito.mockConstruction(ClockTaskUtil.class);
            MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){

            var accountObject = new SettingsJDO();
            accountObject.setPeopleUniquePin("234");

            var contact = new Contact("23534", "new@newMailId.com", "test", "test", "2", 75843984833L);

            var userObject = new PeopleRelationJDO();
            userObject.setUniquepin("234");
            userObject.setToBeMonitoredForExcessClockedIn(true);
            userObject.setId("453");
            userObject.setTimeZone("UTC");
            userObject.setContactId("23534");
            userObject.setContact(contact);
            userObject.setDateAddedLongTime(87656432456677L);
            userObject.setEmailID("newemail@emailid.com");

            Map<String, Object> headersMap = new HashMap<>();
            headersMap.put("srcClientID", "");
            headersMap.put("fromWhere", "");

            ArrayList<String> contactList = new ArrayList<>();
            contactList.add("232");

            Map<String, Object>clockInMap = new HashMap<>();
            clockInMap.put(SchedulingKeys.IP, "22:23");
            clockInMap.put(SchedulingKeys.INFO, "clockInInfo");

            Map<String,Object>locationMap = new HashMap<>();
            locationMap.put(SchedulingKeys.CLOCK_IN_MESSAGE, clockInMap);
            locationMap.put(SchedulingKeys.CLOCK_OUT_MESSAGE, clockInMap);

            Map<String, Object>metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS,"status");
            metaData.put(SchedulingKeys.OUTSOURCE, "outsource");
            metaData.put(SchedulingKeys.SUB_STATUS, "subsStatus");

            Map<String, Object>queryableMeta = new HashMap<>();
            queryableMeta.put(SchedulingKeys.STATUS, "status");

            Map<String, Object> eventMap = new HashMap<>();
            eventMap.put(SchedulingKeys.ID,"242");
            eventMap.put(SchedulingKeys.BRAND, "YoCo");
            eventMap.put(SchedulingKeys.MERCHANT, "23");
            eventMap.put(SchedulingKeys.PROVIDER, contactList);
            eventMap.put(SchedulingKeys.LOCATION, locationMap);
            eventMap.put(SchedulingKeys.METADATA, metaData);
            eventMap.put(SchedulingKeys.CALENDAR, "23");
            eventMap.put(SchedulingKeys.SOURCE, "source");
            eventMap.put(SchedulingKeys.PAYMENT_STATUS, "paymentStatus");
            eventMap.put(SchedulingKeys.IS_DELETED, false);
            eventMap.put(SchedulingKeys.START_TIME, 33432424324L);
            eventMap.put(SchedulingKeys.END_TIME, 484434324324L);
            eventMap.put(SchedulingKeys.QUERYABLE_META, queryableMeta);

            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(eventMap);

            ReportsDTO reportObject = ClockUtil.clockOutHandler(accountObject, userObject, eventMap, headersMap);
            Assertions.assertEquals(reportObject.getId(), eventMap.get(SchedulingKeys.ID));
        }
    }

    @Test
    void clockOutHandler_WithOutsourceAsScheduler_test() throws IOException, NoSuchAlgorithmException {

        try(MockedConstruction<ClockTaskUtil> clockMock = Mockito.mockConstruction(ClockTaskUtil.class);
            MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
            MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class);
            MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){

            var accountObject = new SettingsJDO();
            accountObject.setPeopleUniquePin("24");

            var contact = new Contact("235", "new@MailId.com", "test", "", "2", 75843984833L);

            var userObject = new PeopleRelationJDO();
            userObject.setUniquepin("24");
            userObject.setToBeMonitoredForExcessClockedIn(true);
            userObject.setId("45");
            userObject.setTimeZone("UTC");
            userObject.setContactId("235");
            userObject.setContact(contact);
            userObject.setDateAddedLongTime(87656432456677L);
            userObject.setEmailID("newemail@emailid.com");

            Map<String, Object> headersMap = new HashMap<>();
            headersMap.put("srcClientID", "scheduler");
            headersMap.put("fromWhere", "");

            ArrayList<String> contactList = new ArrayList<>();
            contactList.add("232");

            Map<String, Object>clockInMap = new HashMap<>();
            clockInMap.put(SchedulingKeys.IP, "22:23");
            clockInMap.put(SchedulingKeys.INFO, "clockInInfo");

            Map<String,Object>locationMap = new HashMap<>();
            locationMap.put(SchedulingKeys.CLOCK_IN_MESSAGE, clockInMap);
            locationMap.put(SchedulingKeys.CLOCK_OUT_MESSAGE, clockInMap);

            Map<String, Object>metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS,"status");
            metaData.put(SchedulingKeys.OUTSOURCE, "pomodoro");
            metaData.put(SchedulingKeys.SUB_STATUS, "subsStatus");

            Map<String, Object>queryableMeta = new HashMap<>();
            queryableMeta.put(SchedulingKeys.STATUS, "status");

            Map<String, Object> eventMap = new HashMap<>();
            eventMap.put(SchedulingKeys.ID,"45");
            eventMap.put(SchedulingKeys.BRAND, "YoCo");
            eventMap.put(SchedulingKeys.MERCHANT, "23");
            eventMap.put(SchedulingKeys.PROVIDER, contactList);
            eventMap.put(SchedulingKeys.LOCATION, locationMap);
            eventMap.put(SchedulingKeys.METADATA, metaData);
            eventMap.put(SchedulingKeys.CALENDAR, "23");
            eventMap.put(SchedulingKeys.SOURCE, "pomodoro");
            eventMap.put(SchedulingKeys.PAYMENT_STATUS, "paymentStatus");
            eventMap.put(SchedulingKeys.IS_DELETED, false);
            eventMap.put(SchedulingKeys.START_TIME, 33432424324L);
            eventMap.put(SchedulingKeys.END_TIME, 484434324324L);
            eventMap.put(SchedulingKeys.QUERYABLE_META, queryableMeta);
            eventMap.put("clockOutLongTime", 44443543354L);

            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(eventMap);

            Map<String, String>responseMap = new HashMap<>();
            responseMap.put("scheduler","scheduler");

            ClientSource clientSourceMock = mock(ClientSource.class);
            Mockito.when(clientSourceMock.getClientIdMap()).thenReturn(responseMap);

            ReportsDTO reportObject = ClockUtil.clockOutHandler(accountObject, userObject, eventMap, headersMap);
            Assertions.assertEquals(reportObject.getId(), eventMap.get(SchedulingKeys.ID));
        }
    }

    @Test
    void getClockOutSource_sourceClientIDIsEmpty_test() throws IOException {

        Map<String,Object>responseMap = ClockUtil.getClockOutSource("");
        Map<String,Object>expectedMap = new HashMap<>();
        expectedMap.put("outSource", "web");
        expectedMap.put("outSourceActivity", "web"+"_out");

        Assertions.assertEquals(expectedMap, responseMap);
    }

    @Test
    void getClockOutSource_valid_test() throws IOException {

        try(MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class)){

            Map<String, String>map = new HashMap<>();
            map.put("scheduler","scheduler");

            ClientSource clientSourceMock = mock(ClientSource.class);
            Mockito.when(clientSourceMock.getClientIdMap()).thenReturn(map);

            Map<String,Object>responseMap = ClockUtil.getClockOutSource("scheduler");
            Map<String,Object>expectedMap = new HashMap<>();
            expectedMap.put("outSource", "scheduler");
            expectedMap.put("outSourceActivity", "scheduler"+"_out");

            Assertions.assertEquals(expectedMap, responseMap);

        }

    }

    @Test
    void generateClockOutPayload_withoutLocationInfo_test() throws IOException {

        ArrayList<String> contactList = new ArrayList<>();
        contactList.add("232");

        Map<String, Object>metaData = new HashMap<>();
        metaData.put(SchedulingKeys.STATUS,"status");
        metaData.put(SchedulingKeys.OUTSOURCE, "pomodoro");
        metaData.put(SchedulingKeys.SUB_STATUS, "subsStatus");

        Map<String, Object>queryableMeta = new HashMap<>();
        queryableMeta.put(SchedulingKeys.STATUS, "status");

        var contact = new Contact("235", "new@MailId.com", "test", "", "2", 75843984833L);

        Map<String, Object> eventMap = new HashMap<>();
        eventMap.put(SchedulingKeys.ID,"45");
        eventMap.put(SchedulingKeys.BRAND, "YoCo");
        eventMap.put(SchedulingKeys.MERCHANT, "23");
        eventMap.put(SchedulingKeys.PROVIDER, contactList);
        eventMap.put(SchedulingKeys.METADATA, metaData);
        eventMap.put(SchedulingKeys.LOCATION, new HashMap<>());
        eventMap.put(SchedulingKeys.CALENDAR, "23");
        eventMap.put(SchedulingKeys.SOURCE, "pomodoro");
        eventMap.put(SchedulingKeys.PAYMENT_STATUS, "paymentStatus");
        eventMap.put(SchedulingKeys.IS_DELETED, false);
        eventMap.put(SchedulingKeys.START_TIME, 33432424324L);
        eventMap.put(SchedulingKeys.END_TIME, 484434324324L);
        eventMap.put(SchedulingKeys.QUERYABLE_META, queryableMeta);
        eventMap.put("clockOutLongTime", 44443543354L);

        Map<String, Object>newMetaData = new HashMap<>();
        newMetaData.put(SchedulingKeys.STATUS, "MANUAL_CLOCKED_OUT");
        Map<String, Object>newQueryableMeta = new HashMap<>();
        newQueryableMeta.put(SchedulingKeys.STATUS, "MANUAL_CLOCKED_OUT");

        Map<String, Object>expectedMap = eventMap;
        expectedMap.put(SchedulingKeys.METADATA, newMetaData);
        expectedMap.put(SchedulingKeys.QUERYABLE_META, newQueryableMeta);

        Map<String,Object>responseMap = ClockUtil.generateClockOutPayload(eventMap, 2435453232354L, "newSource");

        Assertions.assertEquals(expectedMap, responseMap);
    }

    @Test
    void generateClockOutPayload_withClockInMessageKey_test() throws IOException {

        Map<String, Object>clockInMap = new HashMap<>();
        clockInMap.put(SchedulingKeys.IP, "22:23");
        clockInMap.put(SchedulingKeys.INFO, "clockInInfo");

        Map<String,Object>locationMap = new HashMap<>();
        locationMap.put(SchedulingKeys.CLOCK_OUT_MESSAGE, clockInMap);

        ArrayList<String> contactList = new ArrayList<>();
        contactList.add("232");

        Map<String, Object>metaData = new HashMap<>();
        metaData.put(SchedulingKeys.STATUS,"status");
        metaData.put(SchedulingKeys.OUTSOURCE, "pomodoro");
        metaData.put(SchedulingKeys.SUB_STATUS, "subsStatus");

        Map<String, Object>queryableMeta = new HashMap<>();
        queryableMeta.put(SchedulingKeys.STATUS, "status");

        Map<String, Object> eventMap = new HashMap<>();
        eventMap.put(SchedulingKeys.ID,"45");
        eventMap.put(SchedulingKeys.BRAND, "YoCo");
        eventMap.put(SchedulingKeys.MERCHANT, "23");
        eventMap.put(SchedulingKeys.PROVIDER, contactList);
        eventMap.put(SchedulingKeys.METADATA, metaData);
        eventMap.put(SchedulingKeys.LOCATION, locationMap);
        eventMap.put(SchedulingKeys.CALENDAR, "23");
        eventMap.put(SchedulingKeys.SOURCE, "pomodoro");
        eventMap.put(SchedulingKeys.PAYMENT_STATUS, "paymentStatus");
        eventMap.put(SchedulingKeys.IS_DELETED, false);
        eventMap.put(SchedulingKeys.START_TIME, 33432424324L);
        eventMap.put(SchedulingKeys.END_TIME, 484434324324L);
        eventMap.put(SchedulingKeys.QUERYABLE_META, queryableMeta);
        eventMap.put("clockOutLongTime", 44443543354L);

        Map<String, Object>newMetaData = new HashMap<>();
        newMetaData.put(SchedulingKeys.STATUS, "MANUAL_CLOCKED_OUT");
        Map<String, Object>newQueryableMeta = new HashMap<>();
        newQueryableMeta.put(SchedulingKeys.STATUS, "MANUAL_CLOCKED_OUT");

        Map<String, Object>expectedMap = eventMap;
        expectedMap.put(SchedulingKeys.METADATA, newMetaData);
        expectedMap.put(SchedulingKeys.QUERYABLE_META, newQueryableMeta);

        var reportObject = new ReportsDTO();
        reportObject.setClockInMessage("newmessage");
        reportObject.setClockOutMessage("newMessage");
        reportObject.setInIP("77:98");
        reportObject.setOutIP("77:98");

        Map<String,Object>responseMap = ClockUtil.generateClockOutPayload(eventMap, 2435453232354L, "newSource");

        Assertions.assertEquals(expectedMap, responseMap);

    }

    @Test
    void generateClockOutPayload_valid_test() throws IOException {

        Map<String, Object>clockInMap = new HashMap<>();
        clockInMap.put(SchedulingKeys.IP, "22:23");
        clockInMap.put(SchedulingKeys.INFO, "clockInInfo");

        Map<String,Object>locationMap = new HashMap<>();
        locationMap.put(SchedulingKeys.CLOCK_IN_MESSAGE, clockInMap);
        locationMap.put(SchedulingKeys.CLOCK_OUT_MESSAGE, clockInMap);

        ArrayList<String> contactList = new ArrayList<>();
        contactList.add("232");

        Map<String, Object>metaData = new HashMap<>();
        metaData.put(SchedulingKeys.STATUS,"status");
        metaData.put(SchedulingKeys.OUTSOURCE, "pomodoro");
        metaData.put(SchedulingKeys.SUB_STATUS, "subsStatus");

        Map<String, Object>queryableMeta = new HashMap<>();
        queryableMeta.put(SchedulingKeys.STATUS, "status");

        Map<String, Object> eventMap = new HashMap<>();
        eventMap.put(SchedulingKeys.ID,"45");
        eventMap.put(SchedulingKeys.BRAND, "YoCo");
        eventMap.put(SchedulingKeys.MERCHANT, "23");
        eventMap.put(SchedulingKeys.PROVIDER, contactList);
        eventMap.put(SchedulingKeys.METADATA, metaData);
        eventMap.put(SchedulingKeys.LOCATION, locationMap);
        eventMap.put(SchedulingKeys.CALENDAR, "23");
        eventMap.put(SchedulingKeys.SOURCE, "pomodoro");
        eventMap.put(SchedulingKeys.PAYMENT_STATUS, "paymentStatus");
        eventMap.put(SchedulingKeys.IS_DELETED, false);
        eventMap.put(SchedulingKeys.START_TIME, 33432424324L);
        eventMap.put(SchedulingKeys.END_TIME, 484434324324L);
        eventMap.put(SchedulingKeys.QUERYABLE_META, queryableMeta);
        eventMap.put("clockOutLongTime", 44443543354L);

        Map<String, Object>newMetaData = new HashMap<>();
        newMetaData.put(SchedulingKeys.STATUS, "MANUAL_CLOCKED_OUT");
        Map<String, Object>newQueryableMeta = new HashMap<>();
        newQueryableMeta.put(SchedulingKeys.STATUS, "MANUAL_CLOCKED_OUT");

        Map<String, Object>expectedMap = eventMap;
        expectedMap.put(SchedulingKeys.METADATA, newMetaData);
        expectedMap.put(SchedulingKeys.QUERYABLE_META, newQueryableMeta);

        var reportObject = new ReportsDTO();
        reportObject.setClockInMessage("newmessage");
        reportObject.setClockOutMessage("newMessage");
        reportObject.setInIP("77:98");
        reportObject.setOutIP("77:98");

        Map<String,Object>responseMap = ClockUtil.generateClockOutPayload(eventMap, 2435453232354L, "newSource");

        Assertions.assertEquals(expectedMap, responseMap);

    }

    @Test
    void generateLocationInfo_nullInput_test(){
        Map<String,Object>expectedMap = new HashMap<>();
        Map<String,Object>responseMap = ClockUtil.generateLocationInfo(null,null, null, null);
        Assertions.assertEquals(expectedMap, responseMap);
    }

    @Test
    void generateLocationInfo_EmptyInput_test(){
        Map<String,Object>expectedMap = new HashMap<>();
        Map<String,Object>responseMap = ClockUtil.generateLocationInfo("","", "", "");
        Assertions.assertEquals(expectedMap, responseMap);
    }

    @Test
    void generateLocationInfo_valid_test(){

        HashMap<String, Object>clockInMessageMap = new HashMap<>();
        clockInMessageMap.put("msg", "msg");

        Map<String, Object>clockInMap = new HashMap<>();
        clockInMap.put(SchedulingKeys.IP, "22:23");
        clockInMap.put(SchedulingKeys.INFO, clockInMessageMap);

        Map<String,Object>clockInMsgMap = new HashMap<>();
        clockInMsgMap.put(SchedulingKeys.IP, "22:23");
        clockInMsgMap.put(SchedulingKeys.INFO, clockInMessageMap);

        Map<String,Object>clockOutMsgMap = new HashMap<>();
        clockOutMsgMap.put(SchedulingKeys.IP, "22:23");
        clockOutMsgMap.put(SchedulingKeys.INFO, clockInMessageMap);

        Map<String,Object>expectedMap = new HashMap<>();
        expectedMap.put(SchedulingKeys.CLOCK_IN_MESSAGE, clockInMsgMap);
        expectedMap.put(SchedulingKeys.CLOCK_OUT_MESSAGE, clockOutMsgMap);

        Map<String,Object>responseMap = ClockUtil.generateLocationInfo(clockInMap.get(SchedulingKeys.IP).toString(),JsonUtil.getJson(clockInMap.get(SchedulingKeys.INFO)),
                                                                        clockInMap.get(SchedulingKeys.IP).toString(),
                                                                        JsonUtil.getJson(clockInMap.get(SchedulingKeys.INFO)));

        Assertions.assertEquals(expectedMap, responseMap);

    }

    @Test
    void updateEvent_withoutPayload_test() throws IOException, NoSuchAlgorithmException {

        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            Map<String, Object>clockInMap = new HashMap<>();
            clockInMap.put(SchedulingKeys.IP, "22:23");
            clockInMap.put(SchedulingKeys.INFO, "clockInInfo");

            ArrayList<String> contactList = new ArrayList<>();
            contactList.add("232");

            Map<String, Object> eventMap = new HashMap<>();
            eventMap.put(SchedulingKeys.ID,"45");
            eventMap.put(SchedulingKeys.BRAND, "YoCo");
            eventMap.put(SchedulingKeys.MERCHANT, "23");
            eventMap.put(SchedulingKeys.PROVIDER, contactList);
            eventMap.put("clockOutLongTime", 44443543354L);

            SchedulingEngineUtil schedulingEngineUtilMock = mock(SchedulingEngineUtil.class);
            Mockito.when(SchedulingEngineUtil.updateEventReq(anyMap())).thenReturn(new HashMap<>());

            Map<String, Object>responseMap = ClockUtil.updateEvent(eventMap);
            Assertions.assertEquals(eventMap.get(SchedulingKeys.ID), responseMap.get(SchedulingKeys.ID));

        }
    }

    @Test
    void updateEvent_IfStartTimeAndEndTimeIsZero_test() throws IOException, NoSuchAlgorithmException {

        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            Map<String, Object>clockInMap = new HashMap<>();
            clockInMap.put(SchedulingKeys.IP, "22:23");
            clockInMap.put(SchedulingKeys.INFO, "clockInInfo");

            ArrayList<String> contactList = new ArrayList<>();
            contactList.add("232");

            Map<String, Object> eventMap = new HashMap<>();
            eventMap.put(SchedulingKeys.ID,"45");
            eventMap.put(SchedulingKeys.BRAND, "YoCo");
            eventMap.put(SchedulingKeys.CALENDAR, "");
            eventMap.put(SchedulingKeys.MERCHANT, "23");
            eventMap.put(SchedulingKeys.PROVIDER, contactList);
            eventMap.put(SchedulingKeys.START_TIME, 0L);
            eventMap.put(SchedulingKeys.END_TIME, 0L);
            eventMap.put("clockOutLongTime", 44443543354L);

            SchedulingEngineUtil schedulingEngineUtilMock = mock(SchedulingEngineUtil.class);
            Mockito.when(SchedulingEngineUtil.updateEventReq(anyMap())).thenReturn(new HashMap<>());

            Map<String, Object>responseMap = ClockUtil.updateEvent(eventMap);
            Assertions.assertEquals(eventMap.get(SchedulingKeys.ID), responseMap.get(SchedulingKeys.ID));

        }
    }

    @Test
    void updateEvent_valid_test() throws IOException, NoSuchAlgorithmException {

        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            Map<String, Object>clockInMap = new HashMap<>();
            clockInMap.put(SchedulingKeys.IP, "22:23");
            clockInMap.put(SchedulingKeys.INFO, "clockInInfo");

            Map<String,Object>locationMap = new HashMap<>();
            locationMap.put(SchedulingKeys.CLOCK_IN_MESSAGE, clockInMap);
            locationMap.put(SchedulingKeys.CLOCK_OUT_MESSAGE, clockInMap);

            ArrayList<String> contactList = new ArrayList<>();
            contactList.add("232");

            Map<String, Object>metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS,"status");
            metaData.put(SchedulingKeys.OUTSOURCE, "pomodoro");
            metaData.put(SchedulingKeys.SUB_STATUS, "subsStatus");

            Map<String, Object>queryableMeta = new HashMap<>();
            queryableMeta.put(SchedulingKeys.STATUS, "status");

            Map<String, Object> eventMap = new HashMap<>();
            eventMap.put(SchedulingKeys.ID,"45");
            eventMap.put(SchedulingKeys.PARENT_ID, "324");
            eventMap.put(SchedulingKeys.BRAND, "YoCo");
            eventMap.put(SchedulingKeys.MERCHANT, "23");
            eventMap.put(SchedulingKeys.PROVIDER, contactList);
            eventMap.put(SchedulingKeys.METADATA, metaData);
            eventMap.put(SchedulingKeys.LOCATION, locationMap);
            eventMap.put(SchedulingKeys.CALENDAR, "23");
            eventMap.put(SchedulingKeys.SOURCE, "pomodoro");
            eventMap.put(SchedulingKeys.PAYMENT_STATUS, "paymentStatus");
            eventMap.put(SchedulingKeys.IS_DELETED, false);
            eventMap.put(SchedulingKeys.START_TIME, 33432424324L);
            eventMap.put(SchedulingKeys.END_TIME, 484434324324L);
            eventMap.put(SchedulingKeys.CALENDAR, "calendar");
            eventMap.put(SchedulingKeys.QUERYABLE_META, queryableMeta);
            eventMap.put("clockOutLongTime", 44443543354L);

            SchedulingEngineUtil schedulingEngineUtilMock = mock(SchedulingEngineUtil.class);
            Mockito.when(SchedulingEngineUtil.updateEventReq(anyMap())).thenReturn(new HashMap<>());

            Map<String, Object>responseMap = ClockUtil.updateEvent(eventMap);
            Assertions.assertEquals(eventMap.get(SchedulingKeys.ID), responseMap.get(SchedulingKeys.ID));

        }
    }

    @Test
    void stopRunningEntryForDeletedUser_NoRunningEntry_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ClockTaskUtil> clockTaskUtilMockedStatic = Mockito.mockStatic(ClockTaskUtil.class);
            MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getActiveEntry("accID","123")).thenReturn(null);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accID");
            userPro.setContactId("123");
            ClockUtil.stopRunningEntryForDeletedUser(userPro,null,"web");
            clockTaskUtilMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void stopRunningEntryForDeletedUser_RunningEntry_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ClockUtil> clockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class);
            MockedStatic<ClockTaskUtil> clockTaskUtilMockedStatic = Mockito.mockStatic(ClockTaskUtil.class);
            MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedConstruction<ReportsDTO> reportsDTOMockedConstruction = Mockito.mockConstruction(ReportsDTO.class, (reportsDTO, context) -> {})){

            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getActiveEntry("accID","123")).thenReturn(List.of(Map.of()));
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accID");
            userPro.setContactId("123");
            userPro.setDateModified(1L);

            SettingsJDO account = new SettingsJDO();

            clockUtilMockedStatic.when(()->ClockUtil.getOutSourceFromSchEvent(Map.of())).thenReturn("outSource");
            clockUtilMockedStatic.when(()->ClockUtil.generateClockOutPayload(Map.of(),1L,"force-del-web")).thenReturn(Map.of());
            clockUtilMockedStatic.when(()->ClockUtil.updateEvent(Map.of())).thenReturn(Map.of());
            clockUtilMockedStatic.when(()->ClockUtil.stopRunningEntryForDeletedUser(any(PeopleRelationJDO.class),any(SettingsJDO.class),anyString())).thenCallRealMethod();
            ClockTaskUtil clockTaskUtilMock = Mockito.mock(ClockTaskUtil.class);
            clockTaskUtilMockedStatic.when(ClockTaskUtil::getClockTaskUtil).thenReturn(clockTaskUtilMock);
            ClockUtil.stopRunningEntryForDeletedUser(userPro,account,"web");
            Mockito.verify(clockTaskUtilMock).clockOutTaskHandler(any(Map.class));
        }
    }

    @Test
    void stopRunningEntryForDeletedUser_RunningEntry_hook_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ClockUtil> clockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class);
            MockedStatic<ClockTaskUtil> clockTaskUtilMockedStatic = Mockito.mockStatic(ClockTaskUtil.class);
            MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedConstruction<ReportsDTO> reportsDTOMockedConstruction = Mockito.mockConstruction(ReportsDTO.class, (reportsDTO, context) -> {})){

            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getActiveEntry("accID","123")).thenReturn(List.of(Map.of()));
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accID");
            userPro.setContactId("123");
            userPro.setDateModified(1L);

            SettingsJDO account = new SettingsJDO();

            clockUtilMockedStatic.when(()->ClockUtil.getOutSourceFromSchEvent(Map.of())).thenReturn("outSource");
            clockUtilMockedStatic.when(()->ClockUtil.generateClockOutPayload(Map.of(),1L,"force-del-hook")).thenReturn(Map.of());
            clockUtilMockedStatic.when(()->ClockUtil.updateEvent(Map.of())).thenReturn(Map.of());
            clockUtilMockedStatic.when(()->ClockUtil.stopRunningEntryForDeletedUser(any(PeopleRelationJDO.class),any(SettingsJDO.class),anyString())).thenCallRealMethod();
            ClockTaskUtil clockTaskUtilMock = Mockito.mock(ClockTaskUtil.class);
            clockTaskUtilMockedStatic.when(ClockTaskUtil::getClockTaskUtil).thenReturn(clockTaskUtilMock);
            ClockUtil.stopRunningEntryForDeletedUser(userPro,account,"hook");
            Mockito.verify(clockTaskUtilMock).clockOutTaskHandler(any(Map.class));
        }
    }

    @Test
    void getOutSourceFromSchEvent_nullEventMap_test(){
        Assertions.assertEquals("",ClockUtil.getOutSourceFromSchEvent(null));
    }

    @Test
    void getOutSourceFromSchEvent_emptyEventMap_test(){
        Assertions.assertEquals("",ClockUtil.getOutSourceFromSchEvent(Map.of()));
    }

    @Test
    void getOutSourceFromSchEvent_noMetaDataKey_test(){
        Assertions.assertEquals("",ClockUtil.getOutSourceFromSchEvent(Map.of("1","1")));
    }

    @Test
    void getOutSourceFromSchEvent_emptyMetaData_test(){
        Assertions.assertEquals("",ClockUtil.getOutSourceFromSchEvent(Map.of("metaData",Map.of())));
    }

    @Test
    void getOutSourceFromSchEvent_noOutsourceKey_test(){
        Assertions.assertEquals("",ClockUtil.getOutSourceFromSchEvent(Map.of("metaData",Map.of("1","1"))));
    }

    @Test
    void getOutSourceFromSchEvent_validOutsource_test(){
        Assertions.assertEquals("web",ClockUtil.getOutSourceFromSchEvent(Map.of("metaData",Map.of("outSource","web"))));
    }

    @Test
    void calculateTotalDurationMillisForEntry_valid_test(){
        Map<String, Object>entryMap = new HashMap<>();
        entryMap.put(SchedulingKeys.START_TIME, 10000000000L);
        entryMap.put(SchedulingKeys.END_TIME, 20000000000L);
        Map<String,Object> statusMap = new HashMap<>();
        statusMap.put(SchedulingKeys.STATUS, REPORT_STATUS.CLOCKED_IN.toString());
        Map<String,Object> entry = new HashMap<>();
        entry.put(SchedulingKeys.METADATA, statusMap);
        Assertions.assertEquals(10000000000L, ClockUtil.calculateTotalDurationMillisForEntry(entryMap));
    }

    @Test
    void calculateTotalDurationMillisForEntry_ifDifferenceIsZero_test(){
        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class);
        ){

            dateUtilMockedStatic.when(() -> DateUtil.getCurrentTime()).thenReturn(10L);
            Map<String, Object>entryMap = new HashMap<>();
            entryMap.put(SchedulingKeys.START_TIME, 100L);
            Map<String,Object> statusMap = new HashMap<>();
            statusMap.put(SchedulingKeys.STATUS,REPORT_STATUS.CLOCKED_IN.toString());
            entryMap.put(SchedulingKeys.METADATA, statusMap);
            Assertions.assertEquals(0L, ClockUtil.calculateTotalDurationMillisForEntry(entryMap));
        }
    }

    @Test
    void calculateTotalDurationMillisForEntry_isNull_test(){
        Map<String, Object>entryMap = new HashMap<>();
        Assertions.assertEquals(0L, ClockUtil.calculateTotalDurationMillisForEntry(entryMap));
    }

    @Test
    void getUserTotalDurationForCurrentDay_ifStartMillisEqualsZero_test() throws IOException, NoSuchAlgorithmException {

        UserDTO userDTO = new UserDTO();
        userDTO.setAccountID("accountID");
        userDTO.setContactID("contactID");
        userDTO.setTimezone("IST");
        userDTO.setZoneId("12");

        Assertions.assertEquals(0L, ClockUtil.getUserTotalDurationForCurrentDay(userDTO, 0L));
    }

    @Test
    void getUserTotalDurationForCurrentDay_valid_test() throws IOException, NoSuchAlgorithmException {

        UserDTO userDTO = new UserDTO();
        userDTO.setAccountID("accountID");
        userDTO.setContactID("contactID");
        userDTO.setTimezone("IST");
        userDTO.setZoneId("12");
        Map<String, Object>entriesMap = new HashMap<>();
        entriesMap.put(SchedulingKeys.START_TIME, 100L);
        entriesMap.put(SchedulingKeys.END_TIME, 1000L);

        List<Map<String,Object>> entriesList = new ArrayList<>();
        entriesList.add(entriesMap);
        entriesList.add(entriesMap);

        Map<String, Object>dataMap = new HashMap<>();
        dataMap.put(SchedulingKeys.EVENTS, entriesList);

        Map<String, Object>schedulingResponse = new HashMap<>();
        schedulingResponse.put(SchedulingKeys.DATA, dataMap);

        try(MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getEntriesForCurrentDay(any())).thenReturn(entriesList);
        })){
            Assertions.assertEquals(1800L, ClockUtil.getUserTotalDurationForCurrentDay(userDTO, 1000L));
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void isClockedInEntry_isEntryNullAndEmpty_test(Map<String,Object> entry){
        Assertions.assertFalse(ClockUtil.isClockedInEntry(entry));
    }

    @Test
    void isClockedInEntry_isStatusValueNull_test(){
        Map<String,Object> statusMap = new HashMap<>();
        Map<String,Object> entry = new HashMap<>();
        entry.put(SchedulingKeys.METADATA, statusMap);
        Assertions.assertFalse(ClockUtil.isClockedInEntry(entry));
    }

    @Test
    void isClockedInEntry_valid_test(){
        Map<String,Object> statusMap = new HashMap<>();
        statusMap.put(SchedulingKeys.STATUS,REPORT_STATUS.CLOCKED_IN.toString());
        Map<String,Object> entry = new HashMap<>();
        entry.put(SchedulingKeys.METADATA, statusMap);
        Assertions.assertTrue(ClockUtil.isClockedInEntry(entry));
    }

    @Test
    void stopRunningEntryForAccountDeletion_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ClockUtil> clockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class);
            MockedConstruction<ReportsDTO> mock = Mockito.mockConstruction(ReportsDTO.class, (reportDtoMock, context) -> {
                Mockito.when(reportDtoMock.getAccountID()).thenReturn("accID");
                Mockito.when(reportDtoMock.getContactID()).thenReturn("123");
            });
            MockedStatic<ClockMetricUtil> clockMetricUtilMockedStatic = Mockito.mockStatic(ClockMetricUtil.class)){
            clockUtilMockedStatic.when(()->ClockUtil.stopRunningEntryForAccountDeletion(Map.of(),1L)).thenCallRealMethod();
            ClockUtil.stopRunningEntryForAccountDeletion(Map.of(),1L);
            clockUtilMockedStatic.verify(()->ClockUtil.generateClockOutPayload(Map.of(),1L,"force-del-web"));
            clockUtilMockedStatic.verify(()->ClockUtil.updateEvent(Map.of()));
            clockMetricUtilMockedStatic.verify(()-> ClockMetricUtil.clockOutMetric("accID","123","deleteUser",mock.constructed().get(0)));
        }
    }
}
