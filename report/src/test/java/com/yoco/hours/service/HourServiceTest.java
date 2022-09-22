package com.yoco.hours.service;

import com.yoco.MockEvent;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.*;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.entity.TaskJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.date.RangeInfoDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.services.FCMService;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import com.yoco.hours.helper.EntryDeleteHelper;
import com.yoco.hours.helper.HoursTaskInitiator;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.mockito.ArgumentMatchers.*;

class HourServiceTest {

    @Test
    void getActiveEntry_empty_accountID_test() {
        try {
            new HourService().getActiveEntry("", new Contact(), "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getActiveEntry_null_accountID_test() {
        try {
            new HourService().getActiveEntry(null, new Contact(), "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getActiveEntry_null_contact_test() {
        try {
            new HourService().getActiveEntry("accId", null, "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void getActiveEntry_empty_events_test() {
        try(MockedConstruction<ReportImpl> objMock = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
            Mockito.when(objReportImpl.getActiveEntry(anyString(), anyString())).thenReturn(new ArrayList<>());
        })) {
            Map<String, Object> resp = new HourService().getActiveEntry("accId", new Contact(), "");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void getActiveEntry_valid_events_test() {
        List eventsList = new ArrayList<>();
        eventsList.add(MockEvent.getRawEvent());

        try(MockedConstruction<ReportImpl> mock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getActiveEntry(anyString(),anyString())).thenReturn(eventsList);
            });
            MockedConstruction<UserImpl> userImplMock = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
                Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(new PeopleRelationJDO());
            })) {

            Contact objContact = new Contact();
            objContact.setId("1234");

            Map<String, Object> resp = new HourService().getActiveEntry("accId", objContact, "");
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void getTimerInfo_empty_accountID_test() {
        try {
            new HourService().getTimerInfo("", new Contact(), "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getTimerInfo_null_accountID_test() {
        try {
            new HourService().getTimerInfo(null, new Contact(), "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getTimerInfo_null_loggedInUserContact_test() {
        try {
            new HourService().getTimerInfo("accId", null, "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getTimerInfo_empty_events_with_empty_user_agent_with_last_clocked_out_entry_test() {

        Contact objContact = new Contact();
        objContact.setId("1234");

        PeopleRelationJDO objPRO = new PeopleRelationJDO();
        objPRO.setTimeZone("Asia/Kolkata");
        objPRO.setContactId("1234");
        objPRO.setEmailID("test@gmail.com");

        RangeInfoDTO objRangeInfo = new RangeInfoDTO();
        objRangeInfo.setFromDateEpochMilliseconds(System.currentTimeMillis());
        objRangeInfo.setToDateEpochMilliseconds(System.currentTimeMillis());

        try( MockedStatic<DateUtil> objDateUtilMock = Mockito.mockStatic(DateUtil.class);
             MockedConstruction<UserImpl> userImplMock = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
                Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPRO);
            });
             MockedConstruction<ReportImpl> reportImplMock = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
                 Mockito.when(objReportImpl.getEntries(anyString(), anyString(), anyString(), anyString(), anyString(), anyBoolean())).thenReturn(new ArrayList<>());
                 Mockito.when(objReportImpl.getActiveEntry(anyString(), anyString())).thenReturn(new ArrayList<>());
                 Mockito.when(objReportImpl.getLastClockedOutEntry(anyString(), anyString())).thenReturn(MockEvent.getRawEvent());
             })) {

            objDateUtilMock.when(()->DateUtil.getRangeDetails(anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(objRangeInfo);

            Map<String, Object> response = new HourService().getTimerInfo("accId", objContact, "");
            Assertions.assertTrue(response.containsKey("last_ClockedIn_Prj_Info"));
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void getTimerInfo_empty_events_with_empty_user_agent_withOut_last_clocked_out_entry_test() {

        Contact objContact = new Contact();
        objContact.setId("1234");

        PeopleRelationJDO objPRO = new PeopleRelationJDO();
        objPRO.setTimeZone("Asia/Kolkata");
        objPRO.setContactId("1234");
        objPRO.setEmailID("test@gmail.com");

        RangeInfoDTO objRangeInfo = new RangeInfoDTO();
        objRangeInfo.setFromDateEpochMilliseconds(System.currentTimeMillis());
        objRangeInfo.setToDateEpochMilliseconds(System.currentTimeMillis());

        try( MockedStatic<DateUtil> objDateUtilMock = Mockito.mockStatic(DateUtil.class);
             MockedConstruction<UserImpl> userImplMock = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
                 Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPRO);
             });
             MockedConstruction<ReportImpl> reportImplMock = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
                 Mockito.when(objReportImpl.getEntries(anyString(), anyString(), anyString(), anyString(), anyString(), anyBoolean())).thenReturn(new ArrayList<>());
                 Mockito.when(objReportImpl.getActiveEntry(anyString(), anyString())).thenReturn(new ArrayList<>());
                 Mockito.when(objReportImpl.getLastClockedOutEntry(anyString(), anyString())).thenReturn(new HashMap<>());
             })) {

            objDateUtilMock.when(()->DateUtil.getRangeDetails(anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(objRangeInfo);

            Map<String, Object> response = new HourService().getTimerInfo("accId", objContact, "");
            Assertions.assertTrue(response.containsKey("last_ClockedIn_Prj_Info"));
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void getTimerInfo_empty_events_with_user_agent_test() {

        Contact objContact = new Contact();
        objContact.setId("1234");

        PeopleRelationJDO objPRO = new PeopleRelationJDO();
        objPRO.setTimeZone("Asia/Kolkata");
        objPRO.setContactId("1234");
        objPRO.setEmailID("test@gmail.com");

        RangeInfoDTO objRangeInfo = new RangeInfoDTO();
        objRangeInfo.setFromDateEpochMilliseconds(System.currentTimeMillis());
        objRangeInfo.setToDateEpochMilliseconds(System.currentTimeMillis());

        try( MockedStatic<DateUtil> objDateUtilMock = Mockito.mockStatic(DateUtil.class);
             MockedConstruction<UserImpl> userImplMock = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
                 Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPRO);
             });
             MockedConstruction<ReportImpl> reportImplMock = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
                 Mockito.when(objReportImpl.getEntries(anyString(), anyString(), anyString(), anyString(), anyString(), anyBoolean())).thenReturn(new ArrayList<>());
                 Mockito.when(objReportImpl.getActiveEntry(anyString(), anyString())).thenReturn(new ArrayList<>());
             })) {

            objDateUtilMock.when(()->DateUtil.getRangeDetails(anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(objRangeInfo);

            Map<String, Object> response = new HourService().getTimerInfo("accId", objContact, "mobile");
            Assertions.assertTrue(!response.containsKey("last_ClockedIn_Prj_Info"));
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void updateEntry_empty_accountID_test() {
        try {
            new HourService().updateEntry("", "", new Contact(), new HashMap<>(), "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void updateEntry_null_accountID_test() {
        try {
            new HourService().updateEntry(null, "", new Contact(), new HashMap<>(), "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void updateEntry_empty_entryID_test() {
        try {
            new HourService().updateEntry("acc", "", new Contact(), new HashMap<>(), "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ENTRY_ID.value(),e.getMessage());
        }
    }

    @Test
    void updateEntry_null_entryID_test() {
        try {
            new HourService().updateEntry("acc", null, new Contact(), new HashMap<>(), "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ENTRY_ID.value(),e.getMessage());
        }
    }

    @Test
    void updateEntry_null_contact_test() {
        try {
            new HourService().updateEntry("acc", "entry", null, new HashMap<>(), "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void updateEntry_invalid_task_or_project_test() {
        try {
            new HourService().updateEntry("acc", "entry", new Contact(), new HashMap<>(), "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void updateEntry_null_getByID_test() {

        HashMap<String, Object> payload = new HashMap<>();
        payload.put(SchedulingKeys.PROJECT_ID , "project");

        try(MockedConstruction<ReportImpl> reportImplMock = Mockito.mockConstruction(ReportImpl.class, (objreportImpl, context)->{
                 Mockito.when(objreportImpl.getEntryByID(anyString())).thenReturn(null);
             })) {
            Map<String, Object> response = new HourService().updateEntry("accId", "ent", new Contact(), payload, "");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void updateEntry_empty_getByID_test() {

        HashMap<String, Object> payload = new HashMap<>();
        payload.put(SchedulingKeys.PROJECT_ID , "project");

        try(MockedConstruction<ReportImpl> reportImplMock = Mockito.mockConstruction(ReportImpl.class, (objreportImpl, context)->{
            Mockito.when(objreportImpl.getEntryByID(anyString())).thenReturn(new HashMap<>());
        })) {
            Map<String, Object> response = new HourService().updateEntry("accId", "ent", new Contact(), payload, "");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void updateEntry_success_project_update_test() {

        Contact objContact = new Contact();
        objContact.setId("1234");
        objContact.setEmailID("test@gmail.com");

        PeopleRelationJDO objPRO = new PeopleRelationJDO();
        objPRO.setTimeZone("Asia/Kolkata");
        objPRO.setContactId("1234");
        objPRO.setEmailID("test@gmail.com");

        HashMap<String, Object> payload = new HashMap<>();
        payload.put(SchedulingKeys.PROJECT_ID , "project");

        HashMap<String, Object> event = new HashMap<>();
        event.put("data", MockEvent.getRawEvent());

        try(MockedConstruction<ReportImpl> reportImplMock = Mockito.mockConstruction(ReportImpl.class, (objreportImpl, context)->{
            Mockito.when(objreportImpl.getEntryByID(anyString())).thenReturn(MockEvent.getRawEvent());
            });
            MockedConstruction<FCMService>mock = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context) -> {
                Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            });
            MockedStatic<SchedulingEngineUtil> objSchedulingEngineUtil = Mockito.mockStatic(SchedulingEngineUtil.class);
            MockedConstruction<UserImpl> userImplMock = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
                Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPRO);
            });
            MockedStatic<RTMService> objRTMService = Mockito.mockStatic(RTMService.class);
            ) {

            objSchedulingEngineUtil.when(()->SchedulingEngineUtil.updateEventReq(any(Map.class))).thenReturn(event);

            Map<String, Object> response = new HourService().updateEntry("accId", "ent", objContact, payload, "client");
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(response));
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void updateEntry_success_task_update_test() {

        Contact objContact = new Contact();
        objContact.setId("1234");
        objContact.setEmailID("test@gmail.com");

        PeopleRelationJDO objPRO = new PeopleRelationJDO();
        objPRO.setTimeZone("Asia/Kolkata");
        objPRO.setContactId("1234");
        objPRO.setEmailID("test@gmail.com");

        HashMap<String, Object> payload = new HashMap<>();
        payload.put(SchedulingKeys.TASK_DESC , "task");

        HashMap<String, Object> event = new HashMap<>();
        event.put("data", MockEvent.getRawEvent());

        try(MockedConstruction<FCMService>mock = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context) -> {
                Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            });
            MockedConstruction<ReportImpl> mockReport = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
                Mockito.when(objReportImpl.getEntryByID(anyString())).thenReturn(MockEvent.getRawEvent());
            });
            MockedConstruction<HourService> mockHourServie = Mockito.mockConstruction(HourService.class, (objHourService, context)->{
                Mockito.when(objHourService.updateTask(any(Map.class), anyString(), anyString())).thenReturn(MockEvent.getRawEvent());
                Mockito.when(objHourService.updateEntry(anyString(),anyString(),any(Contact.class), anyMap(), anyString())).thenCallRealMethod();
            });
            MockedStatic<SchedulingEngineUtil> objSchedulingEngineUtil = Mockito.mockStatic(SchedulingEngineUtil.class);
            MockedConstruction<UserImpl> userImplMock = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
                Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPRO);
            });
            MockedStatic<RTMService> objRTMService = Mockito.mockStatic(RTMService.class);

        ) {

            objSchedulingEngineUtil.when(()->SchedulingEngineUtil.updateEventReq(any(Map.class))).thenReturn(event);

            Map<String, Object> response = new HourService().updateEntry("accId", "entryID", objContact, payload, "client");
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(response));
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void updateTask_empty_task_desc_with_No_taskJDO_test(){

        Map<String, Object> mockEntry = MockEvent.getRawEvent();
        mockEntry.put("taskID", "1234");
        mockEntry.put("taskSource", "1234");
        mockEntry.put("taskDescription", "1234");

        try(MockedConstruction<TaskImpl> userTaskImpl = Mockito.mockConstruction(TaskImpl.class, (objTaskImpl, context)->{
                Mockito.when(objTaskImpl.getEntryByID(anyString())).thenReturn(null);
            })) {

            Map<String, Object> response = new HourService().updateTask(MockEvent.getRawEvent(), "", ClientSource.CLIENT_SOURCE_CONSTANTS.TRELLO.value());
            Assertions.assertTrue(response.containsKey("metaData"));
            Map<String, Object> metaData = (Map<String, Object>) response.get("metaData");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(metaData.get("taskID").toString()));
        }

    }

    @Test
    void updateTask_empty_task_desc_with_taskJDO_test(){

        Map<String, Object> mockEntry = MockEvent.getRawEvent();
        mockEntry.put("taskID", "1234");
        mockEntry.put("taskSource", "1234");
        mockEntry.put("taskDescription", "1234");

        try(MockedConstruction<TaskImpl> userTaskImpl = Mockito.mockConstruction(TaskImpl.class, (objTaskImpl, context)->{
            Mockito.when(objTaskImpl.getEntryByID(anyString())).thenReturn(new TaskJDO());
        })) {

            Map<String, Object> response = new HourService().updateTask(MockEvent.getRawEvent(), "", ClientSource.CLIENT_SOURCE_CONSTANTS.TRELLO.value());
            Assertions.assertTrue(response.containsKey("metaData"));
            Map<String, Object> metaData = (Map<String, Object>) response.get("metaData");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(metaData.get("taskID").toString()));
        }

    }

    @Test
    void updateTask_valid_task_desc_test(){

        Map<String, Object> mockEntry = MockEvent.getRawEvent();

        try(MockedStatic<ClientSource> clientSource = Mockito.mockStatic(ClientSource.class);
            MockedConstruction<TaskImpl> userTaskImpl = Mockito.mockConstruction(TaskImpl.class, (objTaskImpl, context)->{
                Mockito.when(objTaskImpl.getEntryByID(anyString())).thenReturn(null);
        })) {
            Map<String,String> clientIdMap  = new HashMap<>();
            clientIdMap.put(ClientSource.CLIENT_SOURCE_CONSTANTS.TRELLO.value(),"web");

            clientSource.when(()-> ClientSource.getClientIdMap()).thenReturn(clientIdMap);

            Map<String, Object> response = new HourService().updateTask(MockEvent.getRawEvent(), "test", ClientSource.CLIENT_SOURCE_CONSTANTS.TRELLO.value());
            Assertions.assertTrue(response.containsKey("metaData"));
            Map<String, Object> metaData = (Map<String, Object>) response.get("metaData");
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(metaData.get("taskID").toString()));
        }

    }

    @Test
    void getDatesForHoursPage_null_accountID_test() {
        try {
            new HourService().getDatesForHoursPage(null, new Contact(), "", "", "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getDatesForHoursPage_empty_accountID_test() {
        try {
            new HourService().getDatesForHoursPage("", new Contact(), "", "", "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getDatesForHoursPage_null_contact_test() {
        try {
            new HourService().getDatesForHoursPage("acc", null, "", "", "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void getDatesForHoursPage_empty_range_test() {
        try {
            new HourService().getDatesForHoursPage("acc", new Contact(), "", "", "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_RANGE.value(),e.getMessage());
        }
    }

    @Test
    void getDatesForHoursPage_null_range_test() {
        try {
            new HourService().getDatesForHoursPage("acc", new Contact(), null, "", "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_RANGE.value(),e.getMessage());
        }
    }

    @Test
    void getDatesForHoursPage_random_range_test() {
        try {
            new HourService().getDatesForHoursPage("acc", new Contact(), "monday", "", "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_RANGE.value(),e.getMessage());
        }
    }

    @Test
    void getDatesForHoursPage_by_date_range_with_out_from_test() {
        try {
            new HourService().getDatesForHoursPage("acc", new Contact(), "by_date", "", "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.FROM_AND_TO_DATE_MANDATORY.value(),e.getMessage());
        }
    }

    @Test
    void getDatesForHoursPage_by_date_range_with_out_to_test() {
        try {
            new HourService().getDatesForHoursPage("acc", new Contact(), "by_date", "04/24/2022", "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.FROM_AND_TO_DATE_MANDATORY.value(),e.getMessage());
        }
    }

    @Test
    void getDatesForHoursPage_success_test() {

        Contact objContact = new Contact();
        objContact.setId("1234");
        PeopleRelationJDO objPRO = new PeopleRelationJDO();
        objPRO.setTimeZone("Asia/Kolkata");
        SettingsJDO objAcc = new SettingsJDO();
        objAcc.setWeekStartDay("Monday");


        try(MockedStatic<DateUtil> mock = Mockito.mockStatic(DateUtil.class);
            MockedConstruction<UserImpl> mockUser = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
                Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPRO);
        });
            MockedConstruction<AccountImpl> mockAcc = Mockito.mockConstruction(AccountImpl.class, (objAccImpl, context)-> {
                Mockito.when(objAccImpl.getById(anyString())).thenReturn(objAcc);
        })){
            mock.when(()->DateUtil.getDatesList(anyString(),anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(new ArrayList<>());
            Map<String, Object> resp = new HourService().getDatesForHoursPage("acc", objContact, "current_week", "", "");
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));
            Assertions.assertTrue(resp.containsKey("dateList"));
        }

    }

    @Test
    void getAll_null_accountID_test() {
        try {
            new HourService().getAll(null, new Contact(), new HashMap<>());
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getAll_empty_accountID_test() {
        try {
            new HourService().getAll("", new Contact(), new HashMap<>());
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getAll_null_contact_test() {
        try {
            new HourService().getAll("acc",null, new HashMap<>());
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void getAll_null_payload_test() {
        try {
            new HourService().getAll("acc",new Contact(), null);
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void getAll_empty_payload_test() {
        try {
            new HourService().getAll("acc",new Contact(), new HashMap<>());
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void getAll_usernotfound_test() {
        Contact objContact = new Contact();
        objContact.setId("1234");
        Map<String, String> payload = new HashMap<>();
        payload.put("contactID", "1234");
        try(MockedConstruction<UserImpl> mockUserImpl = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
            Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(null);
        })) {
            new HourService().getAll("acc", objContact, payload);
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value(),e.getMessage());
        }
    }

    @Test
    void getAll_empty_getEntries_test() {
        Contact objContact = new Contact();
        objContact.setId("1234");
        PeopleRelationJDO objPRO = new PeopleRelationJDO();
        objPRO.setTimeZone("Asia/Kolkata");
        objPRO.setEmailID("test@gmail.com");
        RangeInfoDTO objRangeInfo = new RangeInfoDTO();
        objRangeInfo.setFromDateEpochMilliseconds(System.currentTimeMillis());
        objRangeInfo.setToDateEpochMilliseconds(System.currentTimeMillis());

        Map<String, String> payload = new HashMap<>();
        payload.put("contactID", "1234");
        payload.put("range", "current_week");
        payload.put("projectID", "234");

        try(MockedStatic<DateUtil> objDateMock = Mockito.mockStatic(DateUtil.class);
            MockedConstruction<UserImpl> mockUserImpl = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
                Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPRO);
        });
            MockedConstruction<ReportImpl> mockReportImpl = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
                Mockito.when(objReportImpl.getEntries(anyString(), anyString(), anyString(), anyString(),anyString(), anyBoolean())).thenReturn(new ArrayList<>());
            })) {

            System.out.println(ZoneId.of(objPRO.getTimeZone()));
            objDateMock.when(()->DateUtil.getRangeDetails(anyString(),anyString(), anyString(), anyString(), anyString())).thenReturn(objRangeInfo);
            objDateMock.when(()->DateUtil.convertMillisToDateTimeText(any(DateFormats.class), anyLong(), anyString())).thenReturn("date");

            Map<String, Object> resp = new HourService().getAll("acc", objContact, payload);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void getAll_project_id_filter_test() {
        Contact objContact = new Contact();
        objContact.setId("1234");
        PeopleRelationJDO objPRO = new PeopleRelationJDO();
        objPRO.setTimeZone("Asia/Kolkata");
        objPRO.setEmailID("test@gmail.com");
        RangeInfoDTO objRangeInfo = new RangeInfoDTO();
        objRangeInfo.setFromDateEpochMilliseconds(System.currentTimeMillis());
        objRangeInfo.setToDateEpochMilliseconds(System.currentTimeMillis());

        Map<String, String> payload = new HashMap<>();
        payload.put("contactID", "1234");
        payload.put("range", "current_week");
        payload.put("projectID", "5678");

        try(MockedStatic<DateUtil> objDateMock = Mockito.mockStatic(DateUtil.class);
            MockedConstruction<UserImpl> mockUserImpl = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
                Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPRO);
            });
            MockedConstruction<ReportImpl> mockReportImpl = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
                Mockito.when(objReportImpl.getEntries(anyString(), anyString(), anyString(), anyString(),anyString(), anyBoolean())).thenReturn(List.of(MockEvent.getRawEvent()));
            })) {
            objDateMock.when(()->DateUtil.getRangeDetails(anyString(),anyString(), anyString(), anyString(), anyString())).thenReturn(objRangeInfo);
            objDateMock.when(()->DateUtil.convertMillisToDateTimeText(any(DateFormats.class), anyLong(), anyString())).thenReturn("date");

            Map<String, Object> resp = new HourService().getAll("acc", objContact, payload);
            Assertions.assertTrue(resp.containsKey(SchedulingKeys.ENTRIES));
            Assertions.assertTrue(ObjUtils.isEmptyList((ArrayList)resp.get(SchedulingKeys.ENTRIES)));
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void getAll_valid_getEntries_test() {
        Contact objContact = new Contact();
        objContact.setId("1234");
        PeopleRelationJDO objPRO = new PeopleRelationJDO();
        objPRO.setTimeZone("Asia/Kolkata");
        objPRO.setEmailID("test@gmail.com");
        RangeInfoDTO objRangeInfo = new RangeInfoDTO();
        objRangeInfo.setFromDateEpochMilliseconds(1653974591813l);
        objRangeInfo.setToDateEpochMilliseconds(1653974591813l);

        Map<String, String> payload = new HashMap<>();
        payload.put("contactID", "1234");
        payload.put("range", "current_week");
        payload.put("projectID", "");

        try(MockedStatic<DateUtil> objDateMock = Mockito.mockStatic(DateUtil.class);
            MockedConstruction<UserImpl> mockUserImpl = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
                Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPRO);
            });
            MockedConstruction<ReportImpl> mockReportImpl = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
                Mockito.when(objReportImpl.getEntries("acc", "1234", "date", "date", SchedulingKeys.DESC, false)).thenReturn(List.of(MockEvent.getRawEvent()));
            })) {

            objDateMock.when(()->DateUtil.getRangeDetails(anyString(),anyString(), anyString(), anyString(), anyString())).thenReturn(objRangeInfo);
            objDateMock.when(()->DateUtil.convertMillisToDateTimeText(any(DateFormats.class), anyLong(), anyString())).thenReturn("date");

            Map<String, Object> resp = new HourService().getAll("acc", objContact, payload);
            Assertions.assertTrue(resp.containsKey(SchedulingKeys.ENTRIES));
            Assertions.assertTrue(!ObjUtils.isEmptyList((ArrayList)resp.get(SchedulingKeys.ENTRIES)));
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void deleteEntry_nullPro_test(){
        Contact contact = new Contact();
        contact.setId("123");
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserPro("accID","123")).thenReturn(null);
            HourService hourService = new HourService();
            Assertions.assertThrows(IllegalArgumentException.class,()->hourService.deleteEntry("accID","entID",contact,null));
        }
    }

    @Test
    void deleteEntry_validOutputTimezone_test() throws IOException, NoSuchAlgorithmException {
        Contact contact = new Contact();
        contact.setId("123");
        contact.setEmailID("email");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<EntryDeleteHelper> entryDeleteHelperMockedStatic = Mockito.mockStatic(EntryDeleteHelper.class);
            MockedConstruction<ContactImpl> contactMockedConstruction = Mockito.mockConstruction(ContactImpl.class,(contactImplMock,context)->{
                Mockito.when(contactImplMock.getByID("123")).thenReturn(contact);
            });
            MockedStatic<HoursTaskInitiator> hoursTaskInitiatorMockedStatic = Mockito.mockStatic(HoursTaskInitiator.class);
            MockedConstruction<ReportsDTO> reportsDTOMockedConstruction = Mockito.mockConstruction(ReportsDTO.class)){
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.validateAndDeleteEntry("entID",userPro)).thenReturn(Map.of("provider",List.of("123")));
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserPro("accID","123")).thenReturn(userPro);
            Map<String,Object> actual = new HourService().deleteEntry("accID","entID",contact,"Asia/Kolkata");
            hoursTaskInitiatorMockedStatic.verify(()->HoursTaskInitiator.initiateDeleteEntryQueue(userPro,contact,Map.of("provider",List.of("123"))));
            Assertions.assertEquals(Map.of("entry",reportsDTOMockedConstruction.constructed().get(0)),actual);
        }
    }

    @Test
    void deleteEntry_invalidOutputTimezone_test() throws IOException, NoSuchAlgorithmException {
        Contact contact = new Contact();
        contact.setId("123");
        contact.setEmailID("email");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setTimeZone("Asia/Colombo");
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<EntryDeleteHelper> entryDeleteHelperMockedStatic = Mockito.mockStatic(EntryDeleteHelper.class);
            MockedConstruction<ContactImpl> contactMockedConstruction = Mockito.mockConstruction(ContactImpl.class,(contactImplMock,context)->{
                Mockito.when(contactImplMock.getByID("123")).thenReturn(contact);
            });
            MockedStatic<HoursTaskInitiator> hoursTaskInitiatorMockedStatic = Mockito.mockStatic(HoursTaskInitiator.class);
            MockedConstruction<ReportsDTO> reportsDTOMockedConstruction = Mockito.mockConstruction(ReportsDTO.class)){
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.validateAndDeleteEntry("entID",userPro)).thenReturn(Map.of("provider",List.of("123")));
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserPro("accID","123")).thenReturn(userPro);
            Map<String,Object> actual = new HourService().deleteEntry("accID","entID",contact,null);
            hoursTaskInitiatorMockedStatic.verify(()->HoursTaskInitiator.initiateDeleteEntryQueue(userPro,contact,Map.of("provider",List.of("123"))));
            Assertions.assertEquals(Map.of("entry",reportsDTOMockedConstruction.constructed().get(0)),actual);
        }
    }
}
