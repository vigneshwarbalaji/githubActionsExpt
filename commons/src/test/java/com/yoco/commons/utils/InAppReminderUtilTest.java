package com.yoco.commons.utils;

import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.fullservices.FullReminders;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.utils.events.InAppReminderUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.modal.user.UserDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class InAppReminderUtilTest {

    InAppReminderUtil inAppReminderUtil = InAppReminderUtil.getInstance();

    @Test
    void handleInAppNotificationForClockOut_valid_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class);
            MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
            MockedConstruction<AccessManager> mock = Mockito.mockConstruction(AccessManager.class, (accessManagerMock, context) -> {
                Mockito.when(accessManagerMock.hasReminderPermission(Mockito.any(PeopleRelationJDO.class))).thenReturn(true);
            })){

            accountUtilMockedStatic.when(() -> AccountUtil.isActiveEnterpriseAccount(Mockito.any(SettingsJDO.class))).thenReturn(true);
            fullRemindersMockedStatic.when(() -> FullReminders.deleteJobs(anyString(),anyString(),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            SettingsJDO settingsJDO = new SettingsJDO();
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("accountId");
            userPRO.setContactId("contactId");
            userPRO.setDelete(false);
            new InAppReminderUtil().handleInAppNotificationForClockOut(settingsJDO,userPRO,"");
            accountUtilMockedStatic.verify(()-> AccountUtil.isActiveEnterpriseAccount(Mockito.any(SettingsJDO.class)),times(1));
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs("accountId","contactId","clock"),times(1));
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs("accountId","contactId", ReportsUtil.POMODORO),times(0));
        }
    }

    @Test
    void handleInAppNotificationForClockOut_pomodoro_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class);
            MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
            MockedConstruction<AccessManager> mock = Mockito.mockConstruction(AccessManager.class, (accessManagerMock, context) -> {
                Mockito.when(accessManagerMock.hasReminderPermission(Mockito.any(PeopleRelationJDO.class))).thenReturn(true);
            })){

            accountUtilMockedStatic.when(() -> AccountUtil.isActiveEnterpriseAccount(Mockito.any(SettingsJDO.class))).thenReturn(true);
            fullRemindersMockedStatic.when(() -> FullReminders.deleteJobs(anyString(),anyString(),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            SettingsJDO settingsJDO = new SettingsJDO();
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("accountId");
            userPRO.setContactId("contactId");
            userPRO.setDelete(false);
            new InAppReminderUtil().handleInAppNotificationForClockOut(settingsJDO,userPRO,ReportsUtil.POMODORO);
            accountUtilMockedStatic.verify(()-> AccountUtil.isActiveEnterpriseAccount(Mockito.any(SettingsJDO.class)),times(1));
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs("accountId","contactId","clock"),times(1));
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs("accountId","contactId",ReportsUtil.POMODORO),times(1));
        }
    }

    @Test
    void handleInAppNotificationForClockOut_noReminder_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class);
            MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
            MockedConstruction<AccessManager> mock = Mockito.mockConstruction(AccessManager.class, (accessManagerMock, context) -> {
                Mockito.when(accessManagerMock.hasReminderPermission(Mockito.any(PeopleRelationJDO.class))).thenReturn(false);
            })){

            accountUtilMockedStatic.when(() -> AccountUtil.isActiveEnterpriseAccount(Mockito.any(SettingsJDO.class))).thenReturn(true);

            SettingsJDO settingsJDO = new SettingsJDO();
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("accountId");
            userPRO.setContactId("contactId");
            userPRO.setDelete(false);
            new InAppReminderUtil().handleInAppNotificationForClockOut(settingsJDO,userPRO,ReportsUtil.POMODORO);
            accountUtilMockedStatic.verify(()-> AccountUtil.isActiveEnterpriseAccount(Mockito.any(SettingsJDO.class)),times(1));
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs("accountId","contactId","clock"),times(0));
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs("accountId","contactId",ReportsUtil.POMODORO),times(1));
        }
    }

    @Test
    void handleInAppNotificationForClockOut_userNotActive_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class);
            MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
            MockedConstruction<AccessManager> mock = Mockito.mockConstruction(AccessManager.class, (accessManagerMock, context) -> {
                Mockito.when(accessManagerMock.hasReminderPermission(Mockito.any(PeopleRelationJDO.class))).thenReturn(true);
            })){

            accountUtilMockedStatic.when(() -> AccountUtil.isActiveEnterpriseAccount(Mockito.any(SettingsJDO.class))).thenReturn(true);

            SettingsJDO settingsJDO = new SettingsJDO();
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("accountId");
            userPRO.setContactId("contactId");
            userPRO.setDelete(true);
            new InAppReminderUtil().handleInAppNotificationForClockOut(settingsJDO,userPRO,"");
            accountUtilMockedStatic.verify(()-> AccountUtil.isActiveEnterpriseAccount(Mockito.any(SettingsJDO.class)),times(1));
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs("accountId","contactId","clock"),times(0));
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs("accountId","contactId",ReportsUtil.POMODORO),times(0));
        }
    }

    @Test
    void handleInAppNotificationForClockOut_inActiveEnterpriseAccount_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class);
            MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
            MockedConstruction<AccessManager> mock = Mockito.mockConstruction(AccessManager.class, (accessManagerMock, context) -> {
                Mockito.when(accessManagerMock.hasReminderPermission(Mockito.any(PeopleRelationJDO.class))).thenReturn(true);
            })){

            accountUtilMockedStatic.when(() -> AccountUtil.isActiveEnterpriseAccount(Mockito.any(SettingsJDO.class))).thenReturn(false);

            SettingsJDO settingsJDO = new SettingsJDO();
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("accountId");
            userPRO.setContactId("contactId");
            userPRO.setDelete(true);
            new InAppReminderUtil().handleInAppNotificationForClockOut(settingsJDO,userPRO,"");
            accountUtilMockedStatic.verify(()-> AccountUtil.isActiveEnterpriseAccount(Mockito.any(SettingsJDO.class)),times(1));
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs("accountId","contactId","clock"),times(0));
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs("accountId","contactId",ReportsUtil.POMODORO),times(0));
        }
    }

    @Test
    void initiateInAppReminderForReminderPermission_ifSkillMapDontHavePermission_test() {
        try(MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class)){
            Map<String, Object> skillMap = new HashMap<>();
            UserDTO userDTO = new UserDTO();
            new InAppReminderUtil().handleInAppNotificationForReminderTimeChange(userDTO, skillMap);
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs(userDTO.getAccountID(),userDTO.getContactID(), "clock"),times(0));
        }
    }

    @Test
    void handleInAppNotificationForReminderTimeChange_emptyActiveEntriesList_test() {

        List<Map<String,Object>> entriesList = new ArrayList<>();
        try(MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
            MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
                Mockito.when(reportImplMock.getActiveEntry(any(), any())).thenReturn(entriesList);
            })){

            Map<String, Object> skillMap = new HashMap<>();
            skillMap.put("reminderTime", 1);
            skillMap.put(IAMPermission.REMINDER.toString(),"reminder");

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountID");
            userDTO.setContactID("contactID");
            userDTO.setTimezone("IST");
            userDTO.setZoneId("12");

            new InAppReminderUtil().handleInAppNotificationForReminderTimeChange(userDTO, skillMap);
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs(userDTO.getAccountID(),userDTO.getContactID(), "clock"),times(0));
        }
    }

    @Test
    void handleInAppNotificationForReminderTimeChange_exception_test() {

        Map<String, Object>entriesMap = new HashMap<>();
        List<Map<String,Object>> entriesList = new ArrayList<>();
        entriesList.add(entriesMap);

        try(MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getActiveEntry(any(), any())).thenReturn(entriesList);
        })){

            Map<String, Object> skillMap = new HashMap<>();
            skillMap.put("reminderTime", 1);
            skillMap.put(IAMPermission.REMINDER.toString(),"reminder");

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountID");
            userDTO.setContactID("contactID");
            userDTO.setTimezone("IST");
            userDTO.setZoneId("12");

            new InAppReminderUtil().handleInAppNotificationForReminderTimeChange(userDTO, skillMap);

        }catch (Exception e){
            Assertions.assertNull(e.getMessage());
        }
    }

    @Test
    void handleInAppNotificationForReminderTimeChange_valid_test() throws IOException {

        Map<String, Object>entriesMap = new HashMap<>();
        entriesMap.put(SchedulingKeys.START_TIME, 10L);
        entriesMap.put(SchedulingKeys.END_TIME, 100000000000000L);

        List<Map<String,Object>> entriesList = new ArrayList<>();
        entriesList.add(entriesMap);
        entriesList.add(entriesMap);

        Map<String, Object>dataMap = new HashMap<>();
        dataMap.put(SchedulingKeys.EVENTS, entriesList);

        Map<String, Object>schedulingResponse = new HashMap<>();
        schedulingResponse.put(SchedulingKeys.DATA, dataMap);

        try(MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getActiveEntry(any(), any())).thenReturn(entriesList);
            Mockito.when(reportImplMock.getEntriesForCurrentDay(any())).thenReturn(entriesList);
        });
            MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
        ){

            Map<String, Object> skillMap = new HashMap<>();
            skillMap.put("reminderTime", 1);
            skillMap.put(IAMPermission.REMINDER.toString(),"reminder");

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountID");
            userDTO.setContactID("contactID");
            userDTO.setTimezone("IST");
            userDTO.setZoneId("12");

            new InAppReminderUtil().handleInAppNotificationForReminderTimeChange(userDTO, skillMap);
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs(userDTO.getAccountID(),userDTO.getContactID(), "clock"),times(1));
        }
    }

    @Test
    void initiateInAppReminderForReminderPermission_ifReminderSkillIsNotEnabled_test() {
        try(MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class)){
            Map<String, Object> skillMap = new HashMap<>();
            UserDTO userDTO = new UserDTO();
            new InAppReminderUtil().initiateInAppReminderForReminderPermission(userDTO, false, skillMap);
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs(userDTO.getAccountID(),userDTO.getContactID(), "clock"),times(1));
        }
    }

    @Test
    void initiateInAppReminderForReminderPermission_ifActiveEntryListIsEmpty_test() {

        List<Map<String,Object>> entriesList = new ArrayList<>();
        try(MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
            MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getActiveEntry(any(), any())).thenReturn(entriesList);
        })){

            Map<String, Object> skillMap = new HashMap<>();
            skillMap.put("reminderTime", 1);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountID");
            userDTO.setContactID("contactID");
            userDTO.setTimezone("IST");
            userDTO.setZoneId("12");

            new InAppReminderUtil().initiateInAppReminderForReminderPermission(userDTO, true, skillMap);
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs(userDTO.getAccountID(),userDTO.getContactID(), "clock"),times(0));
        }
    }

    @Test
    void initiateInAppReminderForReminderPermission_emptyMapException_test() {

        Map<String, Object>entriesMap = new HashMap<>();
        List<Map<String,Object>> entriesList = new ArrayList<>();
        entriesList.add(entriesMap);

        try(MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getActiveEntry(any(), any())).thenReturn(entriesList);
        })){

            Map<String, Object> skillMap = new HashMap<>();
            skillMap.put("reminderTime", 1);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountID");
            userDTO.setContactID("contactID");
            userDTO.setTimezone("IST");
            userDTO.setZoneId("12");

            new InAppReminderUtil().initiateInAppReminderForReminderPermission(userDTO, true, skillMap);

        }catch (Exception e){
            Assertions.assertNull(e.getMessage());
        }
    }

    @Test
    void initiateInAppReminderForReminderPermission_valid_test() throws IOException {

        Map<String, Object>entriesMap = new HashMap<>();
        entriesMap.put(SchedulingKeys.START_TIME, 10L);
        entriesMap.put(SchedulingKeys.END_TIME, 100000000000000L);

        List<Map<String,Object>> entriesList = new ArrayList<>();
        entriesList.add(entriesMap);
        entriesList.add(entriesMap);

        Map<String, Object>dataMap = new HashMap<>();
        dataMap.put(SchedulingKeys.EVENTS, entriesList);

        Map<String, Object>schedulingResponse = new HashMap<>();
        schedulingResponse.put(SchedulingKeys.DATA, dataMap);

        try(MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getActiveEntry(any(), any())).thenReturn(entriesList);
            Mockito.when(reportImplMock.getEntriesForCurrentDay(any())).thenReturn(entriesList);
        });
            MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
        ){

            Map<String, Object> skillMap = new HashMap<>();
            skillMap.put("reminderTime", 1);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountID");
            userDTO.setContactID("contactID");
            userDTO.setTimezone("IST");
            userDTO.setZoneId("12");

            inAppReminderUtil.initiateInAppReminderForReminderPermission(userDTO, true, skillMap);
            fullRemindersMockedStatic.verify(()-> FullReminders.deleteJobs(userDTO.getAccountID(),userDTO.getContactID(), "clock"),times(0));
        }
    }

    @Test
    void calculateAndSetReminder_ifUserTotalDurationHasHigherValue_test() throws IOException, NoSuchAlgorithmException {

        Map<String, Object>entriesMap = new HashMap<>();
        entriesMap.put(SchedulingKeys.START_TIME, 10L);
        entriesMap.put(SchedulingKeys.END_TIME, 100000000000000L);

        List<Map<String,Object>> entriesList = new ArrayList<>();
        entriesList.add(entriesMap);
        entriesList.add(entriesMap);

        Map<String, Object>dataMap = new HashMap<>();
        dataMap.put(SchedulingKeys.EVENTS, entriesList);

        Map<String, Object>schedulingResponse = new HashMap<>();
        schedulingResponse.put(SchedulingKeys.DATA, dataMap);

        try(MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
            MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class);
            MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
                Mockito.when(reportImplMock.getEntriesForCurrentDay(any())).thenReturn(entriesList);})){
            Map<String, Object> skillMap = new HashMap<>();
            skillMap.put("reminderTime", 1);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountID");
            userDTO.setContactID("contactID");
            userDTO.setTimezone("IST");
            userDTO.setZoneId("12");

            String callBackURL = CommonAppProperties.getYoCoDefaultApiUrl() + "/api/v2/account/" + userDTO.getAccountID() + "/contact/" + userDTO.getContactID() + "/clock/remind";

            dateUtilMockedStatic.when(() -> DateUtil.getCurrentTime()).thenReturn(0L);

            inAppReminderUtil.calculateAndSetReminder(userDTO, 10000000000000L, skillMap);
            fullRemindersMockedStatic.verify(()-> FullReminders.createJob(userDTO.getAccountID(), "1532_03_25_03_02_23", userDTO.getTimezone(), userDTO.getContactID(), callBackURL, "clock", 0),times(0));
        }
    }

    @Test
    void calculateAndSetReminder_valid_test() throws IOException, NoSuchAlgorithmException {

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

        try(MockedStatic<FullReminders> fullRemindersMockedStatic = Mockito.mockStatic(FullReminders.class);
            MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class);
            MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
                Mockito.when(reportImplMock.getEntriesForCurrentDay(any())).thenReturn(entriesList);})) {

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountID");
            userDTO.setContactID("contactID");
            userDTO.setTimezone("IST");
            userDTO.setZoneId("12");

            dateUtilMockedStatic.when(() -> DateUtil.convertMillisToDateTimeText(any(), anyLong(), anyString())).thenReturn("1532_03_25_03_02_23");

            String callBackURL = CommonAppProperties.getYoCoDefaultApiUrl() + "/api/v2/account/" + userDTO.getAccountID() + "/contact/" + userDTO.getContactID() + "/clock/remind";

            Map<String, Object> skillMap = new HashMap<>();
            skillMap.put("reminderTime", 7);

            inAppReminderUtil.calculateAndSetReminder(userDTO, 10000L, skillMap);
            fullRemindersMockedStatic.verify(()-> FullReminders.createJob(userDTO.getAccountID(), "1532_03_25_03_02_23", userDTO.getTimezone(), userDTO.getContactID(), callBackURL, "clock", 0),times(1));
        }
    }

    @Test
    void getReminderTimeInMillis_ifReminderTimeIsInvalidInteger_test(){
        Map<String, Object> skillMap = new HashMap<>();
        skillMap.put("reminderTime", -2);
        Assertions.assertEquals(28800000L,inAppReminderUtil.getReminderTimeInMillis(skillMap));
    }

    @Test
    void getReminderTimeInMillis_ifReminderTimeIsNull_test(){
        Map<String, Object> skillMap = new HashMap<>();
        skillMap.put("reminderTime", null);
        Assertions.assertEquals(28800000L,inAppReminderUtil.getReminderTimeInMillis(skillMap));
    }

    @Test
    void getReminderTimeInMillis_valid_test(){
        Map<String, Object> skillMap = new HashMap<>();
        skillMap.put("reminderTime", 7);
        Assertions.assertEquals(25200000L,inAppReminderUtil.getReminderTimeInMillis(skillMap));
    }

    @Test
    void handleInAppNotificationForAdjustment_notCurrentDay_test(){
        try(MockedStatic<InAppReminderUtil> inAppReminderUtilMockedStatic = Mockito.mockStatic(InAppReminderUtil.class)){
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1659015726000l);
            adjustmentDTO.setOriginalInTime(1659019588000l);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Map.of()));

            InAppReminderUtil inAppReminderUtil = Mockito.mock(InAppReminderUtil.class);
            Mockito.doCallRealMethod().when(inAppReminderUtil).handleInAppNotificationForAdjustment(any(),any(),anyString());
            Mockito.doNothing().when(inAppReminderUtil).handleInAppNotificationForReminderTimeChange(any(),any());
            inAppReminderUtilMockedStatic.when(InAppReminderUtil::getInstance).thenReturn(inAppReminderUtil);

            inAppReminderUtil.handleInAppNotificationForAdjustment(adjustmentDTO,userPro, DateConstants.ZONE_ID_UTC);
            Mockito.verify(inAppReminderUtil,times(0)).handleInAppNotificationForReminderTimeChange(any(),any());
        }
    }

    @Test
    void handleInAppNotificationForAdjustment_originalMillis_currentDay_test(){
        try(MockedStatic<InAppReminderUtil> inAppReminderUtilMockedStatic = Mockito.mockStatic(InAppReminderUtil.class)){
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1659015726000l);
            adjustmentDTO.setOriginalInTime(new Date().getTime());
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Map.of()));

            InAppReminderUtil inAppReminderUtil = Mockito.mock(InAppReminderUtil.class);
            Mockito.doCallRealMethod().when(inAppReminderUtil).handleInAppNotificationForAdjustment(any(),any(),anyString());
            Mockito.doNothing().when(inAppReminderUtil).handleInAppNotificationForReminderTimeChange(any(),any());
            inAppReminderUtilMockedStatic.when(InAppReminderUtil::getInstance).thenReturn(inAppReminderUtil);

            inAppReminderUtil.handleInAppNotificationForAdjustment(adjustmentDTO,userPro, DateConstants.ZONE_ID_UTC);
            Mockito.verify(inAppReminderUtil).handleInAppNotificationForReminderTimeChange(new UserDTO(userPro,null),UserPROUtil.extractPROSkills(userPro));
        }
    }

    @Test
    void handleInAppNotificationForAdjustment_adjustedMillis_currentDay_test(){
        try(MockedStatic<InAppReminderUtil> inAppReminderUtilMockedStatic = Mockito.mockStatic(InAppReminderUtil.class)){
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(new Date().getTime());
            adjustmentDTO.setOriginalInTime(1659015726000l);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Map.of()));

            InAppReminderUtil inAppReminderUtil = Mockito.mock(InAppReminderUtil.class);
            Mockito.doCallRealMethod().when(inAppReminderUtil).handleInAppNotificationForAdjustment(any(),any(),anyString());
            Mockito.doNothing().when(inAppReminderUtil).handleInAppNotificationForReminderTimeChange(any(),any());
            inAppReminderUtilMockedStatic.when(InAppReminderUtil::getInstance).thenReturn(inAppReminderUtil);

            inAppReminderUtil.handleInAppNotificationForAdjustment(adjustmentDTO,userPro, DateConstants.ZONE_ID_UTC);
            Mockito.verify(inAppReminderUtil).handleInAppNotificationForReminderTimeChange(new UserDTO(userPro,null),UserPROUtil.extractPROSkills(userPro));
        }
    }

}
