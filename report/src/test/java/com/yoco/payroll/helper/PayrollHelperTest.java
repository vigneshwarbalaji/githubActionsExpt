package com.yoco.payroll.helper;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.PayrollImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.PayrollStatus;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.date.RangeInfoDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.events.ClockUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.constants.EventConstants;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class PayrollHelperTest {

    PayrollHelper payrollHelper = PayrollHelper.getInstance();

    @Test
    void getUserConfirmedPayrollDetails_ifEntriesListIsNullOrEmpty_test() throws IOException, NoSuchAlgorithmException {

        Map<String, Object> eventsMap = new HashMap<>();

        SettingsJDO account = new SettingsJDO();
        account.setPeopleUniquePin("123");
        account.setPayrollAccessSince(72L);

        try(MockedConstruction<AccountImpl> accountMock = Mockito.mockConstruction(AccountImpl.class, (accountImplMock, context) -> {
            Mockito.when(accountImplMock.getById(anyString())).thenReturn(account);
        });
            MockedConstruction<PayrollImpl> payrollMock = Mockito.mockConstruction(PayrollImpl.class, (payrollImplMock, context) -> {
                Mockito.when(payrollImplMock.getPayrollEvents(anyString(), anyString(), anyString(), anyString(), anyString(), anyInt())).thenReturn(eventsMap);
            })
        ) {
            RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
            rangeInfoDTO.setFromDateEpochMilliseconds(111L);
            rangeInfoDTO.setToDateEpochMilliseconds(112L);
            Map<String, Object>responseMap = new PayrollHelper().getUserConfirmedPayrollDetails("accountID", "Asia/Kolkata", rangeInfoDTO,"");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(responseMap));
        }
    }

    @Test
    void getUserConfirmedPayrollDetails_ifThereIsNoPayrollAccessibility_test() throws IOException, NoSuchAlgorithmException {

        SettingsJDO account = new SettingsJDO();
        account.setPeopleUniquePin("123");
        account.setPayrollAccessSince(72222222222222222L);

        try(MockedConstruction<AccountImpl> accountMock = Mockito.mockConstruction(AccountImpl.class, (accountImplMock, context) -> {
            Mockito.when(accountImplMock.getById(anyString())).thenReturn(account);
        })) {
            RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
            rangeInfoDTO.setFromDateEpochMilliseconds(111L);
            rangeInfoDTO.setToDateEpochMilliseconds(112L);
            Map<String, Object>responseMap = new PayrollHelper().getUserConfirmedPayrollDetails("accountID", "Asia/Kolkata", rangeInfoDTO,"");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(responseMap));
        }
    }

    @Test
    void getUserConfirmedPayrollDetails_valid_test() throws IOException, NoSuchAlgorithmException {

        SettingsJDO account = new SettingsJDO();
        account.setPeopleUniquePin("123");
        account.setPayrollAccessSince(72L);

        Map<String, Object>event1 = new HashMap<>();
        event1.put(SchedulingKeys.PAYMENT_STATUS, InternalUsage.DEFAULT_PAYROLL);
        event1.put(SchedulingKeys.START_TIME,1656651883000l);
        event1.put(SchedulingKeys.END_TIME, 1656651883078l);
        event1.put(SchedulingKeys.ID, "id1");
        event1.put(SchedulingKeys.MERCHANT, "111");
        event1.put(SchedulingKeys.PROVIDER,new ArrayList<>(){{add("contId");}});
        event1.put(SchedulingKeys.SOURCE, "PC");
        event1.put(SchedulingKeys.PAYMENT_STATUS, "USER_ACKNOWLEDGED");
        event1.put(SchedulingKeys.IS_DELETED, false);
        List<Map<String, Object>> payrollEventsList = new ArrayList<>();
        payrollEventsList.add(event1);
        Map<String, Object> schedulingResponseDataMap = new HashMap<>();
        schedulingResponseDataMap.put(SchedulingKeys.EVENTS, payrollEventsList);
        Map<String, Object> eventsMap = new HashMap<>();
        eventsMap.put(SchedulingKeys.DATA,schedulingResponseDataMap);

        try(MockedConstruction<AccountImpl> accountMock = Mockito.mockConstruction(AccountImpl.class, (accountImplMock, context) -> {
            Mockito.when(accountImplMock.getById(anyString())).thenReturn(account);
        });
            MockedConstruction<PayrollImpl> payrollMock = Mockito.mockConstruction(PayrollImpl.class, (payrollImplMock, context) -> {
                Mockito.when(payrollImplMock.getPayrollEvents(anyString(), anyString(), anyString(), anyString(), anyString(), anyInt())).thenReturn(eventsMap);
            })
        ) {
            RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
            rangeInfoDTO.setFromDateEpochMilliseconds(111L);
            rangeInfoDTO.setToDateEpochMilliseconds(112L);
            Map<String, Object>responseMap = new PayrollHelper().getUserConfirmedPayrollDetails("accountID", "Asia/Kolkata", rangeInfoDTO, "");
            Assertions.assertTrue(!ObjUtils.isNull(responseMap.get(SchedulingKeys.ENTRIES)));
        }
    }

    @Test
    void verifyPayrollAccessibilityForTheUser_valid_test() {
        SettingsJDO account = new SettingsJDO();
        account.setPeopleUniquePin("123");
        account.setPayrollAccessSince(72L);

        RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
        rangeInfoDTO.setFromDateEpochMilliseconds(123L);
        rangeInfoDTO.setToDateEpochMilliseconds(123L);

        try(MockedConstruction<AccountImpl> accountMock = Mockito.mockConstruction(AccountImpl.class, (accountImplMock, context) -> {
            Mockito.when(accountImplMock.getById(anyString())).thenReturn(account);
        })) {
            Map<String, Object> responseMap = new PayrollHelper().verifyPayrollAccessibilityForTheUser("123", rangeInfoDTO);
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(responseMap));
        }
    }

    @Test
    void checkIfTheGivenTimeIsAccessibleForTheUserBasedOnPayrollDate_ifStartMillisAndEndMillisIsNotWithinThePayrollDate_test() {
        RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
        rangeInfoDTO.setFromDateEpochMilliseconds(123L);
        rangeInfoDTO.setToDateEpochMilliseconds(210L);
        Map<String, Object> responseMap = new PayrollHelper().checkIfTheGivenTimeIsAccessibleForTheUserBasedOnPayrollDate(222L, rangeInfoDTO);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(responseMap));
    }

    @Test
    void checkIfTheGivenTimeIsAccessibleForTheUserBasedOnPayrollDate_ifStartMillisIsLesserThanPayrollDate_test() {
        RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
        rangeInfoDTO.setFromDateEpochMilliseconds(123L);
        rangeInfoDTO.setToDateEpochMilliseconds(231L);
        Map<String, Object> responseMap = new PayrollHelper().checkIfTheGivenTimeIsAccessibleForTheUserBasedOnPayrollDate(222L, rangeInfoDTO);
        Assertions.assertEquals(222L, responseMap.get(DateConstants.START_MILLIS));
    }

    @Test
    void checkIfTheGivenTimeIsAccessibleForTheUserBasedOnPayrollDate_ifStartMillisIsGreaterThanPayrollDate_test() {
        RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
        rangeInfoDTO.setFromDateEpochMilliseconds(123L);
        rangeInfoDTO.setToDateEpochMilliseconds(231L);
        Map<String, Object> responseMap = new PayrollHelper().checkIfTheGivenTimeIsAccessibleForTheUserBasedOnPayrollDate(111L, rangeInfoDTO);
        Assertions.assertEquals(123L, responseMap.get(DateConstants.START_MILLIS));
    }

    @Test
    void convertSchedulingEventsIntoListOfEntries_ifDataMapIsEmpty_test() {
        Map<String, Object> schedulingResponseDataMap = new HashMap<>();
        Map<String, Object> eventsMap = new HashMap<>();
        eventsMap.put(SchedulingKeys.DATA,schedulingResponseDataMap);
        Map<String, Object>responseMap = new PayrollHelper().convertSchedulingEventsIntoListOfEntries(eventsMap, "Asia/Kolkata");
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(responseMap));
    }

    @Test
    void convertSchedulingEventsIntoListOfEntries_ifEventsMapIsEmpty_test() {
        List<Map<String, Object>> payrollEventsList = new ArrayList<>();
        Map<String, Object> schedulingResponseDataMap = new HashMap<>();
        schedulingResponseDataMap.put(SchedulingKeys.EVENTS, payrollEventsList);
        Map<String, Object> eventsMap = new HashMap<>();
        eventsMap.put(SchedulingKeys.DATA,schedulingResponseDataMap);
        Map<String, Object>responseMap = new PayrollHelper().convertSchedulingEventsIntoListOfEntries(eventsMap, "Asia/Kolkata");
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(responseMap));
    }

    @Test
    void convertSchedulingEventsIntoListOfEntries_valid_test() {
        Map<String, Object>event1 = new HashMap<>();
        event1.put(SchedulingKeys.PAYMENT_STATUS, InternalUsage.DEFAULT_PAYROLL);
        event1.put(SchedulingKeys.START_TIME,1656651883000l);
        event1.put(SchedulingKeys.END_TIME, 1656651883078l);
        event1.put(SchedulingKeys.ID, "id1");
        event1.put(SchedulingKeys.MERCHANT, "111");
        event1.put(SchedulingKeys.PROVIDER,new ArrayList<>(){{add("contId");}});
        event1.put(SchedulingKeys.SOURCE, "PC");
        event1.put(SchedulingKeys.PAYMENT_STATUS, "USER_ACKNOWLEDGED");
        event1.put(SchedulingKeys.IS_DELETED, false);
        List<Map<String, Object>> payrollEventsList = new ArrayList<>();
        payrollEventsList.add(event1);
        Map<String, Object> schedulingResponseDataMap = new HashMap<>();
        schedulingResponseDataMap.put(SchedulingKeys.EVENTS, payrollEventsList);
        schedulingResponseDataMap.put(Commons.CURSOR, "newCursor");
        Map<String, Object> eventsMap = new HashMap<>();
        eventsMap.put(SchedulingKeys.DATA,schedulingResponseDataMap);
        Map<String, Object>responseMap = new PayrollHelper().convertSchedulingEventsIntoListOfEntries(eventsMap, "Asia/Kolkata");
        Assertions.assertTrue(!ObjUtils.isNull(responseMap.get(SchedulingKeys.ENTRIES)));
        Assertions.assertEquals("newCursor", responseMap.get(Commons.CURSOR));
    }

    @Test
    void getAdminUpdatedPayrollDetails_ifThereIsNoPayrollAccessForTheUser_test() throws IOException, NoSuchAlgorithmException {
        SettingsJDO account = new SettingsJDO();
        account.setPeopleUniquePin("accountID");
        account.setPayrollAccessSince(72000L);

        try(MockedConstruction<AccountImpl> accountMock = Mockito.mockConstruction(AccountImpl.class, (accountImplMock, context) -> {
            Mockito.when(accountImplMock.getById(anyString())).thenReturn(account);
        })
        ) {
            RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
            rangeInfoDTO.setFromDateEpochMilliseconds(111L);
            rangeInfoDTO.setToDateEpochMilliseconds(112L);
            Map<String, Object>responseMap = new PayrollHelper().getAdminUpdatedPayrollDetails("accountID", "Asia/Kolkata", rangeInfoDTO);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(responseMap));
        }
    }

    @Test
    void getAdminUpdatedPayrollDetails_valid_test() throws IOException, NoSuchAlgorithmException {
        SettingsJDO account = new SettingsJDO();
        account.setPeopleUniquePin("accountID");
        account.setPayrollAccessSince(72L);

        Map<String, Object>event1 = new HashMap<>();
        event1.put(SchedulingKeys.START_TIME, 111L);
        event1.put(SchedulingKeys.END_TIME, 112L);
        event1.put(SchedulingKeys.ID, "id1");
        event1.put(SchedulingKeys.MERCHANT, "accountID");
        event1.put(SchedulingKeys.PROVIDER,new ArrayList<>(){{add("contId");}});
        event1.put(SchedulingKeys.SOURCE, "PC");
        event1.put(SchedulingKeys.PAYMENT_STATUS, "ADMIN_UPDATED");
        event1.put(SchedulingKeys.IS_DELETED, false);
        List<Map<String, Object>> payrollEventsList = new ArrayList<>();
        payrollEventsList.add(event1);
        Map<String, Object> schedulingResponseDataMap = new HashMap<>();
        schedulingResponseDataMap.put(SchedulingKeys.EVENTS, payrollEventsList);
        Map<String, Object> eventsMap = new HashMap<>();
        eventsMap.put(SchedulingKeys.DATA,schedulingResponseDataMap);

        try(MockedConstruction<PayrollImpl> payrollMock = Mockito.mockConstruction(PayrollImpl.class, (payrollImplMock, context) -> {
            Mockito.when(payrollImplMock.getPayrollEventsBasedOnStatus(anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(eventsMap);
        });
            MockedConstruction<AccountImpl> accountMock = Mockito.mockConstruction(AccountImpl.class, (accountImplMock, context) -> {
                Mockito.when(accountImplMock.getById(anyString())).thenReturn(account);
            })) {
            RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
            rangeInfoDTO.setFromDateEpochMilliseconds(111L);
            rangeInfoDTO.setToDateEpochMilliseconds(112L);
            Map<String, Object>responseMap = new PayrollHelper().getAdminUpdatedPayrollDetails("accountID", "Asia/Kolkata", rangeInfoDTO);
            List<AdjustmentDTO> adjustmentList= (List<AdjustmentDTO>) responseMap.get(EventConstants.ADJUSTMENTS);
            Assertions.assertEquals("id1",adjustmentList.get(0).getId());
            Assertions.assertEquals("accountID", adjustmentList.get(0).getAccountID());
        }
    }

    @Test
    void convertSchedulingEventsIntoListOfAdjustments_dataIsNullOrEmpty_test() throws IOException, NoSuchAlgorithmException {
        Map<String, Object> schedulingResponseDataMap = new HashMap<>();
        Map<String, Object> eventsMap = new HashMap<>();
        eventsMap.put(SchedulingKeys.DATA,schedulingResponseDataMap);
        Map<String, Object>responseMap = new PayrollHelper().convertSchedulingEventsIntoListOfAdjustments(eventsMap, "Asia/Kolkata");
        List<AdjustmentDTO> adjustmentList= (List<AdjustmentDTO>) responseMap.get(EventConstants.ADJUSTMENTS);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(adjustmentList));
    }

    @Test
    void convertSchedulingEventsIntoListOfAdjustments_eventsIsNullOrEmpty_test() throws IOException, NoSuchAlgorithmException {
        List<Map<String, Object>> payrollEventsList = new ArrayList<>();
        Map<String, Object> schedulingResponseDataMap = new HashMap<>();
        schedulingResponseDataMap.put(SchedulingKeys.EVENTS, payrollEventsList);
        Map<String, Object> eventsMap = new HashMap<>();
        eventsMap.put(SchedulingKeys.DATA,schedulingResponseDataMap);
        Map<String, Object>responseMap = new PayrollHelper().convertSchedulingEventsIntoListOfAdjustments(eventsMap, "Asia/Kolkata");
        List<AdjustmentDTO> adjustmentList= (List<AdjustmentDTO>) responseMap.get(EventConstants.ADJUSTMENTS);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(adjustmentList));
    }

    @Test
    void convertSchedulingEventsIntoListOfAdjustments_valid_test() throws IOException, NoSuchAlgorithmException {
        Map<String, Object>event1 = new HashMap<>();
        event1.put(SchedulingKeys.START_TIME, 111L);
        event1.put(SchedulingKeys.END_TIME, 112L);
        event1.put(SchedulingKeys.ID, "id1");
        event1.put(SchedulingKeys.MERCHANT, "accountID");
        event1.put(SchedulingKeys.PROVIDER,new ArrayList<>(){{add("contId");}});
        event1.put(SchedulingKeys.SOURCE, "PC");
        event1.put(SchedulingKeys.PAYMENT_STATUS, "ADMIN_UPDATED");
        event1.put(SchedulingKeys.IS_DELETED, false);
        List<Map<String, Object>> payrollEventsList = new ArrayList<>();
        payrollEventsList.add(event1);
        Map<String, Object> schedulingResponseDataMap = new HashMap<>();
        schedulingResponseDataMap.put(SchedulingKeys.EVENTS, payrollEventsList);
        Map<String, Object> eventsMap = new HashMap<>();
        eventsMap.put(SchedulingKeys.DATA,schedulingResponseDataMap);
        Map<String, Object>responseMap = new PayrollHelper().convertSchedulingEventsIntoListOfAdjustments(eventsMap, "Asia/Kolkata");
        List<AdjustmentDTO> adjustmentList= (List<AdjustmentDTO>) responseMap.get(EventConstants.ADJUSTMENTS);
        Assertions.assertEquals("id1",adjustmentList.get(0).getId());
        Assertions.assertEquals("accountID", adjustmentList.get(0).getAccountID());
    }

    @ParameterizedTest
    @NullAndEmptySource
    void validateAndExtractApprovalEntryIDsFromPayload_empty_payload_test(String testValue){
        try{
            payrollHelper.validateAndExtractApprovalEntryIDsFromPayload(testValue);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractApprovalEntryIDsFromPayload_invalid_json_test() {
        try {
            payrollHelper.validateAndExtractApprovalEntryIDsFromPayload(JsonUtil.getJson(""));
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(), e.getMessage());
        }
    }


    @Test
    void validateAndExtractApprovalEntryIDsFromPayload_invalid_json_test2() {
        try {
            payrollHelper.validateAndExtractApprovalEntryIDsFromPayload(JsonUtil.getJson(Map.of()));
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(), e.getMessage());
        }
    }



    @Test
    void validateAndExtractApprovalEntryIDsFromPayload_no_entryID_test() {
        try {
            payrollHelper.validateAndExtractApprovalEntryIDsFromPayload(JsonUtil.getJson(Map.of("test", "value")));
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(), e.getMessage());
        }
    }

    @Test
    void validateAndExtractApprovalEntryIDsFromPayload_empty_entryID_test() {
        try {
            payrollHelper.validateAndExtractApprovalEntryIDsFromPayload(JsonUtil.getJson(Map.of("entryID", "")));
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(), e.getMessage());
        }
    }

    @Test
    void validateAndExtractApprovalEntryIDsFromPayload_valid_test(){
        Assertions.assertEquals(List.of("123","456","789"),payrollHelper.validateAndExtractApprovalEntryIDsFromPayload(JsonUtil.getJson(Map.of("entryID","123,456,789"))));
    }

    @Test
    void isValidPayrollEvent_notValid_account_test(){
        Map<String,Object> event = new HashMap<>();
        event.put(SchedulingKeys.MERCHANT,"differentAccId");
        event.put(SchedulingKeys.PROVIDER,List.of("contId"));
        event.put(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString()));
        event.put(SchedulingKeys.START_TIME,1659961794000l);
        event.put(SchedulingKeys.PAYMENT_STATUS,PayrollStatus.USER_ACKNOWLEDGED.toString());
        Assertions.assertFalse(payrollHelper.isValidPayrollEvent(event,"accId","contId",1660120845000l));
    }

    @Test
    void isValidPayrollEvent_notValid_contact_test(){
        Map<String,Object> event = new HashMap<>();
        event.put(SchedulingKeys.MERCHANT,"accId1");
        event.put(SchedulingKeys.PROVIDER,List.of("differentContactID"));
        event.put(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString()));
        event.put(SchedulingKeys.START_TIME,1659961794000l);
        event.put(SchedulingKeys.PAYMENT_STATUS,PayrollStatus.USER_ACKNOWLEDGED.toString());
        Assertions.assertFalse(payrollHelper.isValidPayrollEvent(event,"accId1","contId1",1660120845000l));
    }


    @Test
    void isValidPayrollEvent_notValid_EventStatus_test(){
        Map<String,Object> event = new HashMap<>();
        event.put(SchedulingKeys.MERCHANT,"accId2");
        event.put(SchedulingKeys.PROVIDER,List.of("contId2"));
        event.put(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, REPORT_STATUS.CLOCKED_IN.toString()));
        event.put(SchedulingKeys.START_TIME,1659961794000l);
        event.put(SchedulingKeys.PAYMENT_STATUS,PayrollStatus.USER_ACKNOWLEDGED.toString());
        Assertions.assertFalse(payrollHelper.isValidPayrollEvent(event,"accId2","contId2",1660120845000l));
    }

    @Test
    void isValidPayrollEvent_eventTime_lessThan_payrollTime_test(){
        Map<String,Object> event = new HashMap<>();
        event.put(SchedulingKeys.MERCHANT,"accId");
        event.put(SchedulingKeys.PROVIDER,List.of("contId"));
        event.put(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString()));
        event.put(SchedulingKeys.START_TIME,1659961794000l);
        event.put(SchedulingKeys.PAYMENT_STATUS,PayrollStatus.USER_ACKNOWLEDGED.toString());
        Assertions.assertFalse(payrollHelper.isValidPayrollEvent(event,"accId","contId",1660120845000l));
    }

    @Test
    void isValidPayrollEvent_eventTime_approvedPayrollStatus_test(){
        Map<String,Object> event = new HashMap<>();
        event.put(SchedulingKeys.MERCHANT,"accId");
        event.put(SchedulingKeys.PROVIDER,List.of("contId"));
        event.put(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString()));
        event.put(SchedulingKeys.START_TIME,1659961794000l);
        event.put(SchedulingKeys.PAYMENT_STATUS,PayrollStatus.ADMIN_APPROVED.toString());
        Assertions.assertFalse(payrollHelper.isValidPayrollEvent(event,"accId","contId",1660120845000l));
    }

    @Test
    void isValidPayrollEvent_valid_test(){
        Map<String,Object> event = new HashMap<>();
        event.put(SchedulingKeys.MERCHANT,"accId");
        event.put(SchedulingKeys.PROVIDER,List.of("contId"));
        event.put(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString()));
        event.put(SchedulingKeys.START_TIME,1660120845000l);
        event.put(SchedulingKeys.PAYMENT_STATUS,PayrollStatus.USER_ACKNOWLEDGED.toString());
        Assertions.assertTrue(payrollHelper.isValidPayrollEvent(event,"accId","contId",1659961794000l));
    }

    @Test
    void getPayrollEvents_noEvents_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getEventsByIds(anyList())).thenReturn(null);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(List.of(),payrollHelper.getPayrollEvents(List.of("id1","id2")));
        }
    }

    @Test
    void getPayrollEvents_events_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Map<String,Object> events = new HashMap<>();
            List<Map<String,Object>> eventsList = new ArrayList<>();
            eventsList.add(new HashMap<>(){{put("event","01");}});
            events.put(SchedulingKeys.EVENTS,eventsList);
            Mockito.when(reportImplMock.getEventsByIds(anyList())).thenReturn(events);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(eventsList,payrollHelper.getPayrollEvents(List.of("id1","id2")));
        }
    }

    @Test
    void updateEvents_valid_test() throws IOException, NoSuchAlgorithmException {
        Map<String,Object> event = new HashMap<>();
        event.put("id","eventData");
        List<Map<String,Object>> eventsList = new ArrayList<>();
        eventsList.add(event);
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.updateEntries(anyString(),anyList())).thenReturn(Map.of("data",eventsList));
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(eventsList,payrollHelper.updateEvents("accId",eventsList));
        }
    }

    @Test
    void validateAndApprovePayrollEvents_no_validEvents_test() throws IOException, NoSuchAlgorithmException {
        Map<String,Object> event = new HashMap<>();
        event.put(SchedulingKeys.MERCHANT,"accId");
        event.put(SchedulingKeys.PROVIDER,List.of("contId"));
        event.put(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, REPORT_STATUS.PENDING.toString()));
        event.put(SchedulingKeys.START_TIME,1660120845000l);
        Map<String,Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.PAYROLL_STATUS,PayrollStatus.USER_ACKNOWLEDGED.toString());
        event.put(SchedulingKeys.QUERYABLE_META,queryMap);
        event.put(SchedulingKeys.PAYMENT_STATUS,PayrollStatus.USER_ACKNOWLEDGED.toString());

        List<Map<String, Object>> payrollEventsList = new ArrayList<>();
        payrollEventsList.add(event);
        SettingsJDO account = new SettingsJDO();
        account.setPeopleUniquePin("accId");
        account.setPayrollAccessSince(0l);
        Assertions.assertEquals(List.of(),payrollHelper.validateAndApprovePayrollEvents(payrollEventsList,account,"contId"));
    }

    @Test
    void validateAndApprovePayrollEvents_validEvents_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){

            Map<String,Object> eventMap = new HashMap<>();
            eventMap.put("id","123");
            List<Map<String,Object>> eventsList = new ArrayList<>();
            eventsList.add(eventMap);

            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.updateEntries(anyString(),anyList())).thenReturn(Map.of("data",eventsList));
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);

            Map<String,Object> event = new HashMap<>();
            event.put(SchedulingKeys.MERCHANT,"accId");
            event.put(SchedulingKeys.PROVIDER,List.of("contId"));
            event.put(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString()));
            event.put(SchedulingKeys.START_TIME,1660120845000l);
            Map<String,Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.PAYROLL_STATUS,PayrollStatus.USER_ACKNOWLEDGED.toString());
            event.put(SchedulingKeys.QUERYABLE_META,queryMap);
            event.put(SchedulingKeys.PAYMENT_STATUS,PayrollStatus.USER_ACKNOWLEDGED.toString());

            List<Map<String, Object>> payrollEventsList = new ArrayList<>();
            payrollEventsList.add(event);
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accId");
            account.setPayrollAccessSince(0l);
            Assertions.assertEquals(List.of("123"),payrollHelper.validateAndApprovePayrollEvents(payrollEventsList,account,"contId"));
        }
    }

    @Test
    void validateAndApprovePayrollEvents_emptyResponse_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){

            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.updateEntries(anyString(),anyList())).thenReturn(Map.of());
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);

            Map<String,Object> event = new HashMap<>();
            event.put(SchedulingKeys.MERCHANT,"accId");
            event.put(SchedulingKeys.PROVIDER,List.of("contId"));
            event.put(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString()));
            event.put(SchedulingKeys.START_TIME,1660120845000l);
            Map<String,Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.PAYROLL_STATUS,PayrollStatus.USER_ACKNOWLEDGED.toString());
            event.put(SchedulingKeys.QUERYABLE_META,queryMap);
            event.put(SchedulingKeys.PAYMENT_STATUS,PayrollStatus.USER_ACKNOWLEDGED.toString());

            List<Map<String, Object>> payrollEventsList = new ArrayList<>();
            payrollEventsList.add(event);
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accId");
            account.setPayrollAccessSince(0l);
            Assertions.assertEquals(List.of(),payrollHelper.validateAndApprovePayrollEvents(payrollEventsList,account,"contId"));
        }
    }

    @Test
    void updateEntriesWithUserVerifiedPaymentStatus_when_EventMapIsEmptyOrNull_test() throws IOException, NoSuchAlgorithmException {

        try (MockedStatic<ReportsUtil> reportUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class);
             MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)) {

            Map<String,Object> map = new HashMap<>();

            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setId("123");
            userPRO.setContactId("contactID");
            userPRO.setTimeZone("Asia/Kolkata");
            userPRO.setEmailID("email@emailID.com");

            reportUtilMockedStatic.when(() -> ReportsUtil.getAccountIDFromEvent(anyMap())).thenReturn("accountID");

            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getEntryByID(anyString())).thenReturn(map);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Map<String, Object>responseMap = PayrollHelper.getInstance().updateEntriesWithUserVerifiedPaymentStatus("accountID", "contactID", "entryID", userPRO);
            Assertions.assertTrue(ObjUtils.isNull(responseMap.get("entry")));
        }
    }

    @Test
    void updateEntriesWithUserVerifiedPaymentStatus_valid_test() throws IOException, NoSuchAlgorithmException {

        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class);
            MockedStatic<ReportsUtil> reportUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class);
            MockedStatic<ClockUtil> clockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class);
            MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)) {
            Map<String,Object> metaDataMap = new HashMap<>();
            metaDataMap.put(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString());
            Map<String,Object> event = new HashMap<>();
            event.put(SchedulingKeys.METADATA,metaDataMap);
            Map<String,Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString());
            queryMap.put(SchedulingKeys.PAYROLL_STATUS, PayrollStatus.USER_CONFIRMED.toString());
            event.put(SchedulingKeys.QUERYABLE_META,queryMap);
            event.put(SchedulingKeys.PAYMENT_STATUS,PayrollStatus.USER_CONFIRMED.toString());
            event.put(SchedulingKeys.MERCHANT,"accId");
            List<String>contactIDList = new ArrayList<>();
            contactIDList.add("contactID");
            event.put(SchedulingKeys.PROVIDER,contactIDList);
            event.put(SchedulingKeys.ID, "ID");
            event.put(SchedulingKeys.SOURCE, "web");
            event.put(SchedulingKeys.IS_DELETED, false);
            event.put(SchedulingKeys.START_TIME, 123L);
            event.put(SchedulingKeys.END_TIME, 222L);
            event.put(SchedulingKeys.QUERYABLE_META, queryMap);

            Map<String, Object>requestMap = new HashMap<>();
            requestMap.put(SchedulingKeys.QUERYABLE_META, new HashMap<>());
            requestMap.put(SchedulingKeys.METADATA, new HashMap<>());
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setId("123");
            userPRO.setContactId("contactID");
            userPRO.setTimeZone("Asia/Kolkata");
            userPRO.setEmailID("email@emailID.com");
            ReportsDTO reportsDTO = new ReportsDTO(event, userPRO.getEmailID(), userPRO.getTimeZone());
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getEntryByID(anyString())).thenReturn(requestMap);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);

            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(), anyString(), anyString(), anyString(),
                    anyString(), anyString(), any(long.class))).thenAnswer((Answer<Void>) invocation -> null);
            dateUtilMockedStatic.when(() -> DateUtil.getCurrentTime()).thenReturn(123L);
            reportUtilMockedStatic.when(() -> ReportsUtil.getAccountIDFromEvent(anyMap())).thenReturn("accountID");
            reportUtilMockedStatic.when(() -> ReportsUtil.getContactIDFromEvent(anyMap())).thenReturn("contactID");
            reportUtilMockedStatic.when(() -> ReportsUtil.getPayrollStatus(anyMap())).thenReturn(PayrollStatus.ADMIN_UPDATED.toString());
            reportUtilMockedStatic.when(() -> ReportsUtil.getEventStatus(anyMap())).thenReturn(REPORT_STATUS.PENDING.toString());
            reportUtilMockedStatic.when(() -> ReportsUtil.isEventDeleted(anyMap())).thenReturn(false);
            clockUtilMockedStatic.when(()-> ClockUtil.updateEvent(anyMap())).thenReturn(event);
            String activityMessage = "User: " + "contactID" + "payrollStatus : " +  PayrollStatus.USER_CONFIRMED + "entryID : " + "entryID";
            Map<String, Object>responseMap = PayrollHelper.getInstance().updateEntriesWithUserVerifiedPaymentStatus("accountID", "contactID", "entryID", userPRO);
            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("accountID","contactID","", "email@emailID.com",
                    activityMessage,"DUMMY", 123L), times(1));
            Assertions.assertEquals(responseMap.get("entry"), reportsDTO);
        }
    }

    @Test
    void updateTheEventsAsUserVerified_valid_test(){

        try(MockedStatic<ReportsUtil> reportUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)) {

            reportUtilMockedStatic.when(() -> ReportsUtil.getAccountIDFromEvent(anyMap())).thenReturn("accountID");
            reportUtilMockedStatic.when(() -> ReportsUtil.getContactIDFromEvent(anyMap())).thenReturn("contactID");
            reportUtilMockedStatic.when(() -> ReportsUtil.getPayrollStatus(anyMap())).thenReturn(PayrollStatus.ADMIN_UPDATED.toString());
            reportUtilMockedStatic.when(() -> ReportsUtil.getEventStatus(anyMap())).thenReturn(REPORT_STATUS.PENDING.toString());
            reportUtilMockedStatic.when(() -> ReportsUtil.isEventDeleted(anyMap())).thenReturn(false);

            Map<String,Object> event = new HashMap<>();
            event.put(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString()));
            Map<String,Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString());
            queryMap.put(SchedulingKeys.PAYROLL_STATUS, PayrollStatus.USER_CONFIRMED.toString());
            event.put(SchedulingKeys.QUERYABLE_META,queryMap);
            event.put(SchedulingKeys.PAYMENT_STATUS,PayrollStatus.USER_CONFIRMED.toString());

            Map<String,Object> metaDataMap = new HashMap<>();
            metaDataMap.put(SchedulingKeys.STATUS, REPORT_STATUS.PENDING.toString());

            Map<String,Object> queryDataMap = new HashMap<>();
            queryDataMap.put(SchedulingKeys.STATUS, REPORT_STATUS.PENDING.toString());
            queryDataMap.put(SchedulingKeys.PAYROLL_STATUS, PayrollStatus.ADMIN_UPDATED.toString());

            Map<String, Object>requestMap = new HashMap<>();
            requestMap.put(SchedulingKeys.QUERYABLE_META, queryDataMap);
            requestMap.put(SchedulingKeys.METADATA, metaDataMap);

            Map<String, Object>responseMap = PayrollHelper.getInstance().validateAndUpdateTheEventsAsUserVerified("accountID","contactID",requestMap);
            Assertions.assertEquals(responseMap, event);
        }
    }

    @Test
    void saveActivityOfTheUserVerifiedEntry_ifResponseMapIsNullOrEmpty_test(){

        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setId("123");
            userPRO.setContactId("contactID");
            userPRO.setEmailID("email@emailID.com");

            Map<String, Object>responseMap = new HashMap<>();
            String activityMessage = "User: " + "contactID" + "payrollStatus : " +  PayrollStatus.USER_CONFIRMED + "entryID : " + "entryID";
            dateUtilMockedStatic.when(() -> DateUtil.getCurrentTime()).thenReturn(123L);

            PayrollHelper.getInstance().saveActivityOfTheUserVerifiedEntry("accountID", "contactID", userPRO, "entryID", responseMap);
            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("accountID","contactID","", "email@emailID.com",
                    activityMessage,"DUMMY", 123L), times(0));
        }
    }

    @Test
    void saveActivityOfTheUserVerifiedEntry_valid_test(){

        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setId("123");
            userPRO.setContactId("contactID");
            userPRO.setEmailID("email@emailID.com");

            Map<String, Object>responseMap = new HashMap<>();
            responseMap.put("map","map");
            String activityMessage = "User: " + "contactID" + "payrollStatus : " +  PayrollStatus.USER_CONFIRMED + "entryID : " + "entryID";
            dateUtilMockedStatic.when(() -> DateUtil.getCurrentTime()).thenReturn(123L);

            PayrollHelper.getInstance().saveActivityOfTheUserVerifiedEntry("accountID", "contactID", userPRO, "entryID", responseMap);
            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("accountID","contactID","", "email@emailID.com",
                    activityMessage,"DUMMY", 123L), times(1));
        }
    }
}

