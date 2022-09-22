package com.yoco.commons.utils;

import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.mockito.ArgumentMatchers.anyString;

class ReportsUtilTest {
    @Test
    void isRunningEntry_nullEvent_test(){
        Assertions.assertFalse(ReportsUtil.isRunningEntry(null));
    }

    @Test
    void isRunningEntry_emptyEvent_test(){
        Assertions.assertFalse(ReportsUtil.isRunningEntry(new HashMap<>()));
    }

    @Test
    void isRunningEntry_nullMetaMap_test(){
        Assertions.assertFalse(ReportsUtil.isRunningEntry(new HashMap(){{put(SchedulingKeys.METADATA,null);}}));
    }

    @Test
    void isRunningEntry_emptyMetaMap_test(){
        Assertions.assertFalse(ReportsUtil.isRunningEntry(new HashMap(){{put(SchedulingKeys.METADATA,new HashMap<>());}}));
    }

    @Test
    void isRunningEntry_StatusNotClockedIn_test(){
        Assertions.assertFalse(ReportsUtil.isRunningEntry(new HashMap(){{put(SchedulingKeys.METADATA,new HashMap(){{put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");}});}}));
    }

    @Test
    void isRunningEntry_StatusClockedIn_test(){
        Assertions.assertTrue(ReportsUtil.isRunningEntry(new HashMap(){{put(SchedulingKeys.METADATA,new HashMap(){{put(SchedulingKeys.STATUS,"CLOCKED_IN");}});}}));
    }

    @Test
    void getPayrollStatus_nullEvent_test(){
        Assertions.assertEquals("",ReportsUtil.getPayrollStatus(null));
    }

    @Test
    void getPayrollStatus_nullPaymentStatus_test(){
        Assertions.assertEquals("",ReportsUtil.getPayrollStatus(new HashMap<>()));
    }

    @Test
    void getPayrollStatus_validPaymentStatus_test(){
        Assertions.assertEquals("payroll",ReportsUtil.getPayrollStatus(Map.of("paymentStatus","payroll")));
    }

    @Test
    void isStatusManualClockedOut_NotManualClockedOut_test(){
        Assertions.assertFalse(ReportsUtil.isStatusManualClockedOut(new HashMap(){{put(SchedulingKeys.METADATA,new HashMap(){{put(SchedulingKeys.STATUS,"CLOCKED_IN");}});}}));
    }

    @Test
    void isStatusManualClockedOut_ManualClockedOut_test(){
        Assertions.assertTrue(ReportsUtil.isStatusManualClockedOut(new HashMap(){{put(SchedulingKeys.METADATA,new HashMap(){{put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");}});}}));
    }

    @Test
    void isStatusPending_NotPending_test(){
        Assertions.assertFalse(ReportsUtil.isStatusPending(new HashMap(){{put(SchedulingKeys.METADATA,new HashMap(){{put(SchedulingKeys.STATUS,"CLOCKED_IN");}});}}));
    }

    @Test
    void isStatusPending_Pending_test(){
        Assertions.assertTrue(ReportsUtil.isStatusPending(new HashMap(){{put(SchedulingKeys.METADATA,new HashMap(){{put(SchedulingKeys.STATUS,"PENDING");}});}}));
    }

    @Test
    void isEventStatusSameAsGivenStatus_nullGivenStatus_test(){
        Assertions.assertFalse(ReportsUtil.isEventStatusSameAsGivenStatus(Map.of(),null));
    }

    @Test
    void isPayrollStatusSameAsGivenStatus_nullGivenStatus_test(){
        Assertions.assertFalse(ReportsUtil.isPayrollStatusSameAsGivenStatus(Map.of(),null));
    }

    @Test
    void isPayrollStatusDefaultPayroll_NotDefault_payroll_test(){
        Assertions.assertFalse(ReportsUtil.isPayrollStatusDefaultPayroll(new HashMap(){{put("paymentStatus","USER_ACKNOWLEDGED");}}));
    }

    @Test
    void isPayrollStatusDefaultPayroll_Default_payroll_test(){
        Assertions.assertTrue(ReportsUtil.isPayrollStatusDefaultPayroll(new HashMap(){{put("paymentStatus","DEFAULT_PAYROLL");}}));
    }

    @Test
    void isPayrollStatusAdminUpdated_StatusDefaultPayroll_ShouldReturnFalse(){
        Assertions.assertFalse(ReportsUtil.isPayrollStatusAdminUpdated(new HashMap(){{put("paymentStatus","DEFAULT_PAYROLL");}}));
    }

    @Test
    void isPayrollStatusAdminUpdated_ValidStatus_ShouldReturnTrue(){
        Assertions.assertTrue(ReportsUtil.isPayrollStatusAdminUpdated(new HashMap(){{put("paymentStatus","ADMIN-UPDATED");}}));
    }

    @Test
    void isPayrollStatusUserAcknowledged_NotUser_Acknowledged_test(){
        Assertions.assertFalse(ReportsUtil.isPayrollStatusUserAcknowledged(new HashMap(){{put("paymentStatus","DEFAULT_PAYROLL");}}));
    }

    @Test
    void isPayrollStatusUserAcknowledged_User_Acknowledged_test(){
        Assertions.assertTrue(ReportsUtil.isPayrollStatusUserAcknowledged(new HashMap(){{put("paymentStatus","USER-ACKNOWLEDGED");}}));
    }

    @Test
    void getContactIDFromEvent_nullEvent_test(){
        Assertions.assertEquals("",ReportsUtil.getContactIDFromEvent(null));
    }

    @Test
    void getContactIDFromEvent_nullProvider_test(){
        Assertions.assertEquals("",ReportsUtil.getContactIDFromEvent(new HashMap(){{put("provider",null);}}));
    }

    @Test
    void getContactIDFromEvent_validProvider_test(){
        Assertions.assertEquals("123",ReportsUtil.getContactIDFromEvent(new HashMap(){{put("provider", List.of("123"));}}));
    }

    @Test
    void isEventDeleted_nullEvent_test(){
        Assertions.assertTrue(ReportsUtil.isEventDeleted(new HashMap()));
    }

    @Test
    void isEventDeleted_false_test(){
        Assertions.assertFalse(ReportsUtil.isEventDeleted(Map.of("isDeleted",false)));
    }

    @Test
    void isEventDeleted_true_test(){
        Assertions.assertTrue(ReportsUtil.isEventDeleted(Map.of("isDeleted",true)));
    }

    @Test
    void getAccountIDFromEvent_nullEvent(){
        Assertions.assertEquals("",ReportsUtil.getAccountIDFromEvent(null));
    }

    @Test
    void getAccountIDFromEvent_nullAccountID(){
        Assertions.assertEquals("",ReportsUtil.getAccountIDFromEvent(Map.of("test","1")));
    }

    @Test
    void getAccountIDFromEvent_validAccountID(){
        Assertions.assertEquals("123",ReportsUtil.getAccountIDFromEvent(Map.of("merchant","123")));
    }

    @Test
    void getIDFromEvent_nullEvent(){
        Assertions.assertEquals("",ReportsUtil.getIDFromEvent(null));
    }

    @Test
    void getAccountIDFromEvent_nullID(){
        Assertions.assertEquals("",ReportsUtil.getIDFromEvent(Map.of("test","1")));
    }

    @Test
    void getAccountIDFromEvent_validID(){
        Assertions.assertEquals("ID",ReportsUtil.getIDFromEvent(Map.of("id","ID")));
    }

    @Test
    void getUpdatedTime_nullEvent(){
        Assertions.assertEquals(0L,ReportsUtil.getUpdatedTimeFromEvent(null));
    }

    @Test
    void getUpdatedTime_nullUpdatedTime(){
        Assertions.assertEquals(0L,ReportsUtil.getUpdatedTimeFromEvent(Map.of("test","1")));
    }

    @Test
    void getUpdatedTime_validUpdatedTime(){
        Assertions.assertEquals(123L,ReportsUtil.getUpdatedTimeFromEvent(Map.of("updatedTime",123L)));
    }

    @Test
    void getParentIDFromEvent_nullEvent_test(){
        Assertions.assertEquals("",ReportsUtil.getParentIDFromEvent(null));
    }

    @Test
    void getParentIDFromEvent_noParent_test(){
        Assertions.assertEquals("",ReportsUtil.getParentIDFromEvent(Map.of("key","value")));
    }

    @Test
    void getParentIDFromEvent_validParent_test(){
        Assertions.assertEquals("111",ReportsUtil.getParentIDFromEvent(Map.of("parentId","111")));
    }

    @Test
    void getParentEventFromEvent_null_parentId_test() throws IOException, NoSuchAlgorithmException {
        Assertions.assertEquals(Map.of(),ReportsUtil.getParentEventFromEvent(Map.of("parentId","")));
    }

    @Test
    void getParentEventFromEvent_empty_response_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.getEventByID(anyString())).thenReturn(Map.of());
            Assertions.assertEquals(Map.of(),ReportsUtil.getParentEventFromEvent(Map.of("parentId","123")));
        }
    }

    @Test
    void getParentEventFromEvent_success_false_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.getEventByID(anyString())).thenReturn(Map.of("response",false));
            Assertions.assertEquals(Map.of(),ReportsUtil.getParentEventFromEvent(Map.of("parentId","123")));
        }
    }

    @Test
    void getParentEventFromEvent_empty_Data_parentId_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.getEventByID(anyString())).thenReturn(Map.of("response",true));
            Assertions.assertEquals(Map.of(),ReportsUtil.getParentEventFromEvent(Map.of("parentId","123")));
        }
    }

    @Test
    void getParentEventFromEvent_valid_Data_parentId_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.getEventByID(anyString())).thenReturn(Map.of("response",true,"data",Map.of("key","value")));
            Assertions.assertEquals(Map.of("key","value"),ReportsUtil.getParentEventFromEvent(Map.of("parentId","123")));
        }
    }

    @Test
    void getParentIdsListFromEvents_nullEventsList_shouldReturnEmptyList(){
        Assertions.assertEquals(List.of(), ReportsUtil.getParentIdsListFromEvents(null));
    }

    @Test
    void getParentIdsListFromEvents_emptyEventsList_shouldReturnEmptyList(){
        Assertions.assertEquals(List.of(), ReportsUtil.getParentIdsListFromEvents(List.of()));
    }

    @Test
    void getParentIdsListFromEvents_oneEventWithParentId_shouldReturnListWithOneParentId(){
        Assertions.assertEquals(List.of("123"), ReportsUtil.getParentIdsListFromEvents(List.of(Map.of("parentId","123"))));
    }

    @Test
    void getParentIdsListFromEvents_TwoEventsWithOneValidParentId_shouldReturnListWithOneParentId(){
        Assertions.assertEquals(List.of("123"), ReportsUtil.getParentIdsListFromEvents(List.of(Map.of("parentId","123"),Map.of("id","234"))));
    }

    @Test
    void getIdsListFromEvents_nullEventsList_shouldReturnEmptyList(){
        Assertions.assertEquals(List.of(), ReportsUtil.getIdsListFromEvents(null));
    }

    @Test
    void getIdsListFromEvents_emptyEventsList_shouldReturnEmptyList(){
        Assertions.assertEquals(List.of(), ReportsUtil.getIdsListFromEvents(List.of()));
    }

    @Test
    void getIdsListFromEvents_oneEventWithId_shouldReturnListWithOneId(){
        Assertions.assertEquals(List.of("123"), ReportsUtil.getIdsListFromEvents(List.of(Map.of("id","123"))));
    }

    @Test
    void getIdsListFromEvents_TwoEventsWithOneValidId_shouldReturnListWithOneId(){
        Assertions.assertEquals(List.of("234"), ReportsUtil.getIdsListFromEvents(List.of(Map.of("parentId","123"),Map.of("id","234"))));
    }

    @Test
    void isAdjustmentNew_noParentId_ShouldReturnTrue(){
        Assertions.assertTrue(ReportsUtil.isAdjustmentNew(Map.of("id","123")));
    }

    @Test
    void isAdjustmentNew_validParentId_ShouldReturnFalse(){
        Assertions.assertFalse(ReportsUtil.isAdjustmentNew(Map.of("id","123","parentId","234")));
    }

    @Test
    void getZuluStartDateTimeOfEvent_nullEvent_test(){
        Assertions.assertEquals("",ReportsUtil.getZuluStartDateTimeOfEvent(null));
    }

    @Test
    void getZuluStartDateTimeOfEvent_noKey_test(){
        Assertions.assertEquals("",ReportsUtil.getZuluStartDateTimeOfEvent(Map.of("key","value")));
    }

    @Test
    void getZuluStartDateTimeOfEvent_validKey_test(){
        Assertions.assertEquals("start date",ReportsUtil.getZuluStartDateTimeOfEvent(Map.of("startDateTime","start date")));
    }

    @Test
    void getZuluEndDateTimeOfEvent_nullEvent_test(){
        Assertions.assertEquals("",ReportsUtil.getZuluEndDateTimeOfEvent(null));
    }

    @Test
    void getZuluEndDateTimeOfEvent_noKey_test(){
        Assertions.assertEquals("",ReportsUtil.getZuluEndDateTimeOfEvent(Map.of("key","value")));
    }

    @Test
    void getZuluEndDateTimeOfEvent_validKey_test(){
        Assertions.assertEquals("end date",ReportsUtil.getZuluEndDateTimeOfEvent(Map.of("endDateTime","end date")));
    }

    @Test
    void getOverlappingEntriesForEvent_nullOriginalEvent_shouldReturnEmptyList() throws IOException, NoSuchAlgorithmException {
        Assertions.assertEquals(List.of(),ReportsUtil.getOverlappingEntriesForEvent(null));
    }

    @Test
    void getOverlappingEntriesForEvent_emptyOriginalEvent_shouldReturnEmptyList() throws IOException, NoSuchAlgorithmException {
        Assertions.assertEquals(List.of(),ReportsUtil.getOverlappingEntriesForEvent(Map.of()));
    }

    @Test
    void getOverlappingEntriesForEvent_NoOverlappingEvents_shouldReturnEmptyList() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getEntries("accID","123","startDate","endDate","DESC",false)).thenReturn(List.of());
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(List.of(),ReportsUtil.getOverlappingEntriesForEvent(Map.of("merchant","accID","provider",List.of("123"),"startDateTime","startDate","endDateTime","endDate")));
        }
    }

    @Test
    void getOverlappingEntriesForEvent_OnlyOriginalEventInOverlappingEvents_shouldReturnEmptyList() throws IOException, NoSuchAlgorithmException {
        Map<String,Object> originalEvent = Map.of("id","eID","merchant","accID","provider",List.of("123"),"startDateTime","startDate","endDateTime","endDate");
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getEntries("accID","123","startDate","endDate","DESC",false)).thenReturn(List.of(originalEvent));
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(List.of(),ReportsUtil.getOverlappingEntriesForEvent(originalEvent));
        }
    }

    @Test
    void getOverlappingEntriesForEvent_TwoOverlappingEvents_shouldReturnOneEventExcludingTheOriginalEvent() throws IOException, NoSuchAlgorithmException {
        Map<String,Object> originalEvent = Map.of("id","eID","merchant","accID","provider",List.of("123"),"startDateTime","startDate","endDateTime","endDate");
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getEntries("accID","123","startDate","endDate","DESC",false)).thenReturn(List.of(originalEvent,Map.of("id","eID2")));
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(List.of(Map.of("id","eID2")),ReportsUtil.getOverlappingEntriesForEvent(originalEvent));
        }
    }

    @Test
    void getMetaDataMap_NullEvent_shouldReturnEmptyMap(){
        Assertions.assertEquals(Map.of(),ReportsUtil.getMetaDataMap(null));
    }

    @Test
    void getMetaDataMap_NullMetaData_shouldReturnEmptyMap(){
        Assertions.assertEquals(Map.of(),ReportsUtil.getMetaDataMap(Map.of("id","1")));
    }

    @Test
    void getMetaDataMap_validMetaData_shouldReturnMetaDataMap(){
        Assertions.assertEquals(Map.of("status","pending"),ReportsUtil.getMetaDataMap(Map.of("metaData",Map.of("status","pending"))));
    }

    @Test
    void getQueryableMetaMap_NullEvent_shouldReturnEmptyMap(){
        Assertions.assertEquals(Map.of(),ReportsUtil.getQueryableMetaMap(null));
    }

    @Test
    void getQueryableMetaMap_NullQueryableMetaData_shouldReturnEmptyMap(){
        Assertions.assertEquals(Map.of(),ReportsUtil.getQueryableMetaMap(Map.of("id","1")));
    }

    @Test
    void getQueryableMetaMap_validQueryableMetaData_shouldReturnMetaDataMap(){
        Assertions.assertEquals(Map.of("status","pending"),ReportsUtil.getQueryableMetaMap(Map.of("queryableMeta",Map.of("status","pending"))));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getStartMillisOfEvent_nullEvent_test(Map<String,Object> testValue){
        Assertions.assertEquals(0l,ReportsUtil.getStartMillisOfEvent(testValue));
    }

    @Test
    void getStartMillisOfEvent_nullStartEventTime_test(){
        Assertions.assertEquals(0l,ReportsUtil.getStartMillisOfEvent(Map.of("merchant","1")));
    }

    @Test
    void getStartMillisOfEvent_valid_test(){
        Assertions.assertEquals(1660638785000l,ReportsUtil.getStartMillisOfEvent(Map.of("startTime",1660638785000l)));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getEndMillisOfEvent_nullEvent_test(Map<String,Object> testValue){
        Assertions.assertEquals(0l,ReportsUtil.getEndMillisOfEvent(testValue));
    }

    @Test
    void getEndMillisOfEvent_nullStartEventTime_test(){
        Assertions.assertEquals(0l,ReportsUtil.getEndMillisOfEvent(Map.of("merchant","1")));
    }

    @Test
    void getEndMillisOfEvent_valid_test(){
        Assertions.assertEquals(1660638785000l,ReportsUtil.getEndMillisOfEvent(Map.of("endTime",1660638785000l)));
    }

    @Test
    void extractUserProFromEvent_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(userPro);
            Map<String,Object> event = new HashMap<>();
            event.put("provider",List.of("contId"));
            event.put("merchant","accId");
            Assertions.assertEquals(userPro,ReportsUtil.extractUserProFromEvent(event));
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserProWithContact("accId","contId"));
        }
    }

}
