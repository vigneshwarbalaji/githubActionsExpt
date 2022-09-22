package com.yoco.adjustment.helper;

import com.yoco.MockEvent;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.AdjustmentImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.ProjectUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;

class AdjustmentOverlapHelperTest {

    @Test
    void validateAndExtractOverlapEntryFromRunningEntry_nullResp_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getActiveEntry(anyString(),anyString())).thenReturn(null);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(Map.of(),AdjustmentOverlapHelper.validateAndExtractOverlapEntryFromRunningEntry("111","123",1659088799000l));
            Mockito.verify(reportImplMock).getActiveEntry("111","123");
        }
    }

    @Test
    void validateAndExtractOverlapEntryFromRunningEntry_emptyResp_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getActiveEntry(anyString(),anyString())).thenReturn(new ArrayList<>());
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(Map.of(),AdjustmentOverlapHelper.validateAndExtractOverlapEntryFromRunningEntry("111","123",1659088799000l));
            Mockito.verify(reportImplMock).getActiveEntry("111","123");
        }
    }

    @Test
    void validateAndExtractOverlapEntryFromRunningEntry_emptyHashMap_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Map<String,Object> entry = new HashMap<>();
            List<Map<String,Object>> activeEntry = new ArrayList<>();
            activeEntry.add(entry);
            Mockito.when(reportImplMock.getActiveEntry(anyString(),anyString())).thenReturn(activeEntry);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(entry,AdjustmentOverlapHelper.validateAndExtractOverlapEntryFromRunningEntry("111","123",1659088799000l));
            Mockito.verify(reportImplMock).getActiveEntry("111","123");
        }
    }

    @Test
    void validateAndExtractOverlapEntryFromRunningEntry_endTime_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Map<String,Object> entry = new HashMap<>();
            entry.put(SchedulingKeys.START_TIME,1659084026000l);
            entry.put(SchedulingKeys.END_TIME,1659084026000l);
            List<Map<String,Object>> entries = new ArrayList<>();
            entries.add(entry);
            Mockito.when(reportImplMock.getActiveEntry(anyString(),anyString())).thenReturn(entries);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(entry,AdjustmentOverlapHelper.validateAndExtractOverlapEntryFromRunningEntry("111","123",1659095648000l));
            Mockito.verify(reportImplMock).getActiveEntry("111","123");
        }
    }

    @Test
    void validateAndExtractOverlapEntryFromRunningEntry_sameTime_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Map<String,Object> entry = new HashMap<>();
            entry.put(SchedulingKeys.START_TIME,1659084026000l);
            entry.put(SchedulingKeys.END_TIME,1659084026000l);
            List<Map<String,Object>> entries = new ArrayList<>();
            entries.add(entry);
            Mockito.when(reportImplMock.getActiveEntry(anyString(),anyString())).thenReturn(entries);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(new HashMap<>(),AdjustmentOverlapHelper.validateAndExtractOverlapEntryFromRunningEntry("111","123",1659084026000l));
            Mockito.verify(reportImplMock).getActiveEntry("111","123");
        }
    }

    @Test
    void validateAndExtractOverlapEntryFromClockedOutEntries_entries_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);
            MockedStatic<AdjustmentImpl> adjustmentMockedStatic = Mockito.mockStatic(AdjustmentImpl.class)){
            List<String> projectIds = new ArrayList<>();
            projectIds.add("pr1");
            projectIds.add("pr2");
            projectUtilMockedStatic.when(()-> ProjectUtil.getAllProjectIdsAssociatedToUser(anyString(),anyString())).thenReturn(projectIds);

            Map<String,Object> eventsMap = new HashMap<>();
            Map<String,Object> events = new HashMap<>();
            events.put(SchedulingKeys.START_TIME,1659084026000l);
            Map<String,Object> events1 = new HashMap<>();
            events1.put(SchedulingKeys.START_TIME,1659095648000l);
            Map<String,Object> events2 = new HashMap<>();
            events2.put(SchedulingKeys.START_TIME,1659081640000l);
            Map<String,Object> events3 = new HashMap<>();
            events3.put(SchedulingKeys.START_TIME,1659095648000l);
            eventsMap.put("events",new ArrayList<>(){{add(events);add(events1);add(events2);add(events3);}});

            AdjustmentImpl adjustmentImplMock = Mockito.mock(AdjustmentImpl.class);
            Mockito.when(adjustmentImplMock.getOverLapEntries(anyString(),anyList(),anyString(),anyString(),anyString(),anyString())).thenReturn(Map.of("data",eventsMap));
            adjustmentMockedStatic.when(AdjustmentImpl::getAdjustmentImplInstance).thenReturn(adjustmentImplMock);

            Assertions.assertEquals(Map.of(SchedulingKeys.START_TIME,1659084026000l),AdjustmentOverlapHelper.validateAndExtractOverlapEntryFromClockedOutEntries("111","123",1659084026000l,1659095648000l,"id1,id2",DateConstants.ZONE_ID_IST));
        }
    }

    @Test
    void validateAndExtractOverlapEntryFromClockedOutEntries_noEntries_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);
            MockedStatic<AdjustmentImpl> adjustmentMockedStatic = Mockito.mockStatic(AdjustmentImpl.class)){
            List<String> projectIds = new ArrayList<>();
            projectIds.add("pr1");
            projectIds.add("pr2");
            projectUtilMockedStatic.when(()-> ProjectUtil.getAllProjectIdsAssociatedToUser(anyString(),anyString())).thenReturn(projectIds);

            Map<String,Object> eventsMap = new HashMap<>();

            AdjustmentImpl adjustmentImplMock = Mockito.mock(AdjustmentImpl.class);
            Mockito.when(adjustmentImplMock.getOverLapEntries(anyString(),anyList(),anyString(),anyString(),anyString(),anyString())).thenReturn(Map.of("data",eventsMap));
            adjustmentMockedStatic.when(AdjustmentImpl::getAdjustmentImplInstance).thenReturn(adjustmentImplMock);

            Assertions.assertEquals(Map.of(),AdjustmentOverlapHelper.validateAndExtractOverlapEntryFromClockedOutEntries("111","123",1659084026000l,1659095648000l,"id1,id2",DateConstants.ZONE_ID_IST));
        }
    }

    @Test
    void validateAndExtractOverlapEntry_noEntries_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);
            MockedStatic<AdjustmentImpl> adjustmentMockedStatic = Mockito.mockStatic(AdjustmentImpl.class)){

            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getActiveEntry(anyString(),anyString())).thenReturn(null);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);

            List<String> projectIds = new ArrayList<>();
            projectIds.add("pr1");
            projectIds.add("pr2");
            projectUtilMockedStatic.when(()-> ProjectUtil.getAllProjectIdsAssociatedToUser(anyString(),anyString())).thenReturn(projectIds);

            Map<String,Object> eventsMap = new HashMap<>();

            AdjustmentImpl adjustmentImplMock = Mockito.mock(AdjustmentImpl.class);
            Mockito.when(adjustmentImplMock.getOverLapEntries(anyString(),anyList(),anyString(),anyString(),anyString(),anyString())).thenReturn(Map.of("data",eventsMap));
            adjustmentMockedStatic.when(AdjustmentImpl::getAdjustmentImplInstance).thenReturn(adjustmentImplMock);

            Assertions.assertEquals(Map.of(),AdjustmentOverlapHelper.validateAndExtractOverlapEntry("111","123",1659084026000l,1659095648000l,"id1,id2",DateConstants.ZONE_ID_IST));
        }
    }

    @Test
    void validateAndExtractOverlapEntry_entries_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);
            MockedStatic<AdjustmentImpl> adjustmentMockedStatic = Mockito.mockStatic(AdjustmentImpl.class)){

            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Map<String,Object> entry = new HashMap<>();
            entry.put(SchedulingKeys.START_TIME,1659084026000l);
            entry.put(SchedulingKeys.END_TIME,1659084026000l);
            List<Map<String,Object>> entries = new ArrayList<>();
            entries.add(entry);
            Mockito.when(reportImplMock.getActiveEntry(anyString(),anyString())).thenReturn(entries);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);

            Assertions.assertEquals(entry,AdjustmentOverlapHelper.validateAndExtractOverlapEntry("111","123",1659084026000l,1659095648000l,"id1,id2",DateConstants.ZONE_ID_IST));
            projectUtilMockedStatic.verifyNoInteractions();
            adjustmentMockedStatic.verifyNoInteractions();
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void extractOverLapEntryFromResponse_test(Map<String,Object> testMap){
        Assertions.assertEquals(Map.of(),AdjustmentOverlapHelper.extractOverLapEntryFromResponse(testMap));
    }

    @Test
    void extractOverLapEntryFromResponse_noDataKey_test(){
        Assertions.assertEquals(Map.of(),AdjustmentOverlapHelper.extractOverLapEntryFromResponse(Map.of("key","value")));
    }

    @Test
    void extractOverLapEntryFromResponse_emptyDataKey_test(){
        Assertions.assertEquals(Map.of(),AdjustmentOverlapHelper.extractOverLapEntryFromResponse(Map.of("data",Map.of())));
    }

    @Test
    void extractOverLapEntryFromResponse_noEventsKey_test(){
        Assertions.assertEquals(Map.of(),AdjustmentOverlapHelper.extractOverLapEntryFromResponse(Map.of("data",Map.of("key","value"))));
    }

    @Test
    void extractOverLapEntryFromResponse_emptyEventsKey_test(){
        Map<String,Object> eventsMap = new HashMap<>();
        eventsMap.put("events",new ArrayList<>());
        Assertions.assertEquals(Map.of(),AdjustmentOverlapHelper.extractOverLapEntryFromResponse(Map.of("data",eventsMap)));
    }

    @Test
    void extractOverLapEntryFromResponse_validEvents_test(){
        Map<String,Object> eventsMap = new HashMap<>();
        Map<String,Object> events = new HashMap<>();
        events.put("key","resp");
        eventsMap.put("events",new ArrayList<>(){{add(events);}});
        Assertions.assertEquals(events,AdjustmentOverlapHelper.extractOverLapEntryFromResponse(Map.of("data",eventsMap)));
    }

    @Test
    void extractOverLapEntryDetails_runningEntry_test(){
        Map<String,Object> entry = new HashMap<>();
        entry.put(SchedulingKeys.START_TIME,1659084026000l);
        entry.put(SchedulingKeys.END_TIME,1659088799000l);
        Map<String,Object> metaMap = new HashMap<>();
        metaMap.put(SchedulingKeys.STATUS, REPORT_STATUS.CLOCKED_IN.toString());
        entry.put(SchedulingKeys.METADATA,metaMap);
        Map<String,Object> response = AdjustmentOverlapHelper.extractOverLapEntryDetails(entry, DateConstants.ZONE_ID_IST);
        Map<String,Object> expectedMap = new HashMap<>();
        expectedMap.put("overlappingEntry","29-Jul-2022 02:10:26 PM - Ongoing ");
        expectedMap.put("message","Adjusted time overlaps with existing entry");
        Assertions.assertEquals(expectedMap,response);
    }

    @Test
    void extractOverLapEntryDetails_clockedOut_test(){
        Map<String,Object> entry = new HashMap<>();
        entry.put(SchedulingKeys.START_TIME,1659084026000l);
        entry.put(SchedulingKeys.END_TIME,1659088799000l);
        Map<String,Object> metaMap = new HashMap<>();
        metaMap.put(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString());
        entry.put(SchedulingKeys.METADATA,metaMap);
        Map<String,Object> response = AdjustmentOverlapHelper.extractOverLapEntryDetails(entry, DateConstants.ZONE_ID_IST);
        Map<String,Object> expectedMap = new HashMap<>();
        expectedMap.put("overlappingEntry","29-Jul-2022 02:10:26 PM - 29-Jul-2022 03:29:59 PM");
        expectedMap.put("message","Adjusted time overlaps with existing entry");
        Assertions.assertEquals(expectedMap,response);
    }

    @Test
    void extractAllOverLapEntryDetails_runningEntry_test(){
        Map<String,Object> entry = MockEvent.getRawEvent();
        entry.put(SchedulingKeys.START_TIME,1659084026000l);
        entry.put(SchedulingKeys.END_TIME,1659088799000l);
        Map<String,Object> metaMap = (Map<String, Object>) entry.get(SchedulingKeys.METADATA);
        metaMap.put(SchedulingKeys.STATUS, REPORT_STATUS.CLOCKED_IN.toString());
        entry.put(SchedulingKeys.METADATA,metaMap);
        Map<String,Object> response = AdjustmentOverlapHelper.extractAllOverLapEntryDetails(entry,DateConstants.ZONE_ID_IST,"test@gmail.com");
        Assertions.assertEquals("current_running",response.get("overlappingEntryType"));
        Assertions.assertEquals("29-Jul-2022 02:10:26 PM - Ongoing ",response.get("overlappingEntry"));
        Assertions.assertEquals("c07b7576-ce1d-4cf1-8763-614a51f3f100",response.get("overlappingEntryID"));
        Assertions.assertEquals("Adjusted time overlaps with existing entry",response.get("overlappingInfo"));
        Assertions.assertEquals("Adjusted time overlaps with existing entry",response.get("message"));
    }

    @Test
    void extractAllOverLapEntryDetails_clockedOut_test(){
        Map<String,Object> entry = MockEvent.getRawEvent();
        entry.put(SchedulingKeys.START_TIME,1659084026000l);
        entry.put(SchedulingKeys.END_TIME,1659088799000l);
        Map<String,Object> metaMap = (Map<String, Object>) entry.get(SchedulingKeys.METADATA);
        metaMap.put(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString());
        entry.put(SchedulingKeys.METADATA,metaMap);
        Map<String,Object> response = AdjustmentOverlapHelper.extractAllOverLapEntryDetails(entry, DateConstants.ZONE_ID_IST,"test@gmail.com");
        Assertions.assertEquals("original",response.get("overlappingEntryType"));
        Assertions.assertEquals("29-Jul-2022 02:10:26 PM - 29-Jul-2022 03:29:59 PM",response.get("overlappingEntry"));
        Assertions.assertEquals(new ReportsDTO(entry,"test@gmail.com",DateConstants.ZONE_ID_IST),response.get("overlappingEntryObj"));
        Assertions.assertEquals("c07b7576-ce1d-4cf1-8763-614a51f3f100",response.get("overlappingEntryID"));
        Assertions.assertEquals("Adjusted time overlaps with existing entry",response.get("overlappingInfo"));
        Assertions.assertEquals("Adjusted time overlaps with existing entry",response.get("message"));
    }

    @Test
    void extractAllOverLapEntryDetails_pending_test(){
        Map<String,Object> entry = MockEvent.getRawEvent();
        entry.put(SchedulingKeys.START_TIME,1659084026000l);
        entry.put(SchedulingKeys.END_TIME,1659088799000l);
        Map<String,Object> metaMap = (Map<String, Object>) entry.get(SchedulingKeys.METADATA);
        metaMap.put(SchedulingKeys.STATUS, REPORT_STATUS.PENDING.toString());
        entry.put(SchedulingKeys.METADATA,metaMap);
        Map<String,Object> response = AdjustmentOverlapHelper.extractAllOverLapEntryDetails(entry, DateConstants.ZONE_ID_IST,"test@gmail.com");
        Assertions.assertEquals("pending",response.get("overlappingEntryType"));
        Assertions.assertEquals("29-Jul-2022 02:10:26 PM - 29-Jul-2022 03:29:59 PM",response.get("overlappingEntry"));
        Assertions.assertEquals(new ReportsDTO(entry,"test@gmail.com",DateConstants.ZONE_ID_IST),response.get("overlappingEntryObj"));
        Assertions.assertEquals("c07b7576-ce1d-4cf1-8763-614a51f3f100",response.get("overlappingEntryID"));
        Assertions.assertEquals("Adjusted time overlaps with existing entry",response.get("overlappingInfo"));
        Assertions.assertEquals("Adjusted time overlaps with existing entry",response.get("message"));
    }


}