package com.yoco.adjustment.util;

import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.modal.report.AdjustmentDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyString;

class AdjustmentUtilTest {

    @Test
    void extractRefinedAdjustmentMessage_test(){
        Assertions.assertEquals("test",AdjustmentUtil.extractRefinedAdjustmentMessage("Others - test"));
    }

    @Test
    void setAdjustmentInfo_test(){
        Map<String,Object> response = AdjustmentUtil.setAdjustmentInfo("admin1","test");
        Assertions.assertEquals("admin1",response.get("by"));
        Assertions.assertEquals("test",response.get("notes"));
        Assertions.assertNotNull(response.get("when"));
    }

    @Test
    void extractAdditionalInfo_empty_metaData_test(){
        Assertions.assertEquals(Map.of(),AdjustmentUtil.extractAdditionalInfo(null));
    }

    @Test
    void extractAdditionalInfo_no_additionalInfo_test(){
        Assertions.assertEquals(Map.of(),AdjustmentUtil.extractAdditionalInfo(Map.of("key","data")));
    }

    @Test
    void extractAdditionalInfo_additionalInfo_test(){
        Map<String,Object> infoMap = new HashMap<>();
        infoMap.put("data","approveInfo");
        Assertions.assertEquals(infoMap,AdjustmentUtil.extractAdditionalInfo(Map.of("additionalInfo",infoMap)));
    }

    @Test
    void extractAdjustmentInfo_empty_metaData_test(){
        Assertions.assertEquals(Map.of(),AdjustmentUtil.extractAdjustmentInfo(null));
    }

    @Test
    void extractAdjustmentInfo_no_adjustmentInfo_test(){
        Assertions.assertEquals(Map.of(),AdjustmentUtil.extractAdjustmentInfo(Map.of("key","data")));
    }

    @Test
    void extractAdjustmentInfo_adjustmentInfo_test(){
        Map<String,Object> infoMap = new HashMap<>();
        infoMap.put("data","approveInfo");
        Assertions.assertEquals(infoMap,AdjustmentUtil.extractAdjustmentInfo(Map.of("adjustment_info",infoMap)));
    }

    @Test
    void isAdjustmentMadeForToday_invalidTimezone_shouldThrowExceptionAndReturnFalse(){
        Assertions.assertFalse(AdjustmentUtil.isAdjustmentMadeForToday(List.of(),"invalid"));
    }

    @Test
    void isAdjustmentMadeForToday_noAdjustmentsList_shouldReturnFalse(){
        Assertions.assertFalse(AdjustmentUtil.isAdjustmentMadeForToday(List.of(),"Asia/Kolkata"));
    }

    @Test
    void isAdjustmentMadeForToday_AdjustmentBeforeToday_shouldReturnFalse(){
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        adjustmentDTO.setAdjustedInTime(1658761200000L);
        adjustmentDTO.setOriginalInTime(1658761100000L);
        Assertions.assertFalse(AdjustmentUtil.isAdjustmentMadeForToday(List.of(adjustmentDTO),"Asia/Kolkata"));
    }

    @Test
    void isAdjustmentMadeForToday_AdjustmentAdjustedInTimeAfterToday_shouldReturnTrue(){
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        adjustmentDTO.setAdjustedInTime(new Date().getTime());
        adjustmentDTO.setOriginalInTime(1658761100000L);
        Assertions.assertTrue(AdjustmentUtil.isAdjustmentMadeForToday(List.of(adjustmentDTO),"Asia/Kolkata"));
    }

    @Test
    void isAdjustmentMadeForToday_AdjustmentOriginalInTimeAfterToday_shouldReturnTrue(){
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        adjustmentDTO.setAdjustedInTime(1658761100000L);
        adjustmentDTO.setOriginalInTime(new Date().getTime());
        Assertions.assertTrue(AdjustmentUtil.isAdjustmentMadeForToday(List.of(adjustmentDTO),"Asia/Kolkata"));
    }

    @Test
    void isAdjustmentMadeForToday_SecondAdjustmentOriginalInTimeAfterToday_shouldReturnTrue(){
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        adjustmentDTO.setAdjustedInTime(1658761100000L);
        adjustmentDTO.setOriginalInTime(1658761200000L);
        AdjustmentDTO adjustmentDTO2 = new AdjustmentDTO();
        adjustmentDTO2.setAdjustedInTime(1658761100000L);
        adjustmentDTO2.setOriginalInTime(new Date().getTime());
        Assertions.assertTrue(AdjustmentUtil.isAdjustmentMadeForToday(List.of(adjustmentDTO,adjustmentDTO2),"Asia/Kolkata"));
    }

    @Test
    void adjustInTimeMillisForBreakSessionOverlapping_empty_previousEntry_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getPreviousClockedOutEntryFromGivenTime(anyString(),anyString(),anyString())).thenReturn(null);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(1659084026000l,AdjustmentUtil.adjustInTimeMillisForBreakSessionOverlapping("111","123",1659084026000l,DateConstants.ZONE_ID_IST));
        }
    }

    @Test
    void adjustInTimeMillisForBreakSessionOverlapping_currentMillis_greaterThan_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Map<String,Object> entry = new HashMap<>();
            entry.put(SchedulingKeys.END_TIME,1659040826000l);
            Mockito.when(reportImplMock.getPreviousClockedOutEntryFromGivenTime(anyString(),anyString(),anyString())).thenReturn(entry);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(1659084026000l,AdjustmentUtil.adjustInTimeMillisForBreakSessionOverlapping("111","123",1659084026000l,DateConstants.ZONE_ID_IST));
        }
    }

    @Test
    void adjustInTimeMillisForBreakSessionOverlapping_currentMillis_lessThan_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Map<String,Object> entry = new HashMap<>();
            entry.put(SchedulingKeys.END_TIME,1659084086000l);
            Mockito.when(reportImplMock.getPreviousClockedOutEntryFromGivenTime(anyString(),anyString(),anyString())).thenReturn(entry);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(1659084026000l,AdjustmentUtil.adjustInTimeMillisForBreakSessionOverlapping("111","123",1659084026000l,DateConstants.ZONE_ID_IST));
        }
    }

    @Test
    void adjustInTimeMillisForBreakSessionOverlapping_same_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Map<String,Object> entry = new HashMap<>();
            entry.put(SchedulingKeys.END_TIME,1659084026009l);
            Mockito.when(reportImplMock.getPreviousClockedOutEntryFromGivenTime(anyString(),anyString(),anyString())).thenReturn(entry);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            Assertions.assertEquals(1659084026010l,AdjustmentUtil.adjustInTimeMillisForBreakSessionOverlapping("111","123",1659084026000l, DateConstants.ZONE_ID_IST));
        }
    }

}