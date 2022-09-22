package com.yoco.commons.utils;

import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.modal.date.DateDetailsDTO;
import com.yoco.commons.modal.date.RangeInfoDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAdjusters;
import java.util.Date;
import java.util.TimeZone;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

class DateUtilTest {

    @Test
    void getCurrentTime_test(){
      assertNotEquals(0L,DateUtil.getCurrentTime());
    }

    @Test
    void truncateMillis_test(){
        assertEquals(345000L,DateUtil.truncateMillis(345678));
    }

    @Test
    void getZonedDateTime_emptyZoneID_test(){
        ZonedDateTime zdt = DateUtil.getZonedDateTime("");
        assertEquals(TimeZone.getDefault().toZoneId().getId(),zdt.getZone().getId());
    }

    @Test
    void getZonedDateTime_milliswithzoneID_test(){
        ZonedDateTime zdt = DateUtil.getZonedDateTime(1646681400000L,"Asia/Kolkata");
        assertEquals("Asia/Kolkata",zdt.getZone().getId());
        assertEquals(1,zdt.getHour());
        assertEquals(0,zdt.getMinute());
        assertEquals(0,zdt.getSecond());
        assertEquals(0,zdt.getNano());
        assertEquals(2022,zdt.getYear());
        assertEquals(3,zdt.getMonthValue());
        assertEquals(8,zdt.getDayOfMonth());
    }

    @Test
    void convertMillisToDateTimeText_zeroMillis_test(){
        String expected = DateUtil.getZonedDateTime("Asia/Kolkata").format(DateTimeFormatter.ofPattern(DateFormats.MM_DD_YYYY.toString()));
        assertEquals(expected,DateUtil.convertMillisToDateTimeText(DateFormats.MM_DD_YYYY,0,"Asia/Kolkata"));
    }

    @Test
    void convertMillisToDateTimeText_valid_test(){
        assertEquals("03/08/2022 01:00:00 AM",DateUtil.convertMillisToDateTimeText(DateFormats.MM_DD_YYYY_HH_MM_SS_A,1646681400000L,"Asia/Kolkata"));
    }

    @Test
    void getMillis_valid_test(){
        assertEquals(1646681400000L,DateUtil.convertDateDetailsIntoMillis("Asia/Kolkata", new DateDetailsDTO(1,0,0,0,8,3,2022)));
    }

    @Test
    void getRangeDetails_invalidRange_test(){
        try{
            DateUtil.getRangeDetails("","range","","","123");
        }catch(Exception e){
            assertEquals("Invalid date range",e.getMessage());
        }
    }

    @Test
    void getRangeDetails_todayRange_test(){
        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)){
            RangeInfoDTO mockRangeInfoDto = new RangeInfoDTO();
            dateUtilMockedStatic.when(()->DateUtil.getRangeDetails("Asia/Kolkata","today","","","123")).thenCallRealMethod();
            dateUtilMockedStatic.when(()->DateUtil.getRangeMillisForToday("Asia/Kolkata")).thenReturn(mockRangeInfoDto);
            RangeInfoDTO actual = DateUtil.getRangeDetails("Asia/Kolkata","today","","","123");
            assertEquals(mockRangeInfoDto,actual);
        }
    }

    @Test
    void getRangeDetails_CURRENT_WEEK_Range_test(){
        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)){
            RangeInfoDTO mockRangeInfoDto = new RangeInfoDTO();
            dateUtilMockedStatic.when(()->DateUtil.getRangeDetails("Asia/Kolkata","current_week","","","123")).thenCallRealMethod();
            dateUtilMockedStatic.when(()->DateUtil.getRangeMillisForCurrentWeek("Asia/Kolkata","123")).thenReturn(mockRangeInfoDto);
            RangeInfoDTO actual = DateUtil.getRangeDetails("Asia/Kolkata","current_week","","","123");
            assertEquals(mockRangeInfoDto,actual);
        }
    }

    @Test
    void getRangeDetails_LAST_WEEK_Range_test(){
        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)){
            RangeInfoDTO mockRangeInfoDto = new RangeInfoDTO();
            dateUtilMockedStatic.when(()->DateUtil.getRangeDetails("Asia/Kolkata","last_week","","","123")).thenCallRealMethod();
            dateUtilMockedStatic.when(()->DateUtil.getRangeMillisForLastWeek("Asia/Kolkata","123")).thenReturn(mockRangeInfoDto);
            RangeInfoDTO actual = DateUtil.getRangeDetails("Asia/Kolkata","last_week","","","123");
            assertEquals(mockRangeInfoDto,actual);
        }
    }

    @Test
    void getRangeDetails_BY_DATE_Range_test(){
        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)){
            RangeInfoDTO mockRangeInfoDto = new RangeInfoDTO();
            dateUtilMockedStatic.when(()->DateUtil.getRangeDetails("Asia/Kolkata","by_date","03/08/2022","03/08/2022","123")).thenCallRealMethod();
            dateUtilMockedStatic.when(()->DateUtil.getRangeMillisForByDate("03/08/2022","03/08/2022","Asia/Kolkata")).thenReturn(mockRangeInfoDto);
            RangeInfoDTO actual = DateUtil.getRangeDetails("Asia/Kolkata","by_date","03/08/2022","03/08/2022","123");
            assertEquals(mockRangeInfoDto,actual);
        }
    }

    @Test
    void convertDateTimeTextToMillis_validDateTimeText_test(){
        assertEquals(1646681400000L, DateUtil.convertDateTimeTextToMillis("03/08/2022 01:00:00 AM",DateFormats.MM_DD_YYYY_HH_MM_SS_A,"Asia/Kolkata"));
    }

    @Test
    void convertDateTimeTextToMillis_validDateText_test(){
        assertEquals(1646677800000L, DateUtil.convertDateTimeTextToMillis("03/08/2022",DateFormats.MM_DD_YYYY,"Asia/Kolkata"));
    }

    @Test
    void convertDateTimeTextToMillis_invalidDateText_test(){
        try{
            assertEquals(1646677800000L, DateUtil.convertDateTimeTextToMillis("03/08/2022invalid",DateFormats.MM_DD_YYYY,"Asia/Kolkata"));
        }catch (Exception e){
            assertEquals("Invalid date format",e.getMessage());
        }
    }
    
    @Test
    void getMillisForToday_test(){
        RangeInfoDTO actual = DateUtil.getRangeMillisForToday("Asia/Kolkata");
        long expectedStartDate = DateUtil.getZonedDateTime("Asia/Kolkata").withHour(0).withMinute(0).withSecond(0).withNano(0).toInstant().toEpochMilli();
        long expectedEndDate = DateUtil.getZonedDateTime("Asia/Kolkata").withHour(23).withMinute(59).withSecond(59).withNano(999999999).toInstant().toEpochMilli();
        assertEquals(expectedStartDate, actual.getFromDateEpochMilliseconds());
        assertEquals(expectedEndDate, actual.getToDateEpochMilliseconds());
    }

    @Test
    void getRangeMillisForCurrentWeek_WeekStartDateSunday_test(){
        try(MockedStatic<AccountUtil> accUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accUtilMockedStatic.when(()->AccountUtil.getStartDayOfWeek("123")).thenReturn(DayOfWeek.SUNDAY);
            RangeInfoDTO actual = DateUtil.getRangeMillisForCurrentWeek("Asia/Kolkata","123");
            ZonedDateTime actualStartDate = DateUtil.getZonedDateTime(actual.getFromDateEpochMilliseconds(),"Asia/Kolkata");
            ZonedDateTime  actualEndDate = DateUtil.getZonedDateTime(actual.getToDateEpochMilliseconds(),"Asia/Kolkata");
            assertEquals(DayOfWeek.SUNDAY,actualStartDate.getDayOfWeek());
            assertEquals(DayOfWeek.SATURDAY,actualEndDate.getDayOfWeek());
            LocalDateTime expectedStartDate = LocalDateTime.now(ZoneId.of("Asia/Kolkata")).with(TemporalAdjusters.previousOrSame(DayOfWeek.SUNDAY)).withHour(0).withMinute(0).withSecond(0).withNano(0);
            LocalDateTime expectedEndDate = LocalDateTime.now(ZoneId.of("Asia/Kolkata")).with(TemporalAdjusters.nextOrSame(DayOfWeek.SATURDAY)).withHour(23).withMinute(59).withSecond(59).withNano(999999999);
            assertEquals(expectedStartDate.getDayOfMonth(),actualStartDate.getDayOfMonth());
            assertEquals(expectedEndDate.getDayOfMonth(),actualEndDate.getDayOfMonth());
        }
    }

    @Test
    void getRangeMillisForCurrentWeek_WeekStartDateMonday_test(){
        try(MockedStatic<AccountUtil> accUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accUtilMockedStatic.when(()->AccountUtil.getStartDayOfWeek("123")).thenReturn(DayOfWeek.MONDAY);
            RangeInfoDTO actual = DateUtil.getRangeMillisForCurrentWeek("Asia/Kolkata","123");
            ZonedDateTime actualStartDate = DateUtil.getZonedDateTime(actual.getFromDateEpochMilliseconds(),"Asia/Kolkata");
            ZonedDateTime  actualEndDate = DateUtil.getZonedDateTime(actual.getToDateEpochMilliseconds(),"Asia/Kolkata");
            assertEquals(DayOfWeek.MONDAY,actualStartDate.getDayOfWeek());
            assertEquals(DayOfWeek.SUNDAY,actualEndDate.getDayOfWeek());
            LocalDateTime expectedStartDate = LocalDateTime.now(ZoneId.of("Asia/Kolkata")).with(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY)).withHour(0).withMinute(0).withSecond(0).withNano(0);
            LocalDateTime expectedEndDate = LocalDateTime.now(ZoneId.of("Asia/Kolkata")).with(TemporalAdjusters.nextOrSame(DayOfWeek.SUNDAY)).withHour(23).withMinute(59).withSecond(59).withNano(999999999);
            assertEquals(expectedStartDate.getDayOfMonth(),actualStartDate.getDayOfMonth());
            assertEquals(expectedEndDate.getDayOfMonth(),actualEndDate.getDayOfMonth());
        }
    }

    @Test
    void getRangeMillisForLastWeek_WeekStartDateSunday_test(){
        try(MockedStatic<AccountUtil> accUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accUtilMockedStatic.when(()->AccountUtil.getStartDayOfWeek("123")).thenReturn(DayOfWeek.SUNDAY);
            RangeInfoDTO actual = DateUtil.getRangeMillisForLastWeek("Asia/Kolkata","123");
            ZonedDateTime actualStartDate = DateUtil.getZonedDateTime(actual.getFromDateEpochMilliseconds(),"Asia/Kolkata");
            ZonedDateTime  actualEndDate = DateUtil.getZonedDateTime(actual.getToDateEpochMilliseconds(),"Asia/Kolkata");
            assertEquals(DayOfWeek.SUNDAY,actualStartDate.getDayOfWeek());
            assertEquals(DayOfWeek.SATURDAY,actualEndDate.getDayOfWeek());
            LocalDateTime expectedStartDate = LocalDateTime.now(ZoneId.of("Asia/Kolkata")).with(TemporalAdjusters.previousOrSame(DayOfWeek.SUNDAY)).minusDays(7).withHour(0).withMinute(0).withSecond(0).withNano(0);
            LocalDateTime expectedEndDate = LocalDateTime.now(ZoneId.of("Asia/Kolkata")).with(TemporalAdjusters.nextOrSame(DayOfWeek.SATURDAY)).minusDays(7).withHour(23).withMinute(59).withSecond(59).withNano(999999999);
            assertEquals(expectedStartDate.getDayOfMonth(),actualStartDate.getDayOfMonth());
            assertEquals(expectedEndDate.getDayOfMonth(),actualEndDate.getDayOfMonth());
        }
    }

    @Test
    void getRangeMillisForLastWeek_WeekStartDateMonday_test(){
        try(MockedStatic<AccountUtil> accUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accUtilMockedStatic.when(()->AccountUtil.getStartDayOfWeek("123")).thenReturn(DayOfWeek.MONDAY);
            RangeInfoDTO actual = DateUtil.getRangeMillisForLastWeek("Asia/Kolkata","123");
            ZonedDateTime actualStartDate = DateUtil.getZonedDateTime(actual.getFromDateEpochMilliseconds(),"Asia/Kolkata");
            ZonedDateTime  actualEndDate = DateUtil.getZonedDateTime(actual.getToDateEpochMilliseconds(),"Asia/Kolkata");
            assertEquals(DayOfWeek.MONDAY,actualStartDate.getDayOfWeek());
            assertEquals(DayOfWeek.SUNDAY,actualEndDate.getDayOfWeek());
            LocalDateTime expectedStartDate = LocalDateTime.now(ZoneId.of("Asia/Kolkata")).with(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY)).minusDays(7).withHour(0).withMinute(0).withSecond(0).withNano(0);
            LocalDateTime expectedEndDate = LocalDateTime.now(ZoneId.of("Asia/Kolkata")).with(TemporalAdjusters.nextOrSame(DayOfWeek.SUNDAY)).minusDays(7).withHour(23).withMinute(59).withSecond(59).withNano(999999999);
            assertEquals(expectedStartDate.getDayOfMonth(),actualStartDate.getDayOfMonth());
            assertEquals(expectedEndDate.getDayOfMonth(),actualEndDate.getDayOfMonth());
        }
    }

    @Test
    void getRangeMillisForByDate_valid_test(){
        RangeInfoDTO actual = DateUtil.getRangeMillisForByDate("03/29/2022","03/29/2022","Asia/Kolkata");
        Assertions.assertEquals(1648492200000L,actual.getFromDateEpochMilliseconds());
        Assertions.assertEquals(1648578599999L,actual.getToDateEpochMilliseconds());
    }

    @Test
    void getNextOrSameDayOfWeek_test(){
        LocalDateTime localDateTime = DateUtil.getNextOrSameDayOfWeek("Asia/Kolkata",DayOfWeek.MONDAY);
        assertEquals(DayOfWeek.MONDAY,localDateTime.getDayOfWeek());
        Assertions.assertTrue(LocalDateTime.now(ZoneId.of("Asia/Kolkata")).getDayOfMonth() <= localDateTime.getDayOfMonth() ||
                LocalDateTime.now(ZoneId.of("Asia/Kolkata")).getMonthValue() <= localDateTime.getMonthValue());

    }

    @Test
    void getNextDayOfWeek_test(){
        LocalDateTime localDateTime = DateUtil.getNextDayOfWeek("Asia/Kolkata",DayOfWeek.MONDAY);
        assertEquals(DayOfWeek.MONDAY,localDateTime.getDayOfWeek());
        Assertions.assertTrue(LocalDateTime.now(ZoneId.of("Asia/Kolkata")).getDayOfMonth() < localDateTime.getDayOfMonth() ||
                LocalDateTime.now(ZoneId.of("Asia/Kolkata")).getMonthValue() < localDateTime.getMonthValue());
    }

    @Test
    void getTimeFormatAsPerCompany_ifDisplayTimeFormatIsnull_test(){
        String resultString = DateUtil.getTimeFormatAsPerCompany(10000L, null);
        Assertions.assertEquals("0h 00m",resultString );
    }

    @Test
    void getTimeFormatAsPerCompany_ifDisplayTimeFormatIsEmpty_test(){
        String resultString = DateUtil.getTimeFormatAsPerCompany(100000000L, "");
        Assertions.assertEquals("27h 46m",resultString );
    }

    @Test
    void getTimeFormatAsPerCompany_withNegativeNoOfMillis_test(){
        String resultString = DateUtil.getTimeFormatAsPerCompany(-100000000L, "HH:MM:SS");
        Assertions.assertEquals("0h 00m 00s",resultString );
    }

    @Test
    void getTimeFormatAsPerCompany_valid_test(){
        String resultString = DateUtil.getTimeFormatAsPerCompany(100000000L, "HH:MM:SS");
        Assertions.assertEquals("27h 46m 40s",resultString );
    }

    @Test
    void getCurrentYear_test(){
        Assertions.assertEquals(Integer.toString(LocalDate.now().getYear()),DateUtil.getCurrentYear());
    }

    @Test
    void isCurrentDay_nullZoneID(){
        Assertions.assertFalse(DateUtil.isCurrentDay(0L,null));
    }

    @Test
    void isCurrentDay_notCurrentDay(){
        Assertions.assertFalse(DateUtil.isCurrentDay(1657391400000L,"Asia/Kolkata"));
    }

    @Test
    void isCurrentDay_CurrentDay(){
        Assertions.assertTrue(DateUtil.isCurrentDay(new Date().getTime(),"Asia/Kolkata"));
    }

    @Test
    void getCurrentDayStartMillis_nullZoneId_shouldReturn0(){
        Assertions.assertEquals(0L,DateUtil.getCurrentDayStartMillis(null));
    }

    @Test
    void getDurationDisplayText_negative_duration_test(){
        Assertions.assertEquals("3 hours and 53 minutes (-)",DateUtil.getDurationDisplayText(1659095648000l,1659081640000l));
    }

    @Test
    void getDurationDisplayText_positive_duration_test(){
        Assertions.assertEquals("3 hours and 53 minutes (+)",DateUtil.getDurationDisplayText(1659081640000l,1659095648000l));
    }

    @Test
    void getDurationDisplayText_minutes_lessThan10_test(){
        Assertions.assertEquals("05 minutes (+)",DateUtil.getDurationDisplayText(1659081640000l,1659081940000l));
    }

    @Test
    void getDurationDisplayText_minutes_lessThan0_test(){
        Assertions.assertEquals("00 minutes (+)",DateUtil.getDurationDisplayText(1659081645000l,1659081645000l));
    }

}
