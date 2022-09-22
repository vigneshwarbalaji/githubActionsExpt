package com.yoco.commons.utils;

import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.services.UrlFetcher;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.util.Locale;
import java.util.Map;

class TimeZoneUtilTest {

    @Test
    void getCurrentOffset_passing_valid_plus_zone_id_test(){

        Assertions.assertEquals("(GMT+05:30) Asia/Kolkata (India Time)", TimeZoneUtil.getCurrentOffset("Asia/Kolkata"));
    }

    @Test
    void getCurrentOffset_passing_utc_zone_id_test(){

        Assertions.assertEquals("(GMT+00:00) UTC (Coordinated Universal Time)", TimeZoneUtil.getCurrentOffset("UTC"));
    }

//    @Test
//    void getCurrentOffset_passing_valid_negative_zone_id_test(){
//
//        Assertions.assertEquals("(GMT-08:00) US/Pacific (Pacific Time)", TimeZoneUtil.getCurrentOffset("US/Pacific"));
//    }

    @Test
    void getAllZoneIds_test(){
        Map<String, Object> zoneList = TimeZoneUtil.getAllZoneIds();
        Assertions.assertTrue(!ObjUtils.isNullOrEmpty(zoneList));
    }

    @Test
    void getTimeZoneID_empty_offset_test() {
        try{
            TimeZoneUtil.getTimeZoneID("");
        }catch (Exception e){
            Assertions.assertEquals(TimeZoneUtil.INVALID_OFFSET, e.getMessage());
        }
    }

    @Test
    void getTimeZoneID_null_offset_test() {
        try{
            TimeZoneUtil.getTimeZoneID(null);
        }catch (Exception e){
            Assertions.assertEquals(TimeZoneUtil.INVALID_OFFSET, e.getMessage());
        }
    }

    @Test
    void getTimeZoneID_invalid_offset_test() {
        Assertions.assertEquals("UTC", TimeZoneUtil.getTimeZoneID("100"));
    }

    @Test
    void getTimeZoneID_valid_offset_test() {
        Assertions.assertEquals("UTC", TimeZoneUtil.getTimeZoneID("0"));
    }

    @Test
    void getTimeZoneID_valid_positive_offset_test() {
        Assertions.assertEquals("Asia/Kolkata", TimeZoneUtil.getTimeZoneID("330"));
    }

    @Test
    void getTimeZoneID_valid_negative_offset_with_dst_and_non_dst_values_test() {
        String zoneId = TimeZoneUtil.getTimeZoneID("-480");
        String alaska = "US/Alaska", pacific = "US/Pacific";
        Boolean isAlaskaOrPacific = zoneId.equals(alaska) || zoneId.equals(pacific);
        Assertions.assertTrue(isAlaskaOrPacific);
    }

    @Test
    void getZoneInFormat_empty_zone_test(){

        try{
            TimeZoneUtil.getZoneInFormat("", TimeZoneUtil.OFFSET, Locale.ENGLISH);
        }catch (Exception e){
            Assertions.assertEquals(TimeZoneUtil.INVALID_TIME_ZONE, e.getMessage());
        }
    }

    @Test
    void getZoneInFormat_null_zone_test(){

        try{
            TimeZoneUtil.getZoneInFormat(null, TimeZoneUtil.OFFSET, Locale.ENGLISH);
        }catch (Exception e){
            Assertions.assertEquals(TimeZoneUtil.INVALID_TIME_ZONE, e.getMessage());
        }
    }

    @Test
    void getZoneInFormat_empty_zone_format_test(){

        try{
            TimeZoneUtil.getZoneInFormat("Asia/kolkata", "", Locale.ENGLISH);
        }catch (Exception e){
            Assertions.assertEquals(TimeZoneUtil.INVALID_ZONE_FORMAT, e.getMessage());
        }
    }

    @Test
    void getZoneInFormat_null_zone_format_test(){

        try{
            TimeZoneUtil.getZoneInFormat("Asia/kolkata", null, Locale.ENGLISH);
        }catch (Exception e){
            Assertions.assertEquals(TimeZoneUtil.INVALID_ZONE_FORMAT, e.getMessage());
        }
    }

    @Test
    void getZoneInFormat_null_Locale_test(){
        String zone =  TimeZoneUtil.getZoneInFormat("Asia/Kolkata", TimeZoneUtil.OFFSET, null);
        Assertions.assertEquals("GMT+05:30", zone);
    }

    @Test
    void getZoneInFormat_success_offset_test(){
        String zone =  TimeZoneUtil.getZoneInFormat("Asia/Kolkata", TimeZoneUtil.OFFSET, Locale.ENGLISH);
        Assertions.assertEquals("GMT+05:30", zone);
    }

    @Test
    void getZoneInFormat_success_display_name_short_test(){
        String zone =  TimeZoneUtil.getZoneInFormat("Asia/Kolkata", TimeZoneUtil.DISPLAY_NAME_SHORT, Locale.ENGLISH);
        Assertions.assertEquals("IT", zone);
    }


    @Test
    void getZoneInFormat_success_display_name_long_test(){
        String zone =  TimeZoneUtil.getZoneInFormat("Asia/Kolkata", TimeZoneUtil.DISPLAY_NAME_LONG, Locale.ENGLISH);
        Assertions.assertEquals("India Time", zone);
    }

    @Test
    void getZoneInFormat_success_zone_display_name_short_test(){
        String zone =  TimeZoneUtil.getZoneInFormat("Asia/Kolkata", TimeZoneUtil.ZONE_DISPLAY_NAME_SHORT, Locale.ENGLISH);
        Assertions.assertEquals("Asia/Kolkata IT", zone);
    }

    @Test
    void getZoneInFormat_success_zone_display_name_long_test(){
        String zone =  TimeZoneUtil.getZoneInFormat("Asia/Kolkata", TimeZoneUtil.ZONE_DISPLAY_NAME_LONG, Locale.ENGLISH);
        Assertions.assertEquals("Asia/Kolkata India Time", zone);
    }

    @Test
    void getZoneInFormat_success_offset_zone_display_name_long_test(){
        String zone =  TimeZoneUtil.getZoneInFormat("Asia/Kolkata", TimeZoneUtil.OFFSET_ZONE_DISPLAY_NAME_LONG, Locale.ENGLISH);
        Assertions.assertEquals("(GMT+05:30) Asia/Kolkata (India Time)", zone);
    }

    @Test
    void getTimezoneApiUrl_valid_test(){
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)){
                commonAppPropertiesMockedStatic.when(CommonAppProperties::getGoogleMapsZoneApiKey).thenReturn("tzkey");
                dateUtilMockedStatic.when(DateUtil::getCurrentTime).thenReturn(123L);
                Assertions.assertEquals("https://maps.googleapis.com/maps/api/timezone/json?location=lat,long&timestamp=123&key=tzkey",TimeZoneUtil.getTimezoneApiUrl("lat","long"));
        }
    }

    @Test
    void getTimezoneFromCoordinates_nullLatitude_test(){
        Assertions.assertEquals("",TimeZoneUtil.getTimezoneFromCoordinates(null,""));
    }

    @Test
    void getTimezoneFromCoordinates_emptyLongitude_test(){
        Assertions.assertEquals("",TimeZoneUtil.getTimezoneFromCoordinates("1.234",""));
    }

    @Test
    void getTimezoneFromCoordinates_exception_test(){
        try(MockedStatic<TimeZoneUtil> timeZoneUtilMockedStatic = Mockito.mockStatic(TimeZoneUtil.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.getTimezoneApiUrl("1.234","2.34")).thenReturn("url");
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.getTimezoneFromCoordinates("1.234","2.34")).thenCallRealMethod();
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url",(Map<String, Object>) null,new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON},null)).thenThrow(new IllegalArgumentException("test"));
            Assertions.assertEquals("",TimeZoneUtil.getTimezoneFromCoordinates("1.234","2.34"));
        }
    }

    @Test
    void getTimezoneFromCoordinates_nullResponse_test(){
        try(MockedStatic<TimeZoneUtil> timeZoneUtilMockedStatic = Mockito.mockStatic(TimeZoneUtil.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.getTimezoneApiUrl("1.234","2.34")).thenReturn("url");
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.getTimezoneFromCoordinates("1.234","2.34")).thenCallRealMethod();
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url",(Map<String, Object>) null,new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON},null)).thenReturn(null);
            Assertions.assertEquals("",TimeZoneUtil.getTimezoneFromCoordinates("1.234","2.34"));
        }
    }

    @Test
    void getTimezoneFromCoordinates_nullTimezoneId_test(){
        try(MockedStatic<TimeZoneUtil> timeZoneUtilMockedStatic = Mockito.mockStatic(TimeZoneUtil.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.getTimezoneApiUrl("1.234","2.34")).thenReturn("url");
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.getTimezoneFromCoordinates("1.234","2.34")).thenCallRealMethod();
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url",(Map<String, Object>) null,new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON},null)).thenReturn(Map.of("key","value"));
            Assertions.assertEquals("",TimeZoneUtil.getTimezoneFromCoordinates("1.234","2.34"));
        }
    }

    @Test
    void getTimezoneFromCoordinates_isZoneAllowedFalse_test(){
        try(MockedStatic<TimeZoneUtil> timeZoneUtilMockedStatic = Mockito.mockStatic(TimeZoneUtil.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.getTimezoneApiUrl("1.234","2.34")).thenReturn("url");
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.getTimezoneFromCoordinates("1.234","2.34")).thenCallRealMethod();
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.isZoneAllowed("value")).thenCallRealMethod();
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url",(Map<String, Object>) null,new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON},null)).thenReturn(Map.of("timeZoneId","value"));
            Assertions.assertEquals("",TimeZoneUtil.getTimezoneFromCoordinates("1.234","2.34"));
        }
    }

    @Test
    void getTimezoneFromCoordinates_valid_test(){
        try(MockedStatic<TimeZoneUtil> timeZoneUtilMockedStatic = Mockito.mockStatic(TimeZoneUtil.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.getTimezoneApiUrl("1.234","2.34")).thenReturn("url");
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.getTimezoneFromCoordinates("1.234","2.34")).thenCallRealMethod();
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.isZoneAllowed("Asia/Kolkata")).thenCallRealMethod();
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url",(Map<String, Object>) null,new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON},null)).thenReturn(Map.of("timeZoneId","Asia/Kolkata"));
            Assertions.assertEquals("Asia/Kolkata",TimeZoneUtil.getTimezoneFromCoordinates("1.234","2.34"));
        }
    }
}
