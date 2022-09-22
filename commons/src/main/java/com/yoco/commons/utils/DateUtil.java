package com.yoco.commons.utils;

import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.modal.date.DateDetailsDTO;
import com.yoco.commons.modal.date.RangeInfoDTO;
import com.yoco.commons.validations.RangeValidator;
import lombok.extern.slf4j.Slf4j;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

@Slf4j
public class DateUtil {

    private DateUtil(){}

    public static final int MAX_NANO_SEC = 999999999;
    public static final int MAX_HOURS = 23;
    public static final int MAX_MINUTES = 59;
    public static final int MAX_SECONDS = 59;

    public static Long getCurrentTime (){
        return getZonedDateTime("").toInstant().toEpochMilli();
    }

    public static long truncateMillis(long millis){
        return (millis / 1000) * 1000;
    }

    public static ZonedDateTime getZonedDateTime(long millis, String zoneID){
        return ZonedDateTime.ofInstant( Instant.ofEpochMilli( millis), ZoneId.of(zoneID));
    }

    public static ZonedDateTime getZonedDateTime(String zoneID){
       return ZonedDateTime.now(ZoneId.of(RangeValidator.validateAndReturnZoneID(zoneID)));
    }

    public static String convertMillisToDateTimeText(DateFormats dateFormats, long millis, String zoneId){
        zoneId   =   RangeValidator.validateAndReturnZoneID(zoneId);
        millis =  (millis <= 0) ? getCurrentTime() : millis;
        var dateFormatString = DateFormats.validateAndReturnDateFormatString(dateFormats);
        return getZonedDateTime(millis,zoneId).format( DateTimeFormatter.ofPattern(dateFormatString));
    }

    public static long convertDateDetailsIntoMillis(String zoneId, DateDetailsDTO dateDetailsDto) throws DateTimeException {
        zoneId  = RangeValidator.validateAndReturnZoneID(zoneId);
        var zonedDateTime = ZonedDateTime.of(dateDetailsDto.getYear(), dateDetailsDto.getMonth(), dateDetailsDto.getDayOfMonth(), dateDetailsDto.getHours(), dateDetailsDto.getMinutes(), dateDetailsDto.getSeconds(), dateDetailsDto.getNanoSeconds(), ZoneId.of(zoneId) );
        return zonedDateTime.toInstant().toEpochMilli();
    }

    public static RangeInfoDTO getRangeDetails(String zoneId, String range, String startDate, String endDate, String accountID) {
        zoneId  =   RangeValidator.validateAndReturnZoneID(zoneId);
        switch (range){
            case DateConstants.TODAY:
                return getRangeMillisForToday(zoneId);
            case DateConstants.CURRENT_WEEK:
                return getRangeMillisForCurrentWeek(zoneId,accountID);
            case DateConstants.LAST_WEEK:
                return getRangeMillisForLastWeek(zoneId,accountID);
            case DateConstants.BY_DATE:
                return getRangeMillisForByDate(startDate,endDate,zoneId);
            default:
                throw new IllegalArgumentException("Invalid date range");
        }
    }

    public static long convertDateTimeTextToMillis(String date, DateFormats dateFormats, String zoneID){
        zoneID = RangeValidator.validateAndReturnZoneID(zoneID);
        var localDateTime = parseDateTime(date,dateFormats);
        if(localDateTime != null){
            return convertDateDetailsIntoMillis(zoneID, new DateDetailsDTO(localDateTime.getHour(),localDateTime.getMinute(),localDateTime.getSecond(),localDateTime.getNano(),localDateTime.getDayOfMonth(),localDateTime.getMonthValue(),localDateTime.getYear()));
        }else{
            var localDate = parseDate(date, dateFormats);
            if(localDate != null){
                return convertDateDetailsIntoMillis(zoneID, DateDetailsDTO.getStartTimeDtoForDate(localDate));
            }else{
                throw new IllegalArgumentException("Invalid date format");
            }
        }
    }

    public static LocalDateTime parseDateTime(String dateString, DateFormats dateFormats){
        try{
            var dateFormatString = DateFormats.validateAndReturnDateFormatString(dateFormats);
            var formatter = DateTimeFormatter.ofPattern(dateFormatString);
            return LocalDateTime.parse(dateString, formatter);
        }catch (Exception e){
              return null;
        }
    }

    public static LocalDate parseDate(String dateString, DateFormats dateFormats){
        try{
            var dateFormatString = DateFormats.validateAndReturnDateFormatString(dateFormats);
            var formatter = DateTimeFormatter.ofPattern(dateFormatString);
            return LocalDate.parse(dateString, formatter);
        }catch (Exception e){
            return null;
        }
    }

    public static RangeInfoDTO getRangeMillisForToday(String zoneId){
        var rangeInfoDTO = new RangeInfoDTO();
        rangeInfoDTO.setFromDateEpochMilliseconds(convertDateDetailsIntoMillis(zoneId,DateDetailsDTO.getStartTimeDtoForCurrentDay(zoneId)));
        rangeInfoDTO.setToDateEpochMilliseconds(convertDateDetailsIntoMillis(zoneId,DateDetailsDTO.getEndTimeDtoForCurrentDay(zoneId)));
        return rangeInfoDTO;
    }

    public static RangeInfoDTO getRangeMillisForCurrentWeek(String zoneId, String accountID){
        var rangeInfoDTO = new RangeInfoDTO();
        var startDay = AccountUtil.getStartDayOfWeek(accountID);
        var endDay = (startDay == DayOfWeek.MONDAY) ? DayOfWeek.SUNDAY : DayOfWeek.SATURDAY;
        LocalDateTime currentWeekStartDay = getCurrentStartDayOfWeek(zoneId,startDay);
        rangeInfoDTO.setFromDateEpochMilliseconds(convertDateDetailsIntoMillis(zoneId, DateDetailsDTO.getStartTimeDtoForLocalDateTime(currentWeekStartDay)));
        LocalDateTime currentWeekEndDay = getCurrentEndDayOfWeek(zoneId,endDay);
        rangeInfoDTO.setToDateEpochMilliseconds(convertDateDetailsIntoMillis(zoneId, DateDetailsDTO.getEndTimeDtoForLocalDateTime(currentWeekEndDay)));
        return rangeInfoDTO;
    }

    public static RangeInfoDTO getRangeMillisForLastWeek(String zoneId, String accountID){
        var rangeInfoDTO = new RangeInfoDTO();
        var startDay = AccountUtil.getStartDayOfWeek(accountID);
        var endDay = (startDay == DayOfWeek.MONDAY) ? DayOfWeek.SUNDAY : DayOfWeek.SATURDAY;
        LocalDateTime previousWeekStartDay = getPreviousStartDayOfWeek(zoneId,startDay);
        rangeInfoDTO.setFromDateEpochMilliseconds(convertDateDetailsIntoMillis(zoneId, DateDetailsDTO.getStartTimeDtoForLocalDateTime(previousWeekStartDay)));
        LocalDateTime previousWeekEndDay = getPreviousEndDayOfWeek(zoneId,endDay);
        rangeInfoDTO.setToDateEpochMilliseconds(convertDateDetailsIntoMillis(zoneId, DateDetailsDTO.getEndTimeDtoForLocalDateTime(previousWeekEndDay)));
        return rangeInfoDTO;
    }

    public static RangeInfoDTO getRangeMillisForByDate(String startDate, String endDate, String zoneId){
        var rangeInfoDTO = new RangeInfoDTO();
        rangeInfoDTO.setFromDateEpochMilliseconds(convertDateTimeTextToMillis(startDate, DateFormats.MM_DD_YYYY, zoneId));
        rangeInfoDTO.setToDateEpochMilliseconds(convertDateTimeTextToMillis(endDate +  " 11:59:59 PM", DateFormats.MM_DD_YYYY_HH_MM_SS_A, zoneId) + 999);
        return rangeInfoDTO;
    }

    public static LocalDateTime getNextOrSameDayOfWeek(String timeZone, DayOfWeek day){
        var localDateTime = LocalDateTime.now(ZoneId.of(timeZone));
        return localDateTime.with(TemporalAdjusters.nextOrSame(day)).withMinute(0).withSecond(0).withNano(0);
    }

    public static LocalDateTime getNextDayOfWeek(String timeZone, DayOfWeek day){
        var localDateTime = LocalDateTime.now(ZoneId.of(timeZone));
        return localDateTime.with(TemporalAdjusters.next(day)).withMinute(0).withSecond(0).withNano(0);
    }

    public static LocalDateTime getCurrentEndDayOfWeek(String timeZone, DayOfWeek day){
        var localDateTime = LocalDateTime.now(ZoneId.of(timeZone));
        return localDateTime.with(TemporalAdjusters.nextOrSame(day)).withHour(MAX_HOURS).withMinute(MAX_MINUTES).withSecond(MAX_SECONDS).withNano(MAX_NANO_SEC);
    }

    public static LocalDateTime getCurrentStartDayOfWeek(String timeZone, DayOfWeek day){
        var localDateTime = LocalDateTime.now(ZoneId.of(timeZone));
        return localDateTime.with(TemporalAdjusters.previousOrSame(day)).withHour(0).withMinute(0).withSecond(0).withNano(0);
    }

    public static LocalDateTime getPreviousEndDayOfWeek(String timeZone, DayOfWeek day){
        var currentWeekEndDay = getCurrentEndDayOfWeek(timeZone,day);
        return currentWeekEndDay.with(TemporalAdjusters.previous(day)).withHour(MAX_HOURS).withMinute(MAX_MINUTES).withSecond(MAX_SECONDS).withNano(MAX_NANO_SEC);
    }

    public static LocalDateTime getPreviousStartDayOfWeek(String timeZone, DayOfWeek day){
        var currentWeekStartDay = getCurrentStartDayOfWeek(timeZone,day);
        return currentWeekStartDay.with(TemporalAdjusters.previous(day)).withHour(0).withMinute(0).withSecond(0).withNano(0);
    }

    public static String getTimeFormatAsPerCompany(long noOfMilliSecs, String configuredDisplayTimeFormat) {
        var dispTimeDiffInRequiredFormat = "";
        if (ObjUtils.isNullOrEmpty(configuredDisplayTimeFormat)) {
            configuredDisplayTimeFormat = "HH MM";
        }
        configuredDisplayTimeFormat = configuredDisplayTimeFormat.toLowerCase();
        if (noOfMilliSecs < 0) {
            noOfMilliSecs = 0;
        }

        int hours = (int) (noOfMilliSecs / (60 * 60 * 1000));
        int minutes = (int) (noOfMilliSecs / (60 * 1000));
        minutes = minutes % 60;
        int seconds = ((int) (noOfMilliSecs / 1000)) - ((hours * 60 * 60) + (minutes * 60));

        if (configuredDisplayTimeFormat.indexOf('s') < 0) {
            dispTimeDiffInRequiredFormat = hours + "h " + padZero(minutes) + "m";
        } else {
            dispTimeDiffInRequiredFormat = hours + "h " + padZero(minutes) + "m " + padZero(seconds) + "s";
        }
        return dispTimeDiffInRequiredFormat;
    }

    public static String padZero(int number){
        return number < 10 ? "0"+number : String.valueOf(number);
    }

    public static List<HashMap<Long, HashMap<String,String>>> getDatesList (String timeZoneID,String range,String fromDate,String toDate,String order,String weekStartDayString) {

        ArrayList<HashMap<Long, HashMap<String,String>>> dateList;
        ZoneId zone = ZoneId.of(timeZoneID);
        DayOfWeek weekStartDay = ("mon".equalsIgnoreCase(weekStartDayString) || "monday".equalsIgnoreCase(weekStartDayString)) ? DayOfWeek.MONDAY : DayOfWeek.SUNDAY;

        LocalDate today = LocalDate.now( zone );
        LocalDate recentWeekStartDay = today.with( TemporalAdjusters.previousOrSame( weekStartDay  ) );

        switch(range) {
            case "current_week":
                fromDate = DateTimeFormatter.ofPattern(DateFormats.MM_DD_YYYY.toString()).format(recentWeekStartDay);
                toDate = DateTimeFormatter.ofPattern(DateFormats.MM_DD_YYYY.toString()).format(today);
                break;
            case "last_week":
                LocalDate lastWeekStartDay = recentWeekStartDay.minusDays(7);
                LocalDate lastWeekEndDay =  recentWeekStartDay.minusDays(1);
                fromDate = DateTimeFormatter.ofPattern(DateFormats.MM_DD_YYYY.toString()).format(lastWeekStartDay);
                toDate = DateTimeFormatter.ofPattern(DateFormats.MM_DD_YYYY.toString()).format(lastWeekEndDay);
                break;
            default:
                break;
        }

        dateList = getDatesListForFromAndToDates(fromDate,toDate,zone);
        if("ascending".equals(order)){
            Collections.reverse(dateList);
        }
        return dateList;
    }

    public static long getMillis( String zoneId, int year, int month, int dayOfMonth, int hour, int minute, int second,
                                  int nanoOfSecond  ) throws  DateTimeException {

        zoneId  =   ObjUtils.isNullOrEmpty( zoneId ) ? ZoneId.systemDefault().getId()  : zoneId;

        ZonedDateTime zonedDateTimeCur =   ZonedDateTime.now(ZoneId.of(zoneId));

        year        =  year == 0 ? zonedDateTimeCur.getYear() : year;
        month       =  month == 0 ? zonedDateTimeCur.getMonthValue() : month;
        dayOfMonth  =  dayOfMonth == 0 ? zonedDateTimeCur.getDayOfMonth() : dayOfMonth;

        ZonedDateTime zonedDateTime = ZonedDateTime.of(year, month, dayOfMonth, hour, minute, second, nanoOfSecond, ZoneId.of(zoneId) );

        return  (nanoOfSecond != 0  ) ? zonedDateTime.toInstant().toEpochMilli() + nanoOfSecond  : zonedDateTime.toInstant().toEpochMilli();
    }

    private static ArrayList getDatesListForFromAndToDates(String fromDate, String toDate,ZoneId zone){

        ArrayList<HashMap<String,Object>> dateList = new ArrayList();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MM/dd/yyyy HH:mm");
        LocalDateTime localtDateAndTimeTo = LocalDateTime.parse(toDate + " 00:00", formatter);
        ZonedDateTime zonedDateTimeTo = ZonedDateTime.of(localtDateAndTimeTo, zone );
        LocalDateTime localtDateAndTimeFrom = LocalDateTime.parse(fromDate + " 00:00", formatter);
        ZonedDateTime zonedDateTimeFrom = ZonedDateTime.of(localtDateAndTimeFrom, zone );
        while(true){
            if((DateTimeFormatter.ofPattern(DateFormats.MM_DD_YYYY.toString()).format(zonedDateTimeTo)).equals(DateTimeFormatter.ofPattern(DateFormats.MM_DD_YYYY.toString()).format(zonedDateTimeFrom.minusDays(1)))){
                break;
            }
            dateList.add(getDateDetailsFromZonedDateTime(zonedDateTimeTo));
            zonedDateTimeTo = zonedDateTimeTo.minusDays(1);
        }
        return dateList;
    }

    private static HashMap<String,Object> getDateDetailsFromZonedDateTime (ZonedDateTime zonedDateTime){
        HashMap<String,Object> dateDetailsMap = new HashMap();
        if(!ObjUtils.isNull(zonedDateTime)){
            dateDetailsMap.put("day",zonedDateTime.getDayOfMonth() < 10 ? "0"+zonedDateTime.getDayOfMonth() : ""+zonedDateTime.getDayOfMonth());
            dateDetailsMap.put("month",zonedDateTime.getMonth().toString());
            dateDetailsMap.put("monthValue", zonedDateTime.getMonthValue() < 10 ? "0"+zonedDateTime.getMonthValue() : ""+zonedDateTime.getMonthValue());
            dateDetailsMap.put("year",zonedDateTime.getYear()+"");
            dateDetailsMap.put("dayOfWeek",zonedDateTime.getDayOfWeek().toString());
            dateDetailsMap.put("milliSeconds",zonedDateTime.toInstant().toEpochMilli());
        }
        return dateDetailsMap;
    }

    public static String getCurrentYear(){
        return Integer.toString(YearMonth.now().getYear());
    }

    public static boolean isCurrentDay ( long millis, String zoneID ){
        return !ObjUtils.isNullOrEmpty(zoneID) && isCurrentDay(millis,getCurrentDayStartMillis(zoneID));
    }

    public static long getCurrentDayStartMillis(String timezone){
        if(ObjUtils.isNullOrEmpty(timezone)){
            return 0L;
        }
        return getMillis( timezone, 0, 0, 0, 0, 0, 0, 0);
    }

    public static boolean isCurrentDay ( long millisToCheck, long currentDayStartMillis ){
        return millisToCheck >= currentDayStartMillis;
    }

    public static String getDurationDisplayText(Long actualDifference, Long editedDifference){
        Long finalDifference = editedDifference - actualDifference;
        String differenceSign = finalDifference < 0 ? "(-)" : "(+)" ;

        finalDifference = Math.abs(finalDifference);

        long minutes = Math.round((finalDifference) / (1000f * 60));
        long hours = minutes / 60;
        minutes = minutes - (hours * 60);

        String differenceText = (hours > 0) ? hours + " hours and " : "";
        differenceText += minutes <= 0 ? "00 minutes " + differenceSign : padZero((int)minutes) + " minutes " + differenceSign;

        return differenceText;
    }
}
