package com.yoco.commons.modal.date;

import com.yoco.commons.utils.DateUtil;
import lombok.Data;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;

@Data
public class DateDetailsDTO {
    private int nanoSeconds;
    private int seconds;
    private int minutes;
    private int hours;
    private int dayOfMonth;
    private int month;
    private int year;

    public DateDetailsDTO(int hours, int minutes, int seconds, int nanoSeconds, int dayOfMonth, int month, int year){
        this.hours = hours;
        this.minutes = minutes;
        this.seconds = seconds;
        this.dayOfMonth = dayOfMonth;
        this.month = month;
        this.year = year;
        this.nanoSeconds = nanoSeconds;
    }

    public static DateDetailsDTO getStartTimeDtoForDate(ZonedDateTime day){
        return new DateDetailsDTO(0,0,0,0,day.getDayOfMonth(), day.getMonthValue(), day.getYear());
    }

    public static DateDetailsDTO getEndTimeDtoForDate(ZonedDateTime day){
        return new DateDetailsDTO(DateUtil.MAX_HOURS,DateUtil.MAX_MINUTES,DateUtil.MAX_SECONDS,DateUtil.MAX_NANO_SEC,day.getDayOfMonth(), day.getMonthValue(), day.getYear());
    }

    public static DateDetailsDTO getStartTimeDtoForDate(LocalDate day){
        return new DateDetailsDTO(0,0,0,0,day.getDayOfMonth(), day.getMonthValue(), day.getYear());
    }

    public static DateDetailsDTO getStartTimeDtoForLocalDateTime(LocalDateTime day){
        return new DateDetailsDTO(0,0,0,0,day.getDayOfMonth(), day.getMonthValue(), day.getYear());
    }

    public static DateDetailsDTO getEndTimeDtoForLocalDateTime(LocalDateTime day){
        return new DateDetailsDTO(DateUtil.MAX_HOURS,DateUtil.MAX_MINUTES,DateUtil.MAX_SECONDS,DateUtil.MAX_NANO_SEC,day.getDayOfMonth(), day.getMonthValue(), day.getYear());
    }

    public static DateDetailsDTO getStartTimeDtoForCurrentDay(String zoneID){
        return getStartTimeDtoForDate(DateUtil.getZonedDateTime(zoneID));
    }

    public static DateDetailsDTO getEndTimeDtoForCurrentDay(String zoneID){
        return getEndTimeDtoForDate(DateUtil.getZonedDateTime(zoneID));
    }
}
