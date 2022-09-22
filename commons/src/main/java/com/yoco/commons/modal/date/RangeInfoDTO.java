package com.yoco.commons.modal.date;

import lombok.Data;
import java.io.Serializable;

@Data
public class RangeInfoDTO implements Serializable {
    private String fromDate;
    private String toDate;
    private long fromDateEpochMilliseconds;
    private long toDateEpochMilliseconds;
    private String timezone;

    public RangeInfoDTO(String fromDate, String toDate, long fromDateEpochMilliseconds, long toDateEpochMilliseconds, String timezone){
        this.fromDate = fromDate;
        this.toDate = toDate;
        this.fromDateEpochMilliseconds = fromDateEpochMilliseconds;
        this.toDateEpochMilliseconds = toDateEpochMilliseconds;
        this.timezone = timezone;
    }

    public RangeInfoDTO(){}
}
