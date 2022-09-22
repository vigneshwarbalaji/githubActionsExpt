package com.yoco.integration.modal;

import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.utils.DateUtil;
import lombok.Data;
import java.io.Serializable;
import java.util.Map;

@Data
public class ZendeskEntry implements Serializable {
    private String inTime;
    private String inDate;
    private String outTime;
    private String outDate;
    private Long duration;

    public ZendeskEntry(){}

    public ZendeskEntry(Map<String,Object> eventMap, String timezone){
        Long inTimeMillis =   (Long)eventMap.get(SchedulingKeys.START_TIME);
        Long outTimeMillis =  (Long) eventMap.get(SchedulingKeys.END_TIME);
        this.inDate = DateUtil.convertMillisToDateTimeText(DateFormats.E_MMM_DD_YYYY ,inTimeMillis,timezone);
        this.outDate = DateUtil.convertMillisToDateTimeText(DateFormats.E_MMM_DD_YYYY ,outTimeMillis,timezone);
        this.inTime = DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A ,inTimeMillis,timezone);
        this.outTime = DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A ,outTimeMillis,timezone);
        this.duration = outTimeMillis - inTimeMillis;
    }
}
