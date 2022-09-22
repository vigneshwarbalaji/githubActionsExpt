package com.yoco.downloads.payroll.helper;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.constants.SchedulingKeys;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class PayrollDownloadHelperTest {

    @Test
    void getPayrollEvents_no_data_test(){
        Map<String,Object> response = PayrollDownloadHelper.getPayrollEvents(Map.of(),"zone","payrollStatus");
        Assertions.assertEquals(Map.of(SchedulingKeys.ENTRIES,new ArrayList<>()),response);
    }

    @Test
    void getPayrollEvents_no_events_test(){
        Map<String,Object> eventsMap = new HashMap<>();
        eventsMap.put(SchedulingKeys.EVENTS,new ArrayList<>());
        Map<String,Object> dataMap = new HashMap<>();
        dataMap.put(SchedulingKeys.DATA,eventsMap);
        Map<String,Object> response = PayrollDownloadHelper.getPayrollEvents(dataMap,"zone","payrollStatus");
        Assertions.assertEquals(Map.of(SchedulingKeys.ENTRIES,new ArrayList<>()),response);
    }

    @Test
    void getPayrollEvents_valid_test(){
        Map<String,Object> event1 = new HashMap<>();
        event1.put(SchedulingKeys.PAYMENT_STATUS,InternalUsage.DEFAULT_PAYROLL);
        event1.put(SchedulingKeys.START_TIME,1656651883000l);
        event1.put(SchedulingKeys.PROVIDER,new ArrayList<>(){{add("contId");}});
        List<Map<String,Object>> eventsList = new ArrayList<>();
        eventsList.add(event1);
        Map<String,Object> eventsMap = new HashMap<>();
        eventsMap.put(SchedulingKeys.EVENTS,eventsList);
        Map<String,Object> dataMap = new HashMap<>();
        dataMap.put(SchedulingKeys.DATA,eventsMap);
        Map<String,Object> response = PayrollDownloadHelper.getPayrollEvents(dataMap,"Asia/Kolkata", InternalUsage.DEFAULT_PAYROLL);
        Assertions.assertEquals(Map.of("entries",List.of(Map.of("contactID","contId","start","2022-07-01T10:34:43 AM+0530"))),response);
    }

    @Test
    void getPayrollEvents_valid_cursor_test(){
        Map<String,Object> event1 = new HashMap<>();
        event1.put(SchedulingKeys.PAYMENT_STATUS,InternalUsage.DEFAULT_PAYROLL);
        event1.put(SchedulingKeys.START_TIME,1656651883000l);
        event1.put(SchedulingKeys.PROVIDER,new ArrayList<>(){{add("contId");}});
        Map<String,Object> event2 = new HashMap<>();
        event2.put(SchedulingKeys.PAYMENT_STATUS,InternalUsage.DEFAULT_PAYROLL);
        event2.put(SchedulingKeys.START_TIME,1656651883000l);
        event2.put(SchedulingKeys.PROVIDER,new ArrayList<>(){{add("contId2");}});
        List<Map<String,Object>> eventsList = new ArrayList<>();
        eventsList.add(event1);
        eventsList.add(event2);
        Map<String,Object> eventsMap = new HashMap<>();
        eventsMap.put(SchedulingKeys.EVENTS,eventsList);
        eventsMap.put(Commons.CURSOR,"cursor");
        Map<String,Object> dataMap = new HashMap<>();
        dataMap.put(SchedulingKeys.DATA,eventsMap);
        Map<String,Object> response = PayrollDownloadHelper.getPayrollEvents(dataMap,"Asia/Kolkata", InternalUsage.DEFAULT_PAYROLL);
        Assertions.assertEquals(Map.of("cursor","cursor","entries",List.of(Map.of("contactID","contId","start","2022-07-01T10:34:43 AM+0530"),Map.of("contactID","contId2", "start", "2022-07-01T10:34:43 AM+0530"))),response);
    }

}