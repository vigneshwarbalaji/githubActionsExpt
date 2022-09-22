package com.yoco.downloads.payroll.helper;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PayrollDownloadHelper {

    private PayrollDownloadHelper(){}

    public static Map<String, Object> getPayrollEvents(Map<String,Object> payrollEventsResp, String zone, String status){

        Map<String,Object> response = new HashMap<>();
        List<Map<String, Object>> events = new ArrayList<>();

        Map<String, Object> schRespData = (HashMap<String, Object>) payrollEventsResp.get(SchedulingKeys.DATA);

        if(!ObjUtils.isNullOrEmpty(schRespData)) {

            List<Map<String, Object>> payrollEventsList = (ArrayList) schRespData.get(SchedulingKeys.EVENTS);

            if (!ObjUtils.isNullOrEmpty(payrollEventsList)) {

                events = arrangePayrollEvents(zone,payrollEventsList);

                if (schRespData.containsKey(Commons.CURSOR)) {
                    response.put(Commons.CURSOR, schRespData.get(Commons.CURSOR));
                }
            }
        }
        response.put(SchedulingKeys.ENTRIES,events);
        return response;
    }

    private static List<Map<String,Object>> arrangePayrollEvents(String zone,List<Map<String, Object>> payrollEventsList) {

        List<Map<String,Object>> filteredPayrollEvents = new ArrayList<>();

        payrollEventsList.stream().forEach(event -> {

            String startDateTimeText = DateUtil.convertMillisToDateTimeText(DateFormats.YYYY_MM_DD_T_HH_MM_SS_A_Z,
                    (long)event.get(SchedulingKeys.START_TIME), zone);

            List<String> contactIdList = (ArrayList<String>) event.get(SchedulingKeys.PROVIDER);

            filteredPayrollEvents.add(Map.of("contactID",contactIdList.get(0),"start",startDateTimeText));

        });
        return filteredPayrollEvents;
    }

}
