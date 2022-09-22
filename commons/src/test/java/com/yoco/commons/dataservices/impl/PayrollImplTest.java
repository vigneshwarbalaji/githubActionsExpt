package com.yoco.commons.dataservices.impl;

import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.Mockito.times;

class PayrollImplTest {

    PayrollImpl payroll = PayrollImpl.getInstance();

    @Test
    void getPayrollEvents_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put("data",Map.of("events", List.of()));

            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(mockResp);
            Map<String,Object> resp =  payroll.getPayrollEvents("accountID", "payrollStatus","from","to","", 1000);
            Assertions.assertEquals(resp,mockResp);

            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.BRAND, DcmConstants.YOCO_BRAND_ID);
            queryMap.put(SchedulingKeys.PAYMENT_STATUS, "payrollStatus");
            queryMap.put(SchedulingKeys.START_DATE_TIME, "from");
            queryMap.put(SchedulingKeys.END_DATE_TIME, "to");
            queryMap.put(SchedulingKeys.LIMIT, 1000);

            schedulingEngineUtilMockedStatic.verify(()->SchedulingEngineUtil.getEventByQueryString(queryMap),times(1));
        }
    }


    @Test
    void getPayrollEvents_cursor_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put("data",Map.of("events", List.of()));

            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(mockResp);
            Map<String,Object> resp =  payroll.getPayrollEvents("accountID", "payrollStatus","from","to","cursor", 1000);
            Assertions.assertEquals(resp,mockResp);

            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.BRAND, DcmConstants.YOCO_BRAND_ID);
            queryMap.put(SchedulingKeys.PAYMENT_STATUS, "payrollStatus");
            queryMap.put(SchedulingKeys.START_DATE_TIME, "from");
            queryMap.put(SchedulingKeys.END_DATE_TIME, "to");
            queryMap.put(SchedulingKeys.LIMIT, 1000);
            queryMap.put(SchedulingKeys.CURSOR_STR,"cursor");

            schedulingEngineUtilMockedStatic.verify(()->SchedulingEngineUtil.getEventByQueryString(queryMap),times(1));
        }
    }

    @Test
    void getPayrollEvents_withoutLimit_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put("data",Map.of("events", List.of()));

            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(mockResp);
            Map<String,Object> resp =  payroll.getPayrollEvents("accountID", "payrollStatus","from","to","cursor", 0);
            Assertions.assertEquals(resp,mockResp);

            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.BRAND, DcmConstants.YOCO_BRAND_ID);
            queryMap.put(SchedulingKeys.PAYMENT_STATUS, "payrollStatus");
            queryMap.put(SchedulingKeys.START_DATE_TIME, "from");
            queryMap.put(SchedulingKeys.END_DATE_TIME, "to");
            queryMap.put(SchedulingKeys.CURSOR_STR,"cursor");

            schedulingEngineUtilMockedStatic.verify(()->SchedulingEngineUtil.getEventByQueryString(queryMap),times(1));
        }
    }

    @Test
    void getPayrollEventsBasedOnStatus_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put("data",Map.of("events", List.of()));

            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(mockResp);
            Map<String,Object> resp =  payroll.getPayrollEventsBasedOnStatus("accountID","status", "payrollStatus","from","to");
            Assertions.assertEquals(resp,mockResp);

            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.BRAND, DcmConstants.YOCO_BRAND_ID);
            queryMap.put(SchedulingKeys.PAYMENT_STATUS, "payrollStatus");
            queryMap.put(SchedulingKeys.STATUS, "status");
            queryMap.put(SchedulingKeys.START_DATE_TIME, "from");
            queryMap.put(SchedulingKeys.END_DATE_TIME, "to");

            schedulingEngineUtilMockedStatic.verify(()->SchedulingEngineUtil.getEventByQueryString(queryMap),times(1));
        }
    }

}