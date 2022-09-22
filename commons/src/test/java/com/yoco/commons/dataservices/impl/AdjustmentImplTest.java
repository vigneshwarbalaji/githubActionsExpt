package com.yoco.commons.dataservices.impl;

import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.Mockito.times;

class AdjustmentImplTest {

    AdjustmentImpl adjustmentImpl = AdjustmentImpl.getAdjustmentImplInstance();

    @Test
    void getAllAdjustments_IfStatusAndContactIDIsEmpty_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.STATUS, "");
            queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);
            String status = "";
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("324");
            userPro.setContactId("322");
            List<Map<String,Object>> entriesList = new ArrayList<>();
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(new HashMap(){{
                put("data",new HashMap(){{
                    put("events",entriesList);
                }});
            }});
            Map<String,Object> eventListMap = new HashMap<>();
            Map<String,Object> actual = new AdjustmentImpl().getAllAdjustments("accountID", "", "11/12/2021", "12/12/2021", status, "");
            Assertions.assertEquals(eventListMap,actual);
        }
    }

    @Test
    void getAllAdjustments_IfStatusIsRejected_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{"contactID"});
            queryMap.put(SchedulingKeys.STATUS, "REJECTED");
            queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);
            String status = "REJECTED";
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("214");
            userPro.setContactId("212");
            List<Map<String,Object>> entriesList = new ArrayList<>();
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(new HashMap(){{
                put("data",new HashMap(){{
                    put("events",entriesList);
                }});
            }});
            Map<String,Object> eventListMap = new HashMap<>();
            Map<String,Object> actual = new AdjustmentImpl().getAllAdjustments("accountID", "contactID", "11/12/2021", "12/12/2021", status, "");
            Assertions.assertEquals(actual,eventListMap);
        }
    }

    @Test
    void getAllAdjustments_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            HashMap<String, Object> queryMap = new HashMap<>();
            queryMap.put(SchedulingKeys.MERCHANT_ID, "accountID");
            queryMap.put(SchedulingKeys.REFERRERS_KEY, new String[]{"contactID"});
            queryMap.put(SchedulingKeys.STATUS, "APPROVED");
            queryMap.put(SchedulingKeys.TYPE,SchedulingKeys.EVENT);
            String status = "APPROVED";
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("123");
            userPro.setContactId("123");
            List<Map<String,Object>> entriesList = new ArrayList<>();
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(queryMap)).thenReturn(new HashMap(){{
                put("data",new HashMap(){{
                    put("events",entriesList);
                }});
            }});
            Map<String,Object> eventListMap = new HashMap<>();
            Map<String,Object> actual = new AdjustmentImpl().getAllAdjustments("accountID", "contactID", "11/12/2021", "12/12/2021", status, "");
            Assertions.assertEquals(eventListMap, actual);
        }
    }

    @Test
    void getOverLapEntries_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put("data",Map.of("events",List.of()));
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.getEventByQueryString(anyMap())).thenReturn(mockResp);
            List<String> calendarIds = List.of("id1","id2");
            Assertions.assertEquals(mockResp,adjustmentImpl.getOverLapEntries("accId",calendarIds,"contactId","11/12/2021","12/12/2021","entry1"));
            schedulingEngineUtilMockedStatic.verify(()->SchedulingEngineUtil.getEventByQueryString(anyMap()),times(1));
        }
    }

}
