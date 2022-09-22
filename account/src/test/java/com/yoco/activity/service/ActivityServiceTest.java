package com.yoco.activity.service;

import com.yoco.MockPRO;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.UserClockinSubStatusJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.TimeZoneUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.HashMap;

import static org.mockito.ArgumentMatchers.*;

class ActivityServiceTest {

    @Test
    void createActivity_empty_accountID_test() {
        try {
            new ActivityService().createActivity("", new Contact(), "payload");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void createActivity_null_accountID_test() {
        try {
            new ActivityService().createActivity(null, new Contact(), "payload");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void createActivity_null_Contact_test() {
        try {
            new ActivityService().createActivity("accountID", null, "payload");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void createActivity_empty_payload_test() {
        try {
            new ActivityService().createActivity("accountID", new Contact(), "");
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void createActivity_null_payload_test() {
        try {
            new ActivityService().createActivity("accountID", new Contact(), null);
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void createActivity_activity_missing_test() {
        try {
            Contact objContact = new Contact();
            objContact.setId("1234");

            HashMap<String, Object> payload = new HashMap<>();

            new ActivityService().createActivity("accountID", objContact, JsonUtil.getJson(payload));
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void createActivity_activity_empty_test() {
        try {
            Contact objContact = new Contact();
            objContact.setId("1234");

            HashMap<String, Object> payload = new HashMap<>();
            payload.put("activity", "");

            new ActivityService().createActivity("accountID", objContact, JsonUtil.getJson(payload));
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void createActivity_activity_null_test() {
        try {
            Contact objContact = new Contact();
            objContact.setId("1234");

            HashMap<String, Object> payload = new HashMap<>();
            payload.put("activity", null);

            new ActivityService().createActivity("accountID", objContact, JsonUtil.getJson(payload));
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void createActivity_timeZoneID_missing_test() {
        try {
            Contact objContact = new Contact();
            objContact.setId("1234");

            HashMap<String, Object> payload = new HashMap<>();
            payload.put("activity", "activity");
            payload.put("activityType", "activity");

            new ActivityService().createActivity("accountID", objContact, JsonUtil.getJson(payload));
        }catch(Exception e ){
            Assertions.assertEquals(TimeZoneUtil.INVALID_TIME_ZONE,e.getMessage());
        }
    }

    @Test
    void createActivity_timeZoneID_empty_test() {
        try {
            Contact objContact = new Contact();
            objContact.setId("1234");

            HashMap<String, Object> payload = new HashMap<>();
            payload.put("activity", "activity");
            payload.put("activityType", "activity");
            payload.put("timeZoneID", "");

            new ActivityService().createActivity("accountID", objContact, JsonUtil.getJson(payload));
        }catch(Exception e ){
            Assertions.assertEquals(TimeZoneUtil.INVALID_TIME_ZONE,e.getMessage());
        }
    }

    @Test
    void createActivity_timeZoneID_null_test() {
        try {
            Contact objContact = new Contact();
            objContact.setId("1234");

            HashMap<String, Object> payload = new HashMap<>();
            payload.put("activity", "activity");
            payload.put("activityType", "activity");
            payload.put("timeZoneID", null);

            new ActivityService().createActivity("accountID", objContact, JsonUtil.getJson(payload));
        }catch(Exception e ){
            Assertions.assertEquals(TimeZoneUtil.INVALID_TIME_ZONE,e.getMessage());
        }
    }

    @Test
    void createActivity_success_test () {

        UserClockinSubStatusJDO mockRespUserClockInJDO = new UserClockinSubStatusJDO();
        mockRespUserClockInJDO.setEventLongTime(1648810995884l);

        try (MockedStatic<ActivityUtil> mockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedConstruction<UserImpl> objMock = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)-> {
                 Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(MockPRO.getMockPRO());
             });
          ) {

        mockedStatic.when(()->ActivityUtil.saveActivity(any(UserClockinSubStatusJDO.class), anyLong())).thenReturn(mockRespUserClockInJDO);

        Contact objContact = new Contact();
            objContact.setId("1234");

            HashMap<String, Object> payload = new HashMap<>();
            payload.put("activity", "activity");
            payload.put("activityType", "activity");
            payload.put("timeZoneID", "Asia/Kolkata");
            payload.put("eventDate","01-Apr-2022 04:24:47 PM");

            HashMap<String, Object> resp = (HashMap<String, Object>) new ActivityService().createActivity("accountID", objContact, JsonUtil.getJson(payload));

            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));
            Assertions.assertTrue(resp.containsKey("activity"));
        }
    }

}
