package com.yoco.zone.service;

import com.yoco.MockPRO;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.FCMService;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.*;
import com.yoco.user.helper.skill.UserSkillFullMetricHelper;
import com.yoco.zone.enums.ZONE_ERROR_MESSAGE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.util.Assert;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.sql.Time;
import java.util.*;

import static org.mockito.ArgumentMatchers.*;

class ZoneServiceTest {

    @Test
    void getAutoDetectedZoneSevice_empty_offset_test() {
        try {
            new ZoneService().getAutoDetectedZoneSevice("");
        } catch (Exception e) {
            Assertions.assertEquals(ZONE_ERROR_MESSAGE.INVALID_SYSTEM_OFFSET.value(), e.getMessage());
        }
    }

    @Test
    void getAutoDetectedZoneSevice_null_offset_test() {
        try {
            new ZoneService().getAutoDetectedZoneSevice(null);
        } catch (Exception e) {
            Assertions.assertEquals(ZONE_ERROR_MESSAGE.INVALID_SYSTEM_OFFSET.value(), e.getMessage());
        }
    }

    @Test
    void getAutoDetectedZoneSevice_passing_negative_offset_test() {
        String timeZone =  new ZoneService().getAutoDetectedZoneSevice("-420");
        Assertions.assertTrue(!ObjUtils.isNullOrEmpty(timeZone));
    }

    @Test
    void getAutoDetectedZoneSevice_passing_positive_offset_test() {
        String timeZone =  new ZoneService().getAutoDetectedZoneSevice("330");
        Assertions.assertTrue(!ObjUtils.isNullOrEmpty(timeZone));
    }

    @Test
    void getDates_empty_timezone_test() {
        try {
            new ZoneService().getDates("", "current_week", "", "", "", "");
        } catch (Exception e) {
            Assertions.assertEquals(TimeZoneUtil.INVALID_TIME_ZONE, e.getMessage());
        }
    }

    @Test
    void getDates_null_timezone_test() {
        try {
            new ZoneService().getDates(null, "current_week", "", "", "", "");
        } catch (Exception e) {
            Assertions.assertEquals(TimeZoneUtil.INVALID_TIME_ZONE, e.getMessage());
        }
    }

    @Test
    void getDates_empty_range_test() {
        try {
            new ZoneService().getDates("Asia/Kolkata", "", "", "", "", "");
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_RANGE.value(), e.getMessage());
        }
    }

    @Test
    void getDates_null_range_test() {
        try {
            new ZoneService().getDates("Asia/Kolkata", null, "", "", "", "");
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_RANGE.value(), e.getMessage());
        }
    }

    @Test
    void getDates_empty_list_return_test() {
        Map<String, Object> mockMap = new HashMap<>();
        mockMap.put("success",false);
        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)) {
            dateUtilMockedStatic.when(()->DateUtil.getDatesList(anyString(), anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(new ArrayList<>());
            Map<String, Object> resp = new ZoneService().getDates("Asia/Kolkata", "current_week", "", ", ", "","");
            Assertions.assertEquals(resp, mockMap);
        }
    }

    @Test
    void getDates_valid_list_return_test() {

        ArrayList respList = new ArrayList();
        respList.add("1647455400000~2022-03-17");

        Map<String, Object> mockMap = new HashMap<>();
        mockMap.put("success",true);
        mockMap.put("datesList",respList);

        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)) {
            dateUtilMockedStatic.when(()->DateUtil.getDatesList(anyString(), anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(respList);
            Map<String, Object> resp = new ZoneService().getDates("Asia/Kolkata", "current_week", "", ", ", "","");
            Assertions.assertEquals(resp, mockMap);
        }
    }

    @Test
    void updateTimeZone_empty_accountID_test() {
        try {
            new ZoneService().updateTimezone("", "contactID", "payload", "srcClient");
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(), e.getMessage());
        }
    }

    @Test
    void updateTimeZone_null_accountID_test() {
        try {
            new ZoneService().updateTimezone(null, "contactID", "payload", "srcClient");
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(), e.getMessage());
        }
    }

    @Test
    void updateTimeZone_empty_contactID_test() {
        try {
            new ZoneService().updateTimezone("accountID", "", "payload", "srcClient");
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value(), e.getMessage());
        }
    }

    @Test
    void updateTimeZone_null_contactID_test() {
        try {
            new ZoneService().updateTimezone("accountID", null, "payload", "srcClient");
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value(), e.getMessage());
        }
    }

    @Test
    void updateTimeZone_empty_payload_test() {
        try {
            new ZoneService().updateTimezone("accountID", "contact", "", "srcClient");
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(), e.getMessage());
        }
    }

    @Test
    void updateTimeZone_null_payload_test() {
        try {
            new ZoneService().updateTimezone("accountID", "contactId", null, "srcClient");
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(), e.getMessage());
        }
    }

    @Test
    void updateTimeZone_missing_timezoneID_key_test() {
        try {
            HashMap payload = new HashMap();
            new ZoneService().updateTimezone("accountID", "contactId", JsonUtil.getJson(payload), "srcClient");
        } catch (Exception e) {
            Assertions.assertEquals(TimeZoneUtil.INVALID_TIME_ZONE, e.getMessage());
        }
    }

    @Test
    void updateTimeZone_empty_timezoneID_test() throws IOException, NoSuchAlgorithmException {

        HashMap payload = new HashMap();
        payload.put("timezoneID", "");
        Map<String, Object> resp = new ZoneService().updateTimezone("accountID", "contactId", JsonUtil.getJson(payload), "srcClient");
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
    }

    @Test
    void updateTimeZone_valid_test() throws IOException, NoSuchAlgorithmException {

        HashMap payload = new HashMap();
        payload.put("timezoneID", "Asia/Kolkata");

        try(MockedConstruction<UserImpl> mock = Mockito.mockConstruction(UserImpl.class, (userImplMocked, context)->{
            Mockito.when(userImplMocked.getUserWithoutContact(anyString(), anyString())).thenReturn(MockPRO.getMockPRO());});
            MockedStatic<TimeZoneUtil> timeZoneUtilMockedStatic = Mockito.mockStatic(TimeZoneUtil.class);
            MockedConstruction<FCMService> mock1 = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context)-> {
               Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(), any(Set.class), anyString(), anyBoolean(), any(Object.class), anyString());});
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<DateUtil> dateUtil = Mockito.mockStatic(DateUtil.class);

        ) {

            timeZoneUtilMockedStatic.when(() -> TimeZoneUtil.getZoneInFormat(anyString(), anyString(), any(Locale.class))).thenReturn("(GMT+05:30) Asia/Kolkata (India Time)");
            dateUtil.when(()->DateUtil.getMillis(anyString(), anyInt(), anyInt(), anyInt(), anyInt(), anyInt(), anyInt(), anyInt())).thenReturn(System.currentTimeMillis());
            Map<String, Object> resp = new ZoneService().updateTimezone("accountID", "contactId", JsonUtil.getJson(payload), "srcClient");

            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));
        }
    }
}
