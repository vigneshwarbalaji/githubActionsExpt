package com.yoco.commons.utils;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.services.FCMService;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.events.ClockMetricUtil;
import com.yoco.commons.utils.events.ClockPublishUtil;
import com.yoco.commons.utils.events.ClockTaskUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;

class ClockTaskUtilTest {

    @Test
    void initiateClockOperationsTaskQueue_valid_test() throws IOException {

        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){

            Map<String, Object>clockMap = new HashMap<>();
            clockMap.put("clockoperation", "new ClockOperation");

            ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
            ObjectOutputStream out = new ObjectOutputStream(byteOut);
            out.writeObject(clockMap);

            ClockTaskUtil.getClockTaskUtil().initiateClockOperationsTaskQueue(clockMap);

            taskCreatorMockedStatic.verify(()->TaskCreator.createPostTask("clockoperation", CommonAppProperties.getAppUrl() + "/task/common/clockTaskHandler", byteOut.toByteArray()));
        }
    }

    @Test
    void clockOperationsTaskHandler_forceClockOut_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ClockPublishUtil> clockPublishUtilMockedStatic = Mockito.mockStatic(ClockPublishUtil.class);
            MockedStatic<ClockMetricUtil> clockMetricUtilMockedStatic = Mockito.mockStatic(ClockMetricUtil.class);
            MockedConstruction<FCMService>mock = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context) -> {
                Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            })) {

            Contact contact = new Contact("123", "new@mailId.com", "firstName", "lastName", "222e", 23454554422323L);

            PeopleRelationJDO userObject = new PeopleRelationJDO();
            userObject.setUniquepin("12121");
            userObject.setContactId("231");
            userObject.setContact(contact);
            userObject.setDefault(false);

            SettingsJDO accountObject = new SettingsJDO();
            accountObject.setDisplayTimeFormat("HH:MM:SS");

            var reportObject = new ReportsDTO();
            reportObject.setId("123");
            reportObject.setContactID("3242");
            reportObject.setAccountID("876");

            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put("action", "force");
            payloadMap.put("source", "force");
            payloadMap.put("sourceClientID", "clientID");
            payloadMap.put("outSourceActivity", "activity");
            payloadMap.put("receivedLongTime", 12345345334L);
            payloadMap.put("userPRO", userObject);
            payloadMap.put("settingsJDO", accountObject);
            payloadMap.put("clockOutObject", reportObject);
            ClockTaskUtil.getClockTaskUtil().clockOperationsTaskHandler(payloadMap);

            rtmServiceMockedStatic.verifyNoInteractions();

        }
    }

    @Test
    void clockOperationsTaskHandler_valid_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ClockPublishUtil> clockPublishUtilMockedStatic = Mockito.mockStatic(ClockPublishUtil.class);
            MockedStatic<ClockMetricUtil> clockMetricUtilMockedStatic = Mockito.mockStatic(ClockMetricUtil.class);
            MockedConstruction<FCMService>mock = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context) -> {
                Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            })) {

            Contact contact = new Contact("123", "new@mailId.com", "firstName", "lastName", "222e", 23454554422323L);

            PeopleRelationJDO userObject = new PeopleRelationJDO();
            userObject.setUniquepin("12121");
            userObject.setContactId("231");
            userObject.setContact(contact);
            userObject.setDefault(false);

            SettingsJDO accountObject = new SettingsJDO();
            accountObject.setDisplayTimeFormat("HH:MM:SS");

            var reportObject = new ReportsDTO();
            reportObject.setId("123");
            reportObject.setContactID("3242");
            reportObject.setAccountID("876");

            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put("action", "clockOut");
            payloadMap.put("source", "force");
            payloadMap.put("sourceClientID", "clientID");
            payloadMap.put("outSourceActivity", "activity");
            payloadMap.put("receivedLongTime", 12345345334L);
            payloadMap.put("userPRO", userObject);
            payloadMap.put("settingsJDO", accountObject);
            payloadMap.put("clockOutObject", reportObject);
            ClockTaskUtil.getClockTaskUtil().clockOperationsTaskHandler(payloadMap);

            rtmServiceMockedStatic.verifyNoInteractions();

        }
    }

    @Test
    void clockOutTaskHandler_exception_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ClockPublishUtil> clockPublishUtilMockedStatic = Mockito.mockStatic(ClockPublishUtil.class);
            MockedStatic<ClockMetricUtil> clockMetricUtilMockedStatic = Mockito.mockStatic(ClockMetricUtil.class);
            MockedConstruction<FCMService>mock = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context) -> {
                Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            })){

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),anyString(),any()))
                    .thenThrow(new IOException("InvalidData"));

            Contact contact = new Contact("123", "new@mailId.com", "firstName", "lastName", "222e", 23454554422323L);

            PeopleRelationJDO userObject = new PeopleRelationJDO();
            userObject.setUniquepin("12121");
            userObject.setContactId("231");
            userObject.setContact(contact);
            userObject.setDefault(true);

            SettingsJDO accountObject = new SettingsJDO();
            accountObject.setDisplayTimeFormat("HH:MM:SS");

            var reportObject = new ReportsDTO();
            reportObject.setId("123");
            reportObject.setContactID("3242");
            reportObject.setAccountID("876");

            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put("source", "force");
            payloadMap.put("sourceClientID","clientID");
            payloadMap.put("outSourceActivity", "activity");
            payloadMap.put("receivedLongTime",12345345334L);
            payloadMap.put("userPRO", userObject);
            payloadMap.put("settingsJDO", accountObject);
            payloadMap.put("clockOutObject", reportObject);

            clockPublishUtilMockedStatic.when(() -> ClockPublishUtil.publishClockToChannel(reportObject, userObject, "activity", 12345345334L))
                    .thenAnswer((Answer<Void>) invocation -> null);

            ClockTaskUtil.getClockTaskUtil().clockOutTaskHandler(payloadMap);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToAW(userObject.getUniquepin(), userObject.getContactId(),"FORCE_CLOCKOUT",
                    "entry",reportObject));

        }catch(Exception e){
            Assertions.assertEquals("InvalidData", e.getMessage());
        }

    }

    @Test
    void clockOutTaskHandler_valid_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ClockPublishUtil> clockPublishUtilMockedStatic = Mockito.mockStatic(ClockPublishUtil.class);
            MockedStatic<ClockMetricUtil> clockMetricUtilMockedStatic = Mockito.mockStatic(ClockMetricUtil.class);
            MockedConstruction<FCMService>mock = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context) -> {
            Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
        })){

            Contact contact = new Contact("123", "new@mailId.com", "firstName", "lastName", "222e", 23454554422323L);

            PeopleRelationJDO userObject = new PeopleRelationJDO();
            userObject.setUniquepin("12121");
            userObject.setContactId("231");
            userObject.setContact(contact);
            userObject.setDefault(false);

            SettingsJDO accountObject = new SettingsJDO();
            accountObject.setDisplayTimeFormat("HH:MM:SS");

            var reportObject = new ReportsDTO();
            reportObject.setId("123");
            reportObject.setContactID("3242");
            reportObject.setAccountID("876");

            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put("source", "CLOCK_OUT");
            payloadMap.put("sourceClientID","clientID");
            payloadMap.put("outSourceActivity", "activity");
            payloadMap.put("receivedLongTime",12345345334L);
            payloadMap.put("userPRO", userObject);
            payloadMap.put("settingsJDO", accountObject);
            payloadMap.put("clockOutObject", reportObject);

            clockPublishUtilMockedStatic.when(() -> ClockPublishUtil.publishClockToChannel(reportObject, userObject, "activity", 12345345334L))
                    .thenAnswer((Answer<Void>) invocation -> null);

            ClockTaskUtil.getClockTaskUtil().clockOutTaskHandler(payloadMap);

            rtmServiceMockedStatic.verifyNoInteractions();

            clockMetricUtilMockedStatic.verify(() -> ClockMetricUtil.clockOutMetric(userObject.getUniquepin(),userObject.getContactId() , "CLOCK_OUT", reportObject));

        }

    }

    @Test
    void clockOutTaskHandler_validDefault_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ClockPublishUtil> clockPublishUtilMockedStatic = Mockito.mockStatic(ClockPublishUtil.class);
            MockedStatic<ClockMetricUtil> clockMetricUtilMockedStatic = Mockito.mockStatic(ClockMetricUtil.class);
            MockedConstruction<FCMService>mock = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context) -> {
                Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            })){

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>) invocation -> null);

            Contact contact = new Contact("123", "new@mailId.com", "firstName", "lastName", "222e", 23454554422323L);

            PeopleRelationJDO userObject = new PeopleRelationJDO();
            userObject.setUniquepin("12121");
            userObject.setContactId("231");
            userObject.setContact(contact);
            userObject.setDefault(true);

            SettingsJDO accountObject = new SettingsJDO();
            accountObject.setDisplayTimeFormat("HH:MM:SS");

            var reportObject = new ReportsDTO();
            reportObject.setId("123");
            reportObject.setContactID("3242");
            reportObject.setAccountID("876");

            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put("source", "force");
            payloadMap.put("sourceClientID","clientID");
            payloadMap.put("outSourceActivity", "activity");
            payloadMap.put("receivedLongTime",12345345334L);
            payloadMap.put("userPRO", userObject);
            payloadMap.put("settingsJDO", accountObject);
            payloadMap.put("clockOutObject", reportObject);

            clockPublishUtilMockedStatic.when(() -> ClockPublishUtil.publishClockToChannel(reportObject, userObject, "activity", 12345345334L))
                    .thenAnswer((Answer<Void>) invocation -> null);

            ClockTaskUtil.getClockTaskUtil().clockOutTaskHandler(payloadMap);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToAW(userObject.getUniquepin(), userObject.getContactId(),"FORCE_CLOCKOUT",
                    "entry",reportObject));

            clockMetricUtilMockedStatic.verify(() -> ClockMetricUtil.clockOutMetric(userObject.getUniquepin(),userObject.getContactId() , "force", reportObject));

        }

    }

}
