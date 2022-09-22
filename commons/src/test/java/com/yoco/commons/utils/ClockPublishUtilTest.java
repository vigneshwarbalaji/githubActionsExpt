package com.yoco.commons.utils;

import com.yoco.commons.dataservices.impl.*;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.IntegrationsJDO;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.events.ClockPublishUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;


class ClockPublishUtilTest {

    @Test
    void publishClockToChannel_ifActivityEqualsNull_IN_test()  {

        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){

            var reportObject = new ReportsDTO();
            reportObject.setId("123");
            reportObject.setEmailID("test@gammaMail.com");
            reportObject.setContactID("233");
            reportObject.setAccountID("45");
            reportObject.setClockInMessage("newMessage");
            reportObject.setClockOutMessage("newMessage");
            reportObject.setDeleted(false);
            reportObject.setInTime(2435465656543L);
            reportObject.setOutTime(2435465656700L);
            reportObject.setPayrollStatus("Status");
            reportObject.setProjectID("3423");
            reportObject.setProjectName("test");
            reportObject.setStatus("newStatus");

            Map<String, Object>profileMap = new HashMap<>();
            profileMap.put("photoID", false);
            Map<String, Object> dataProtectionMap = new HashMap<>();
            dataProtectionMap.put("profile", profileMap);

            var contact = new Contact("233", "test@gmail.com", "testName", "test", "232", 23456543234L);

            var userPro = new PeopleRelationJDO();
            userPro.setId("232");
            userPro.setLogin("newLogin");
            userPro.setUniquepin("432");
            userPro.setDataProtectionSkillset(JsonUtil.getJson(dataProtectionMap));
            userPro.setTimeZone("UTC");
            userPro.setContact(contact);

            ClockPublishUtil.publishClockToChannel(reportObject,userPro, "null_in",23456543234L);

            activityUtilMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void publishClockToChannel_ifActivityIsNullOrEmpty_test()  {

        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){

            var reportObject = new ReportsDTO();
            reportObject.setId("123");
            reportObject.setEmailID("test@gammaMail.com");
            reportObject.setContactID("233");
            reportObject.setAccountID("45");
            reportObject.setClockInMessage("newMessage");
            reportObject.setClockOutMessage("newMessage");
            reportObject.setDeleted(false);
            reportObject.setInTime(2435465656543L);
            reportObject.setOutTime(2435465656700L);
            reportObject.setPayrollStatus("Status");
            reportObject.setProjectID("3423");
            reportObject.setProjectName("test");
            reportObject.setStatus("newStatus");

            Map<String, Object>profileMap = new HashMap<>();
            profileMap.put("photoID", false);
            Map<String, Object> dataProtectionMap = new HashMap<>();
            dataProtectionMap.put("profile", profileMap);

            var contact = new Contact("233", "test@gmail.com", "testName", "test", "232", 23456543234L);

            var userPro = new PeopleRelationJDO();
            userPro.setId("232");
            userPro.setLogin("newLogin");
            userPro.setUniquepin("432");
            userPro.setDataProtectionSkillset(JsonUtil.getJson(dataProtectionMap));
            userPro.setTimeZone("UTC");
            userPro.setContact(contact);

            ClockPublishUtil.publishClockToChannel(reportObject,userPro, "",23456543234L);

            activityUtilMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void publishClockToChannel_valid_test()  {

        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){

            var reportObject = new ReportsDTO();
            reportObject.setId("123");
            reportObject.setEmailID("test@gammaMail.com");
            reportObject.setContactID("233");
            reportObject.setAccountID("45");
            reportObject.setClockInMessage("newMessage");
            reportObject.setClockOutMessage("newMessage");
            reportObject.setDeleted(false);
            reportObject.setInTime(2435465656543L);
            reportObject.setOutTime(2435465656700L);
            reportObject.setPayrollStatus("newStatus");
            reportObject.setProjectID("3423");
            reportObject.setProjectName("testProject");
            reportObject.setStatus("newStatus");

            Map<String, Object>profileMap = new HashMap<>();
            profileMap.put("photoID", true);
            Map<String, Object> dataProtectionMap = new HashMap<>();
            dataProtectionMap.put("profile", profileMap);

            var contact = new Contact("233", "test@gmail.com", "test", "test", "232", 23456543234L);

            var userPro = new PeopleRelationJDO();
            userPro.setId("232");
            userPro.setLogin("newLogin");
            userPro.setUniquepin("432");
            userPro.setDataProtectionSkillset(JsonUtil.getJson(dataProtectionMap));
            userPro.setTimeZone("UTC");
            userPro.setContact(contact);

            ClockPublishUtil.publishClockToChannel(reportObject,userPro, "newActivity",23456543234L);

            activityUtilMockedStatic.verify(()->ActivityUtil.saveActivity(reportObject.getAccountID(), reportObject.getContactID(), reportObject.getProjectID(), reportObject.getEmailID(), "newActivity",ActivityUtil.ACTIVITIES.DUMMY.value(), 23456543234L));
        }
    }

    @Test
    void publishMessageToSlack_Exception_test()  {

        try{
            var contact = new Contact("233", "test@gmail.com", "test", "test", "232", 23456543234L);
            ClockPublishUtil.publishMessageToSlack("CLOCK_OUT_WEB", null,"#0.00",contact);
        } catch (Exception e) {
            Assertions.assertEquals(null, e.getMessage());
        }
    }

    @Test
    void publishMessageToSlack_IfTheActionIsEmpty_test()  {

        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){

            var reportObject = new ReportsDTO();
            reportObject.setAccountID("123");
            reportObject.setContactID("233");

            var contact = new Contact("233", "test@gmail.com", "test", "test", "232", 23456543234L);
            ClockPublishUtil.publishMessageToSlack("", reportObject,"#0.00",contact);

            urlFetcherMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void publishMessageToSlack_valid_test()  {

        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            var reportObject = new ReportsDTO();
            reportObject.setId("123");
            reportObject.setEmailID("test@gammaMail.com");
            reportObject.setContactID("233");
            reportObject.setAccountID("45");
            reportObject.setClockInMessage("newMessage");
            reportObject.setClockOutMessage("newMessage");
            reportObject.setDeleted(false);
            reportObject.setInTime(2435465656543L);
            reportObject.setOutTime(2435465656700L);
            reportObject.setPayrollStatus("newStatus");
            reportObject.setProjectID("3423");
            reportObject.setProjectName("testProject");
            reportObject.setStatus("newStatus");

            HashMap<String, Object> payloadMap = new HashMap<>();
            var contact = new Contact("233", "test@gmail.com", "test", "test", "232", 23456543234L);
            var headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON};
            ClockPublishUtil.publishMessageToSlack("CLOCK_OUT_WEB", reportObject,"#0.00",contact);

            urlFetcherMockedStatic.verify(()->UrlFetcher.sendPostRequest("url", payloadMap, headers,  UrlFetcher.getHttpClientInstance()));
        }
    }

    @Test
    void getClockOutMessageFormatForSlack_exception_test()  {

        try(MockedConstruction<DecimalFormat> decimalMock = Mockito.mockConstruction(DecimalFormat.class, (decimalImplMock, context) -> {
            Mockito.when(decimalImplMock.format(anyDouble())).thenThrow(new IllegalArgumentException("invalid data"));
        })){
            var currentUser = new Contact("123", "new@email.com", "firstName", "lastName", "232", 121212121212L);
            String responseString = ClockPublishUtil.getClockOutMessageFormatForSlack(0L, 0L, currentUser,"#0.00");
            Assertions.assertEquals("*firstName* has *Clocked Out* \n*Session Duration :* 0.00", responseString);
        }
    }

    @Test
    void getClockOutMessageFormatForSlack_withoutdecimalFormat_valid_test()  {

        var currentUser = new Contact("123", "new@email.com", "firstName", "lastName", "232", 121212121212L);

        String responseString = ClockPublishUtil.getClockOutMessageFormatForSlack(2222222222L, 1111111111L, currentUser,"");

        Assertions.assertEquals("*firstName* has *Clocked Out* \n*Session Duration :* 308h 38m", responseString);
    }

    @Test
    void getClockOutMessageFormatForSlack_valid_test()  {

        var currentUser = new Contact("123", "new@email.com", "firstName", "lastName", "232", 121212121212L);

        String responseString = ClockPublishUtil.getClockOutMessageFormatForSlack(2222222222L, 1111111111L, currentUser,"#0.00");

        Assertions.assertEquals("*firstName* has *Clocked Out* \n*Session Duration :* 308.64", responseString);
    }

    @Test
    void getSlackWebHookUrl_exception_test()  {

        try(MockedConstruction<IntegrationsImpl> integrationMock = Mockito.mockConstruction(IntegrationsImpl.class, (integrationImplMock, context) -> {
            Mockito.when(integrationImplMock.getIntegrationByType(anyString(), anyString(), anyString())).thenThrow(new IllegalArgumentException("invalid data"));
        })){
            ClockPublishUtil.getSlackWebHookUrl("23", "438");
        }catch (Exception e){
            Assertions.assertEquals("invalid data", e.getMessage());
        }
    }

    @Test
    void getSlackWebHookUrl_integrationResponseIsNull_test()  {

        try(MockedConstruction<IntegrationsImpl> integrationMock = Mockito.mockConstruction(IntegrationsImpl.class, (integrationImplMock, context) -> {
            Mockito.when(integrationImplMock.getIntegrationByType(anyString(), anyString(), anyString())).thenReturn(null);
        })){
            String responseString = ClockPublishUtil.getSlackWebHookUrl("22", "432");
            Assertions.assertEquals("", responseString);
        }
    }

    @Test
    void getSlackWebHookUrl_integrationDetailsPropertyIsEmpty_test()  {

        IntegrationsJDO integrationObject = new IntegrationsJDO();

        try(MockedConstruction<IntegrationsImpl> integrationMock = Mockito.mockConstruction(IntegrationsImpl.class, (integrationImplMock, context) -> {
            Mockito.when(integrationImplMock.getIntegrationByType(anyString(), anyString(), anyString())).thenReturn(integrationObject);
        })){
            String responseString = ClockPublishUtil.getSlackWebHookUrl("292", "4389");
            Assertions.assertEquals("", responseString);
        }
    }

    @Test
    void getSlackWebHookUrl_integrationDetailsMapIsEmpty_test()  {

        Map<String, Object> integrationMap = new HashMap<>();

        IntegrationsJDO integrationObject = new IntegrationsJDO();
        integrationObject.setIntegrationDetails(JsonUtil.getJson(integrationMap));

        try(MockedConstruction<IntegrationsImpl> integrationMock = Mockito.mockConstruction(IntegrationsImpl.class, (integrationImplMock, context) -> {
            Mockito.when(integrationImplMock.getIntegrationByType(anyString(), anyString(), anyString())).thenReturn(integrationObject);
        })){
            String responseString = ClockPublishUtil.getSlackWebHookUrl("32", "67");
            Assertions.assertEquals("", responseString);
        }
    }

    @Test
    void getSlackWebHookUrl_doesNotContainKey_test()  {

        Map<String, Object> integrationMap = new HashMap<>();
        integrationMap.put("integration", "url");

        IntegrationsJDO integrationObject = new IntegrationsJDO();
        integrationObject.setIntegrationDetails(JsonUtil.getJson(integrationMap));

        try(MockedConstruction<IntegrationsImpl> integrationMock = Mockito.mockConstruction(IntegrationsImpl.class, (integrationImplMock, context) -> {
            Mockito.when(integrationImplMock.getIntegrationByType(anyString(), anyString(), anyString())).thenReturn(integrationObject);
        })){
            String responseString = ClockPublishUtil.getSlackWebHookUrl("78", "77");
            Assertions.assertEquals("", responseString);
        }
    }

    @Test
    void getSlackWebHookUrl_valid_test()  {

        Map<String, Object> integrationMap = new HashMap<>();
        integrationMap.put("url", "url");

        IntegrationsJDO integrationObject = new IntegrationsJDO();
        integrationObject.setIntegrationDetails(JsonUtil.getJson(integrationMap));

        try(MockedConstruction<IntegrationsImpl> integrationMock = Mockito.mockConstruction(IntegrationsImpl.class, (integrationImplMock, context) -> {
            Mockito.when(integrationImplMock.getIntegrationByType(anyString(), anyString(), anyString())).thenReturn(integrationObject);
        })){
            String responseString = ClockPublishUtil.getSlackWebHookUrl("232", "43");
            Assertions.assertEquals("url", responseString);
        }
    }

}
