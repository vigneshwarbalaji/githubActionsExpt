package com.yoco.integration.helper;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.IntegrationsImpl;
import com.yoco.commons.dataservices.impl.TaskImpl;
import com.yoco.commons.entity.IntegrationsJDO;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.TaskJDO;
import com.yoco.integration.modal.ZendeskEntry;
import com.yoco.integration.modal.ZendeskRunningEntry;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;

class IntegrationHelperTest {
    @Test
    void isAccessAllowed_nullAccessToken_test(){
        Assertions.assertFalse(IntegrationHelper.isAccessAllowed(null,"123"));
    }

    @Test
    void isAccessAllowed_nullContactID_test(){
        Assertions.assertFalse(IntegrationHelper.isAccessAllowed(new OauthAccessToken(),null));
    }

    @Test
    void isAccessAllowed_emptyContactID_test(){
        Assertions.assertFalse(IntegrationHelper.isAccessAllowed(new OauthAccessToken(),""));
    }

    @Test
    void isAccessAllowed_serverToken_test(){
        OauthAccessToken accessToken = new OauthAccessToken();
        accessToken.setType("server");
        Assertions.assertTrue(IntegrationHelper.isAccessAllowed(accessToken,"123"));
    }

    @Test
    void isAccessAllowed_mismatchingUserID_test(){
        OauthAccessToken accessToken = new OauthAccessToken();
        accessToken.setType("user");
        accessToken.setUserId("234");
        Assertions.assertFalse(IntegrationHelper.isAccessAllowed(accessToken,"123"));
    }

    @Test
    void isAccessAllowed_validUserID_test(){
        OauthAccessToken accessToken = new OauthAccessToken();
        accessToken.setType("user");
        accessToken.setUserId("123");
        Assertions.assertTrue(IntegrationHelper.isAccessAllowed(accessToken,"123"));
    }

    @Test
    void extractIntegrationDetails_nullPayload_test(){
        String actual = IntegrationHelper.extractIntegrationDetails(null);
        Assertions.assertEquals("{}",actual);
    }

    @Test
    void extractIntegrationDetails_emptyPayload_test(){
        String actual = IntegrationHelper.extractIntegrationDetails(new HashMap<>());
        Assertions.assertEquals("{}",actual);
    }

    @Test
    void extractIntegrationDetails_nullIntegrationDetails_test(){
        String actual = IntegrationHelper.extractIntegrationDetails(new HashMap(){{put("integrationDetails",null);}});
        Assertions.assertEquals("{}",actual);
    }

    @Test
    void extractIntegrationDetails_emptyIntegrationDetails_test(){
        String actual = IntegrationHelper.extractIntegrationDetails(new HashMap(){{put("integrationDetails","");}});
        Assertions.assertEquals("{}",actual);
    }

    @Test
    void extractIntegrationDetails_validIntegrationDetails_test(){
        String actual = IntegrationHelper.extractIntegrationDetails(new HashMap(){{put("integrationDetails","{\"key\":\"value\"}");}});
        Assertions.assertEquals("{\"key\":\"value\"}",actual);
    }

    @Test
    void disableIntegration_nullIntegration_test(){
        Assertions.assertNull(IntegrationHelper.disableIntegration(new IntegrationsImpl(),null));
    }

    @Test
    void disableIntegration_validIntegration_test(){
        IntegrationsImpl integrationsImplMock = Mockito.mock(IntegrationsImpl.class);
        IntegrationsJDO userIntegration = new IntegrationsJDO();
        IntegrationsJDO response = IntegrationHelper.disableIntegration(integrationsImplMock,userIntegration);
        Assertions.assertEquals("{}",response.getIntegrationDetails());
        Assertions.assertFalse(response.getIsActive());
        Mockito.verify(integrationsImplMock, times(1)).delete(userIntegration);
    }

    @Test
    void updateIntegration_nullIntegration_test(){
        Assertions.assertNull(IntegrationHelper.updateIntegration(new IntegrationsImpl(),null,"{}"));
    }

    @Test
    void updateIntegration_validIntegration_test(){
        IntegrationsImpl integrationsImplMock = Mockito.mock(IntegrationsImpl.class);
        IntegrationsJDO userIntegration = new IntegrationsJDO();
        IntegrationsJDO response = IntegrationHelper.updateIntegration(integrationsImplMock,userIntegration,"details");
        Assertions.assertEquals("details",response.getIntegrationDetails());
        Mockito.verify(integrationsImplMock, times(1)).saveIntegration(userIntegration);
    }

    @Test
    void createIntegration_test(){
        IntegrationsImpl integrationsImplMock = Mockito.mock(IntegrationsImpl.class);
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("123");
        userPro.setContactId("234");
        IntegrationsJDO response = IntegrationHelper.createIntegration(integrationsImplMock,userPro,"details","slack");
        Assertions.assertEquals("123",response.getUniquePin());
        Assertions.assertEquals("234",response.getContactID());
        Assertions.assertEquals("slack",response.getIntegrationType());
        Assertions.assertEquals(true,response.getIsActive());
        Assertions.assertEquals("details",response.getIntegrationDetails());
        Mockito.verify(integrationsImplMock, times(1)).saveIntegration(response);
    }

    @Test
    void sortEventsBasedOnStartTimeDesc_nullListTest(){
        assertDoesNotThrow(() -> IntegrationHelper.sortEventsBasedOnStartTimeDesc(null));
    }

    @Test
    void sortEventsBasedOnStartTimeDesc_emptyListTest(){
        assertDoesNotThrow(() -> IntegrationHelper.sortEventsBasedOnStartTimeDesc(new ArrayList<>()));
    }

    @Test
    void sortEventsBasedOnStartTimeDesc_validTest(){
        Map<String,Object> event1 = new HashMap<>();
        event1.put(SchedulingKeys.START_TIME,123L);
        Map<String,Object> event2= new HashMap<>();
        event2.put(SchedulingKeys.START_TIME,50L);
        Map<String,Object> event3= new HashMap<>();
        event3.put(SchedulingKeys.START_TIME,250L);
        List<Map<String,Object>> events = new ArrayList<>();
        events.add(event1);
        events.add(event2);
        events.add(event3);
        assertDoesNotThrow(() -> IntegrationHelper.sortEventsBasedOnStartTimeDesc(events));
        Assertions.assertEquals(event3,events.get(0));
        Assertions.assertEquals(event1,events.get(1));
        Assertions.assertEquals(event2,events.get(2));
    }

    @Test
    void getRunningSession_nullEntry_test(){
        Assertions.assertNull(IntegrationHelper.getRunningSession(null,"timezone"));
    }

    @Test
    void getRunningSession_emptyEntry_test(){
        Assertions.assertNull(IntegrationHelper.getRunningSession(new HashMap<>(),"timezone"));
    }

    @Test
    void getRunningSession_notRunningEntry_test(){
        Assertions.assertNull(IntegrationHelper.getRunningSession(new HashMap(){{put(SchedulingKeys.METADATA,new HashMap(){{put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");}});}},"timezone"));
    }

    @Test
    void getRunningSession_RunningEntry_test(){
        try(MockedStatic<TaskImpl> taskMockedStatic = Mockito.mockStatic(TaskImpl.class)){
            TaskImpl taskImplMock = Mockito.mock(TaskImpl.class);
            TaskJDO taskJDO = new TaskJDO();
            taskJDO.setTaskDescription("task");
            Mockito.when(taskImplMock.getEntryByID("id")).thenReturn(taskJDO);
            taskMockedStatic.when(TaskImpl::getTaskImplInstance).thenReturn(taskImplMock);
            Map<String,Object> event = new HashMap<>();
            event.put(SchedulingKeys.START_TIME,1643049000000L);
            event.put(SchedulingKeys.END_TIME,1643092200000L);
            event.put(SchedulingKeys.METADATA,new HashMap(){{put(SchedulingKeys.STATUS,"CLOCKED_IN");}});
            event.put(SchedulingKeys.CALENDAR,"prjID");
            event.put(SchedulingKeys.ID,"id");
            ZendeskRunningEntry response = IntegrationHelper.getRunningSession(event,"Asia/Kolkata");
            Assertions.assertEquals("id",response.getId());
            Assertions.assertEquals("prjID",response.getProjectID());
            Assertions.assertEquals("CLOCKED_IN",response.getStatus());
            Assertions.assertEquals("task",response.getTaskDescription());
            Assertions.assertEquals("Tue, Jan 25 2022",response.getInDate());
            Assertions.assertEquals("12:00:00 AM",response.getInTime());
            Assertions.assertNotEquals("Tue, Jan 25 2022",response.getOutDate());
            Assertions.assertNotEquals("12:00:00 PM",response.getOutTime());
            Assertions.assertNotEquals(43200000L,response.getDuration());
            Assertions.assertEquals(1643049000000L,response.getInTimeMilliseconds());
        }
    }

    @Test
    void updateEntriesMap_keyNotPresent_test(){
        ZendeskEntry entry = new ZendeskEntry();
        entry.setDuration(10000L);
        entry.setInDate("Thu, 27 Jan 2022");
        Map<String,Object> entriesMap = new HashMap<>();
        IntegrationHelper.updateEntriesMap(entriesMap,entry);
        Assertions.assertTrue(entriesMap.containsKey("Thu, 27 Jan 2022"));
        Map<String,Object> dayLevelMap = (HashMap<String,Object>)entriesMap.get("Thu, 27 Jan 2022");
        Assertions.assertEquals(10000L,dayLevelMap.get("dayLevelDuration"));
        Assertions.assertEquals(entry,((List<ZendeskEntry>)dayLevelMap.get("sessions")).get(0));
        Assertions.assertEquals(1,((List<ZendeskEntry>)dayLevelMap.get("sessions")).size());
    }

    @Test
    void updateEntriesMap_keyPresent_test(){
        ZendeskEntry entry2 = new ZendeskEntry();
        entry2.setDuration(10000L);
        entry2.setInDate("Thu, 27 Jan 2022");
        Map<String,Object> entriesMap = new HashMap<>();

        ZendeskEntry entry1 = new ZendeskEntry();
        entry2.setDuration(10000L);
        entry2.setInDate("Thu, 27 Jan 2022");
        entriesMap.put("Thu, 27 Jan 2022",new HashMap(){{
            put("dayLevelDuration",10000L);
            put("sessions",new ArrayList(){{
                add(entry1);
            }});
        }});
        IntegrationHelper.updateEntriesMap(entriesMap,entry2);
        Assertions.assertTrue(entriesMap.containsKey("Thu, 27 Jan 2022"));
        Map<String,Object> dayLevelMap = (HashMap<String,Object>)entriesMap.get("Thu, 27 Jan 2022");
        Assertions.assertEquals(20000L,dayLevelMap.get("dayLevelDuration"));
        Assertions.assertEquals(entry1,((List<ZendeskEntry>)dayLevelMap.get("sessions")).get(0));
        Assertions.assertEquals(entry2,((List<ZendeskEntry>)dayLevelMap.get("sessions")).get(1));
        Assertions.assertEquals(2,((List<ZendeskEntry>)dayLevelMap.get("sessions")).size());
    }

    @Test
    void getEntriesMapForTaskID_nullUserEntriesList_test(){
        Assertions.assertEquals(new HashMap<>(),IntegrationHelper.getEntriesMapForTaskID(null,"timezone"));
    }

    @Test
    void getEntriesMapForTaskID_emptyUserEntriesList_test(){
        Assertions.assertEquals(new HashMap<>(),IntegrationHelper.getEntriesMapForTaskID(new ArrayList<>(),"timezone"));
    }

    @Test
    void getEntriesMapForTaskID_runningSessionPresent_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class)){
            Map<String,Object> event1 = new HashMap<>();
            Map<String,Object> event2 = new HashMap<>();
            event2.put(SchedulingKeys.START_TIME,1643221800000L);
            event2.put(SchedulingKeys.END_TIME,1643239800000L);
            List<Map<String,Object>> userEntries = new ArrayList(){{add(event1);add(event2);}};
            ZendeskRunningEntry runningSession = new ZendeskRunningEntry();
            integrationHelperMockedStatic.when(()->IntegrationHelper.getRunningSession(event1,"Asia/Kolkata")).thenReturn(runningSession);
            Assertions.assertEquals(new HashMap<>(),IntegrationHelper.getEntriesMapForTaskID(new ArrayList<>(),"timezone"));
            integrationHelperMockedStatic.when(()->IntegrationHelper.getEntriesMapForTaskID(userEntries,"Asia/Kolkata")).thenCallRealMethod();
            integrationHelperMockedStatic.when(()->IntegrationHelper.updateEntriesMap(any(HashMap.class),any(ZendeskEntry.class))).thenCallRealMethod();
            Map<String,Object> response = IntegrationHelper.getEntriesMapForTaskID(userEntries,"Asia/Kolkata");
            Assertions.assertEquals(runningSession,response.get("runningSession"));
            Assertions.assertEquals(18000000L,response.get("totalDuration"));
            Assertions.assertEquals(new HashMap(){{
                put("Thu, Jan 27 2022",new HashMap(){{
                    put("sessions",new ArrayList(){{
                        add(new ZendeskEntry(event2,"Asia/Kolkata"));
                    }});
                    put("dayLevelDuration",18000000L);
                }});
            }},response.get("entriesMap"));
        }
    }

    @Test
    void getEntriesMapForTaskID_runningSessionNotPresent_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class)){
            Map<String,Object> event1 = new HashMap<>();
            event1.put(SchedulingKeys.START_TIME,1643221800000L);
            event1.put(SchedulingKeys.END_TIME,1643239800000L);
            Map<String,Object> event2 = new HashMap<>();
            event2.put(SchedulingKeys.START_TIME,1643243400000L);
            event2.put(SchedulingKeys.END_TIME,1643247000000L);
            List<Map<String,Object>> userEntries = new ArrayList(){{add(event1);add(event2);}};
            integrationHelperMockedStatic.when(()->IntegrationHelper.getRunningSession(event1,"Asia/Kolkata")).thenReturn(null);
            Assertions.assertEquals(new HashMap<>(),IntegrationHelper.getEntriesMapForTaskID(new ArrayList<>(),"timezone"));
            integrationHelperMockedStatic.when(()->IntegrationHelper.getEntriesMapForTaskID(userEntries,"Asia/Kolkata")).thenCallRealMethod();
            integrationHelperMockedStatic.when(()->IntegrationHelper.updateEntriesMap(any(HashMap.class),any(ZendeskEntry.class))).thenCallRealMethod();
            Map<String,Object> response = IntegrationHelper.getEntriesMapForTaskID(userEntries,"Asia/Kolkata");
            Assertions.assertEquals(new HashMap<>(),response.get("runningSession"));
            Assertions.assertEquals(21600000L,response.get("totalDuration"));
            Assertions.assertEquals(new HashMap(){{
                put("Thu, Jan 27 2022",new HashMap(){{
                    put("sessions",new ArrayList(){{
                        add(new ZendeskEntry(event1,"Asia/Kolkata"));
                        add(new ZendeskEntry(event2,"Asia/Kolkata"));
                    }});
                    put("dayLevelDuration",21600000L);
                }});
            }},response.get("entriesMap"));
        }
    }

}
