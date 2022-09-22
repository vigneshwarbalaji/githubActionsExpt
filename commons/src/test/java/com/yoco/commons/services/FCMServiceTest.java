package com.yoco.commons.services;

import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.dataservices.impl.FcmImpl;
import com.yoco.commons.entity.FcmJDO;
import com.yoco.commons.utils.ObjUtils;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.net.http.HttpClient;
import java.util.*;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class FCMServiceTest {

    FCMService fcmService = FCMService.getFCMService();

    @Test
    void updateFcmSyncObject_error_test(){
        FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
        Mockito.when(fcmMock.getAllDevicesOfContact(anyString(),any(int.class),any())).thenThrow(new IllegalArgumentException("exception"));
        FCMService.fcmDao = fcmMock;

        Set<String> contacts = new HashSet<>();
        contacts.add("contact1");
        Map<String, Object> response = fcmService.updateFcmSyncObject("accountId",contacts, FCMService.FCM_SERVICE_CONSTANTS.FCM_PROFILE.value(),null);

        Map<String,Object> expectedMap = new HashMap<>();
        expectedMap.put(FCMService.FCM_SERVICE_CONSTANTS.IS_DATA_UPDATED.value(),false);
        expectedMap.put(FCMService.FCM_SERVICE_CONSTANTS.ERROR_MESSAGE.value(),"exception");
        Assertions.assertEquals(expectedMap,response);
        Mockito.verify(fcmMock).getAllDevicesOfContact("contact1",50,null);
    }

    @Test
    void getFcmSyncList_profile_nonEmpty_test(){
        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setAccountID("accountId");
        fcmJDO.setDeviceModel("abc");
        List<FcmJDO> fcmJDOList = new ArrayList<>();
        fcmJDOList.add(fcmJDO);
        FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
        Mockito.when(fcmMock.getAllDevicesOfContact(anyString(),any(int.class),any())).thenReturn(fcmJDOList);
        FCMService.fcmDao = fcmMock;

        Set<String> contacts = new HashSet<>();
        contacts.add("contact1");
        List<FcmJDO> response = fcmService.getFcmSyncList("accountId",contacts,FCMService.FCM_SERVICE_CONSTANTS.FCM_PROFILE.value());
        Assertions.assertEquals(fcmJDOList,response);
        Mockito.verify(fcmMock).getAllDevicesOfContact("contact1",50,null);
    }

    @Test
    void getFcmSyncList_profile_empty_test(){
        List<FcmJDO> fcmJDOList = new ArrayList<>();
        FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
        Mockito.when(fcmMock.getAllDevicesOfContact(anyString(),any(int.class),any())).thenReturn(fcmJDOList);
        FCMService.fcmDao = fcmMock;

        Set<String> contacts = new HashSet<>();
        contacts.add("contact1");
        List<FcmJDO> response = fcmService.getFcmSyncList("accountId",contacts,FCMService.FCM_SERVICE_CONSTANTS.FCM_PROFILE.value());
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        Mockito.verify(fcmMock).getAllDevicesOfContact("contact1",50,null);
    }

    @Test
    void getFcmSyncList_empty_test(){
        List<FcmJDO> fcmJDOList = new ArrayList<>();
        FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
        Mockito.when(fcmMock.getAllDevicesOfContactUnderAccount(anyString(),anyString(),any(int.class),any())).thenReturn(fcmJDOList);
        FCMService.fcmDao = fcmMock;

        Set<String> contacts = new HashSet<>();
        contacts.add("contact1");
        List<FcmJDO> response = fcmService.getFcmSyncList("accountId",contacts,FCMService.FCM_SERVICE_CONSTANTS.FCM_PROJECT.value());
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        Mockito.verify(fcmMock).getAllDevicesOfContactUnderAccount("accountId","contact1",50,null);
    }

    @Test
    void updateFcmSyncList_empty_list_test() throws JSONException, IOException {
        FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
        FCMService.fcmDao = fcmMock;
        List<FcmJDO> fcmJDOList = new ArrayList<>();
        Map<String,Object> response = fcmService.updateFcmSyncList(fcmJDOList,"",null);
        Map<String,Object> expectedMap = new HashMap<>();
        expectedMap.put(FCMService.FCM_SERVICE_CONSTANTS.IS_DATA_UPDATED.value(),false);
        expectedMap.put(FCMService.FCM_SERVICE_CONSTANTS.ERROR_MESSAGE.value(),"Seems The user dose not have any device info to store");
        Assertions.assertEquals(expectedMap,response);
        Mockito.verifyNoInteractions(fcmMock);
    }

    @Test
    void updateFcmSyncList_valid_test() throws JSONException, IOException {
        Set<String> projectIds = new HashSet<>();
        projectIds.add("projectId1");
        projectIds.add("projectId2");
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("project",projectIds);
        jsonObject.put("timeZone","Asia/Kolkata");

        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setDeviceModel("abc");
        fcmJDO.setSyncObject(jsonObject.toString());

        List<FcmJDO> fcmJDOList = new ArrayList<>();
        fcmJDOList.add(fcmJDO);

        List<String> projectsList = new ArrayList<>();
        projectsList.add("projectId3");

        FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
        Mockito.when(fcmMock.saveFcm(any(FcmJDO.class))).thenReturn(fcmJDO);
        FCMService.fcmDao = fcmMock;

        Map<String,Object> response = fcmService.updateFcmSyncList(fcmJDOList,"project",projectsList);

        Map<String,Object> expectedMap = new HashMap<>();
        expectedMap.put(FCMService.FCM_SERVICE_CONSTANTS.IS_DATA_UPDATED.value(),true);
        Assertions.assertEquals(expectedMap,response);
        Mockito.verify(fcmMock).saveFcm(any(FcmJDO.class));
    }

    @Test
    void updateFcmSyncList_noKey_test() throws JSONException, IOException {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("timeZone","Asia/Kolkata");

        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setDeviceModel("abc");
        fcmJDO.setSyncObject(jsonObject.toString());

        List<FcmJDO> fcmJDOList = new ArrayList<>();
        fcmJDOList.add(fcmJDO);

        List<String> projectsList = new ArrayList<>();
        projectsList.add("projectId3");

        FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
        Mockito.when(fcmMock.saveFcm(any(FcmJDO.class))).thenReturn(fcmJDO);
        FCMService.fcmDao = fcmMock;

        Map<String,Object> response = fcmService.updateFcmSyncList(fcmJDOList,"project",projectsList);

        Map<String,Object> expectedMap = new HashMap<>();
        expectedMap.put(FCMService.FCM_SERVICE_CONSTANTS.IS_DATA_UPDATED.value(),true);
        Assertions.assertEquals(expectedMap,response);
        Mockito.verify(fcmMock).saveFcm(any(FcmJDO.class));
    }

    @Test
    void updateFcmSyncList_non_list_test2() throws JSONException, IOException {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("clock","123");

        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setDeviceModel("abc");
        fcmJDO.setSyncObject(jsonObject.toString());

        List<FcmJDO> fcmJDOList = new ArrayList<>();
        fcmJDOList.add(fcmJDO);

        FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
        Mockito.when(fcmMock.saveFcm(any(FcmJDO.class))).thenReturn(fcmJDO);
        FCMService.fcmDao = fcmMock;

        Map<String,Object> response = fcmService.updateFcmSyncList(fcmJDOList,"timeZone","Asia/Kolkata");

        Map<String,Object> expectedMap = new HashMap<>();
        expectedMap.put(FCMService.FCM_SERVICE_CONSTANTS.IS_DATA_UPDATED.value(),true);
        Assertions.assertEquals(expectedMap,response);
        Mockito.verify(fcmMock).saveFcm(any(FcmJDO.class));
    }

    @Test
    void getFcmDeviceToken_empty_contacts_test(){
        FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
        FCMService.fcmDao = fcmMock;
        Map<String,Object> response = fcmService.getFcmDeviceToken(null,"accountId");
        Map<String,Object> expectedMap = new HashMap<>();
        expectedMap.put(FCMService.FCM_SERVICE_CONSTANTS.IS_FCM_TOKEN_AVAILABLE.value(), false);
        expectedMap.put(FCMService.FCM_SERVICE_CONSTANTS.ERROR_MESSAGE.value(),"You sent contactID as Null");
        Assertions.assertEquals(expectedMap,response);
        Mockito.verifyNoInteractions(fcmMock);
    }

    @Test
    void getFcmDeviceToken_error_test(){
        FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
        Mockito.when(fcmMock.getFcmDeviceToken(anySet(),anyString())).thenThrow(new IllegalArgumentException("exception"));
        FCMService.fcmDao = fcmMock;
        Set<String> contacts = new HashSet<>();
        contacts.add("contact1");
        Map<String,Object> response = fcmService.getFcmDeviceToken(contacts,"accountId");
        Map<String,Object> expectedMap = new HashMap<>();
        expectedMap.put(FCMService.FCM_SERVICE_CONSTANTS.IS_FCM_TOKEN_AVAILABLE.value(), false);
        expectedMap.put(FCMService.FCM_SERVICE_CONSTANTS.ERROR_MESSAGE.value(),"exception");
        Assertions.assertEquals(expectedMap,response);
        Mockito.verify(fcmMock).getFcmDeviceToken(contacts,"accountId");
    }

    @Test
    void notifyFCM_IOS_ClientSource_test() {
        try(MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class)){

            Map<String,String> clientIdMap  = new HashMap<>();
            clientIdMap.put("ios","ios");
            clientSourceMockedStatic.when(()-> ClientSource.getClientIdMap()).thenReturn(clientIdMap);

            FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
            FCMService.fcmDao = fcmMock;

            Set<String> contact = new HashSet<>();
            contact.add("contact1");

            fcmService.notifyFCM("accountId",contact, FCMService.FCM_SERVICE_CONSTANTS.FCM_PROJECT.value(),false,null,"ios");
            Mockito.verifyNoInteractions(fcmMock);
        }
    }

    @Test
    void notifyFCM_Android_ClientSource_test() {
        try(MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class)){

            Map<String,String> clientIdMap  = new HashMap<>();
            clientIdMap.put("android","android");
            clientSourceMockedStatic.when(()-> ClientSource.getClientIdMap()).thenReturn(clientIdMap);

            FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
            FCMService.fcmDao = fcmMock;

            Set<String> contact = new HashSet<>();
            contact.add("contact1");

            fcmService.notifyFCM("accountId",contact, FCMService.FCM_SERVICE_CONSTANTS.FCM_PROJECT.value(),false,null,"android");
            Mockito.verifyNoInteractions(fcmMock);
        }
    }

    @Test
    void notifyFCM_publishToFCM_nullSrcClient_test() {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFcmKey).thenReturn("fcmToken");

            List<String> fcmDeviceToken = new ArrayList<>();
            fcmDeviceToken.add("token");
            FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
            Mockito.when(fcmMock.getFcmDeviceToken(anySet(),anyString())).thenReturn(fcmDeviceToken);
            FCMService.fcmDao = fcmMock;

            Set<String> contact = new HashSet<>();
            contact.add("contact1");

            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(),anyMap(),any(String[].class),any(HttpClient.class))).thenReturn(null);

            fcmService.notifyFCM("accountId",contact, FCMService.FCM_SERVICE_CONSTANTS.FCM_PROJECT.value(),false,null,null);

            Map<String,Object> payload = new HashMap<>();
            payload.put("collapse_key","PROJECT");
            Map<String,Object> dataMap = new HashMap<>();
            dataMap.put("notify","project");
            payload.put("data",dataMap);
            List<String> regIds = new ArrayList<>();
            regIds.add("token");
            payload.put("registration_ids",regIds);

            String[] headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,"key=fcmToken" };

            urlFetcherMockedStatic.verify(()-> UrlFetcher.sendPostRequest("https://fcm.googleapis.com/fcm/send", payload,headers,UrlFetcher.getHttpClientInstance()));
        }
    }

    @Test
    void notifyFCM_publishToFCM_validSrcClient_test() {
        try(MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFcmKey).thenReturn("fcmToken");

            Map<String,String> clientIdMap  = new HashMap<>();
            clientIdMap.put("web","web");

            clientSourceMockedStatic.when(()-> ClientSource.getClientIdMap()).thenReturn(clientIdMap);

            List<String> fcmDeviceToken = new ArrayList<>();
            fcmDeviceToken.add("token");
            FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
            Mockito.when(fcmMock.getFcmDeviceToken(anySet(),anyString())).thenReturn(fcmDeviceToken);
            FCMService.fcmDao = fcmMock;

            Set<String> contact = new HashSet<>();
            contact.add("contact1");

            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(),anyMap(),any(String[].class),any(HttpClient.class))).thenReturn(null);

            fcmService.notifyFCM("accountId",contact, FCMService.FCM_SERVICE_CONSTANTS.FCM_PROJECT.value(),false,null,"web");

            Map<String,Object> payload = new HashMap<>();
            payload.put("collapse_key","PROJECT");
            Map<String,Object> dataMap = new HashMap<>();
            dataMap.put("notify","project");
            payload.put("data",dataMap);
            List<String> regIds = new ArrayList<>();
            regIds.add("token");
            payload.put("registration_ids",regIds);

            String[] headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,"key=fcmToken" };

            urlFetcherMockedStatic.verify(()-> UrlFetcher.sendPostRequest("https://fcm.googleapis.com/fcm/send", payload,headers,UrlFetcher.getHttpClientInstance()));
        }
    }

    @Test
    void notifyFCM_updateFcmSyncObject_test() {

        try(MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){

            Map<String,String> clientIdMap  = new HashMap<>();
            clientIdMap.put("web","web");

            clientSourceMockedStatic.when(()-> ClientSource.getClientIdMap()).thenReturn(clientIdMap);
            FcmImpl fcmMock = Mockito.mock(FcmImpl.class);

            FcmJDO fcmJDO = new FcmJDO();
            fcmJDO.setAccountID("accountId");
            fcmJDO.setDeviceModel("abc");

            List<FcmJDO> fcmJDOList = new ArrayList<>();
            fcmJDOList.add(fcmJDO);
            Mockito.when(fcmMock.getAllDevicesOfContactUnderAccount(anyString(),anyString(),any(int.class),any())).thenReturn(fcmJDOList);
            Mockito.when(fcmMock.saveFcm(any(FcmJDO.class))).thenReturn(null);

            List<String> fcmDeviceToken = new ArrayList<>();
            Mockito.when(fcmMock.getFcmDeviceToken(anySet(),anyString())).thenReturn(fcmDeviceToken);
            FCMService.fcmDao = fcmMock;

            Set<String> contact = new HashSet<>();
            contact.add("contact1");

            List<String> projectIDList = new ArrayList<>();
            projectIDList.add("id1");

            fcmService.notifyFCM("accountId",contact, FCMService.FCM_SERVICE_CONSTANTS.FCM_PROJECT.value(),true,projectIDList,"web");

            urlFetcherMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void notifyFCM_error_test() {
        try(MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class)){
            Map<String,String> clientIdMap  = new HashMap<>();
            clientIdMap.put("web","web");

            clientSourceMockedStatic.when(()-> ClientSource.getClientIdMap()).thenThrow(new IllegalArgumentException("exception"));

            Set<String> contact = new HashSet<>();
            contact.add("contact1");

            fcmService.notifyFCM("accountId",contact, FCMService.FCM_SERVICE_CONSTANTS.FCM_PROJECT.value(),true,null,"web");
            clientSourceMockedStatic.verify(() -> ClientSource.getClientIdMap());
        }
    }

    @Test
    void notifyFCM_nullSyncObj_test() {
        try(MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class)){
            Map<String,String> clientIdMap  = new HashMap<>();
            clientIdMap.put("web","web");
            clientSourceMockedStatic.when(()-> ClientSource.getClientIdMap()).thenReturn(clientIdMap);

            FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
            Mockito.when(fcmMock.getFcmDeviceToken(anySet(),anyString())).thenReturn(new ArrayList<>());
            FCMService.fcmDao = fcmMock;

            Set<String> contact = new HashSet<>();
            contact.add("contact1");

            fcmService.notifyFCM("accountId",contact, FCMService.FCM_SERVICE_CONSTANTS.FCM_PROJECT.value(),true,null,"web");
             Mockito.verify(fcmMock).getFcmDeviceToken(contact,"accountId");
        }
    }

    @Test
    void publishToFCM_nullDeviceTokens_test(){
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){
            FCMService.getFCMService().publishToFCM("activity",null);
            commonAppPropertiesMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void publishToFCM_deleteAccount_test() throws IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)) {

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFcmKey).thenReturn("fcmToken");

            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(),anyMap(),any(String[].class),any(HttpClient.class))).thenReturn(null);

            List<String> deviceTokens = new ArrayList<>();
            deviceTokens.add("token1");
            fcmService.publishToFCM(FCMService.FCM_SERVICE_CONSTANTS.FCM_DELETE_ACCOUNT.value(),deviceTokens);

            Map<String,Object> payload = new HashMap<>();
            Map<String,Object> dataMap = new HashMap<>();
            dataMap.put("notify","deleteAccount");
            payload.put("data",dataMap);
            List<String> regIds = new ArrayList<>();
            regIds.add("token1");
            payload.put("registration_ids",regIds);

            String[] headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,"key=fcmToken" };

            urlFetcherMockedStatic.verify(()-> UrlFetcher.sendPostRequest("https://fcm.googleapis.com/fcm/send", payload,headers,UrlFetcher.getHttpClientInstance()));
        }
    }

    @Test
    void publishToFCM_clockInOut_test(){
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)) {

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFcmKey).thenReturn("fcmToken");

            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(),anyMap(),any(String[].class),any(HttpClient.class))).thenReturn(null);

            List<String> deviceTokens = new ArrayList<>();
            deviceTokens.add("token1");
            fcmService.publishToFCM(FCMService.FCM_SERVICE_CONSTANTS.FCM_CLOCK_IN_OUT.value(),deviceTokens);

            Map<String,Object> payload = new HashMap<>();
            payload.put("collapse_key","CLOCK_STATUS");
            Map<String,Object> dataMap = new HashMap<>();
            dataMap.put("notify","clockInOut");
            payload.put("data",dataMap);
            List<String> regIds = new ArrayList<>();
            regIds.add("token1");
            payload.put("registration_ids",regIds);
            payload.put("time_to_live",60);

            String[] headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,"key=fcmToken" };

            urlFetcherMockedStatic.verify(()-> UrlFetcher.sendPostRequest("https://fcm.googleapis.com/fcm/send", payload,headers,UrlFetcher.getHttpClientInstance()));
        }
    }

    @Test
    void publishToFCM_IOException_test(){
        try(MockedStatic<UrlFetcher> mockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFcmKey).thenReturn("fcmToken");
            mockedStatic.when(() -> UrlFetcher.sendPostRequest(any(),anyMap(),any(),any())).thenThrow(new IOException("exception"));

            List<String> deviceTokens = new ArrayList<>();
            deviceTokens.add("token1");
            fcmService.publishToFCM(FCMService.FCM_SERVICE_CONSTANTS.FCM_CLOCK_IN_OUT.value(),deviceTokens);
            mockedStatic.verify(()-> UrlFetcher.sendPostRequest(any(),anyMap(),any(),any()),times(1));
        }
    }
}

