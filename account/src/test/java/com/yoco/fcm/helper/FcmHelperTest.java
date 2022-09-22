package com.yoco.fcm.helper;

import com.yoco.commons.dataservices.impl.FcmImpl;
import com.yoco.commons.entity.FcmJDO;
import com.yoco.commons.services.FCMService;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.fcm.enums.FCM_ERROR_MESSAGE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

import java.util.*;

import static org.mockito.ArgumentMatchers.*;

class FcmHelperTest {

    @Test
    void getFcmSyncInfo_invalidDeviceID_test(){

        try (MockedConstruction<FcmImpl> mock = Mockito.mockConstruction(FcmImpl.class, (fcmImplMock, context) -> {
            Mockito.when(fcmImplMock.getFcmDeviceInfoById(anyString())).thenReturn(null);
        })){
            Map<String, Object> expectedMap = new HashMap<>();
            expectedMap.put(FcmHelper.IS_DEVICE_INFO_AVAILABLE, false);
            expectedMap.put(FcmHelper.ERROR_MESSAGE, FCM_ERROR_MESSAGE.DEVICE_NOT_FOUND.value());
            Map<String, Object> responseMap = new FcmHelper().getFcmSyncInfo("deviceID");
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void getFcmSyncInfo_ifSyncObjectIsNotUpdated_test(){

        String syncObjectString = "";
        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setAccountID("123");
        fcmJDO.setContactID("123");
        fcmJDO.setEmailID("new@gmail.com");
        fcmJDO.setSyncObject(syncObjectString);
        try (MockedConstruction<FcmImpl> mock = Mockito.mockConstruction(FcmImpl.class, (fcmImplMock, context) -> {
            Mockito.when(fcmImplMock.getFcmDeviceInfoById(anyString())).thenReturn(fcmJDO);
        })){
            Map<String, Object> expectedMap = new HashMap<>();
            expectedMap.put(FcmHelper.IS_SYNC_OBJECT_UPDATED, false);
            expectedMap.put(FcmHelper.IS_DEVICE_INFO_AVAILABLE, true);
            Map<String, Object> responseMap = new FcmHelper().getFcmSyncInfo("deviceID");
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void getFcmSyncInfo_valid_test(){

        Map<String, Object>syncObjectMap = new HashMap<>();
        syncObjectMap.put("dummy", "map");
        String syncObjectString = JsonUtil.getJson(syncObjectMap);
        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setAccountID("123");
        fcmJDO.setContactID("123");
        fcmJDO.setEmailID("new@gmail.com");
        fcmJDO.setSyncObject(syncObjectString);
        try (MockedConstruction<FcmImpl> mock = Mockito.mockConstruction(FcmImpl.class, (fcmImplMock, context) -> {
                 Mockito.when(fcmImplMock.getFcmDeviceInfoById(anyString())).thenReturn(fcmJDO);
             })){
            Map<String, Object> expectedMap = new HashMap<>();
            expectedMap.put(FcmHelper.IS_SYNC_OBJECT_UPDATED, true);
            expectedMap.put(FcmHelper.IS_DEVICE_INFO_AVAILABLE, true);
            expectedMap.put(FcmHelper.SYNC_OBJECT, syncObjectMap);
            Map<String, Object> responseMap = new FcmHelper().getFcmSyncInfo("deviceID");
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void deleteDeviceInfo_inValidDeviceID_test(){
        try (MockedConstruction<FcmImpl> mock = Mockito.mockConstruction(FcmImpl.class, (fcmImplMock, context) -> {
            Mockito.doNothing().when(fcmImplMock).deleteDevice(any());
            Mockito.when(fcmImplMock.getFcmDeviceInfoById(anyString())).thenReturn(null);
        })){
            Map<String, Object> expectedMap = new HashMap<>();
            expectedMap.put(FcmHelper.IS_DEVICE_UN_REGISTERED, false);
            expectedMap.put(FcmHelper.ERROR_MESSAGE, FCM_ERROR_MESSAGE.DEVICE_ID_NOT_FOUND.value());
            Map<String, Object> responseMap = new FcmHelper().deleteDeviceInfo("deviceID");
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void deleteDeviceInfo_valid_test(){

        Map<String, Object>syncObjectMap = new HashMap<>();
        syncObjectMap.put("dummy", "map");
        String syncObjectString = JsonUtil.getJson(syncObjectMap);
        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setAccountID("123");
        fcmJDO.setContactID("123");
        fcmJDO.setEmailID("new@gmail.com");
        fcmJDO.setSyncObject(syncObjectString);
        try (MockedConstruction<FcmImpl> mock = Mockito.mockConstruction(FcmImpl.class, (fcmImplMock, context) -> {
             Mockito.doNothing().when(fcmImplMock).deleteDevice(any());
             Mockito.when(fcmImplMock.getFcmDeviceInfoById(anyString())).thenReturn(fcmJDO);
        })){
            Map<String, Object> expectedMap = new HashMap<>();
            expectedMap.put(FcmHelper.IS_DEVICE_UN_REGISTERED, true);
            Map<String, Object> responseMap = new FcmHelper().deleteDeviceInfo("deviceID");
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void persistFcmInfo_exception_test(){

        FcmJDO fcmJDO = new FcmJDO();
        try (MockedConstruction<FcmImpl> mock = Mockito.mockConstruction(FcmImpl.class, (fcmImplMock, context) -> {
            Mockito.when(fcmImplMock.saveFcm(any(FcmJDO.class))).thenThrow(new NullPointerException());
        })){
            Map<String, Object> dataPersistenceMap = new HashMap<>();
            dataPersistenceMap.put(FcmHelper.IS_DATA_PERSISTED, false);
            dataPersistenceMap.put(FcmHelper.ERROR_MESSAGE, null);
            Map<String, Object> expectedMap = new HashMap<>();
            expectedMap.put(FcmHelper.DEVICE_PERSISTED_INFO,dataPersistenceMap);
            Map<String, Object> responseMap = new FcmHelper().persistFcmInfo(fcmJDO);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void persistFcmInfo_valid_test(){

        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setAccountID("123");
        fcmJDO.setContactID("123");
        fcmJDO.setEmailID("new@gmail.com");
        fcmJDO.setSyncObject("test");
        try (MockedConstruction<FcmImpl> mock = Mockito.mockConstruction(FcmImpl.class, (fcmImplMock, context) -> {
            Mockito.when(fcmImplMock.saveFcm(any(FcmJDO.class))).thenReturn(fcmJDO);
        })){
            Map<String, Object> dataPersistenceMap = new HashMap<>();
            dataPersistenceMap.put(FcmHelper.IS_DATA_PERSISTED, true);
            Map<String, Object> expectedMap = new HashMap<>();
            expectedMap.put(FcmHelper.DEVICE_PERSISTED_INFO,dataPersistenceMap);
            Map<String, Object> responseMap = new FcmHelper().persistFcmInfo(fcmJDO);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

}
