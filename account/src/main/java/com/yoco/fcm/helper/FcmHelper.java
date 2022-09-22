package com.yoco.fcm.helper;

import com.yoco.commons.dataservices.impl.FcmImpl;
import com.yoco.commons.entity.FcmJDO;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.fcm.enums.FCM_ERROR_MESSAGE;

import java.util.*;

public class FcmHelper {

    public static FcmHelper getInstance(){
        return new FcmHelper();
    }

    public static final String IS_DEVICE_INFO_AVAILABLE = "isDeviceInfoAvailable";
    public static final String SYNC_OBJECT = "SyncObject";
    public static final String IS_SYNC_OBJECT_UPDATED = "isSyncObjectUpdated";
    public static final String ERROR_MESSAGE = "Error Message";
    public static final String IS_DEVICE_UN_REGISTERED = "isDeviceUnRegister";
    public static final String SYNC_OBJECT_INFO = "SyncObjectInfo";
    public static final String UNREGISTER_DEVICE_INFO = "unRegisterDeviceInfo";
    public static final String DEVICE_PERSISTED_INFO = "DevicePersistedInfo";
    public static final String IS_DATA_PERSISTED = "isDataPersisted";

    public Map<String, Object> getFcmSyncInfo(String deviceID) {

        Map<String, Object> responseMap = new HashMap<>();
        var fcmInfoObject = FcmImpl.getFcmImplInstance().getFcmDeviceInfoById(deviceID);
        if(!ObjUtils.isNull(fcmInfoObject)){
            responseMap = updateSyncObject(fcmInfoObject);
            responseMap.put(IS_DEVICE_INFO_AVAILABLE, true);
        }else{
            responseMap.put(IS_DEVICE_INFO_AVAILABLE, false);
            responseMap.put(ERROR_MESSAGE, FCM_ERROR_MESSAGE.DEVICE_NOT_FOUND.value());
        }

        return responseMap;
    }

    public Map<String, Object> updateSyncObject(FcmJDO fcmInfoObject){
        String syncObject = fcmInfoObject.getSyncObject();
        Map<String, Object>responseMap = new HashMap<>();

        if(!ObjUtils.isNullOrEmpty(syncObject)){
            fcmInfoObject.setSyncObject("");
            FcmImpl.getFcmImplInstance().saveFcm(fcmInfoObject);
            responseMap.put(IS_SYNC_OBJECT_UPDATED, true);
            responseMap.put(SYNC_OBJECT, JsonUtil.convertJsonToMap(syncObject));
        } else {
            responseMap.put(IS_SYNC_OBJECT_UPDATED, false);
        }

        return responseMap;
    }

    public Map<String, Object> deleteDeviceInfo(String deviceID) {

        Map<String, Object> responseMap = new HashMap<>();
        var fcmInfoObject = FcmImpl.getFcmImplInstance().getFcmDeviceInfoById(deviceID);

        if(!ObjUtils.isNull(fcmInfoObject)){
            FcmImpl.getFcmImplInstance().deleteDevice(fcmInfoObject);
            responseMap.put(IS_DEVICE_UN_REGISTERED, true);
        }else{
            responseMap.put(IS_DEVICE_UN_REGISTERED, false);
            responseMap.put(ERROR_MESSAGE, FCM_ERROR_MESSAGE.DEVICE_ID_NOT_FOUND.value());
        }

        return responseMap;
    }

    public Map<String, Object> persistFcmInfo(FcmJDO fcmObject){
        Map<String, Object> responseMap = new HashMap<>();
        Map<String, Object> dataPersistenceMap = new HashMap<>();
        try{
            FcmImpl.getFcmImplInstance().saveFcm(fcmObject);
            dataPersistenceMap.put(IS_DATA_PERSISTED,true);
        }catch (Exception e){
            dataPersistenceMap.put(ERROR_MESSAGE, e.getMessage());
            dataPersistenceMap.put(IS_DATA_PERSISTED,false);
        }
        responseMap.put(DEVICE_PERSISTED_INFO, dataPersistenceMap);
        return responseMap;
    }

}
