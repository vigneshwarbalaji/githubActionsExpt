package com.yoco.commons.utils.fcm;

import com.yoco.commons.dataservices.impl.FcmImpl;
import com.yoco.commons.entity.FcmJDO;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.utils.ObjUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class FcmUtil {
    private FcmUtil(){}

    public static List<String> deleteAllDevicesInAccountAndReturnDeletedDeviceTokens(String accountID){
        List<FcmJDO> devicesUnderAccount = getAllDevicesUnderAccount(accountID);
        if(!ObjUtils.isNullOrEmpty(devicesUnderAccount)){
            FcmImpl.getFcmImplInstance().delete((List)devicesUnderAccount);
            return devicesUnderAccount.stream().map(FcmJDO::getFcmDeviceToken).collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    public static List<FcmJDO> getAllDevicesUnderAccount(String accountID){
        if(ObjUtils.isNullOrEmpty(accountID)){
            return new ArrayList<>();
        }
        var fcmImpl = FcmImpl.getFcmImplInstance();
        List<FcmJDO> devices = new ArrayList<>();
        var isQueryIncomplete = true;
        CollectionResponse<FcmJDO> fcmJDOCollectionResponse;
        var cursor = "";
        while(isQueryIncomplete){
            fcmJDOCollectionResponse  = fcmImpl.getAllDevicesUnderAccount(accountID,1000,cursor);
            devices.addAll(fcmJDOCollectionResponse.getItems());
            if(!ObjUtils.isNullOrEmpty(fcmJDOCollectionResponse.getCursor())){
                cursor = fcmJDOCollectionResponse.getCursor();
            }else{
                isQueryIncomplete = false;
            }
        }
        return devices;
    }
}
