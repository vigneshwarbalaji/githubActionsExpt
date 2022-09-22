package com.yoco.fcm.service;

import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.FcmJDO;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.fcm.enums.FCM_ERROR_MESSAGE;
import com.yoco.fcm.helper.FcmHelper;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

@Slf4j
public class FcmService {

    public Map<String, Object> getFcmSyncInfo(String accountID, String contactID, String deviceID) {

        PeopleRelationJDO userPRO = UserPROUtil.validateAndExtractUserPRO(accountID, contactID);
        Validator.checkArgument(ObjUtils.isNull(userPRO), FCM_ERROR_MESSAGE.USER_NOT_ASSOCIATED.value());
        Map<String, Object>responseMap = new HashMap<>();

        if(!ObjUtils.isNullOrEmpty(deviceID)){
            responseMap = FcmHelper.getInstance().getFcmSyncInfo(deviceID);
        }else{
            responseMap.put(FcmHelper.IS_DEVICE_INFO_AVAILABLE, false);
            responseMap.put(FcmHelper.ERROR_MESSAGE, FCM_ERROR_MESSAGE.INVALID_DEVICE_ID);
        }

        return responseMap;
    }

    public Map<String, Object> deleteDeviceInfo(String accountID, String contactID, String deviceID) {

        PeopleRelationJDO userPRO = UserPROUtil.validateAndExtractUserPRO(accountID, contactID);
        Validator.checkArgument(ObjUtils.isNull(userPRO), FCM_ERROR_MESSAGE.USER_NOT_ASSOCIATED.value());
        Map<String, Object>responseMap = new HashMap<>();

        if(!ObjUtils.isNullOrEmpty(deviceID)){
            responseMap = FcmHelper.getInstance().deleteDeviceInfo(deviceID);
        }else{
            responseMap.put(FcmHelper.IS_DEVICE_UN_REGISTERED, false);
            responseMap.put(FcmHelper.ERROR_MESSAGE, FCM_ERROR_MESSAGE.INVALID_DEVICE_ID.value());
        }

        return responseMap;
    }

    public Map<String, Object> registerDeviceInfo(String accountID, Contact loggedInContactInfo, String payload) {
        String contactID = loggedInContactInfo.getId();
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload), FCM_ERROR_MESSAGE.EMPTY_PAYLOAD.value());
        PeopleRelationJDO userPRO = UserPROUtil.validateAndExtractUserPRO(accountID, contactID);
        Validator.checkArgument(ObjUtils.isNull(userPRO), FCM_ERROR_MESSAGE.USER_NOT_ASSOCIATED.value());
        FcmJDO fcmObject = JsonUtil.convertSafeToType(payload, FcmJDO.class);
        fcmObject.setAccountID(accountID);
        fcmObject.setContactID(loggedInContactInfo.getId());
        fcmObject.setEmailID(loggedInContactInfo.getEmailID());

        return FcmHelper.getInstance().persistFcmInfo(fcmObject);
    }

}
