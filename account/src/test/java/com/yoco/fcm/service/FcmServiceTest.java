package com.yoco.fcm.service;

import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.FcmJDO;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.fcm.enums.FCM_ERROR_MESSAGE;
import com.yoco.fcm.helper.FcmHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;

class FcmServiceTest {

    @Test
    void getFcmSyncInfo_ifDeviceIdIsNullOrEmpty_test(){

        Map<String, Object> expectedMap = new HashMap<>();
        expectedMap.put(FcmHelper.IS_DEVICE_INFO_AVAILABLE, false);
        expectedMap.put(FcmHelper.ERROR_MESSAGE, FCM_ERROR_MESSAGE.INVALID_DEVICE_ID);

        try (MockedStatic<UserPROUtil> userProUtilMockedStatic = mockStatic(UserPROUtil.class)){
            PeopleRelationJDO peopleRelationJDO = new PeopleRelationJDO();
            peopleRelationJDO.setUniquepin("123");
            peopleRelationJDO.setContactId("123");
            userProUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO(anyString(),anyString())).thenReturn(peopleRelationJDO);
            Map<String, Object>responseMap = new FcmService().getFcmSyncInfo("123","123","");
            Assertions.assertEquals(expectedMap, responseMap);
        }
    }

    @Test
    void getFcmSyncInfo_valid_test(){

        Map<String, Object> expectedMap = new HashMap<>();
        expectedMap.put("dummy", "mock");

        try (MockedStatic<UserPROUtil> userProUtilMockedStatic = mockStatic(UserPROUtil.class);
             MockedConstruction<FcmHelper> mock = Mockito.mockConstruction(FcmHelper.class, (fcmHelperMock, context) -> {
                 Mockito.when(fcmHelperMock.getFcmSyncInfo(anyString())).thenReturn(expectedMap);
             })){
            PeopleRelationJDO peopleRelationJDO = new PeopleRelationJDO();
            peopleRelationJDO.setUniquepin("123");
            peopleRelationJDO.setContactId("123");
            userProUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO(anyString(),anyString())).thenReturn(peopleRelationJDO);
            Map<String, Object>responseMap = new FcmService().getFcmSyncInfo("123","123","123");
            Assertions.assertEquals(expectedMap, responseMap);
        }
    }

    @Test
    void deleteDeviceInfo_ifDeviceIdIsNullOrEmpty_test(){

        Map<String, Object> expectedMap = new HashMap<>();
        expectedMap.put(FcmHelper.IS_DEVICE_UN_REGISTERED, false);
        expectedMap.put(FcmHelper.ERROR_MESSAGE, FCM_ERROR_MESSAGE.INVALID_DEVICE_ID.value());

        try (MockedStatic<UserPROUtil> userProUtilMockedStatic = mockStatic(UserPROUtil.class)){
            PeopleRelationJDO peopleRelationJDO = new PeopleRelationJDO();
            peopleRelationJDO.setUniquepin("123");
            peopleRelationJDO.setContactId("123");
            userProUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO(anyString(),anyString())).thenReturn(peopleRelationJDO);
            Map<String, Object>responseMap = new FcmService().deleteDeviceInfo("123","123","");
            Assertions.assertEquals(expectedMap, responseMap);
        }
    }

    @Test
    void deleteDeviceInfo_valid_test(){

        Map<String, Object> expectedMap = new HashMap<>();
        expectedMap.put("dummy", "mock");

        try (MockedStatic<UserPROUtil> userProUtilMockedStatic = mockStatic(UserPROUtil.class);
             MockedConstruction<FcmHelper> mock = Mockito.mockConstruction(FcmHelper.class, (fcmHelperMock, context) -> {
                 Mockito.when(fcmHelperMock.deleteDeviceInfo(anyString())).thenReturn(expectedMap);
             })){
            PeopleRelationJDO peopleRelationJDO = new PeopleRelationJDO();
            peopleRelationJDO.setUniquepin("123");
            peopleRelationJDO.setContactId("123");
            userProUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO(anyString(),anyString())).thenReturn(peopleRelationJDO);
            Map<String, Object>responseMap = new FcmService().deleteDeviceInfo("123","123","123");
            Assertions.assertEquals(expectedMap, responseMap);
        }
    }

    @Test
    void registerDeviceInfo_validResponse_test(){

        Map<String, Object>expectedMap = new HashMap<>();
        expectedMap.put("dummy","mock");
        try(MockedStatic<UserPROUtil> userProUtilMockedStatic = mockStatic(UserPROUtil.class);
            MockedStatic<FcmHelper> fcmHelperMockedStatic = Mockito.mockStatic(FcmHelper.class)){
            FcmHelper fcmHelperMock = Mockito.mock(FcmHelper.class);
            Mockito.doReturn(expectedMap).when(fcmHelperMock).persistFcmInfo(any(FcmJDO.class));
            fcmHelperMockedStatic.when(FcmHelper::getInstance).thenReturn(fcmHelperMock);

            PeopleRelationJDO peopleRelationJDO = new PeopleRelationJDO();
            peopleRelationJDO.setUniquepin("123");
            peopleRelationJDO.setContactId("123");
            userProUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO(anyString(),anyString())).thenReturn(peopleRelationJDO);

            Contact contact = new Contact();
            contact.setId("123");
            contact.setEmailID("newEmail@gmail.com");

            FcmJDO fcmObject = new FcmJDO();
            fcmObject.setAccountID("accountID");
            fcmObject.setContactID("123");
            fcmObject.setEmailID("newEmail@gmail.com");
            String payload = JsonUtil.getJson(fcmObject);

            Map<String,Object>responseMap = new FcmService().registerDeviceInfo("accountID", contact, payload);
            Assertions.assertEquals(expectedMap,responseMap);
            Mockito.verify(fcmHelperMock, times(1)).persistFcmInfo(fcmObject);
        }
    }

}
