package com.yoco.commons.utils;

import com.yoco.commons.dataservices.impl.FcmImpl;
import com.yoco.commons.entity.FcmJDO;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.utils.fcm.FcmUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.List;

class FcmUtilTest {
    @Test
    void getAllDevicesUnderAccount_nullAccID_test(){
        try(MockedStatic<FcmImpl> fcmMockedStatic = Mockito.mockStatic(FcmImpl.class)){
            FcmUtil.getAllDevicesUnderAccount(null);
            fcmMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void getAllDevicesUnderAccount_validAccID_test(){
        try(MockedStatic<FcmImpl> fcmMockedStatic = Mockito.mockStatic(FcmImpl.class)){
            FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
            fcmMockedStatic.when(FcmImpl::getFcmImplInstance).thenReturn(fcmMock);
            CollectionResponse<FcmJDO> respWithCursor = new CollectionResponse<>();
            respWithCursor.setItems(List.of(new FcmJDO(),new FcmJDO()));
            respWithCursor.setCursor("cursor");
            CollectionResponse<FcmJDO> respWithoutCursor = new CollectionResponse<>();
            respWithoutCursor.setItems(List.of(new FcmJDO(),new FcmJDO()));
            respWithoutCursor.setCursor(null);
            Mockito.when(fcmMock.getAllDevicesUnderAccount("accID",1000,"")).thenReturn(respWithCursor);
            Mockito.when(fcmMock.getAllDevicesUnderAccount("accID",1000,"cursor")).thenReturn(respWithoutCursor);
            Assertions.assertEquals(4,FcmUtil.getAllDevicesUnderAccount("accID").size());
        }
    }

    @Test
    void deleteAllDevicesInAccountAndReturnDeletedDeviceTokens_noDevices_Test(){
        try(MockedStatic<FcmUtil> fcmUtilMockedStatic = Mockito.mockStatic(FcmUtil.class)){
            fcmUtilMockedStatic.when(()->FcmUtil.getAllDevicesUnderAccount("accID")).thenReturn(List.of());
            fcmUtilMockedStatic.when(()->FcmUtil.deleteAllDevicesInAccountAndReturnDeletedDeviceTokens("accID")).thenCallRealMethod();
            Assertions.assertEquals(List.of(),FcmUtil.deleteAllDevicesInAccountAndReturnDeletedDeviceTokens("accID"));
        }
    }

    @Test
    void deleteAllDevicesInAccountAndReturnDeletedDeviceTokens_valid_Test(){
        try(MockedStatic<FcmUtil> fcmUtilMockedStatic = Mockito.mockStatic(FcmUtil.class);
            MockedStatic<FcmImpl> fcmMockedStatic = Mockito.mockStatic(FcmImpl.class)){
            FcmImpl fcmMock = Mockito.mock(FcmImpl.class);
            fcmMockedStatic.when(FcmImpl::getFcmImplInstance).thenReturn(fcmMock);
            FcmJDO fcm1 = new FcmJDO();
            fcm1.setFcmDeviceToken("1");
            FcmJDO fcm2 = new FcmJDO();
            fcm2.setFcmDeviceToken("2");
            fcmUtilMockedStatic.when(()->FcmUtil.getAllDevicesUnderAccount("accID")).thenReturn(List.of(fcm1,fcm2));
            fcmUtilMockedStatic.when(()->FcmUtil.deleteAllDevicesInAccountAndReturnDeletedDeviceTokens("accID")).thenCallRealMethod();
            Assertions.assertEquals(List.of("1","2"),FcmUtil.deleteAllDevicesInAccountAndReturnDeletedDeviceTokens("accID"));
            Mockito.verify(fcmMock).delete(List.of(fcm1,fcm2));
        }
    }
}
