package com.yoco.commons.utils;

import com.yoco.commons.enums.AppMode;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mockStatic;

class GaeUtilsTest {

    @Test
    void getAppId_valid_test() {
        assertEquals(null, GaeUtils.getAppId());
    }

    @Test
    void getAppMode_dev_test(){
        try (MockedStatic<GaeUtils> gaeUtilsMock = mockStatic(GaeUtils.class)) {
            gaeUtilsMock.when(() -> GaeUtils.getAppId()).thenReturn(null);
            gaeUtilsMock.when(() -> GaeUtils.getAppMode()).thenCallRealMethod();
            assertEquals(AppMode.DEV,GaeUtils.getAppMode());
        }
    }

    @Test
    void getAppMode_dev_test2(){
        try (MockedStatic<GaeUtils> gaeUtilsMock = mockStatic(GaeUtils.class)) {
            gaeUtilsMock.when(() -> GaeUtils.getAppId()).thenReturn(AppMode.DEV.toString());
            gaeUtilsMock.when(() -> GaeUtils.getAppMode()).thenCallRealMethod();
            assertEquals(AppMode.DEV,GaeUtils.getAppMode());
        }
    }

    @Test
    void getAppMode_staging_test(){
        try (MockedStatic<GaeUtils> gaeUtilsMock = mockStatic(GaeUtils.class)) {
            gaeUtilsMock.when(() -> GaeUtils.getAppId()).thenReturn(GaeUtils.APP_ID_STAGING);
            gaeUtilsMock.when(() -> GaeUtils.getAppMode()).thenCallRealMethod();
           assertEquals(AppMode.STAGING,GaeUtils.getAppMode());
        }
    }

    @Test
    void getAppMode_live_test(){
        try (MockedStatic<GaeUtils> gaeUtilsMock = mockStatic(GaeUtils.class)) {
            gaeUtilsMock.when(() -> GaeUtils.getAppId()).thenReturn(GaeUtils.APP_ID_LIVE);
            gaeUtilsMock.when(() -> GaeUtils.getAppMode()).thenCallRealMethod();
            assertEquals(AppMode.LIVE,GaeUtils.getAppMode());
        }
    }

    @Test
    void isAppModeLive_true_test(){
        try (MockedStatic<GaeUtils> gaeUtilsMock = mockStatic(GaeUtils.class)) {
            gaeUtilsMock.when(GaeUtils::getAppId).thenReturn(GaeUtils.APP_ID_LIVE);
            gaeUtilsMock.when(GaeUtils::getAppMode).thenCallRealMethod();
            gaeUtilsMock.when(GaeUtils::isAppModeLive).thenCallRealMethod();
            assertTrue(GaeUtils.isAppModeLive());
        }
    }

    @Test
    void isAppModeLive_false_test(){
        try (MockedStatic<GaeUtils> gaeUtilsMock = mockStatic(GaeUtils.class)) {
            gaeUtilsMock.when(GaeUtils::getAppId).thenReturn(GaeUtils.APP_ID_STAGING);
            gaeUtilsMock.when(GaeUtils::getAppMode).thenCallRealMethod();
            gaeUtilsMock.when(GaeUtils::isAppModeLive).thenCallRealMethod();
            assertFalse(GaeUtils.isAppModeLive());
        }
    }

}
