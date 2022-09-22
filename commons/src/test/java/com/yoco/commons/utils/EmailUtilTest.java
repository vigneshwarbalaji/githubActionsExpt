package com.yoco.commons.utils;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.mockito.Mockito.times;

class EmailUtilTest {

    @Test
    void getSubject_staging_test() {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            Assertions.assertEquals("test (staging-goclockin-dashboard) ",EmailUtil.getSubject("test"));
            gaeUtilsMockedStatic.verify(GaeUtils::isAppModeLive,times(1));
        }
    }

    @Test
    void getSubject_live_test() {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(true);
            Assertions.assertEquals("test",EmailUtil.getSubject("test"));
            gaeUtilsMockedStatic.verify(GaeUtils::isAppModeLive,times(1));
        }
    }
}