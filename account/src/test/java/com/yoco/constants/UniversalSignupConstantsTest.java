package com.yoco.constants;

import com.yoco.commons.constants.UniversalSignupConstants;
import com.yoco.commons.enums.AppMode;
import com.yoco.commons.utils.GaeUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

class UniversalSignupConstantsTest {
    @Test
    void universalSignupConstantsLive_Test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.LIVE);
            UniversalSignupConstants.initializeUniversalSignupConstants(AppMode.LIVE);
            Assertions.assertEquals("https://live-universalsignup.appspot.com/api/freesignup",UniversalSignupConstants.getFreeSignupApiUrl());
        }
    }

    @Test
    void universalSignupConstantsStaging_Test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.STAGING);
            UniversalSignupConstants.initializeUniversalSignupConstants(AppMode.STAGING);
            Assertions.assertEquals("https://staging-universalsignup.appspot.com/api/freesignup",UniversalSignupConstants.getFreeSignupApiUrl());
        }
    }
}
