package com.yoco.commons.fullservices;

import co.anywhere.awconfigurator.model.ratelimit.RateLimitInfo;
import co.anywhere.awconfigurator.services.RateLimitApi;
import com.yoco.commons.utils.GaeUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.HashSet;

import static org.mockito.ArgumentMatchers.*;

class RateConfiguratorTest {

    @ParameterizedTest
    @NullAndEmptySource
    void checkRateLimitUsage_null_test(String testValue) {
        Assertions.assertNull(RateConfigurator.checkRateLimitUsage(testValue,2,1000));
    }

    @Test
    void checkRateLimitUsage_valid_test() {
        RateLimitInfo rateLimitInfo = new RateLimitInfo();
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedConstruction<RateLimitApi> rateLimitApiMockedConstruction = Mockito.mockConstruction(RateLimitApi.class,(mock,context)-> {
                Mockito.when(mock.check(any())).thenReturn(rateLimitInfo);
            })){

            fullAuthServiceMockedStatic.when(()-> FullAuthService.getServerAccessToken(anyString(),anySet())).thenReturn("serverToken");
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            Assertions.assertEquals(rateLimitInfo,RateConfigurator.checkRateLimitUsage("rateKey",2,1000));
            fullAuthServiceMockedStatic.verify(()-> FullAuthService.getServerAccessToken("rateLimitingServerToken",new HashSet<>(){{add("awconfigurator.ratelimit.access");}}));
        }
    }

    @Test
    void checkRateLimitUsage_exception_test() {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedConstruction<RateLimitApi> rateLimitApiMockedConstruction = Mockito.mockConstruction(RateLimitApi.class,(mock,context)-> {
                Mockito.when(mock.check(any())).thenThrow(new IllegalArgumentException("exception"));
            })){

            fullAuthServiceMockedStatic.when(()-> FullAuthService.getServerAccessToken(anyString(),anySet())).thenReturn("serverToken");
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            Assertions.assertNull(RateConfigurator.checkRateLimitUsage("rateKey",2,1000));
            fullAuthServiceMockedStatic.verify(()-> FullAuthService.getServerAccessToken("rateLimitingServerToken",new HashSet<>(){{add("awconfigurator.ratelimit.access");}}));
        }
    }

}