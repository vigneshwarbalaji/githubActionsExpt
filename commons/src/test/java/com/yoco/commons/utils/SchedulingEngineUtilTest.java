package com.yoco.commons.utils;

import com.yoco.commons.constants.SchedulingEngineUrlConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.enums.AppMode;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.net.http.HttpClient;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;

class SchedulingEngineUtilTest {

    @Test
    void getEventByID_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.LIVE);
            SchedulingEngineUrlConstants.initializeSchedulingEngineConstants(AppMode.LIVE);
            Map<String,Object> response = new HashMap(){{put("entries","entries");}};
            urlFetcherMockedStatic.when(()->UrlFetcher.sendGetRequest(eq("https://events-dot-schedulingengine.uc.r.appspot.com/api/v1/events/123"), eq(new String[]{}),any(HttpClient.class))).thenReturn(response);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenCallRealMethod();
            Assertions.assertEquals(response, SchedulingEngineUtil.getEventByID("123"));
        }
    }

   @Test
    void getEventByQueryStringLiveModeValid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.LIVE);
            SchedulingEngineUrlConstants.initializeSchedulingEngineConstants(AppMode.LIVE);
            Map<String,Object> response = new HashMap(){{put("entries","entries");}};
            urlFetcherMockedStatic.when(()->UrlFetcher.sendGetRequest(eq("https://events-dot-schedulingengine.uc.r.appspot.com/api/v1/events/fetch?q=%7B%7D"), eq(new String[]{}),any(HttpClient.class))).thenReturn(response);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenCallRealMethod();
            Assertions.assertEquals(response, SchedulingEngineUtil.getEventByQueryString(new HashMap<>()));
        }
    }

    @Test
    void getEventByQueryStringStagingModeValid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            SchedulingEngineUrlConstants.initializeSchedulingEngineConstants(AppMode.STAGING);
            Map<String,Object> response = new HashMap(){{put("entries","entries");}};
            urlFetcherMockedStatic.when(()->UrlFetcher.sendGetRequest(eq("https://events-dot-staging-schedulingengine.appspot.com/api/v1/events/fetch?q=%7B%7D"), eq(new String[]{}),any(HttpClient.class))).thenReturn(response);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenCallRealMethod();
            Assertions.assertEquals(response,SchedulingEngineUtil.getEventByQueryString(new HashMap<>()));
        }
    }

    @Test
    void updateEventReq_ValidLive_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.LIVE);
            Map<String,Object> response = new HashMap<>();
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPutRequest(eq("https://events-dot-schedulingengine.uc.r.appspot.com/api/v1/events/254"),eq(response), eq(new String[]{}),any(HttpClient.class))).thenReturn(response);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenCallRealMethod();
            Map<String,Object> payload = new HashMap<>();
            payload.put(SchedulingKeys.ID,"254");
            Assertions.assertEquals(response,SchedulingEngineUtil.updateEventReq(payload));
        }
    }

    @Test
    void updateEventReq_ValidStaging_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            SchedulingEngineUrlConstants.initializeSchedulingEngineConstants(AppMode.STAGING);
            Map<String,Object> response = new HashMap<>();
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPutRequest(eq("https://staging-schedulingengine.appspot.com/api/v1/events/254"),eq(response), eq(new String[]{}),any(HttpClient.class))).thenReturn(response);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenCallRealMethod();
            Map<String,Object> payload = new HashMap<>();
            payload.put(SchedulingKeys.ID,"254");
            Assertions.assertEquals(response,SchedulingEngineUtil.updateEventReq(payload));
        }
    }

    @Test
    void updateBatchEventsReq_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            SchedulingEngineUrlConstants.initializeSchedulingEngineConstants(AppMode.STAGING);
            Map<String,Object> response = new HashMap<>();
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPutRequest(eq("https://staging-schedulingengine.appspot.com/api/v1/events/batch"),eq(response), eq(new String[]{}),any(HttpClient.class))).thenReturn(response);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenCallRealMethod();
            Map<String,Object> payload = new HashMap<>();
            payload.put(SchedulingKeys.ID,"254");
            Assertions.assertEquals(response,SchedulingEngineUtil.updateBatchEventsReq(payload));
        }
    }

    @Test
    void createEvent_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            SchedulingEngineUrlConstants.initializeSchedulingEngineConstants(AppMode.STAGING);
            Map<String,Object> response = new HashMap<>();
            Map<String,Object> payload = new HashMap<>();
            payload.put(SchedulingKeys.ID,"254");
            String payloadStr = JsonUtil.getJson(payload);
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest(eq("https://staging-schedulingengine.appspot.com/api/v1/events"),eq(payloadStr), eq(new String[]{}),any(HttpClient.class))).thenReturn(response);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenCallRealMethod();
            Assertions.assertEquals(response,SchedulingEngineUtil.createEvent(payloadStr));
        }
    }
}
