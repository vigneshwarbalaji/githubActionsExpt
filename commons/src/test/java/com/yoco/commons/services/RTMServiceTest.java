package com.yoco.commons.services;

import com.rtmserver.exception.RTMServerException;
import com.rtmserver.service.CryptoService;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.utils.JsonUtil;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.net.http.HttpClient;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class RTMServiceTest {

    RTMService rtmService = RTMService.getRTMService();

    @Test
    void makeRTMServerRequest_test() throws RTMServerException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){

            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(() -> UrlFetcher.getHttpClientInstance()).thenReturn(client);
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(), anyMap(),any(String[].class),any(HttpClient.class))).thenReturn(new HashMap<>());

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerBaseURL).thenReturn("https://url");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerSecretKey).thenReturn("rtmServerSecretK");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerApiKey).thenReturn("rtmServerApiKey");

            rtmService.makeRTMServerRequest(Map.of());

            String rtmSignature = CryptoService.createSignature(JsonUtil.getJson(Map.of()), "rtmServerSecretK");
            String[] headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,"Bearer service", "X-RTMServer-Signature",rtmSignature };

            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest("https://url/app/sendmessage",Map.of(),headers,UrlFetcher.getHttpClientInstance()));
        }
    }

    @Test
    void makeRTMServerRequest_RTMServerException_test(){
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<CryptoService> cryptoServiceMockedStatic = Mockito.mockStatic(CryptoService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){

            cryptoServiceMockedStatic.when(()-> CryptoService.createSignature(anyString(),anyString())).thenThrow(new RTMServerException("exception"));

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerBaseURL).thenReturn("https://url");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerSecretKey).thenReturn("rtmServerSecretK");

            rtmService.makeRTMServerRequest(Map.of());
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(anyString(), anyMap(),any(String[].class),any(HttpClient.class)),times(0));
        }
    }

    @Test
    void makeRTMServerRequest_IOException_test(){

        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){

            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(() -> UrlFetcher.getHttpClientInstance()).thenReturn(client);
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(), anyMap(),any(String[].class),any(HttpClient.class))).thenThrow(new IOException("exception"));

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerBaseURL).thenReturn("https://url");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerSecretKey).thenReturn("rtmServerSecretK");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerApiKey).thenReturn("rtmServerApiKey");

            rtmService.makeRTMServerRequest(Map.of());
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(anyString(), anyMap(),any(String[].class),any(HttpClient.class)),times(1));
        }
    }

    @Test
    void publishToChannel_valid_test() throws RTMServerException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)) {

            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(() -> UrlFetcher.getHttpClientInstance()).thenReturn(client);
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(), anyMap(),any(String[].class),any(HttpClient.class))).thenReturn(new HashMap<>());

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerBaseURL).thenReturn("https://url");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerSecretKey).thenReturn("rtmServerSecretK");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerApiKey).thenReturn("rtmServerApiKey");

            Map<String, String> objectMap = new HashMap<>();
            objectMap.put("keys", "data");
            rtmService.publishToChannel("accountId", "action", "key", objectMap);

            Map<String, String> channelPublishReq = new HashMap<>();
            channelPublishReq.put("status", "action");
            channelPublishReq.put("key", JsonUtil.getJson(objectMap));

            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put("channel", "public/yoco/accountId");
            payloadMap.put("type", "message");
            payloadMap.put("data", channelPublishReq);

            String rtmSignature = CryptoService.createSignature(JsonUtil.getJson(payloadMap), "rtmServerSecretK");
            String[] headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,"Bearer service", "X-RTMServer-Signature",rtmSignature };

            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest("https://url/app/sendmessage", payloadMap,headers,UrlFetcher.getHttpClientInstance()));
        }
    }

    @Test
    void publishToChannel_nullKey_test() throws RTMServerException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)) {

            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(() -> UrlFetcher.getHttpClientInstance()).thenReturn(client);
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(), anyMap(),any(String[].class),any(HttpClient.class))).thenReturn(new HashMap<>());

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerBaseURL).thenReturn("https://url");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerSecretKey).thenReturn("rtmServerSecretK");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerApiKey).thenReturn("rtmServerApiKey");

            Map<String, String> objectMap = new HashMap<>();
            objectMap.put("keys", "data");
            rtmService.publishToChannel("accountId", "action", null, objectMap);
            Map<String, String> channelPublishReq = new HashMap<>();
            channelPublishReq.put("status", "action");
            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put("channel", "public/yoco/accountId");
            payloadMap.put("type", "message");
            payloadMap.put("data", channelPublishReq);
           // urlFetcherMockedStatic.verify(() -> UrlFetcher.makeRTMServerRequest(payloadMap));
            String rtmSignature = CryptoService.createSignature(JsonUtil.getJson(payloadMap), "rtmServerSecretK");
            String[] headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,"Bearer service", "X-RTMServer-Signature",rtmSignature };

            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest("https://url/app/sendmessage", payloadMap,headers,UrlFetcher.getHttpClientInstance()));
        }
    }

    @Test
    void publishToChannel_nullObjectMap_test() throws RTMServerException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)) {

            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(() -> UrlFetcher.getHttpClientInstance()).thenReturn(client);
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(), anyMap(),any(String[].class),any(HttpClient.class))).thenReturn(new HashMap<>());

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerBaseURL).thenReturn("https://url");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerSecretKey).thenReturn("rtmServerSecretK");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerApiKey).thenReturn("rtmServerApiKey");

            rtmService.publishToChannel("accountId", "action", "key", null);
            Map<String, String> channelPublishReq = new HashMap<>();
            channelPublishReq.put("status", "action");
            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put("channel", "public/yoco/accountId");
            payloadMap.put("type", "message");
            payloadMap.put("data", channelPublishReq);
        //    urlFetcherMockedStatic.verify(() -> UrlFetcher.makeRTMServerRequest(payloadMap));
            String rtmSignature = CryptoService.createSignature(JsonUtil.getJson(payloadMap), "rtmServerSecretK");
            String[] headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,"Bearer service", "X-RTMServer-Signature",rtmSignature };

            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest("https://url/app/sendmessage", payloadMap,headers,UrlFetcher.getHttpClientInstance()));
        }
    }

    @Test
    void publishToAW_valid_test() throws IOException, NoSuchAlgorithmException, RTMServerException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAwChannelPublishUrl).thenReturn("https://publish/");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerSecretKey).thenReturn("rtmServerSecretK");
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("service");
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(),anyMap(),any(),any(HttpClient.class))).thenReturn(null);

            Map<String, String> objectMap = new HashMap<>();
            objectMap.put("keys", "data");
            rtmService.publishToAW("accountId","contactId","action","key",objectMap);

            Map<String, Object> awPayload = new HashMap<>();
            awPayload.put("action", "action");
            awPayload.put("accountID", "accountId");
            awPayload.put("key", objectMap);

            Map<String, Object> awMap = new HashMap<>();
            awMap.put("type", "user");
            awMap.put("typeId", "contactId");
            awMap.put("data", awPayload);

            String rtmSignature = CryptoService.createSignature(JsonUtil.getJson(awMap), "rtmServerSecretK");
            String[] headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,"Bearer service", "X-RTMServer-Signature",rtmSignature };

            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest("https://url/app/sendmessage", awMap,headers,UrlFetcher.getHttpClientInstance()));
        }
    }

    @Test
    void publishToAdminChannel_valid_test() throws RTMServerException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)) {

            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(() -> UrlFetcher.getHttpClientInstance()).thenReturn(client);
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(), anyMap(),any(String[].class),any(HttpClient.class))).thenReturn(new HashMap<>());

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerBaseURL).thenReturn("https://url");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerSecretKey).thenReturn("rtmServerSecretK");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerApiKey).thenReturn("rtmServerApiKey");

            Map<String, String> objectMap = new HashMap<>();
            objectMap.put("keys", "data");
            rtmService.publishToAdminChannel("accountId", objectMap);

            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put("channel", "public/yoco/accountId/admin");
            payloadMap.put("type", "message");
            payloadMap.put("data", objectMap);

            String rtmSignature = CryptoService.createSignature(JsonUtil.getJson(payloadMap), "rtmServerSecretK");
            String[] headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,"Bearer service", "X-RTMServer-Signature",rtmSignature };

            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest("https://url/app/sendmessage", payloadMap,headers,UrlFetcher.getHttpClientInstance()),times(1));
        }
    }

    @Test
    void publishToUserChannel_valid_test() throws RTMServerException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)) {

            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(() -> UrlFetcher.getHttpClientInstance()).thenReturn(client);
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(), anyMap(),any(String[].class),any(HttpClient.class))).thenReturn(new HashMap<>());

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerBaseURL).thenReturn("https://url");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerSecretKey).thenReturn("rtmServerSecretK");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getRtmServerApiKey).thenReturn("rtmServerApiKey");

            Map<String, String> objectMap = new HashMap<>();
            objectMap.put("keys", "data");
            rtmService.publishToUserChannel("accountId","contactId",objectMap);

            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put("channel", "public/yoco/accountId/contact/contactId");
            payloadMap.put("type", "message");
            payloadMap.put("data", objectMap);

            String rtmSignature = CryptoService.createSignature(JsonUtil.getJson(payloadMap), "rtmServerSecretK");
            String[] headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,"Bearer service", "X-RTMServer-Signature",rtmSignature };

            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest("https://url/app/sendmessage", payloadMap,headers,UrlFetcher.getHttpClientInstance()),times(1));
        }
    }
}
