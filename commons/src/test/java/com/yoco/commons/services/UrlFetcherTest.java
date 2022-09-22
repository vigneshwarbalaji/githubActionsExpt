package com.yoco.commons.services;

import com.yoco.commons.utils.JsonUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.Mockito;
import java.io.IOException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;

class UrlFetcherTest{

    @Test
    void getHttpClientInstance_test(){
        HttpClient httpClient = UrlFetcher.getHttpClientInstance();
        assertEquals(HttpClient.Version.HTTP_2,httpClient.version());
        assertEquals(Duration.ofSeconds(60),httpClient.connectTimeout().get());
    }

    @ParameterizedTest
    @NullAndEmptySource
    void sendGetRequest_invalid_URL_Request_test(String testValue) throws IOException {
        assertEquals(new HashMap<>(), UrlFetcher.sendGetRequest(testValue, new String[] {}, HttpClient.newHttpClient()));
    }

    @Test
    void sendGetRequest_invalid_headers_Request_test() throws IOException {
        assertEquals(new HashMap<>(), UrlFetcher.sendGetRequest("https://url", new String[] {}, HttpClient.newHttpClient()));
    }

    @Test
    void sendGetRequest_responseNull_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(null);
        assertEquals(new HashMap<>(), UrlFetcher.sendGetRequest("https://url", new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendGetRequest_responseBodyNull_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn(null);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        assertEquals(new HashMap<>(), UrlFetcher.sendGetRequest("https://url", new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendGetRequest_responseBodyInvalidJson_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn("invalid");
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        assertEquals(new HashMap<>(), UrlFetcher.sendGetRequest("https://url", new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendGetRequest_validResponse_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn("{\"success\":true}");
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        assertEquals(Map.of("success",true), UrlFetcher.sendGetRequest("https://url", new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendGetRequest_interrupted_Exception_test() throws IOException,InterruptedException {
        HttpResponse<String> mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn(null);
        HttpClient client = Mockito.mock(HttpClient.class);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenThrow(new InterruptedException());
        UrlFetcher.sendGetRequest("https://url",new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON},client);
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void sendGetRequestWithBooleanResp_nullRequest_test(String testValue) throws IOException {
        assertFalse(UrlFetcher.sendGetRequestWithBooleanResp(testValue, new String[] {}, HttpClient.newHttpClient()));
    }

    @Test
    void sendGetRequestWithBooleanResp_responseNull_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(null);
        assertFalse(UrlFetcher.sendGetRequestWithBooleanResp("https://url", new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendGetRequestWithBooleanResp_responseBodyNull_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn(null);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        assertFalse(UrlFetcher.sendGetRequestWithBooleanResp("https://url", new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendGetRequestWithBooleanResp_validResponse_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn("true");
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        assertTrue(UrlFetcher.sendGetRequestWithBooleanResp("https://url",new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendGetRequestWithBooleanResp_interrupted_Exception_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenThrow(new InterruptedException());
        assertFalse(UrlFetcher.sendGetRequestWithBooleanResp("https://url",null,client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }


    @ParameterizedTest
    @NullAndEmptySource
    void sendPostRequest_nullRequest_test(String testValue) throws IOException {
        assertEquals(new HashMap<>(), UrlFetcher.sendPostRequest(testValue, new HashMap<>(), new String[] {}, HttpClient.newHttpClient()));
    }

    @Test
    void sendPostRequest_responseNull_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(null);
        assertEquals(new HashMap<>(), UrlFetcher.sendPostRequest("https://url", new HashMap<>(), new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendPostRequest_responseBodyNull_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn(null);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        assertEquals(new HashMap<>(), UrlFetcher.sendPostRequest("https://url", new HashMap<>(), new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendPostRequest_responseBodyInvalidJson_test() throws IOException,InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn("invalid");
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        assertEquals(new HashMap<>(), UrlFetcher.sendPostRequest("https://url", new HashMap<>(), new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendPostRequest_validResponse_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn("{\"success\":true}");
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        Map<String,Object> payloadMap = new HashMap<>();
        payloadMap.put("success",true);
        assertEquals(payloadMap, UrlFetcher.sendPostRequest("https://url", new HashMap<>(), new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendPostRequest_interrupted_Exception_test() throws IOException, InterruptedException {
        HttpResponse<String> mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn(null);
        HttpClient client = Mockito.mock(HttpClient.class);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenThrow(new InterruptedException());

        Map<String,Object> payloadMap = new HashMap<>();
        UrlFetcher.sendPostRequest("https://url",payloadMap,new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON},client);
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void sendPostRequest_nullRequest_test2(String testValue) throws IOException {
        assertEquals(new HashMap<>(), UrlFetcher.sendPostRequest(testValue, "", new String[] {}, HttpClient.newHttpClient()));
    }

    @Test
    void sendPostRequest_validResponse_test2() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn("{\"success\":true}");
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        Map<String,Object> payloadMap = new HashMap<>();
        payloadMap.put("success",true);
        assertEquals(payloadMap, UrlFetcher.sendPostRequest("https://url", "payload", new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void sendPutRequest_nullRequest_test(String testValue) throws IOException {
        assertEquals(new HashMap<>(), UrlFetcher.sendPutRequest(testValue, new HashMap<>(), new String[] {}, HttpClient.newHttpClient()));
    }

    @Test
    void sendPutRequest_responseNull_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(null);
        assertEquals(new HashMap<>(), UrlFetcher.sendPutRequest("https://url", new HashMap<>(),new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendPutRequest_responseBodyNull_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn(null);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        assertEquals(new HashMap<>(), UrlFetcher.sendPutRequest("https://url", new HashMap<>(),new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendPutRequest_responseBodyInvalidJson_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn("invalid");
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        assertEquals(new HashMap<>(), UrlFetcher.sendPutRequest("https://url", new HashMap<>(), new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendPutRequest_validResponse_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn("{\"success\":true}");
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        Map<String,Object> payloadMap = new HashMap<>();
        payloadMap.put("success",true);
        assertEquals(payloadMap, UrlFetcher.sendPutRequest("https://url", new HashMap<>(), new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendPutRequest_interrupted_Exception_test() throws IOException, InterruptedException {
        HttpResponse<String> mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn(null);
        HttpClient client = Mockito.mock(HttpClient.class);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenThrow(new InterruptedException());

        Map<String,Object> payloadMap = new HashMap<>();
        UrlFetcher.sendPutRequest("https://url",payloadMap,new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON},client);
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }


    @ParameterizedTest
    @NullAndEmptySource
    void sendDeleteRequest_nullRequest_test(String testValue) throws IOException {
        assertEquals(new HashMap<>(), UrlFetcher.sendDeleteRequest(testValue, new String[] {}, HttpClient.newHttpClient()));
    }

    @Test
    void sendDeleteRequest_responseNull_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(null);
        assertEquals(new HashMap<>(), UrlFetcher.sendDeleteRequest("https://url", new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendDeleteRequest_responseBodyNull_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn(null);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        assertEquals(new HashMap<>(), UrlFetcher.sendDeleteRequest("https://url", new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendDeleteRequest_responseBodyInvalidJson_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn("invalid");
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        assertEquals(new HashMap<>(), UrlFetcher.sendDeleteRequest("https://url", new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendDeleteRequest_validResponse_test() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn("{\"success\":true}");
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(mockResponse);
        Map<String,Object> payloadMap = new HashMap<>();
        payloadMap.put("success",true);
        assertEquals(payloadMap, UrlFetcher.sendDeleteRequest("https://url",new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON}, client));
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendDeleteRequest_interrupted_Exception_test() throws IOException, InterruptedException {
        HttpResponse<String> mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn(null);
        HttpClient client = Mockito.mock(HttpClient.class);
        Mockito.when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenThrow(new InterruptedException());
        UrlFetcher.sendDeleteRequest("https://url",new String[] {UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON},client);
        Mockito.verify(client, times(1)).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
    }

    @Test
    void sendPatchRequest_invalidUrl_shouldReturnEmptyMap() throws IOException {
        Assertions.assertEquals(Map.of(),UrlFetcher.sendPatchRequest("","",new String[]{}, UrlFetcher.getHttpClientInstance()));
    }

    @Test
    void sendPatchRequest_validUrl_shouldReturnMockedResponse() throws IOException, InterruptedException {
        HttpClient client = Mockito.mock(HttpClient.class);
        HttpResponse mockResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(mockResponse.body()).thenReturn(JsonUtil.getJson(Map.of("success",true)));
        Mockito.when(client.send(any(HttpRequest.class),eq(HttpResponse.BodyHandlers.ofString()))).thenReturn(mockResponse);
        Assertions.assertEquals(Map.of("success",true),UrlFetcher.sendPatchRequest("https://www.test.com","",null, client));
    }
}
