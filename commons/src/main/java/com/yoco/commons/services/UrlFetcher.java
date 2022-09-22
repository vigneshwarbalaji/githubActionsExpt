package com.yoco.commons.services;

import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpMethod;
import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Map;

@Slf4j
public class UrlFetcher {

    private UrlFetcher(){}

    public static final String CONTENT_TYPE = "Content-Type";
    public static final String APPLICATION_JSON = "application/json";
    public static final String APPLICATION_JSON_UTF8 = "application/json; charset=utf-8";
    public static final String AUTHORIZATION = "Authorization";
    public static final String BEARER = "Bearer";


    public static HttpClient getHttpClientInstance(){
        return HttpClient.newBuilder()
                .version(HttpClient.Version.HTTP_2)
                .connectTimeout(Duration.ofSeconds(60))
                .build();
    }

    private static HttpRequest.Builder constructHttpRequestBuilder(String url, String[] headers, HttpRequest.Builder requestBuilder){

        if (ObjUtils.isNullOrEmpty(url))
            return null;

        requestBuilder = requestBuilder.uri(URI.create(url));

        if(headers != null)
            requestBuilder = requestBuilder.headers(headers);

        return requestBuilder;
    }

    private static HttpRequest.BodyPublisher buildBodyContent(String requestBody){
        return  ObjUtils.isNullOrEmpty(requestBody) ? HttpRequest.BodyPublishers.noBody()
                : HttpRequest.BodyPublishers.ofString(requestBody);
    }


    private static HttpRequest buildRequest(String url, String[] headers, HttpRequest.Builder requestBuilder,
                                           String type, String requestBody) {
        try{
            requestBuilder = constructHttpRequestBuilder(url,headers,requestBuilder);
            return requestBuilder == null
                    ? null
                    : requestBuilder.method(type,buildBodyContent(requestBody)).build();
        }catch(Exception e){
            log.info("exception in building request::  " + e.getMessage());
        }
        return null;
    }


    private static HttpResponse<String> sendRequest(HttpRequest request, HttpClient client) throws IOException {
        try{
            return client.send(request, HttpResponse.BodyHandlers.ofString());
        }catch (InterruptedException e){
            log.info(" interrupted exception in sending request : " + e.getMessage());
            Thread.currentThread().interrupt();
        }
        return null;
    }


    private static Map<String,Object> responseParser(HttpResponse<String> response){
        if(response == null || response.body() == null || Boolean.FALSE.equals(JsonUtil.isValidJson(response.body()))){
            return Map.of();
        }
        return JsonUtil.convertJsonToMap(response.body());
    }

    private static boolean convertToBooleanObject(HttpResponse<String> response){
        if(response == null || response.body() == null){
            return false;
        }
        return Boolean.parseBoolean(response.body());
    }


    public static Map<String, Object> sendGetRequest(String url, String[] headers, HttpClient client) throws IOException {
        HttpRequest request = buildRequest(url, headers, HttpRequest.newBuilder(),HttpMethod.GET.toString(),"");
        return request == null ? Map.of() : responseParser(sendRequest(request,client));
    }

    public static boolean sendGetRequestWithBooleanResp(String url, String[] headers, HttpClient client) throws IOException {
        HttpRequest request = buildRequest(url, headers, HttpRequest.newBuilder(),HttpMethod.GET.toString(),"");
        return request == null ? Boolean.FALSE : convertToBooleanObject(sendRequest(request,client));
    }


    public static Map<String, Object> sendPostRequest(String url, Map<String,Object> payloadMap, String[] headers, HttpClient client) throws IOException {
        return sendPostRequest(url,JsonUtil.getJson(payloadMap),headers,client);
    }

    public static Map<String, Object> sendPostRequest(String url, String payload, String[] headers,HttpClient client) throws IOException {
        HttpRequest request = buildRequest(url,headers,HttpRequest.newBuilder(),HttpMethod.POST.toString(),payload);
        return request == null ? Map.of() : responseParser(sendRequest(request,client));
    }


    public static Map<String, Object> sendPutRequest(String url, Map<String,Object> requestPayload, String[] headers, HttpClient client) throws IOException {
        HttpRequest request = buildRequest(url,headers,HttpRequest.newBuilder(),HttpMethod.PUT.toString(),JsonUtil.getJson(requestPayload));
        return request == null ? Map.of() : responseParser(sendRequest(request,client));
    }


    public static Map<String, Object> sendDeleteRequest(String url, String[] headers, HttpClient client) throws IOException {
        HttpRequest request = buildRequest(url,headers,HttpRequest.newBuilder(),HttpMethod.DELETE.toString(),"");
        return request == null ? Map.of() : responseParser(sendRequest(request,client));
    }

    public static Map<String, Object> sendPatchRequest(String url, String payload, String[] headers,HttpClient client) throws IOException {
        HttpRequest request = buildRequest(url,headers,HttpRequest.newBuilder(),HttpMethod.PATCH.toString(),payload);
        return request == null ? Map.of() : responseParser(sendRequest(request,client));
    }
}
