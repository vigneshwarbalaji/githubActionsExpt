package com.yoco.commons.services;

import com.rtmserver.exception.RTMServerException;
import com.rtmserver.service.CryptoService;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public class RTMService {

    private RTMService(){}

    public static final String STATUS = "status";

    private static final String YOCO_CHANNEL_PATH = "public/yoco";
    private static final String DATA = "data";
    private static final String TYPE = "type";
    private static final String X_RTM_SERVER_SIGNATURE = "X-RTMServer-Signature";

    public static RTMService getRTMService(){
        return new RTMService();
    }

    public static void makeRTMServerRequest(Map<String,Object> requestPayload) {

        try {
            String url = CommonAppProperties.getRtmServerBaseURL() + "/app/sendmessage";
            String rtmSignature = CryptoService.createSignature(JsonUtil.getJson(requestPayload), CommonAppProperties.getRtmServerSecretKey());
            String authToken = UrlFetcher.BEARER + " " +CommonAppProperties.getRtmServerApiKey();

            var headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,authToken, X_RTM_SERVER_SIGNATURE,rtmSignature };

            UrlFetcher.sendPostRequest(url,requestPayload, headers, UrlFetcher.getHttpClientInstance());

        }catch (RTMServerException |IOException e) {
            log.info(" RTM Server Exception :: " + e.getMessage());
        }
    }

    public static void publishToRTMService(String channelName, Object data){
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put("channel", channelName);
        payloadMap.put(TYPE, "message");
        payloadMap.put(DATA, data);
        makeRTMServerRequest(payloadMap);
    }

    public static void publishToChannel(String accountID, Object channelPublishMap){
        String channelName = RTMService.YOCO_CHANNEL_PATH + "/" + accountID;
        RTMService.publishToRTMService(channelName,channelPublishMap);
    }

    public static void publishToChannel(String accountID, String action, String key, Object entityMap){
        Map<String, String> channelPublishReq = new HashMap<>();
        channelPublishReq.put(STATUS, action);
        if(!ObjUtils.isNullOrEmpty(key) && !ObjUtils.isNull(entityMap)){
            channelPublishReq.put(key, JsonUtil.getJson(entityMap));
        }
        publishToChannel(accountID,channelPublishReq);
    }


    public static void publishToAW(String contactId, Map<String, Object> awPayload) throws NoSuchAlgorithmException, IOException {
        String url = CommonAppProperties.getAwChannelPublishUrl() + contactId;
        Map<String, Object> awMap = new HashMap<>();
        awMap.put(TYPE, "user");
        awMap.put("typeId", contactId);
        awMap.put(DATA, awPayload);
        UrlFetcher.sendPostRequest(url,awMap,HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static void publishToAW(String accountID, String contactId, String action, String key, Object entityMap) throws IOException, NoSuchAlgorithmException {
        Map<String, Object> awPayload = new HashMap<>();
        awPayload.put("action", action);
        awPayload.put("accountID", accountID);
        if(!ObjUtils.isNullOrEmpty(key)){
            awPayload.put(key, entityMap);
        }
        publishToAW(contactId,awPayload);
    }

    public static void publishToAdminChannel(String accountID, Map<String,String> dataToPublish) {
        String channelName = YOCO_CHANNEL_PATH + "/" + accountID + "/admin";
        publishToRTMService(channelName,dataToPublish);
    }

    public static void publishToUserChannel(String accountID, String contactID, Map<String, String> dataToPublish) {
        String channelName = YOCO_CHANNEL_PATH + "/" + accountID + "/contact/" + contactID;
        publishToRTMService(channelName,dataToPublish);
    }

}
