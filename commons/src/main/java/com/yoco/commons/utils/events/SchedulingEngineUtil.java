package com.yoco.commons.utils.events;

import com.yoco.commons.constants.SchedulingEngineUrlConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.JsonUtil;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

@Slf4j
public class SchedulingEngineUtil {

    private SchedulingEngineUtil(){}

    public static Map<String, Object> getEventByID(String entryID) throws IOException, NoSuchAlgorithmException {
        String url = SchedulingEngineUrlConstants.getSchedulingEngineEventUrl()+"/"+entryID;
        return UrlFetcher.sendGetRequest(url,HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(), UrlFetcher.getHttpClientInstance());
    }

    public static Map<String, Object> getEventByQueryString (Map<String, Object> queryString) throws IOException, NoSuchAlgorithmException {
        String requestQuery = JsonUtil.getJson(queryString);
        String url = SchedulingEngineUrlConstants.getSchedulingEngineEventUrl()+"/fetch?q="+ URLEncoder.encode(requestQuery, StandardCharsets.UTF_8.toString());
        return UrlFetcher.sendGetRequest(url,HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(), UrlFetcher.getHttpClientInstance());
    }

    public static Map<String, Object> updateEventReq (Map<String, Object> payload) throws IOException, NoSuchAlgorithmException {
        String url = SchedulingEngineUrlConstants.getSchedulingEngineEventUrl() + "/" + payload.get(SchedulingKeys.ID);
        return UrlFetcher.sendPutRequest(url, payload, HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(), UrlFetcher.getHttpClientInstance());
    }

    public static Map<String, Object> updateBatchEventsReq (Map<String, Object> payload) throws IOException, NoSuchAlgorithmException {
        String url = SchedulingEngineUrlConstants.getSchedulingEngineEventUrl() + "/batch";
        return UrlFetcher.sendPutRequest(url, payload, HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(), UrlFetcher.getHttpClientInstance());
    }

    public static Map<String, Object> createEvent (String payload) throws IOException, NoSuchAlgorithmException {
        String url = SchedulingEngineUrlConstants.getSchedulingEngineEventUrl();
        return UrlFetcher.sendPostRequest(url, payload, HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(), UrlFetcher.getHttpClientInstance());
    }

}
