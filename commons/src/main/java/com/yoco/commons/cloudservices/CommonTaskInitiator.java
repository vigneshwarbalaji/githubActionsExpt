package com.yoco.commons.cloudservices;

import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.utils.CloudTaskUtil;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.Map;

@Slf4j
public class CommonTaskInitiator {

    private CommonTaskInitiator(){}

    private static final String API_REQUEST_QUEUE = "api-request";

    private static String getClearCacheTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/common/clearCacheTaskHandler";
    }

    public static void initiateClearCacheQueue(String token,Map<String,Object> payloadMap) throws IOException {
        TaskCreator.createPostTask(API_REQUEST_QUEUE,getClearCacheTaskCallBackUrl(),CloudTaskUtil.convertObjectToByteArray(Map.of("token",token,"payloadMap",payloadMap)));
    }

}
