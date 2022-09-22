package com.yoco.commons.cloudservices;

import com.yoco.commons.utils.events.ClockTaskUtil;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.commons.utils.HeaderUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/task/common")
public class CommonTaskController {

    @PostMapping("/clockTaskHandler")
    @ResponseStatus(HttpStatus.OK)
    public void createClockTaskHandler(@RequestBody byte[] payload) throws IOException, ClassNotFoundException {
        log.info(" received in createClockTask");
        Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
        log.info(" payloadMap " + payloadMap);
        ClockTaskUtil.getClockTaskUtil().clockOperationsTaskHandler(payloadMap);
    }

    @PostMapping("/clearCacheTaskHandler")
    @ResponseStatus(HttpStatus.OK)
    public void clearCacheTaskHandler(@RequestBody byte[] payload) throws IOException, ClassNotFoundException {
        log.info(" reached task controller");
        Map<String,Object> requestPayload = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
        String url = CommonAppProperties.getYoCoDefaultApiUrl() + "/api/v2/contact/clearTokenFromCache";
        UrlFetcher.sendPutRequest(url, (Map<String, Object>) requestPayload.get("payloadMap"),HeaderUtil.getContentTypedAccessTokenAuthHeader(requestPayload.get("token").toString()),UrlFetcher.getHttpClientInstance());
    }


}


