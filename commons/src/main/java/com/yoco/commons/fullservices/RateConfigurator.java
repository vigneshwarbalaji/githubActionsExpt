package com.yoco.commons.fullservices;

import co.anywhere.awconfigurator.model.ratelimit.RateCheckConfig;
import co.anywhere.awconfigurator.model.ratelimit.RateLimitInfo;
import co.anywhere.awconfigurator.services.RateLimitApi;
import com.yoco.commons.utils.GaeUtils;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashSet;
import java.util.Set;

@Slf4j
public class RateConfigurator {

    private RateConfigurator(){}

    private static final Set<String> getRateLimitTokenScopes(){
        Set<String> scopeSet = new HashSet<>();
        scopeSet.add("awconfigurator.ratelimit.access");
        return scopeSet;
    }

    private static RateLimitApi getRateLimitingInstance() throws NoSuchAlgorithmException, IOException {
        return new RateLimitApi(FullAuthService.getServerAccessToken(FullAuthService.RATE_SERVICE_TOKEN_KEY,getRateLimitTokenScopes()),GaeUtils.isAppModeLive());
    }

    public static RateLimitInfo checkRateLimitUsage(String key, int limit, int timeLimit) {
        try {
            if (ObjUtils.isNullOrEmpty(key)) {
                return null;
            }
            return getRateLimitingInstance().check(new RateCheckConfig(key, limit, timeLimit));
        } catch (Exception e) {
            log.info(e.getMessage());
            return null;
        }
    }

}
