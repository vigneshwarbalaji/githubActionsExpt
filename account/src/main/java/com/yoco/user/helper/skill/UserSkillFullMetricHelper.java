package com.yoco.user.helper.skill;

import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.yoco.commons.fullservices.FullMetrics;
import lombok.NoArgsConstructor;

@NoArgsConstructor
public class UserSkillFullMetricHelper {

    public static UserSkillFullMetricHelper getInstance(){
        return new UserSkillFullMetricHelper();
    }

    public static final String WEEKLY_DIGEST_ENABLED = "Weekly_Digest_Enabled";
    public static final String WEEKLY_DIGEST_DISABLED = "Weekly_Digest_Disabled";


    public void weeklyDigestEnabledMetric(String accountID){
        var metrics = new MetricsMap();
        metrics.incr(WEEKLY_DIGEST_ENABLED,1);
        FullMetrics.pushMetricForAppLevel( accountID, metrics );
    }

    public void weeklyDigestDisabledMetric(String accountID){
        var metrics = new MetricsMap();
        metrics.incr(WEEKLY_DIGEST_DISABLED,1);
        FullMetrics.pushMetricForAppLevel( accountID, metrics );
    }

}