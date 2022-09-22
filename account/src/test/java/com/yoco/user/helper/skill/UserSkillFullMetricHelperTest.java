package com.yoco.user.helper.skill;

import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.yoco.commons.fullservices.FullMetrics;
import com.yoco.user.helper.skill.UserSkillFullMetricHelper;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class UserSkillFullMetricHelperTest {

    UserSkillFullMetricHelper userSkillFullMetricHelper = UserSkillFullMetricHelper.getInstance();

    @Test
    void weeklyDigestEnabledMetric_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class)){
            fullMetricsMockedStatic.when(() -> FullMetrics.pushMetricForAppLevel(anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            userSkillFullMetricHelper.weeklyDigestEnabledMetric("accountId");

            var metrics = new MetricsMap();
            metrics.incr(UserSkillFullMetricHelper.WEEKLY_DIGEST_ENABLED,1);
            fullMetricsMockedStatic.verify(()-> FullMetrics.pushMetricForAppLevel("accountId",metrics));
        }
    }

    @Test
    void weeklyDigestDisabledMetric_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class)){
            fullMetricsMockedStatic.when(() -> FullMetrics.pushMetricForAppLevel(anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            userSkillFullMetricHelper.weeklyDigestDisabledMetric("accountId");

            var metrics = new MetricsMap();
            metrics.incr(UserSkillFullMetricHelper.WEEKLY_DIGEST_DISABLED,1);
            fullMetricsMockedStatic.verify(()-> FullMetrics.pushMetricForAppLevel("accountId",metrics));
        }
    }
}