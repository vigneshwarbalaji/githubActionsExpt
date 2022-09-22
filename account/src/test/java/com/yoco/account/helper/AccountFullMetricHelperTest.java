package com.yoco.account.helper;

import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.yoco.commons.fullservices.FullMetrics;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

class AccountFullMetricHelperTest {
    @Test
    void updateAccountDeletionMetric_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class)){
            AccountFullMetricHelper.getInstance().updateAccountDeletionMetric("accID");
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.incr("accounts_deleted",1);
            fullMetricsMockedStatic.verify(()->FullMetrics.pushMetricForAppLevel("accID",metricsMap));
        }
    }

    @Test
    void updateAccountCreationMetric_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class)){
            AccountFullMetricHelper.getInstance().updateAccountCreationMetric("accID","web");
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.incr("accounts_created",1);
            metricsMap.incr("account_src_web",1);
            fullMetricsMockedStatic.verify(()->FullMetrics.pushMetricForAppLevel("accID",metricsMap));
        }
    }
}
