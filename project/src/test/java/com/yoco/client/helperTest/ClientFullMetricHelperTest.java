package com.yoco.client.helperTest;

import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.yoco.client.helper.ClientFullMetricHelper;
import com.yoco.commons.fullservices.FullMetrics;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class ClientFullMetricHelperTest {

    @Test
    void clientCreationMetric_test(){

        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class)){

            fullMetricsMockedStatic.when(() -> FullMetrics.pushMetricForAppLevel(anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            ClientFullMetricHelper.clientCreationMetric("accountId");

            var metrics = new MetricsMap();
            metrics.incr(ClientFullMetricHelper.CLIENTS_CREATED,1);
            fullMetricsMockedStatic.verify(()-> FullMetrics.pushMetricForAppLevel("accountId",metrics));
        }
    }

    @Test
    void clientDeletionMetric_test(){

        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class)){

            fullMetricsMockedStatic.when(() -> FullMetrics.pushMetricForAppLevel(anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            ClientFullMetricHelper.clientDeletionMetric("accountId");

            var metrics = new MetricsMap();
            metrics.incr(ClientFullMetricHelper.CLIENTS_DELETED,1);
            fullMetricsMockedStatic.verify(()-> FullMetrics.pushMetricForAppLevel("accountId",metrics));
        }
    }
}
