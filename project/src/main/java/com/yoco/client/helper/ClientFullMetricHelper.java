package com.yoco.client.helper;

import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.yoco.commons.fullservices.FullMetrics;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ClientFullMetricHelper {

    private ClientFullMetricHelper(){}

    public static final String CLIENTS_CREATED = "clients_created";
    public static final String CLIENTS_DELETED = "clients_deleted";

    public static void clientCreationMetric(String accountID){
        var metrics = new MetricsMap();
        metrics.incr(ClientFullMetricHelper.CLIENTS_CREATED,1);
        FullMetrics.pushMetricForAppLevel( accountID, metrics );
    }

    public static void clientDeletionMetric(String accountID){
        var metrics = new MetricsMap();
        metrics.incr(ClientFullMetricHelper.CLIENTS_DELETED,1);
        FullMetrics.pushMetricForAppLevel( accountID, metrics);
    }

}
