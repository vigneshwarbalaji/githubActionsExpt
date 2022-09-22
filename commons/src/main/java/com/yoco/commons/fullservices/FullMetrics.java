package com.yoco.commons.fullservices;

import com.fullmetric.api.helper.push.v1.FullMetricsPushHelper;
import com.fullmetric.api.helper.push.v1.FullMetricsPushWorker;
import com.fullmetric.api.helper.push.v1.PubSubPullManager;
import com.fullmetric.api.helper.push.v1.PubsubManager;
import com.fullmetrics.api.client.FullMetricsApi;
import com.fullmetrics.api.client.enums.MetricsType;
import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.fullmetrics.api.client.model.metrics.MetricsPushRequest;
import com.fullmetrics.api.client.services.AppMetricsApi;

import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.utils.GaeUtils;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.Date;

@Slf4j
public class FullMetrics {

    private FullMetrics(){}
    private static Boolean shouldPushToMetrics = Boolean.TRUE;

    private static PubsubManager pubsubManager = new PubsubManager();
    private static FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
    private static PubSubPullManager pubSubPullManager = new PubSubPullManager(CommonAppProperties.getFullMetricsSubscriptionName());

    public static void setFullMetricsPushHelper(FullMetricsPushHelper fullMetricsPushHelper){
        FullMetrics.fullMetricsPushHelper = fullMetricsPushHelper;
    }
    public static void setShouldPushToMetrics(boolean shouldPushToMetrics){
        FullMetrics.shouldPushToMetrics = shouldPushToMetrics;
    }

    public static void pushToFullMetrics() {
        try {

            if(!ObjUtils.isNullOrEmpty(CommonAppProperties.getFullMetricsApiKey()) &&
                    !ObjUtils.isNullOrEmpty(CommonAppProperties.getFullMetricsSubscriptionName())){

                log.info(" came in pull Subscription Messages ");

                AppMetricsApi metricsApi = new FullMetricsApi(CommonAppProperties.getFullMetricsApiKey(), GaeUtils.isAppModeLive()).metricsApi();

                var worker = new FullMetricsPushWorker(pubSubPullManager, metricsApi);

                worker.processTasks(30);

                log.info(" processed tasks ");
            }

        } catch (IOException e) {
            log.info(" exception in processing metrics cron " + e.getMessage());
        }
    }

    public static void publishToTopic(MetricsPushRequest request) {
        try {

            fullMetricsPushHelper.pushMessage(CommonAppProperties.getFullMetricsTopicName(),request);

            log.info(" metrics successfully queued for processing ");

        } catch (IOException e) {
            log.info("exception in pushing the metrics to topic: " + e.getMessage());
        }
    }

    public static void pushMetricForAccountLevel(String accountID, MetricsMap metrics){

        if(Boolean.TRUE.equals(FullMetrics.shouldPushToMetrics) && !ObjUtils.isNullOrEmpty(accountID)){

            log.info("account level: metrics " + metrics );

            MetricsPushRequest request = MetricsPushRequest.builder()
                    .type(MetricsType.ACCOUNT)
                    .acctId(accountID)
                    .date(new Date())
                    .build();

            request.addAllMetrics(metrics);

            FullMetrics.publishToTopic(request);
        }
    }

    public static void pushMetricForAppLevel(String accountID, MetricsMap metrics){

        if(Boolean.TRUE.equals(FullMetrics.shouldPushToMetrics)  && !ObjUtils.isNullOrEmpty(accountID)){

            log.info(" app level: metrics " + metrics );

            MetricsPushRequest request = MetricsPushRequest.builder()
                    .type(MetricsType.ACCOUNT)
                    .acctId(accountID)
                    .addAggregateTo(MetricsType.APP)
                    .date(new Date())
                    .build();

            request.addAllMetrics(metrics);

          FullMetrics.publishToTopic(request);
        }
    }

    public static void pushMetricForUserAccountLevel(String accountID, String userContactID, MetricsMap metrics){

        if(Boolean.TRUE.equals(FullMetrics.shouldPushToMetrics) && !ObjUtils.isNullOrEmpty(accountID) &&
                !ObjUtils.isNullOrEmpty(userContactID)){

            log.info(" user account level: metrics " + metrics );

            MetricsPushRequest request =  MetricsPushRequest.builder()
                    .type(MetricsType.USER)
                    .acctId(accountID)
                    .userId(userContactID)
                    .addAggregateTo(MetricsType.ACCOUNT)
                    .date(new Date())
                    .build();

            request.addAllMetrics(metrics);

            FullMetrics.publishToTopic(request);
        }
    }

    public static void pushMetricForUserAppLevel(String accountID, String userContactID, MetricsMap metrics){

        if(Boolean.TRUE.equals(FullMetrics.shouldPushToMetrics) && !ObjUtils.isNullOrEmpty(accountID) &&
                !ObjUtils.isNullOrEmpty(userContactID)){

            log.info(" user app level: metrics " + metrics );

            MetricsPushRequest request = MetricsPushRequest.builder()
                    .type(MetricsType.USER)
                    .acctId(accountID)
                    .userId(userContactID)
                    .addAggregateTo(MetricsType.ACCOUNT)
                    .addAggregateTo(MetricsType.APP)
                    .date(new Date())
                    .build();

            request.addAllMetrics(metrics);

            FullMetrics.publishToTopic(request);
        }
    }

}
