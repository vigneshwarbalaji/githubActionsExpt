package com.yoco.project.helper;

import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.yoco.commons.fullservices.FullMetrics;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ProjectFullMetricHelper {

    private ProjectFullMetricHelper(){}

    public static final String PROJECTS_CREATED = "projects_created";
    public static final String PROJECTS_DELETED = "projects_deleted";
    public static final String PROJECTS_ENABLED = "projects_enabled";
    public static final String BILLABLE_PROJS = "billable_projs";

    public static void projectCreationMetric(String accountID, Boolean billable){

        log.info(" came in projectCreationMetric with accountID :: " + accountID + " billable :: " + billable);

        var metrics = new MetricsMap();
        metrics.incr(PROJECTS_CREATED,1);

        if(Boolean.TRUE.equals(billable)) {
            metrics.incr(BILLABLE_PROJS,1);
        }

        FullMetrics.pushMetricForAppLevel( accountID, metrics);
    }

    public static void projectDeletionMetric(String accountID){

        log.info(" came in projectDeletionMetric with accountID :: " + accountID);

        var metrics = new MetricsMap();
        metrics.incr(PROJECTS_DELETED,1);

        FullMetrics.pushMetricForAppLevel( accountID, metrics);
    }

    public static void projectActivationMetric(String accountID){

        log.info(" came in projectEnabledMetric with accountID :: " + accountID);

        var metrics = new MetricsMap();
        metrics.incr(PROJECTS_ENABLED,1);

        FullMetrics.pushMetricForAppLevel( accountID, metrics);
    }

    public static void projectBillableUpdateMetric(Boolean billable, String accountID){

        log.info(" came in projectBillableUpdateMetric with billable :: " + billable + " and accountID as :: " + accountID);

        var metrics = new MetricsMap();

        if(Boolean.TRUE.equals(billable)){
            metrics.incr(BILLABLE_PROJS,1);
        }else{
            metrics.incr(BILLABLE_PROJS,-1);
        }

        FullMetrics.pushMetricForAppLevel( accountID, metrics);
    }

}
