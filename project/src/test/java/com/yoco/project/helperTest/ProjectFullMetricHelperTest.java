package com.yoco.project.helperTest;

import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.yoco.commons.fullservices.FullMetrics;
import com.yoco.project.helper.ProjectFullMetricHelper;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import static org.mockito.Mockito.mockStatic;

class ProjectFullMetricHelperTest {

    @Test
    void projectCreationMetric_non_billable_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = mockStatic(FullMetrics.class)){
            ProjectFullMetricHelper.projectCreationMetric("accId",false);
            MetricsMap metrics = new MetricsMap();
            metrics.incr(ProjectFullMetricHelper.PROJECTS_CREATED,1);
            fullMetricsMockedStatic.verify(() -> FullMetrics.pushMetricForAppLevel("accId",metrics));
        }
    }

    @Test
    void projectCreationMetric_billable_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = mockStatic(FullMetrics.class)){
            ProjectFullMetricHelper.projectCreationMetric("accId",true);
            MetricsMap metrics = new MetricsMap();
            metrics.incr(ProjectFullMetricHelper.PROJECTS_CREATED,1);
            metrics.incr(ProjectFullMetricHelper.BILLABLE_PROJS,1);
            fullMetricsMockedStatic.verify(() -> FullMetrics.pushMetricForAppLevel("accId",metrics));
        }
    }

    @Test
    void projectDeletionMetric_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = mockStatic(FullMetrics.class)){
            ProjectFullMetricHelper.projectDeletionMetric("accId");
            MetricsMap metrics = new MetricsMap();
            metrics.incr(ProjectFullMetricHelper.PROJECTS_DELETED,1);
            fullMetricsMockedStatic.verify(() -> FullMetrics.pushMetricForAppLevel("accId",metrics));
        }
    }

    @Test
    void projectActivationMetric_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = mockStatic(FullMetrics.class)){
            ProjectFullMetricHelper.projectActivationMetric("accId");
            MetricsMap metrics = new MetricsMap();
            metrics.incr(ProjectFullMetricHelper.PROJECTS_ENABLED,1);
            fullMetricsMockedStatic.verify(() -> FullMetrics.pushMetricForAppLevel("accId",metrics));
        }
    }

    @Test
    void projectBillableUpdateMetric_non_billable_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = mockStatic(FullMetrics.class)){
            ProjectFullMetricHelper.projectBillableUpdateMetric(false,"accId");
            MetricsMap metrics = new MetricsMap();
            metrics.incr(ProjectFullMetricHelper.BILLABLE_PROJS,-1);
            fullMetricsMockedStatic.verify(() -> FullMetrics.pushMetricForAppLevel("accId",metrics));
        }
    }

    @Test
    void projectBillableUpdateMetric_billable_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = mockStatic(FullMetrics.class)){
            ProjectFullMetricHelper.projectBillableUpdateMetric(true,"accId");
            MetricsMap metrics = new MetricsMap();
            metrics.incr(ProjectFullMetricHelper.BILLABLE_PROJS,1);
            fullMetricsMockedStatic.verify(() -> FullMetrics.pushMetricForAppLevel("accId",metrics));
        }
    }

}
