package com.yoco.commons.utils;

import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.yoco.commons.dataservices.impl.ProjectImpl;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.fullservices.FullMetrics;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.events.ClockMetricUtil;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.mockito.ArgumentMatchers.any;

class ClockMetricUtilTest {

    @Test
    void clockOutMetric_ifGetBillableFalse_Test(){

        var projectObject = new ProjectJDO();
        projectObject.setBillable(false);

        try(MockedConstruction<ProjectImpl> mock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.getProject(any())).thenReturn(projectObject);
        });
            MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class);
        ){
            var reportObject = new ReportsDTO();
            reportObject.setId("123");
            reportObject.setEmailID("newEmail.com");
            reportObject.setInTime(12345543234543L);
            reportObject.setOutTime(12345543234700L);
            reportObject.setProjectID("23");
            reportObject.setInSource("inSource");
            reportObject.setOutSource("outSource");

            ClockMetricUtil.clockOutMetric("123", "34", "update", reportObject);
            var metrics = new MetricsMap();
            metrics.incr("clockout_medium" + "_" + "outSource", 1);
            metrics.incr("clockout_type_self", 1);
            metrics.incr("CLOCK_DURATION_MINS_inSource", 0L);
            metrics.incr("clock_dur_mins", 0L);

            fullMetricsMockedStatic.verify(()->FullMetrics.pushMetricForUserAppLevel("123", "34", metrics));
        }
    }

    @Test
    void clockOutMetric_ForDeleteUserValid_Test(){

        var projectObject = new ProjectJDO();
        projectObject.setBillable(true);

        try(MockedConstruction<ProjectImpl> mock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.getProject(any())).thenReturn(projectObject);
        });
            MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class);
        ){
            var reportObject = new ReportsDTO();
            reportObject.setId("123");
            reportObject.setEmailID("newEmail.com");
            reportObject.setInTime(12345543234543L);
            reportObject.setOutTime(12345543234700L);
            reportObject.setInSource("inSource");
            reportObject.setOutSource("outSource");

            ClockMetricUtil.clockOutMetric("123", "34", "deleteUser", reportObject);
            var metrics = new MetricsMap();
            metrics.incr("clockout_medium" + "_" + "outSource", 1);
            metrics.incr("clockout_type_force", 1);
            metrics.incr("CLOCK_DURATION_MINS_inSource", 0L);
            metrics.incr("clock_dur_mins", 0L);

            fullMetricsMockedStatic.verify(()->FullMetrics.pushMetricForUserAppLevel("123", "34", metrics));
        }
    }

    @Test
    void clockOutMetric_Valid_Test(){

        var projectObject = new ProjectJDO();
        projectObject.setBillable(true);

        try(MockedConstruction<ProjectImpl> mock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.getProject(any())).thenReturn(projectObject);
        });
            MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class);
        ){
            var reportObject = new ReportsDTO();
            reportObject.setId("123");
            reportObject.setEmailID("newEmail.com");
            reportObject.setInTime(12345543234543L);
            reportObject.setOutTime(12345543234700L);
            reportObject.setProjectID("2334");
            reportObject.setInSource("inSource");
            reportObject.setOutSource("outSource");

            ClockMetricUtil.clockOutMetric("123", "34", "force", reportObject);
            var metrics = new MetricsMap();
            metrics.incr("clockout_medium" + "_" + "outSource", 1);
            metrics.incr("clockout_type_force", 1);
            metrics.incr("CLOCK_DURATION_MINS_inSource", 0L);
            metrics.incr("clock_dur_mins", 0L);
            metrics.incr("billable_dur_mins",0L);

            fullMetricsMockedStatic.verify(()->FullMetrics.pushMetricForUserAppLevel("123", "34", metrics));
        }
    }

}
