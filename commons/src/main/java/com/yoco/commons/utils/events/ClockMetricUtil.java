package com.yoco.commons.utils.events;

import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.yoco.commons.dataservices.impl.ProjectImpl;
import com.yoco.commons.fullservices.FullMetrics;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.ObjUtils;
import lombok.NoArgsConstructor;

public class ClockMetricUtil {

    private ClockMetricUtil(){}

    @NoArgsConstructor
    private enum CLOCK_METRIC_KEYS {

        DELETE_USER("deleteUser"),
        FORCE("force"),
        CLOCK_OUT_MEDIUM("clockout_medium"),
        CLOCK_OUT_TYPE_FORCE("clockout_type_force"),
        CLOCK_OUT_TYPE_SELF("clockout_type_self"),
        CLOCK_DURATION_MINS("clock_dur_mins"),
        BILLABLE_DURATION_MINS("billable_dur_mins");

        private String value;

        CLOCK_METRIC_KEYS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    public static void clockOutMetric(String accountID, String contactID, String action, ReportsDTO reportObject){

        String inSource = reportObject.getInSource();
        String outSource = reportObject.getOutSource();
        String projectID = reportObject.getProjectID();
        long inTime = reportObject.getInTime();
        long outTime = reportObject.getOutTime();
        long clockDurationInMinutes = (outTime - inTime) / (1000 * 60);

        var metrics = new MetricsMap();
        metrics.incr(CLOCK_METRIC_KEYS.CLOCK_OUT_MEDIUM.value() + "_" + outSource, 1);

        if (CLOCK_METRIC_KEYS.DELETE_USER.value().equalsIgnoreCase(action) || CLOCK_METRIC_KEYS.FORCE.value().equalsIgnoreCase(action)) {
            metrics.incr(CLOCK_METRIC_KEYS.CLOCK_OUT_TYPE_FORCE.value(), 1);
        } else {
            metrics.incr(CLOCK_METRIC_KEYS.CLOCK_OUT_TYPE_SELF.value(), 1);
        }

        metrics.incr(CLOCK_METRIC_KEYS.CLOCK_DURATION_MINS.value(), clockDurationInMinutes);
        metrics.incr(CLOCK_METRIC_KEYS.CLOCK_DURATION_MINS + "_" + inSource, clockDurationInMinutes);

        if (!ObjUtils.isNullOrEmpty(projectID)) {
            var projectObj = ProjectImpl.getProjectImplInstance().getProject(projectID);

            if (Boolean.TRUE.equals(projectObj.getBillable())) {
                metrics.incr(CLOCK_METRIC_KEYS.BILLABLE_DURATION_MINS.value(), clockDurationInMinutes);
            }
        }
        FullMetrics.pushMetricForUserAppLevel(accountID, contactID, metrics);
    }

}
