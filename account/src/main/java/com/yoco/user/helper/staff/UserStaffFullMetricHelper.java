package com.yoco.user.helper.staff;

import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.yoco.commons.fullservices.FullMetrics;
import com.yoco.commons.utils.ObjUtils;

public class UserStaffFullMetricHelper {

    public static UserStaffFullMetricHelper getInstance(){
        return new UserStaffFullMetricHelper();
    }

    private static final String STAFF_CREATED = "staff_created";
    private static final String STAFF_CREATED_SRC = "staff_created_src";
    private static final String STAFF_ENABLED = "staff_enabled";
    private static final String STAFF_ENABLED_SRC = "staff_enabled_src";
    public static final String STAFF_DELETED = "staff_deleted";
    public static final String STAFF_DELETED_SRC = "staff_deleted_src";

    public void userCreationMetric(String accountID,String source){
        var metrics = new MetricsMap();
        metrics.incr(STAFF_CREATED,1);
        if(!ObjUtils.isBlank(source)){
            metrics.incr(STAFF_CREATED_SRC + "_" + source, 1);
        }
        FullMetrics.pushMetricForAppLevel(accountID, metrics);
    }

    public void userActivationMetric(String accountID,String source){
        var metrics = new MetricsMap();
        metrics.incr(STAFF_ENABLED,1);
        if(!ObjUtils.isBlank(source)){
            metrics.incr(STAFF_ENABLED_SRC + "_" + source, 1);
        }
        FullMetrics.pushMetricForAppLevel(accountID, metrics);
    }

    public void updateUserDeletionMetric(String accountID,String source){
        updateUserDeletionMetric(accountID,source,1);
    }

    public void updateUserDeletionMetric(String accountID,String source,int value){
        var metrics = new MetricsMap();
        metrics.incr(STAFF_DELETED,value);
        if(!ObjUtils.isBlank(source)){
            metrics.incr(STAFF_DELETED_SRC + "_" + source, value);
        }
        FullMetrics.pushMetricForAppLevel(accountID, metrics);
    }


}
