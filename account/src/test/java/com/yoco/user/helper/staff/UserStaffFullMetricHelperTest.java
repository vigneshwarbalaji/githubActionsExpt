package com.yoco.user.helper.staff;

import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.fullservices.FullMetrics;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class UserStaffFullMetricHelperTest {

    UserStaffFullMetricHelper userFullMetricHelper = UserStaffFullMetricHelper.getInstance();

    @Test
    void userCreationMetric_validSource_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class)){
            fullMetricsMockedStatic.when(() -> FullMetrics.pushMetricForAppLevel(anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            userFullMetricHelper.userCreationMetric("accountId", ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value());

            var metrics = new MetricsMap();
            metrics.incr("staff_created",1);
            metrics.incr("staff_created_src_web",1);
            fullMetricsMockedStatic.verify(()-> FullMetrics.pushMetricForAppLevel("accountId",metrics));
        }
    }

    @Test
    void userCreationMetric_emptySource_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class)){
            fullMetricsMockedStatic.when(() -> FullMetrics.pushMetricForAppLevel(anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            userFullMetricHelper.userCreationMetric("accountId","");

            var metrics = new MetricsMap();
            metrics.incr("staff_created",1);
            fullMetricsMockedStatic.verify(()-> FullMetrics.pushMetricForAppLevel("accountId",metrics));
        }
    }

    @Test
    void userActivationMetric_validSource_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class)){
            fullMetricsMockedStatic.when(() -> FullMetrics.pushMetricForAppLevel(anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            userFullMetricHelper.userActivationMetric("accountId", ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value());

            var metrics = new MetricsMap();
            metrics.incr("staff_enabled",1);
            metrics.incr("staff_enabled_src_web",1);
            fullMetricsMockedStatic.verify(()-> FullMetrics.pushMetricForAppLevel("accountId",metrics));
        }
    }

    @Test
    void userActivationMetric_emptySource_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class)){
            fullMetricsMockedStatic.when(() -> FullMetrics.pushMetricForAppLevel(anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            userFullMetricHelper.userActivationMetric("accountId","");

            var metrics = new MetricsMap();
            metrics.incr("staff_enabled",1);
            fullMetricsMockedStatic.verify(()-> FullMetrics.pushMetricForAppLevel("accountId",metrics));
        }
    }

    @Test
    void userDeletionMetric_validSource_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class)){

            userFullMetricHelper.updateUserDeletionMetric("accountId", ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value());

            var metrics = new MetricsMap();
            metrics.incr("staff_deleted",1);
            metrics.incr("staff_deleted_src_web",1);
            fullMetricsMockedStatic.verify(()-> FullMetrics.pushMetricForAppLevel("accountId",metrics));
        }
    }

    @Test
    void userDeletionMetric_emptySource_test(){
        try(MockedStatic<FullMetrics> fullMetricsMockedStatic = Mockito.mockStatic(FullMetrics.class)){
            userFullMetricHelper.updateUserDeletionMetric("accountId","");

            var metrics = new MetricsMap();
            metrics.incr("staff_deleted",1);
            fullMetricsMockedStatic.verify(()-> FullMetrics.pushMetricForAppLevel("accountId",metrics));
        }
    }
}