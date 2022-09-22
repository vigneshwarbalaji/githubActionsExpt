package com.yoco.commons.cloudservices;

import com.yoco.commons.fullservices.FullMetrics;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

class CronHandlerTest {

    @Test
    void processFullMetrics_test() throws Exception {
        try (MockedStatic<FullMetrics> metrics = Mockito.mockStatic(FullMetrics.class)){
             metrics.when(() -> FullMetrics.pushToFullMetrics()).thenAnswer((Answer<Void>) invocation -> null);
             new CronHandler().processFullMetrics();
             metrics.verify(() -> FullMetrics.pushToFullMetrics());
        }
    }
}
