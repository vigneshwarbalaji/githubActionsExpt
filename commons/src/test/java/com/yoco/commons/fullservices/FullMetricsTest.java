package com.yoco.commons.fullservices;

import com.fullmetric.api.helper.push.v1.FullMetricsPushHelper;
import com.fullmetric.api.helper.push.v1.FullMetricsPushWorker;
import com.fullmetric.api.helper.push.v1.PubSubPullManager;
import com.fullmetric.api.helper.push.v1.PubsubManager;
import com.fullmetrics.api.client.FullMetricsApi;
import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.fullmetrics.api.client.model.metrics.MetricsPushRequest;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.utils.GaeUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;

import static org.mockito.ArgumentMatchers.*;

class FullMetricsTest {

    @Test
    void pushMetricForUserAppLevel_flag_false_test(){
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(false);
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForUserAppLevel("","",metricsMap);
            Mockito.verifyNoInteractions(fullMetricsPushHelper);
        }
    }

    @Test
    void pushMetricForUserAppLevel_empty_accountID_test2(){
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(true);
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForUserAppLevel("","contactId",metricsMap);
            Mockito.verifyNoInteractions(fullMetricsPushHelper);
        }
    }

    @Test
    void pushMetricForUserAppLevel_empty_contactID_test3(){
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(true);
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForUserAppLevel("accountId","",metricsMap);
            Mockito.verifyNoInteractions(fullMetricsPushHelper);
        }
    }

    @Test
    void pushMetricForUserAppLevel_valid_test() throws IOException {
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(true);
            Mockito.when(fullMetricsPushHelper.pushMessage(anyString(),any(MetricsPushRequest.class))).thenReturn("");
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForUserAppLevel("accountId","contactId",metricsMap);
            Mockito.verify(fullMetricsPushHelper).pushMessage(anyString(),any(MetricsPushRequest.class));
        }
    }

    @Test
    void pushMetricForUserAccountLevel_flag_false_test(){
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(false);
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForUserAccountLevel("","",metricsMap);
            Mockito.verifyNoInteractions(fullMetricsPushHelper);
        }
    }

    @Test
    void pushMetricForUserAccountLevel_empty_accountID_test2(){
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(true);
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForUserAccountLevel("","contactId",metricsMap);
            Mockito.verifyNoInteractions(fullMetricsPushHelper);
        }
    }

    @Test
    void pushMetricForUserAccountLevel_empty_contactID_test3(){
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(true);
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForUserAccountLevel("accountId","",metricsMap);
            Mockito.verifyNoInteractions(fullMetricsPushHelper);
        }
    }

    @Test
    void pushMetricForUserAccountLevel_valid_test() throws IOException {
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(true);
            Mockito.when(fullMetricsPushHelper.pushMessage(anyString(),any(MetricsPushRequest.class))).thenReturn("");
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForUserAccountLevel("accountId","contactId",metricsMap);
            Mockito.verify(fullMetricsPushHelper).pushMessage(anyString(),any(MetricsPushRequest.class));
        }
    }

    @Test
    void pushMetricForAppLevel_flag_false_test(){
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(false);
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForAppLevel("",metricsMap);
            Mockito.verifyNoInteractions(fullMetricsPushHelper);
        }
    }

    @Test
    void pushMetricForAppLevel_empty_accountID_test2(){
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(true);
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForAppLevel("",metricsMap);
            Mockito.verifyNoInteractions(fullMetricsPushHelper);
        }
    }

    @Test
    void pushMetricForAppLevel_valid_test() throws IOException {
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(true);
            Mockito.when(fullMetricsPushHelper.pushMessage(anyString(),any(MetricsPushRequest.class))).thenReturn("");
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForAppLevel("accountId",metricsMap);
            Mockito.verify(fullMetricsPushHelper).pushMessage(anyString(),any(MetricsPushRequest.class));
        }
    }

    @Test
    void pushMetricForAccountLevel_flag_false_test(){
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(false);
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForAccountLevel("",metricsMap);
            Mockito.verifyNoInteractions(fullMetricsPushHelper);
        }
    }

    @Test
    void pushMetricForAccountLevel_empty_accountID_test2(){
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(true);
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForAccountLevel("",metricsMap);
            Mockito.verifyNoInteractions(fullMetricsPushHelper);
        }
    }

    @Test
    void pushMetricForAccountLevel_valid_test() throws IOException {
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(true);
            Mockito.when(fullMetricsPushHelper.pushMessage(anyString(),any(MetricsPushRequest.class))).thenReturn("");
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForAccountLevel("accountId",metricsMap);
            Mockito.verify(fullMetricsPushHelper).pushMessage(anyString(),any(MetricsPushRequest.class));
        }
    }

    @Test
    void pushMetricForAccountLevel_error_test() throws IOException {
        PubsubManager pubsubManager = Mockito.mock(PubsubManager.class);
        try(MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushHelper.class)){
            FullMetricsPushHelper fullMetricsPushHelper = new FullMetricsPushHelper(pubsubManager);
            FullMetrics.setFullMetricsPushHelper(fullMetricsPushHelper);
            FullMetrics.setShouldPushToMetrics(true);
            Mockito.when(fullMetricsPushHelper.pushMessage(anyString(),any(MetricsPushRequest.class))).thenThrow(new IOException("exception"));
            MetricsMap metricsMap = new MetricsMap();
            metricsMap.put("key",1345L);
            FullMetrics.pushMetricForAccountLevel("accountId",metricsMap);
            Mockito.verify(fullMetricsPushHelper).pushMessage(anyString(),any(MetricsPushRequest.class));
        }
    }

    @Test
    void pushToFullMetrics_test(){
        PubSubPullManager pubSubPullManager = Mockito.mock(PubSubPullManager.class);
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushWorker.class);
            MockedConstruction mockedConstruction = Mockito.mockConstruction(FullMetricsApi.class)){

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullMetricsApiKey).thenReturn("fullMetricsApiKey");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullMetricsSubscriptionName).thenReturn("fullMetricsSubscriptionName");

            FullMetricsApi metricsApi = new FullMetricsApi( "key", false );
            FullMetricsPushWorker fullMetricsPushWorker = new FullMetricsPushWorker(pubSubPullManager,metricsApi.metricsApi());
            Mockito.doNothing().when(fullMetricsPushWorker).processTasks(Mockito.any(int.class));
            FullMetrics.pushToFullMetrics();
            Mockito.verify(metricsApi).metricsApi();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void pushToFullMetrics_invalidFullMetricsApiKeys_test(String testValue){
        PubSubPullManager pubSubPullManager = Mockito.mock(PubSubPullManager.class);
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushWorker.class);
            MockedConstruction mockedConstruction = Mockito.mockConstruction(FullMetricsApi.class)){

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullMetricsApiKey).thenReturn(testValue);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullMetricsSubscriptionName).thenReturn("fullMetricsSubscriptionName");

            FullMetricsApi metricsApi = new FullMetricsApi( "apiKey", false );
            FullMetricsPushWorker fullMetricsPushWorker = new FullMetricsPushWorker(pubSubPullManager,metricsApi.metricsApi());

            FullMetrics.pushToFullMetrics();

            Mockito.verify(fullMetricsPushWorker,Mockito.times(0)).processTasks(Mockito.any(int.class));

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void pushToFullMetrics_invalidFullMetricsSubscriptionNameKeys_test(String testValue){
        PubSubPullManager pubSubPullManager = Mockito.mock(PubSubPullManager.class);
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction mocked = Mockito.mockConstruction(FullMetricsPushWorker.class);
            MockedConstruction mockedConstruction = Mockito.mockConstruction(FullMetricsApi.class)){

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullMetricsApiKey).thenReturn("fullMetricsApiKey");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullMetricsSubscriptionName).thenReturn(testValue);

            FullMetricsApi metricsApi = new FullMetricsApi( "apiKey", false );
            FullMetricsPushWorker fullMetricsPushWorker = new FullMetricsPushWorker(pubSubPullManager,metricsApi.metricsApi());

            FullMetrics.pushToFullMetrics();

            Mockito.verify(fullMetricsPushWorker,Mockito.times(0)).processTasks(Mockito.any(int.class));

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
