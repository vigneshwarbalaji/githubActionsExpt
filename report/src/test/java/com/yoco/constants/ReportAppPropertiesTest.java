package com.yoco.constants;

import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.enums.AppMode;
import com.yoco.commons.utils.KMSUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

import java.io.IOException;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.anyString;

public class ReportAppPropertiesTest {

    static Properties properties = new Properties();

    @BeforeAll
    public static void setProperties(){
        properties.setProperty("FULLAUTH_API_KEY","fullAuthApiKey");
        properties.setProperty("FULL_REMINDER_API_KEY","fullReminderApiKey");
        properties.setProperty("FULL_METRICS_API_KEY","fullMetricApiKey");
        properties.setProperty("FULL_METRICS_TOPIC_NAME","fullMetricsTopicName");
        properties.setProperty("FULL_METRICS_SUBSCRIPTION_NAME","fullMetricsSubscriptionName");
        properties.setProperty("rtm_server_apikey","rtm_server_apikey");
        properties.setProperty("rtm_server_secretKey","rtm_server_secretKey");
        properties.setProperty("SERVICE_ACCOUNT_CLIENT_ID","SERVICE_ACCOUNT_CLIENT_ID");
        properties.setProperty("SERVICE_ACCOUNT_PUBLIC_KEY_ID","SERVICE_ACCOUNT_PUBLIC_KEY_ID");
        properties.setProperty("SERVICE_ACCOUNT_PRIVATE_KEY","SERVICE_ACCOUNT_PRIVATE_KEY");
        properties.setProperty("SEND_GRID_API_KEY","sendGridApiKey");
    }

    @Test
    void setupAccountEnvVariableForStaging_test() throws IOException {
        try(MockedConstruction<KMSUtil> kmsUtilMockedConstruction = Mockito.mockConstruction(KMSUtil.class, (kmsUtilMock, context) -> {
            Mockito.when(kmsUtilMock.loadCredentials(anyString(),anyString(),anyString())).thenReturn(properties);
        })){
            ReportAppProperties.setupEnvVariables(AppMode.STAGING);
            Assertions.assertEquals("fullAuthApiKey", CommonAppProperties.getFullAuthApiKey());
            Assertions.assertEquals("fullReminderApiKey", CommonAppProperties.getFullReminderApiKey());
            Assertions.assertEquals("fullMetricApiKey",CommonAppProperties.getFullMetricsApiKey());
            Assertions.assertEquals("fullMetricsTopicName",CommonAppProperties.getFullMetricsTopicName());
            Assertions.assertEquals("fullMetricsSubscriptionName",CommonAppProperties.getFullMetricsSubscriptionName());
            Assertions.assertEquals("rtm_server_apikey",CommonAppProperties.getRtmServerApiKey());
            Assertions.assertEquals("rtm_server_secretKey",CommonAppProperties.getRtmServerSecretKey());
            Assertions.assertEquals("SERVICE_ACCOUNT_CLIENT_ID",CommonAppProperties.getServiceAccountClientId());
            Assertions.assertEquals("SERVICE_ACCOUNT_PUBLIC_KEY_ID",CommonAppProperties.getServiceAccountPublicKeyId());
            Assertions.assertEquals("SERVICE_ACCOUNT_PRIVATE_KEY",CommonAppProperties.getServiceAccountPrivateKey());
            Assertions.assertEquals("sendGridApiKey",CommonAppProperties.getSendGridApiKey());
            Assertions.assertEquals("https://stagingrtm.anywhereworks.com",CommonAppProperties.getRtmServerBaseURL());
            Assertions.assertEquals("https://api.staging.anywhereworks.com/api/v1/channel/publish/message?userId=",CommonAppProperties.getAwChannelPublishUrl());
            Assertions.assertEquals("https://api-dot-staging-goclockin-dashboard.appspot.com",CommonAppProperties.getYoCoDefaultApiUrl());
        }
    }

    @Test
    void setupAccountEnvVariableForLive_test() throws IOException {
        try(MockedConstruction<KMSUtil> kmsUtilMockedConstruction = Mockito.mockConstruction(KMSUtil.class, (kmsUtilMock, context) -> {
            Mockito.when(kmsUtilMock.loadCredentials(anyString(),anyString(),anyString())).thenReturn(properties);
        })){
            ReportAppProperties.setupEnvVariables(AppMode.LIVE);
            Assertions.assertEquals("fullAuthApiKey", CommonAppProperties.getFullAuthApiKey());
            Assertions.assertEquals("fullReminderApiKey", CommonAppProperties.getFullReminderApiKey());
            Assertions.assertEquals("fullMetricApiKey",CommonAppProperties.getFullMetricsApiKey());
            Assertions.assertEquals("fullMetricsTopicName",CommonAppProperties.getFullMetricsTopicName());
            Assertions.assertEquals("fullMetricsSubscriptionName",CommonAppProperties.getFullMetricsSubscriptionName());
            Assertions.assertEquals("rtm_server_apikey",CommonAppProperties.getRtmServerApiKey());
            Assertions.assertEquals("rtm_server_secretKey",CommonAppProperties.getRtmServerSecretKey());
            Assertions.assertEquals("SERVICE_ACCOUNT_CLIENT_ID",CommonAppProperties.getServiceAccountClientId());
            Assertions.assertEquals("SERVICE_ACCOUNT_PUBLIC_KEY_ID",CommonAppProperties.getServiceAccountPublicKeyId());
            Assertions.assertEquals("SERVICE_ACCOUNT_PRIVATE_KEY",CommonAppProperties.getServiceAccountPrivateKey());
            Assertions.assertEquals("sendGridApiKey",CommonAppProperties.getSendGridApiKey());
            Assertions.assertEquals("https://rtmserver.anywhereworks.com",CommonAppProperties.getRtmServerBaseURL());
            Assertions.assertEquals("https://api.anywhereworks.com/api/v1/channel/publish/message?userId=",CommonAppProperties.getAwChannelPublishUrl());
            Assertions.assertEquals("https://api.yocoboard.com",CommonAppProperties.getYoCoDefaultApiUrl());
        }
    }
}
