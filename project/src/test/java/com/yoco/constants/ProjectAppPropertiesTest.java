package com.yoco.constants;

import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.enums.AppMode;
import com.yoco.commons.utils.KMSUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.*;

class ProjectAppPropertiesTest {

    public Properties getProperties(){
        Properties properties = new Properties();
        properties.setProperty("FULL_METRICS_API_KEY","fullMetricsApiKey");
        properties.setProperty("FULL_METRICS_TOPIC_NAME","fullMetricsTopicName");
        properties.setProperty("FULL_METRICS_SUBSCRIPTION_NAME","fullMetricsSubscriptionName");
        properties.setProperty("strFullAuthClientID","fullAuthClientId");
        properties.setProperty("strFullAuthClientSecret","fullAuthClientSecret");
        properties.setProperty("FCM_KEY","fcmKey");
        properties.setProperty("rtm_server_apikey","rtmServerApiKey");
        properties.setProperty("rtm_server_secretKey","rtmServerSecretKey");
        properties.setProperty("SERVICE_ACCOUNT_CLIENT_ID","serviceAccountClientId");
        properties.setProperty("SERVICE_ACCOUNT_PUBLIC_KEY_ID","serviceAccountPublicKeyId");
        properties.setProperty("SERVICE_ACCOUNT_PRIVATE_KEY","serviceAccountPrivateKey");
        return properties;
    }

    @Test
    void setupEnvVariables_staging_test() throws IOException {
        try(MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class);
            MockedConstruction<KMSUtil> kmsUtilMockedConstruction = Mockito.mockConstruction(KMSUtil.class, (kmsUtilMock, context) -> {
            Mockito.when(kmsUtilMock.loadCredentials(anyString(),anyString(),anyString())).thenReturn(getProperties());
        })){
            clientSourceMockedStatic.when(() -> ClientSource.setClientIdMap(anyBoolean())).thenCallRealMethod();
            ProjectAppProperties.setupEnvVariables(AppMode.STAGING);
            Assertions.assertEquals("fullMetricsApiKey",CommonAppProperties.getFullMetricsApiKey());
            Assertions.assertEquals("fullMetricsTopicName",CommonAppProperties.getFullMetricsTopicName());
            Assertions.assertEquals("fullMetricsSubscriptionName",CommonAppProperties.getFullMetricsSubscriptionName());
            Assertions.assertEquals("fullAuthClientId",CommonAppProperties.getFullOauthClientId());
            Assertions.assertEquals("fullAuthClientSecret",CommonAppProperties.getFullOauthClientSecret());
            Assertions.assertEquals("fcmKey",CommonAppProperties.getFcmKey());
            Assertions.assertEquals("rtmServerApiKey",CommonAppProperties.getRtmServerApiKey());
            Assertions.assertEquals("rtmServerSecretKey",CommonAppProperties.getRtmServerSecretKey());
            Assertions.assertEquals("serviceAccountClientId",CommonAppProperties.getServiceAccountClientId());
            Assertions.assertEquals("serviceAccountPublicKeyId",CommonAppProperties.getServiceAccountPublicKeyId());
            Assertions.assertEquals("serviceAccountPrivateKey",CommonAppProperties.getServiceAccountPrivateKey());
            Assertions.assertEquals("https://stagingrtm.anywhereworks.com",CommonAppProperties.getRtmServerBaseURL());
            Assertions.assertEquals("https://api.staging.anywhereworks.com/api/v1/channel/publish/message?userId=",CommonAppProperties.getAwChannelPublishUrl());
            Assertions.assertEquals("https://project-dot-staging-goclockin-dashboard.appspot.com",CommonAppProperties.getAppUrl());
            clientSourceMockedStatic.verify(() -> ClientSource.setClientIdMap(false));
        }
    }

    @Test
    void setupEnvVariables_live_test() throws IOException {
        try(MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class);
            MockedConstruction<KMSUtil> kmsUtilMockedConstruction = Mockito.mockConstruction(KMSUtil.class, (kmsUtilMock, context) -> {
                Mockito.when(kmsUtilMock.loadCredentials(anyString(),anyString(),anyString())).thenReturn(getProperties());
            })){
            clientSourceMockedStatic.when(() -> ClientSource.setClientIdMap(anyBoolean())).thenCallRealMethod();
            ProjectAppProperties.setupEnvVariables(AppMode.LIVE);
            Assertions.assertEquals("fullMetricsApiKey",CommonAppProperties.getFullMetricsApiKey());
            Assertions.assertEquals("fullMetricsTopicName",CommonAppProperties.getFullMetricsTopicName());
            Assertions.assertEquals("fullMetricsSubscriptionName",CommonAppProperties.getFullMetricsSubscriptionName());
            Assertions.assertEquals("fullAuthClientId",CommonAppProperties.getFullOauthClientId());
            Assertions.assertEquals("fullAuthClientSecret",CommonAppProperties.getFullOauthClientSecret());
            Assertions.assertEquals("fcmKey",CommonAppProperties.getFcmKey());
            Assertions.assertEquals("rtmServerApiKey",CommonAppProperties.getRtmServerApiKey());
            Assertions.assertEquals("rtmServerSecretKey",CommonAppProperties.getRtmServerSecretKey());
            Assertions.assertEquals("serviceAccountClientId",CommonAppProperties.getServiceAccountClientId());
            Assertions.assertEquals("serviceAccountPublicKeyId",CommonAppProperties.getServiceAccountPublicKeyId());
            Assertions.assertEquals("serviceAccountPrivateKey",CommonAppProperties.getServiceAccountPrivateKey());
            Assertions.assertEquals("https://rtmserver.anywhereworks.com",CommonAppProperties.getRtmServerBaseURL());
            Assertions.assertEquals("https://api.anywhereworks.com/api/v1/channel/publish/message?userId=",CommonAppProperties.getAwChannelPublishUrl());
            Assertions.assertEquals("https://project.yocoboard.com",CommonAppProperties.getAppUrl());
            clientSourceMockedStatic.verify(() -> ClientSource.setClientIdMap(true));
        }
    }

}
