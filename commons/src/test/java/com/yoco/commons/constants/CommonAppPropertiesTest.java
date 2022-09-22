package com.yoco.commons.constants;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class CommonAppPropertiesTest {

    @Test
    void getAppUrl_test(){
        CommonAppProperties.setAppUrl("https://url");
        Assertions.assertEquals("https://url",CommonAppProperties.getAppUrl());
    }

    @Test
    void getFullMetricsApiKey_test(){
        CommonAppProperties.setFullMetricsApiKey("fullMetricsApiKey");
        Assertions.assertEquals("fullMetricsApiKey",CommonAppProperties.getFullMetricsApiKey());
    }

    @Test
    void getFullMetricsTopicName_test(){
        CommonAppProperties.setFullMetricsTopicName("fullMetricsTopicName");
        Assertions.assertEquals("fullMetricsTopicName",CommonAppProperties.getFullMetricsTopicName());
    }

    @Test
    void getFullMetricsSubscriptionName_test(){
        CommonAppProperties.setFullMetricsSubscriptionName("fullMetricsSubscriptionName");
        Assertions.assertEquals("fullMetricsSubscriptionName",CommonAppProperties.getFullMetricsSubscriptionName());
    }

    @Test
    void getFullOAuthClientId_test(){
        CommonAppProperties.setFullOauthClientId("fullOAuthClientId");
        Assertions.assertEquals("fullOAuthClientId",CommonAppProperties.getFullOauthClientId());
    }

    @Test
    void getFullOAuthClientSecret_test(){
        CommonAppProperties.setFullOauthClientSecret("fullOAuthClientSecret");
        Assertions.assertEquals("fullOAuthClientSecret",CommonAppProperties.getFullOauthClientSecret());
    }

    @Test
    void getFcmKey_test(){
        CommonAppProperties.setFcmKey("fcmKey");
        Assertions.assertEquals("fcmKey",CommonAppProperties.getFcmKey());
    }

    @Test
    void getRtmServerApiKey_test(){
        CommonAppProperties.setRtmServerApiKey("rtmServerApiKey");
        Assertions.assertEquals("rtmServerApiKey",CommonAppProperties.getRtmServerApiKey());
    }

    @Test
    void getRtmServerSecretKey_test(){
        CommonAppProperties.setRtmServerSecretKey("rtmServerSecretKey");
        Assertions.assertEquals("rtmServerSecretKey",CommonAppProperties.getRtmServerSecretKey());
    }

    @Test
    void getRtmServerBaseURL_test(){
        CommonAppProperties.setRtmServerBaseURL("rtmServerBaseURL");
        Assertions.assertEquals("rtmServerBaseURL",CommonAppProperties.getRtmServerBaseURL());
    }

    @Test
    void getAwChannelPublishUrl_test(){
        CommonAppProperties.setAwChannelPublishUrl("awChannelPublishUrl");
        Assertions.assertEquals("awChannelPublishUrl",CommonAppProperties.getAwChannelPublishUrl());
    }

    @Test
    void getServiceAccountClientId_test(){
        CommonAppProperties.setServiceAccountClientId("serviceAccountClientId");
        Assertions.assertEquals("serviceAccountClientId",CommonAppProperties.getServiceAccountClientId());
    }

    @Test
    void getServiceAccountPrivateKey_test(){
        CommonAppProperties.setServiceAccountPrivateKey("serviceAccountPrivateKey");
        Assertions.assertEquals("serviceAccountPrivateKey",CommonAppProperties.getServiceAccountPrivateKey());
    }

    @Test
    void getServiceAccountPublicKeyId_test(){
        CommonAppProperties.setServiceAccountPublicKeyId("serviceAccountPublicKeyId");
        Assertions.assertEquals("serviceAccountPublicKeyId",CommonAppProperties.getServiceAccountPublicKeyId());
    }

    @Test
    void getFullReminderApiKey_test(){
        CommonAppProperties.setFullReminderApiKey("fullReminderApiKey");
        Assertions.assertEquals("fullReminderApiKey",CommonAppProperties.getFullReminderApiKey());
    }

    @Test
    void getFullAuthApiKey_test(){
        CommonAppProperties.setFullAuthApiKey("fullAuthApiKey");
        Assertions.assertEquals("fullAuthApiKey",CommonAppProperties.getFullAuthApiKey());
    }

    @Test
    void getYoCoDefaultApiUrl_test(){
        CommonAppProperties.setYoCoDefaultApiUrl("https://api");
        Assertions.assertEquals("https://api",CommonAppProperties.getYoCoDefaultApiUrl());
    }

    @Test
    void getSendGridApiKey_test(){
        CommonAppProperties.setSendGridApiKey("sendGridKey");
        Assertions.assertEquals("sendGridKey",CommonAppProperties.getSendGridApiKey());
    }

    @Test
    void getYoCoDashboardUrl_test(){
        CommonAppProperties.setYoCoDashboardUrl("https://yoco");
        Assertions.assertEquals("https://yoco",CommonAppProperties.getYoCoDashboardUrl());
    }

    @Test
    void getGoogleMapsZoneApiKey_test(){
        CommonAppProperties.setGoogleMapsZoneApiKey("tzkey");
        Assertions.assertEquals("tzkey",CommonAppProperties.getGoogleMapsZoneApiKey());
    }

    @Test
    void getAwHostUrl_test(){
        CommonAppProperties.setAwHostUrl("hosturl");
        Assertions.assertEquals("hosturl",CommonAppProperties.getAwHostUrl());
    }

}
