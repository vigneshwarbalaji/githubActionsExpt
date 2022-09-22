package com.yoco.constants;

import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.enums.AppMode;
import com.yoco.commons.utils.KMSUtil;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.Properties;

@Slf4j
public class ProjectAppProperties {

    private ProjectAppProperties(){}

    public static final void setupEnvVariables(AppMode appMode) throws IOException {
        if(AppMode.LIVE.equals(appMode)){
            setupProjectEnvVariableForLive();
        }else{
            setupProjectEnvVariableForStaging();
        }
    }

    private static void setupProjectEnvVariableForStaging() throws IOException {
        var fileName = "staging-credentials.properties.enc";
        var kmsResourceId = "projects/secretsmanagement-staging/locations/global/keyRings/yocoboard/cryptoKeys/yocoboard-default";
        var kmsApplicationName = "secretsmanagement-staging";

        var properties = new KMSUtil().loadCredentials(fileName, kmsResourceId, kmsApplicationName);
        setProjectKMSProperties(properties);

        CommonAppProperties.setRtmServerBaseURL("https://stagingrtm.anywhereworks.com");
        CommonAppProperties.setAwChannelPublishUrl("https://api.staging.anywhereworks.com/api/v1/channel/publish/message?userId=");
        CommonAppProperties.setAppUrl("https://project-dot-staging-goclockin-dashboard.appspot.com");
        CommonAppProperties.setJiraUrl("https://api.atlassian.com/oauth/token/accessible-resources");
        ClientSource.setClientIdMap(false);
    }

    private static void setupProjectEnvVariableForLive() throws IOException {
        var fileName = "live-credentials.properties.enc";
        var kmsResourceId ="projects/secretsmanagement-production/locations/global/keyRings/yocoboard/cryptoKeys/yocoboard-default";
        var kmsApplicationName = "secretsmanagement-production";

        var properties = new KMSUtil().loadCredentials(fileName, kmsResourceId, kmsApplicationName);
        setProjectKMSProperties(properties);

        CommonAppProperties.setRtmServerBaseURL("https://rtmserver.anywhereworks.com");
        CommonAppProperties.setAwChannelPublishUrl("https://api.anywhereworks.com/api/v1/channel/publish/message?userId=");
        CommonAppProperties.setAppUrl("https://project.yocoboard.com");
        CommonAppProperties.setJiraUrl("https://api.atlassian.com/oauth/token/accessible-resources");

        ClientSource.setClientIdMap(true);
    }

    private static void setProjectKMSProperties(Properties properties){
        CommonAppProperties.setFullMetricsApiKey(properties.getProperty("FULL_METRICS_API_KEY"));
        CommonAppProperties.setFullMetricsTopicName(properties.getProperty("FULL_METRICS_TOPIC_NAME"));
        CommonAppProperties.setFullMetricsSubscriptionName(properties.getProperty("FULL_METRICS_SUBSCRIPTION_NAME"));
        CommonAppProperties.setFullReminderApiKey(properties.getProperty("FULL_REMINDER_API_KEY"));
        CommonAppProperties.setFullAuthApiKey(properties.getProperty("FULLAUTH_API_KEY"));

        CommonAppProperties.setFullOauthClientId(properties.getProperty("strFullAuthClientID"));
        CommonAppProperties.setFullOauthClientSecret(properties.getProperty("strFullAuthClientSecret"));

        CommonAppProperties.setFcmKey(properties.getProperty("FCM_KEY"));

        CommonAppProperties.setRtmServerApiKey(properties.getProperty("rtm_server_apikey"));
        CommonAppProperties.setRtmServerSecretKey(properties.getProperty("rtm_server_secretKey"));

        CommonAppProperties.setServiceAccountClientId(properties.getProperty("SERVICE_ACCOUNT_CLIENT_ID"));
        CommonAppProperties.setServiceAccountPublicKeyId(properties.getProperty("SERVICE_ACCOUNT_PUBLIC_KEY_ID"));
        CommonAppProperties.setServiceAccountPrivateKey(properties.getProperty("SERVICE_ACCOUNT_PRIVATE_KEY"));
    }

}
