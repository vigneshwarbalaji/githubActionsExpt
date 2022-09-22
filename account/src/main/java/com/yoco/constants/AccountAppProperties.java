package com.yoco.constants;

import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.enums.AppMode;
import com.yoco.commons.utils.KMSUtil;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.util.Properties;

@Slf4j
public class AccountAppProperties {

    private AccountAppProperties(){}

    public static final void setupEnvVariables(AppMode appMode) throws IOException {
        if(AppMode.LIVE.equals(appMode)){
            setupAccountEnvVariableForLive();
        }else{
            setupAccountEnvVariableForStaging();
        }
    }

    private static void setupAccountEnvVariableForStaging() throws IOException {
        var fileName = "staging-credentials.properties.enc";
        var kmsResourceId = "projects/secretsmanagement-staging/locations/global/keyRings/yocoboard/cryptoKeys/yocoboard-default";
        var kmsApplicationName = "secretsmanagement-staging";

        var properties = new KMSUtil().loadCredentials(fileName, kmsResourceId, kmsApplicationName);
        setKMSProperties(properties);

        CommonAppProperties.setRtmServerBaseURL("https://stagingrtm.anywhereworks.com");
        CommonAppProperties.setAwHostUrl("https://api.staging.anywhereworks.com/");
        CommonAppProperties.setAwChannelPublishUrl(CommonAppProperties.getAwHostUrl()+"api/v1/channel/publish/message?userId=");
        CommonAppProperties.setAppUrl("https://account-dot-staging-goclockin-dashboard.appspot.com");
        CommonAppProperties.setYoCoDefaultApiUrl("https://api-dot-staging-goclockin-dashboard.appspot.com");
        CommonAppProperties.setYoCoDashboardUrl("https://my.staging.yocoboard.com/");
        ClientSource.setClientIdMap(false);
    }

    private static void setupAccountEnvVariableForLive() throws IOException {
        var fileName = "live-credentials.properties.enc";
        var kmsResourceId ="projects/secretsmanagement-production/locations/global/keyRings/yocoboard/cryptoKeys/yocoboard-default";
        var kmsApplicationName = "secretsmanagement-production";

        var properties = new KMSUtil().loadCredentials(fileName, kmsResourceId, kmsApplicationName);
        setKMSProperties(properties);

        CommonAppProperties.setRtmServerBaseURL("https://rtmserver.anywhereworks.com");
        CommonAppProperties.setAwHostUrl("https://api.anywhereworks.com/");
        CommonAppProperties.setAwChannelPublishUrl(CommonAppProperties.getAwHostUrl()+"api/v1/channel/publish/message?userId=");
        CommonAppProperties.setAppUrl("https://account.yocoboard.com");
        CommonAppProperties.setYoCoDashboardUrl("https://my.yocoboard.com/");
        CommonAppProperties.setYoCoDefaultApiUrl("https://api.yocoboard.com");
        ClientSource.setClientIdMap(true);
    }

    private static void setKMSProperties(Properties properties){

        CommonAppProperties.setFullMetricsApiKey(properties.getProperty("FULL_METRICS_API_KEY"));
        CommonAppProperties.setFullMetricsTopicName(properties.getProperty("FULL_METRICS_TOPIC_NAME"));
        CommonAppProperties.setFullMetricsSubscriptionName(properties.getProperty("FULL_METRICS_SUBSCRIPTION_NAME"));

        CommonAppProperties.setFullAuthApiKey(properties.getProperty("FULLAUTH_API_KEY"));
        CommonAppProperties.setFullReminderApiKey(properties.getProperty("FULL_REMINDER_API_KEY"));

        CommonAppProperties.setRtmServerApiKey(properties.getProperty("rtm_server_apikey"));
        CommonAppProperties.setRtmServerSecretKey(properties.getProperty("rtm_server_secretKey"));

        CommonAppProperties.setServiceAccountClientId(properties.getProperty("SERVICE_ACCOUNT_CLIENT_ID"));
        CommonAppProperties.setServiceAccountPublicKeyId(properties.getProperty("SERVICE_ACCOUNT_PUBLIC_KEY_ID"));
        CommonAppProperties.setServiceAccountPrivateKey(properties.getProperty("SERVICE_ACCOUNT_PRIVATE_KEY"));

        CommonAppProperties.setSendGridApiKey(properties.getProperty("SEND_GRID_API_KEY"));

        CommonAppProperties.setFullOauthClientId(properties.getProperty("strFullAuthClientID"));
        CommonAppProperties.setFullOauthClientSecret(properties.getProperty("strFullAuthClientSecret"));
        CommonAppProperties.setFcmKey(properties.getProperty("FCM_KEY"));

        CommonAppProperties.setGoogleMapsZoneApiKey(properties.getProperty("TZ_API_KEY"));
    }

}