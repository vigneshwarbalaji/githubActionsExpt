package com.yoco.commons.constants;

import org.springframework.security.web.header.writers.StaticHeadersWriter;

public class CommonAppProperties {

    private CommonAppProperties(){}

    private static String appUrl = "";
    private static String fullMetricsApiKey = "";
    private static String fullMetricsTopicName = "";
    private static String fullMetricsSubscriptionName = "";
    private static String fullOauthClientId = "";
    private static String fullOauthClientSecret = "";
    private static String fcmKey = "";
    private static String rtmServerApiKey = "";
    private static String rtmServerSecretKey = "";
    private static String rtmServerBaseURL = "";
    private static String awHostUrl = "";
    private static String awChannelPublishUrl = "";
    private static String serviceAccountClientId = "";
    private static String serviceAccountPrivateKey = "";
    private static String serviceAccountPublicKeyId = "";
    private static String fullReminderApiKey = "";
    private static String fullAuthApiKey = "";
    private static String yoCoDefaultApiUrl = "";
    private static String yoCoDashboardUrl = "";
    private static String sendGridApiKey = "";
    private static String jiraUrl = "";
    public static final StaticHeadersWriter strictTransportSecurityHeader = new StaticHeadersWriter("strict-transport-security","max-age=31536000; includeSubDomains; preload");
    private static String googleMapsZoneApiKey = "";

    public static String getFullMetricsApiKey() {
        return fullMetricsApiKey;
    }

    public static void setFullMetricsApiKey(String fullMetricsApiKey) {
        CommonAppProperties.fullMetricsApiKey = fullMetricsApiKey;
    }

    public static String getFullMetricsTopicName() {
        return fullMetricsTopicName;
    }

    public static void setFullMetricsTopicName(String fullMetricsTopicName) {
        CommonAppProperties.fullMetricsTopicName = fullMetricsTopicName;
    }

    public static String getFullMetricsSubscriptionName() {
        return fullMetricsSubscriptionName;
    }

    public static void setFullMetricsSubscriptionName(String fullMetricsSubscriptionName) {
        CommonAppProperties.fullMetricsSubscriptionName = fullMetricsSubscriptionName;
    }

    public static String getAppUrl() {
        return appUrl;
    }

    public static void setAppUrl(String appUrl) {
        CommonAppProperties.appUrl = appUrl;
    }

    public static String getFullOauthClientId() {
        return fullOauthClientId;
    }

    public static void setFullOauthClientId(String fullOauthClientId) {
        CommonAppProperties.fullOauthClientId = fullOauthClientId;
    }

    public static String getFullOauthClientSecret() {
        return fullOauthClientSecret;
    }

    public static void setFullOauthClientSecret(String fullOauthClientSecret) {
        CommonAppProperties.fullOauthClientSecret = fullOauthClientSecret;
    }

    public static String getFcmKey() {
        return fcmKey;
    }

    public static void setFcmKey(String fcmKey) {
        CommonAppProperties.fcmKey = fcmKey;
    }

    public static String getRtmServerApiKey() {
        return rtmServerApiKey;
    }

    public static void setRtmServerApiKey(String rtmServerApiKey) {
        CommonAppProperties.rtmServerApiKey = rtmServerApiKey;
    }

    public static String getRtmServerSecretKey() {
        return rtmServerSecretKey;
    }

    public static void setRtmServerSecretKey(String rtmServerSecretKey) {
        CommonAppProperties.rtmServerSecretKey = rtmServerSecretKey;
    }

    public static String getRtmServerBaseURL() {
        return rtmServerBaseURL;
    }

    public static void setRtmServerBaseURL(String rtmServerBaseURL) {
        CommonAppProperties.rtmServerBaseURL = rtmServerBaseURL;
    }

    public static String getAwChannelPublishUrl() {
        return awChannelPublishUrl;
    }

    public static void setAwChannelPublishUrl(String awChannelPublishUrl) {
        CommonAppProperties.awChannelPublishUrl = awChannelPublishUrl;
    }

    public static String getServiceAccountClientId() {
        return serviceAccountClientId;
    }

    public static void setServiceAccountClientId(String serviceAccountClientId) {
        CommonAppProperties.serviceAccountClientId = serviceAccountClientId;
    }

    public static String getServiceAccountPrivateKey() {
        return serviceAccountPrivateKey;
    }

    public static void setServiceAccountPrivateKey(String serviceAccountPrivateKey) {
        CommonAppProperties.serviceAccountPrivateKey = serviceAccountPrivateKey;
    }

    public static String getServiceAccountPublicKeyId() {
        return serviceAccountPublicKeyId;
    }

    public static void setServiceAccountPublicKeyId(String serviceAccountPublicKeyId) {
        CommonAppProperties.serviceAccountPublicKeyId = serviceAccountPublicKeyId;
    }

    public static String getFullReminderApiKey() {
        return fullReminderApiKey;
    }

    public static void setFullReminderApiKey(String fullReminderApiKey) {
        CommonAppProperties.fullReminderApiKey = fullReminderApiKey;
    }

    public static String getFullAuthApiKey() {
        return fullAuthApiKey;
    }

    public static void setFullAuthApiKey(String fullAuthApiKey) {
        CommonAppProperties.fullAuthApiKey = fullAuthApiKey;
    }

    public static String getYoCoDefaultApiUrl() {
        return yoCoDefaultApiUrl;
    }

    public static void setYoCoDefaultApiUrl(String yoCoDefaultApiUrl) {
        CommonAppProperties.yoCoDefaultApiUrl = yoCoDefaultApiUrl;
    }

    public static String getSendGridApiKey() {
        return sendGridApiKey;
    }

    public static void setSendGridApiKey(String sendGridApiKey) {
        CommonAppProperties.sendGridApiKey = sendGridApiKey;
    }

    public static String getYoCoDashboardUrl() {
        return yoCoDashboardUrl;
    }

    public static void setYoCoDashboardUrl(String yoCoDashboardUrl) {
        CommonAppProperties.yoCoDashboardUrl = yoCoDashboardUrl;
    }

    public static String getJiraUrl() {
        return jiraUrl;
    }

    public static void setJiraUrl(String jiraUrl) {
        CommonAppProperties.jiraUrl = jiraUrl;
    }

    public static String getGoogleMapsZoneApiKey() { return googleMapsZoneApiKey; }

    public static void setGoogleMapsZoneApiKey(String googleMapsZoneApiKey) {
        CommonAppProperties.googleMapsZoneApiKey = googleMapsZoneApiKey;
    }

    public static String getAwHostUrl() {return awHostUrl;}

    public static void setAwHostUrl(String awHostUrl) {
        CommonAppProperties.awHostUrl = awHostUrl;
    }

}
