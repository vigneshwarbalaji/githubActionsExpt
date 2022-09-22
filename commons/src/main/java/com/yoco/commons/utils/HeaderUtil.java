package com.yoco.commons.utils;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.fullauth.api.utils.TokenUtil;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.services.UrlFetcher;
import lombok.extern.slf4j.Slf4j;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public class HeaderUtil {

    private HeaderUtil() {
    }

    private static final String X_CLIENT_ID = "X-ClientID";
    private static final String USER_AGENT = "User-Agent";

    public static final String APP_ENGINE_REGION_HEADER = "X-AppEngine-Region";
    public static final String APP_ENGINE_CITY_HEADER = "X-AppEngine-City";
    public static final String APP_ENGINE_COUNTRY_HEADER = "X-AppEngine-Country";
    public static final String APP_ENGINE_LAT_LONG_HEADER = "X-AppEngine-CityLatLong";
    public static final String CUSTOM_REGION_HEADER = "X-Region";
    public static final String CUSTOM_CITY_HEADER = "X-City";
    public static final String CUSTOM_COUNTRY_HEADER = "X-Country";
    public static final String CUSTOM_LAT_LONG_HEADER = "X-LatLong";
    public static final String CUSTOM_IP_HEADER = "X-ip";

    private static final String getUserAgentHeader() {
        return "AppEngine-Google; (+http://code.google.com/appengine; appid: s~" + (GaeUtils.isAppModeLive() ? GaeUtils.APP_ID_LIVE : GaeUtils.APP_ID_STAGING) + ")";
    }

    public static String extractIpAddressFromRequest(HttpServletRequest request){
        try{
            var ip = request.getHeader(CUSTOM_IP_HEADER);
            if(ObjUtils.isNullOrEmpty(ip)){
                ip = request.getRemoteAddr();
            }
            return ObjUtils.isNullOrEmpty(ip) ? "" : ip.trim();
        }catch (Exception e){
            return "";
        }
    }

    public static String extractClientIdFromRequest(HttpServletRequest request) {

        var srcClientID = "";

        OauthAccessToken accessToken = AnnotationHelper.extractCurrentUserAccessTokenFromRequest(request);

        if (!ObjUtils.isNull(accessToken)) {
            srcClientID = accessToken.getIssuedTo();
        }

        if(!ObjUtils.isNull(request) && !ObjUtils.isNullOrEmpty(request.getHeader(X_CLIENT_ID))) {
            srcClientID = TokenUtil.extractAuthToken( request.getHeader(X_CLIENT_ID), "Client");
            if(ObjUtils.isNull(srcClientID)) {
                srcClientID = request.getHeader(X_CLIENT_ID);
            }
        }

        return srcClientID == null ? "" : srcClientID.trim();
    }

    public static String extractSrcClientFromRequest(HttpServletRequest request){
        var srcClientID = extractClientIdFromRequest(request);
        var client = ClientSource.getClientIdMap().get(srcClientID);
        return ObjUtils.isNullOrEmpty(client) ? "" : client.trim();
    }

    public static String extractRegionFromRequest(HttpServletRequest request){
        String region = request.getHeader(CUSTOM_REGION_HEADER);
        if(ObjUtils.isNullOrEmpty(region)){
            region = request.getHeader(APP_ENGINE_REGION_HEADER);
        }
        return ObjUtils.isNullOrEmpty(region) ? "" : region.trim();
    }

    public static String extractCityFromRequest(HttpServletRequest request){
        String city = request.getHeader(CUSTOM_CITY_HEADER);
        if(ObjUtils.isNullOrEmpty(city)){
            city = request.getHeader(APP_ENGINE_CITY_HEADER);
        }
        return ObjUtils.isNullOrEmpty(city) ? "" : city.trim();
    }

    public static String extractCountryFromRequest(HttpServletRequest request){
        String country = request.getHeader(CUSTOM_COUNTRY_HEADER);
        if(ObjUtils.isNullOrEmpty(country)){
            country = request.getHeader(APP_ENGINE_COUNTRY_HEADER);
        }
        return ObjUtils.isNullOrEmpty(country) ? "" : country.trim();
    }

    public static String extractLatLongFromRequest(HttpServletRequest request){
        String latLong = request.getHeader(CUSTOM_LAT_LONG_HEADER);
        if(ObjUtils.isNullOrEmpty(latLong)){
            latLong = request.getHeader(APP_ENGINE_LAT_LONG_HEADER);
        }
        return ObjUtils.isNullOrEmpty(latLong) ? "" : latLong.trim();
    }

    public static String[] getContentJsonTypeHeader(){
        return new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON};
    }

    public static String[] getServerTokenAuthHeader() throws NoSuchAlgorithmException, IOException {
        return new String[]{UrlFetcher.AUTHORIZATION, UrlFetcher.BEARER + " " + FullAuthService.getServerAccessToken()};
    }

    public static String[] getContentTypedServerTokenAuthHeader() throws NoSuchAlgorithmException, IOException {
        return getContentTypedServerTokenAuthHeader(FullAuthService.getServerAccessToken());
    }

    public static String[] getContentTypedServerTokenAuthHeader(String token){
        return new String[]{ UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION, UrlFetcher.BEARER + " " + token};
    }

    public static String[] getJsonUtf8ContentTypedServerTokenAuthHeader() throws NoSuchAlgorithmException, IOException {
        return new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON_UTF8, UrlFetcher.AUTHORIZATION, UrlFetcher.BEARER + " " + FullAuthService.getServerAccessToken()};
    }

    public static String isMobileUserAgent(HttpServletRequest request) {
        String userAgent = request.getHeader(USER_AGENT);
        if (!ObjUtils.isNullOrEmpty(userAgent) && (userAgent.contains("Dalvik") || userAgent.contains("Android")
                || userAgent.contains("Darwin") || userAgent.contains("PostmanRuntime"))) {
            userAgent = "";
        } else {
            userAgent = "web";
        }

        return userAgent;
    }

    public static Map<String, Object> extractHeaderDetails(HttpServletRequest request) {

        Map<String, Object> headersMap = new HashMap<>();

        headersMap.put("message", getRequestHeaderDetails(request));
        headersMap.put("ip", getRemoteAddress(request));

        return headersMap;
    }

    public static String[] getContentTypedAccessTokenAuthHeader(String userAccessToken) {
        return new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION, UrlFetcher.BEARER + " " + userAccessToken};
    }

    public static String[] getContentTypedServerTokenWithUserAgentHeader() throws NoSuchAlgorithmException, IOException {
        return new String[]{ UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON,
                             UrlFetcher.AUTHORIZATION, UrlFetcher.BEARER + " " + FullAuthService.getServerAccessToken() ,
                             USER_AGENT, getUserAgentHeader()};
    }

    private static String getRequestHeaderDetails(HttpServletRequest request) {

        String region = request.getHeader("X-AppEngine-Region");
        String latLong = request.getHeader("X-AppEngine-CityLatLong");
        String city = request.getHeader("X-AppEngine-City");
        String country = ObjUtils.isNullOrEmpty(request.getHeader("X-Country")) ? request.getHeader("X-AppEngine-Country") :
                request.getHeader("X-Country");

        Map<String, Object> reqHeaderDetails = new HashMap<>();
        reqHeaderDetails.put("region", region);
        reqHeaderDetails.put("latlong", latLong);
        reqHeaderDetails.put("city", city);
        reqHeaderDetails.put("country", country);

        return JsonUtil.getJson(reqHeaderDetails);
    }

    private static String getRemoteAddress(HttpServletRequest request) {
        return ObjUtils.isNullOrEmpty(request.getHeader("X-ip"))
                ? request.getRemoteAddr()
                : request.getHeader("X-ip");
    }
}
