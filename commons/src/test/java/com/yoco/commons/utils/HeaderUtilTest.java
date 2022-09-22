package com.yoco.commons.utils;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.fullservices.FullAuthService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.mock.web.MockHttpServletRequest;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Map;
import static org.mockito.ArgumentMatchers.any;

class HeaderUtilTest {

    @Test
    void extractClientIdFromRequest_nullToken_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)) {
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class)))
                    .thenReturn(null);
            Assertions.assertEquals("",HeaderUtil.extractClientIdFromRequest(null));
        }
    }

    @Test
    void extractClientIdFromRequest_validToken_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)) {

            OauthAccessToken accessToken = new OauthAccessToken();
            accessToken.setAccessToken("token");
            accessToken.setIssuedTo("id");
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class)))
                    .thenReturn(accessToken);
            HttpServletRequest servletRequest = Mockito.mock(HttpServletRequest.class);
            Assertions.assertEquals("id",HeaderUtil.extractClientIdFromRequest(servletRequest));
        }
    }

    @Test
    void extractClientIdFromRequest_validX__with_prefix_client_Header_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)) {
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class)))
                    .thenReturn(null);
            HttpServletRequest servletRequest = Mockito.mock(HttpServletRequest.class);
            Mockito.when(servletRequest.getHeader("X-ClientID")).thenReturn("Client id");
            Assertions.assertEquals("id",HeaderUtil.extractClientIdFromRequest(servletRequest));
        }
    }

    @Test
    void extractClientIdFromRequest_validX_Header_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)) {
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class)))
                    .thenReturn(null);
            HttpServletRequest servletRequest = Mockito.mock(HttpServletRequest.class);
            Mockito.when(servletRequest.getHeader("X-ClientID")).thenReturn("id");
            Assertions.assertEquals("id",HeaderUtil.extractClientIdFromRequest(servletRequest));
        }
    }

    @Test
    void extractClientIdFromRequest_InValidX_Header_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)) {
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class)))
                    .thenReturn(null);
            HttpServletRequest servletRequest = Mockito.mock(HttpServletRequest.class);
            Mockito.when(servletRequest.getHeader("X-ClientID")).thenReturn(null);
            Assertions.assertEquals("",HeaderUtil.extractClientIdFromRequest(servletRequest));
        }
    }

    @Test
    void getServerTokenAuthHeader_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("serverToken");
            String[] resp = HeaderUtil.getServerTokenAuthHeader();
            String[] expected = new String[]{ "Authorization", "Bearer serverToken"};
            Assertions.assertEquals(Arrays.asList(expected), Arrays.asList(resp));
        }
    }

    @Test
    void getContentTypedServerTokenAuthHeader_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("serverToken");
            String[] resp = HeaderUtil.getContentTypedServerTokenAuthHeader();
            String[] expected = new String[]{ "Content-Type","application/json","Authorization", "Bearer serverToken"};
            Assertions.assertEquals(Arrays.asList(expected), Arrays.asList(resp));
        }
    }

    @Test
    void getContentTypedAccessTokenAuthHeader_test(){
        String[] resp = HeaderUtil.getContentTypedAccessTokenAuthHeader("token");
        String[] expected = new String[]{ "Content-Type","application/json","Authorization", "Bearer token"};
        Assertions.assertEquals(Arrays.asList(expected), Arrays.asList(resp));
    }

    @Test
    void getJsonUtf8ContentTypedServerTokenAuthHeader_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){
            fullAuthServiceMockedStatic.when(() -> FullAuthService.getServerAccessToken()).thenReturn("serverToken");
            String[] resp = HeaderUtil.getJsonUtf8ContentTypedServerTokenAuthHeader();
            String[] expected = new String[]{ "Content-Type","application/json; charset=utf-8","Authorization", "Bearer serverToken"};
            Assertions.assertEquals(Arrays.asList(expected), Arrays.asList(resp));
        }
    }

    @Test
    void getContentTypedServerTokenWithUserAgentHeader_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){
            fullAuthServiceMockedStatic.when(() -> FullAuthService.getServerAccessToken()).thenReturn("serverToken");
            String[] resp = HeaderUtil.getContentTypedServerTokenWithUserAgentHeader();
            String[] expected = new String[]{ "Content-Type","application/json","Authorization", "Bearer serverToken","User-Agent", "AppEngine-Google; (+http://code.google.com/appengine; appid: s~staging-goclockin-dashboard)"};
            Assertions.assertEquals(Arrays.asList(expected), Arrays.asList(resp));
        }
    }

    @Test
    void getContentTypedServerTokenWithUserAgentHeader_live_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(()->GaeUtils.isAppModeLive()).thenReturn(true);
            fullAuthServiceMockedStatic.when(() -> FullAuthService.getServerAccessToken()).thenReturn("serverToken");
            String[] resp = HeaderUtil.getContentTypedServerTokenWithUserAgentHeader();
            String[] expected = new String[]{ "Content-Type","application/json","Authorization", "Bearer serverToken","User-Agent", "AppEngine-Google; (+http://code.google.com/appengine; appid: s~live-goclockin-dashboard)"};
            Assertions.assertEquals(Arrays.asList(expected), Arrays.asList(resp));
        }
    }

    @Test
    void extractIpAddressFromRequest_nullRequest_test(){
        Assertions.assertEquals("",HeaderUtil.extractIpAddressFromRequest(null));
    }

    @Test
    void extractIpAddressFromRequest_nullCustomIP_nullRemoteAddr_test(){
        HttpServletRequest request = Mockito.mock(HttpServletRequest.class);
        Mockito.when(request.getRemoteAddr()).thenReturn(null);
        Mockito.when(request.getHeader("X-ip")).thenReturn(null);
        Assertions.assertEquals("",HeaderUtil.extractIpAddressFromRequest(request));
    }

    @Test
    void extractIpAddressFromRequest_validCustomHeaderIP_test(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader("X-ip","1.1.1.1");
        Assertions.assertEquals("1.1.1.1",HeaderUtil.extractIpAddressFromRequest(request));
    }

    @Test
    void extractIpAddressFromRequest_validRemoteAddr_test(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.setRemoteAddr("1.1.1.1");
        Assertions.assertEquals("1.1.1.1",HeaderUtil.extractIpAddressFromRequest(request));
    }

    @Test
    void extractSrcClientFromRequest_nullClient_test(){
        HttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class)){
            headerUtilMockedStatic.when(()->HeaderUtil.extractClientIdFromRequest(request)).thenReturn("client1");
            headerUtilMockedStatic.when(()->HeaderUtil.extractSrcClientFromRequest(request)).thenCallRealMethod();
            clientSourceMockedStatic.when(ClientSource::getClientIdMap).thenReturn(Map.of());
            Assertions.assertEquals("",HeaderUtil.extractSrcClientFromRequest(request));
        }
    }

    @Test
    void extractSrcClientFromRequest_ValidClient_test(){
        HttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class)){
            headerUtilMockedStatic.when(()->HeaderUtil.extractClientIdFromRequest(request)).thenReturn("client1");
            headerUtilMockedStatic.when(()->HeaderUtil.extractSrcClientFromRequest(request)).thenCallRealMethod();
            clientSourceMockedStatic.when(ClientSource::getClientIdMap).thenReturn(Map.of("client1","client"));
            Assertions.assertEquals("client",HeaderUtil.extractSrcClientFromRequest(request));
        }
    }

    @Test
    void extractLocationDetailsFromRequest_nullHeaders_test(){
        HttpServletRequest request = new MockHttpServletRequest();
        Assertions.assertEquals("",HeaderUtil.extractRegionFromRequest(request));
        Assertions.assertEquals("",HeaderUtil.extractCityFromRequest(request));
        Assertions.assertEquals("",HeaderUtil.extractCountryFromRequest(request));
        Assertions.assertEquals("",HeaderUtil.extractLatLongFromRequest(request));
    }

    @Test
    void extractLocationDetailsFromRequest_validCustomHeaders_test(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader("X-Region","TN");
        request.addHeader("X-City","che");
        request.addHeader("X-Country","IN");
        request.addHeader("X-LatLong","1.1,2.2");
        Assertions.assertEquals("TN",HeaderUtil.extractRegionFromRequest(request));
        Assertions.assertEquals("che",HeaderUtil.extractCityFromRequest(request));
        Assertions.assertEquals("IN",HeaderUtil.extractCountryFromRequest(request));
        Assertions.assertEquals("1.1,2.2",HeaderUtil.extractLatLongFromRequest(request));
    }

    @Test
    void extractLocationDetailsFromRequest_validAppEngineHeaders_test(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader("X-AppEngine-Region","TN");
        request.addHeader("X-AppEngine-City","che");
        request.addHeader("X-AppEngine-Country","IN");
        request.addHeader("X-AppEngine-CityLatLong","1.1,2.2");
        Assertions.assertEquals("TN",HeaderUtil.extractRegionFromRequest(request));
        Assertions.assertEquals("che",HeaderUtil.extractCityFromRequest(request));
        Assertions.assertEquals("IN",HeaderUtil.extractCountryFromRequest(request));
        Assertions.assertEquals("1.1,2.2",HeaderUtil.extractLatLongFromRequest(request));
    }


}
