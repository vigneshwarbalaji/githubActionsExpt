package com.yoco.account.helper;

import co.anywhere.awconfigurator.model.ratelimit.RateLimitInfo;
import com.fullauth.api.exception.TokenResponseException;
import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.account.modal.AccountCreationPayloadDTO;
import com.yoco.account.modal.GeoLocationDTO;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.fullservices.RateConfigurator;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.UserPROUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.mock.web.MockHttpServletRequest;
import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;

class AccountCreationHelperTest {
    @Test
    void isSignUpRateLimitExceeded_nullRateLimitInfo(){
        HttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<RateConfigurator> rateConfiguratorMockedStatic = Mockito.mockStatic(RateConfigurator.class)){
            headerUtilMockedStatic.when(()->HeaderUtil.extractIpAddressFromRequest(request)).thenReturn("1.1");
            rateConfiguratorMockedStatic.when(()->RateConfigurator.checkRateLimitUsage("1.1_signup",10,24*60*60)).thenReturn(null);
            Assertions.assertFalse(AccountCreationHelper.isSignUpRateLimitExceeded(request));
        }
    }

    @Test
    void isSignUpRateLimitExceeded_RateLimitInfo_exceeded(){
        HttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<RateConfigurator> rateConfiguratorMockedStatic = Mockito.mockStatic(RateConfigurator.class)){
            headerUtilMockedStatic.when(()->HeaderUtil.extractIpAddressFromRequest(request)).thenReturn("1.1");
            RateLimitInfo info = new RateLimitInfo();
            info.setAllow(false);
            rateConfiguratorMockedStatic.when(()->RateConfigurator.checkRateLimitUsage("1.1_signup",10,24*60*60)).thenReturn(info);
            Assertions.assertTrue(AccountCreationHelper.isSignUpRateLimitExceeded(request));
        }
    }

    @Test
    void isSignUpRateLimitExceeded_RateLimitInfo_allowed(){
        HttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<RateConfigurator> rateConfiguratorMockedStatic = Mockito.mockStatic(RateConfigurator.class)){
            headerUtilMockedStatic.when(()->HeaderUtil.extractIpAddressFromRequest(request)).thenReturn("1.1");
            RateLimitInfo info = new RateLimitInfo();
            info.setAllow(true);
            rateConfiguratorMockedStatic.when(()->RateConfigurator.checkRateLimitUsage("1.1_signup",10,24*60*60)).thenReturn(info);
            Assertions.assertFalse(AccountCreationHelper.isSignUpRateLimitExceeded(request));
        }
    }

    @Test
    void validatePayloadAndExtractDTO_invalidJson_test(){
        try{
            AccountCreationHelper.validatePayloadAndExtractDTO("invalid",null,"");
        }catch (Exception e){
            Assertions.assertEquals("Payload cannot be empty or null.",e.getMessage());
        }
    }

    @Test
    void validatePayloadAndExtractDTO_empty_test(){
        try{
            AccountCreationHelper.validatePayloadAndExtractDTO("{}",null,"");
        }catch (Exception e){
            Assertions.assertEquals("Payload cannot be empty or null.",e.getMessage());
        }
    }

    @Test
    void validatePayloadAndExtractDTO_userAlreadyAssociated_test(){
        try(MockedConstruction<AccountCreationPayloadDTO> mock = Mockito.mockConstruction(AccountCreationPayloadDTO.class,(accountCreationPayloadMock,context)->{
            Mockito.when(accountCreationPayloadMock.getEmailID()).thenReturn("email");});
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAssociatedToYoco("email")).thenReturn(true);
            AccountCreationHelper.validatePayloadAndExtractDTO("{\"key\":\"value\"}",null,"");
        }catch (Exception e){
            Assertions.assertEquals("Email is already registered. Please login",e.getMessage());
        }
    }

    @Test
    void validatePayloadAndExtractDTO_valid_test(){
        try(MockedConstruction<AccountCreationPayloadDTO> mock = Mockito.mockConstruction(AccountCreationPayloadDTO.class,(accountCreationPayloadMock,context)->{
            Mockito.when(accountCreationPayloadMock.getEmailID()).thenReturn("email");});
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAssociatedToYoco("email")).thenReturn(false);
            AccountCreationPayloadDTO actual = AccountCreationHelper.validatePayloadAndExtractDTO("{\"key\":\"value\"}",null,"");
            Assertions.assertEquals(mock.constructed().get(0),actual);
        }
    }

    @Test
    void generateSignupPayload_NoOptionalFields_test(){
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        try(MockedStatic<AccountCreationHelper> accountCreationHelperMockedStatic = Mockito.mockStatic(AccountCreationHelper.class)){
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.generateMandatoryFieldsForPayload(payloadDTO)).thenReturn(Map.of());
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.generateSignupPayload(payloadDTO)).thenCallRealMethod();
            Assertions.assertEquals(Map.of(),AccountCreationHelper.generateSignupPayload(payloadDTO));
        }
    }

    @Test
    void generateSignupPayload_validOptionalFields_test(){
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        payloadDTO.setPassword("pass");
        payloadDTO.setLastName("last");
        payloadDTO.setCountryCode("91");
        payloadDTO.setPhoneNumber("123");
        payloadDTO.setIpAddress("ip");
        try(MockedStatic<AccountCreationHelper> accountCreationHelperMockedStatic = Mockito.mockStatic(AccountCreationHelper.class)){
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.generateMandatoryFieldsForPayload(payloadDTO)).thenReturn(new HashMap<String,Object>());
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.generateSignupPayload(payloadDTO)).thenCallRealMethod();
            Assertions.assertEquals(Map.of("password","pass","lastName","last","countryCode","91","nationalNumber","123","ipAddress","ip"),
                    AccountCreationHelper.generateSignupPayload(payloadDTO));
        }
    }

    @Test
    void generateMandatoryFieldsForPayload_valid_test(){
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        payloadDTO.setEmailID("email");
        payloadDTO.setAccountName("accName");
        payloadDTO.setSource("web");
        payloadDTO.setTimeZone("zone");
        payloadDTO.setFirstName("first");
        payloadDTO.setGeoLocation(new GeoLocationDTO());
        try(MockedStatic<AccountCreationHelper> accountCreationHelperMockedStatic = Mockito.mockStatic(AccountCreationHelper.class)){
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.generateMandatoryFieldsForPayload(payloadDTO)).thenCallRealMethod();
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.generateGeolocationPayload(new GeoLocationDTO())).thenReturn(Map.of());
            Map<String,Object> actual = AccountCreationHelper.generateMandatoryFieldsForPayload(payloadDTO);
            Map<String,Object> expected = Map.of("login","email","accountName","accName","brandId","d56194e1-b98b-4068-86f8-d442777d2a16","source","web","firstName","first","timeZone","zone","geoLocation",Map.of());
            Assertions.assertEquals(actual,expected);
        }
    }

    @Test
    void generateGeolocationPayload_noData_test(){
        GeoLocationDTO geoLocationDTO = new GeoLocationDTO();
        Assertions.assertEquals(Map.of("country","US"),AccountCreationHelper.generateGeolocationPayload(geoLocationDTO));
    }

    @Test
    void generateGeolocationPayload_validData_test(){
        GeoLocationDTO geoLocationDTO = new GeoLocationDTO();
        geoLocationDTO.setCity("city");
        geoLocationDTO.setCountry("CA");
        geoLocationDTO.setLatitude("lat");
        geoLocationDTO.setLongitude("long");
        geoLocationDTO.setRegion("region");
        Assertions.assertEquals(Map.of("country","CA","city","city","region","region","latitude","lat","longitude","long"),AccountCreationHelper.generateGeolocationPayload(geoLocationDTO));
    }

    @Test
    void generateSsoSetupUrl_password_nullToken_test() throws TokenResponseException {
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        payloadDTO.setPassword("pass");
        payloadDTO.setEmailID("email");
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){
            fullAuthServiceMockedStatic.when(()->FullAuthService.generateAccessTokenForCredentials("email","pass")).thenReturn(null);
            CommonAppProperties.setYoCoDashboardUrl("url");
            Assertions.assertEquals("url",AccountCreationHelper.generateSsoSetupUrl(payloadDTO));
        }
    }

    @Test
    void generateSsoSetupUrl_googleAccessToken_nullToken_test() throws TokenResponseException {
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        payloadDTO.setGoogleToken("token");
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){
            fullAuthServiceMockedStatic.when(()->FullAuthService.generateAccessTokenForGoogleToken("token")).thenReturn(null);
            CommonAppProperties.setYoCoDashboardUrl("url");
            Assertions.assertEquals("url",AccountCreationHelper.generateSsoSetupUrl(payloadDTO));
        }
    }

    @Test
    void generateSsoSetupUrl_googleAccessToken_validToken_test() throws TokenResponseException {
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        payloadDTO.setGoogleToken("token");
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){
            fullAuthServiceMockedStatic.when(()->FullAuthService.generateAccessTokenForGoogleToken("token")).thenReturn(new OauthAccessToken());
            CommonAppProperties.setYoCoDashboardUrl("url");
            fullAuthServiceMockedStatic.when(()->FullAuthService.generateSsoSetupUrl(new OauthAccessToken(),"url")).thenReturn("ssoUrl");
            Assertions.assertEquals("ssoUrl",AccountCreationHelper.generateSsoSetupUrl(payloadDTO));
        }
    }
}
