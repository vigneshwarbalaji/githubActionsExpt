package com.yoco.contact.service;

import co.anywhere.awconfigurator.model.ratelimit.RateLimitInfo;
import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.annotation.helper.AccessTokenCacheService;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.RateConfigurator;
import com.yoco.commons.utils.HashUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.contact.enums.CONTACT_ERROR_MESSAGE;
import com.yoco.contact.helper.password.PasswordDCMHelper;
import com.yoco.contact.helper.password.PasswordEmailHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;


class PasswordServiceTest {

    @ParameterizedTest
    @NullAndEmptySource
    void  updatePassword_invalid_payload_test(String testValue){
        try{
            new PasswordService().updatePassword("accountId","contactId",testValue,"token");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void  updatePassword_invalid_payloadJson_test(){
        try{
            new PasswordService().updatePassword("accountId","contactId","json","token");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void  updatePassword_invalid_constraints_test(){
        try{
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("data","dummy");
            new PasswordService().updatePassword("accountId","contactId", JsonUtil.getJson(payloadMap),"token");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void  updatePassword_invalid_oldPassword_test(String testValue){
        try{
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("oldPassword",testValue);
            new PasswordService().updatePassword("accountId","contactId", JsonUtil.getJson(payloadMap),"token");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void  updatePassword_invalid_newPassword_test(String testValue){
        try{
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("oldPassword","oldPass");
            payloadMap.put("newPassword",testValue);
            new PasswordService().updatePassword("accountId","contactId", JsonUtil.getJson(payloadMap),"token");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(),e.getMessage());
        }
    }

    @Test
    void updatePassword_valid_test() throws NoSuchAlgorithmException, IOException {
       try(MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)){
           passwordHelperMockedStatic.when(()-> PasswordDCMHelper.updatePassword(anyString(),anyString(),anyMap())).thenReturn(null);
           Map<String,Object> payloadMap = new HashMap<>();
           payloadMap.put("oldPassword","oldPass");
           payloadMap.put("newPassword","newPass");
           new PasswordService().updatePassword("accountId","contactId", JsonUtil.getJson(payloadMap),"token");

           Map<String, Object> requestPayload = new HashMap<>();
           requestPayload.put("id", "contactId");
           requestPayload.put("oldPassword", HashUtil.generateSHA1("oldPass"));
           requestPayload.put("newPassword", HashUtil.generateSHA1("newPass"));

           passwordHelperMockedStatic.verify(()-> PasswordDCMHelper.updatePassword("accountId","token",requestPayload));
       }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void forgotPassword_invalid_email_test(String testValue){
       try{
           new PasswordService().forgotPassword(testValue);
       }catch (Exception e){
           Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.value(),e.getMessage());
       }
    }

    @Test
    void forgotPassword_invalid_emailID_test(){
        try{
            new PasswordService().forgotPassword("email");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.value(),e.getMessage());
        }
    }

    @Test
    void forgotPassword_limitReached_test(){
       try(MockedStatic<RateConfigurator> rateConfiguratorMockedStatic = Mockito.mockStatic(RateConfigurator.class)){
           RateLimitInfo rateLimitInfo = new RateLimitInfo();
           rateLimitInfo.setAllow(false);
           rateConfiguratorMockedStatic.when(()-> RateConfigurator.checkRateLimitUsage(anyString(),anyInt(),anyInt())).thenReturn(rateLimitInfo);

           new PasswordService().forgotPassword("test@gmail.com");
           rateConfiguratorMockedStatic.verify(()-> RateConfigurator.checkRateLimitUsage("399893507_forgotPass",5,86400));
       }catch (Exception e){
           Assertions.assertEquals(COMMON_ERROR_RESPONSE.REQUEST_LIMIT_REACHED.value(),e.getMessage());
       }
    }

    @Test
    void forgotPassword_noContact_test(){
        try(MockedStatic<RateConfigurator> rateConfiguratorMockedStatic = Mockito.mockStatic(RateConfigurator.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            rateConfiguratorMockedStatic.when(()-> RateConfigurator.checkRateLimitUsage(anyString(),anyInt(),anyInt())).thenReturn(null);
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getContactByEmailID(anyString())).thenReturn(null);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            new PasswordService().forgotPassword("test@gmail.com");
            rateConfiguratorMockedStatic.verify(()-> RateConfigurator.checkRateLimitUsage("399893507_forgotPass",5,86400));
        }catch (Exception e){
            Assertions.assertEquals(CONTACT_ERROR_MESSAGE.MAIL_DOES_NOT_EXIST.value(),e.getMessage());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void forgotPassword_invalidResponse_test(Map<String,Object> testMap) throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<RateConfigurator> rateConfiguratorMockedStatic = Mockito.mockStatic(RateConfigurator.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<PasswordDCMHelper> passwordDCMHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)){
            RateLimitInfo rateLimitInfo = new RateLimitInfo();
            rateLimitInfo.setAllow(true);
            rateConfiguratorMockedStatic.when(()-> RateConfigurator.checkRateLimitUsage(anyString(),anyInt(),anyInt())).thenReturn(rateLimitInfo);

            Contact contact = new Contact();
            contact.setId("contactId");
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getContactByEmailID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);

            passwordDCMHelperMockedStatic.when(()-> PasswordDCMHelper.getVerificationID(anyString())).thenReturn(testMap);

            Map<String,Object> response = new PasswordService().forgotPassword("test@gmail.com");

            Assertions.assertEquals(testMap,response);
            rateConfiguratorMockedStatic.verify(()-> RateConfigurator.checkRateLimitUsage("399893507_forgotPass",5,86400));
            passwordDCMHelperMockedStatic.verify(()-> PasswordDCMHelper.getVerificationID("contactId"));
        }
    }

    @Test
    void forgotPassword_success_False_Response_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<RateConfigurator> rateConfiguratorMockedStatic = Mockito.mockStatic(RateConfigurator.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<PasswordDCMHelper> passwordDCMHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)){
            RateLimitInfo rateLimitInfo = new RateLimitInfo();
            rateLimitInfo.setAllow(true);
            rateConfiguratorMockedStatic.when(()-> RateConfigurator.checkRateLimitUsage(anyString(),anyInt(),anyInt())).thenReturn(rateLimitInfo);

            Contact contact = new Contact();
            contact.setId("contactId");
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getContactByEmailID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            Map<String,Object> mockResp = Map.of(Commons.SUCCESS,false);
            passwordDCMHelperMockedStatic.when(()-> PasswordDCMHelper.getVerificationID(anyString())).thenReturn(mockResp);

            Map<String,Object> response = new PasswordService().forgotPassword("test@gmail.com");

            Assertions.assertEquals(mockResp,response);
            rateConfiguratorMockedStatic.verify(()-> RateConfigurator.checkRateLimitUsage("399893507_forgotPass",5,86400));
            passwordDCMHelperMockedStatic.verify(()-> PasswordDCMHelper.getVerificationID("contactId"));
        }
    }

    @Test
    void forgotPassword_success_True_Response_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<RateConfigurator> rateConfiguratorMockedStatic = Mockito.mockStatic(RateConfigurator.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<PasswordDCMHelper> passwordDCMHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class);
            MockedStatic<PasswordEmailHelper> passwordEmailHelperMockedStatic = Mockito.mockStatic(PasswordEmailHelper.class)){

            RateLimitInfo rateLimitInfo = new RateLimitInfo();
            rateLimitInfo.setAllow(true);
            rateConfiguratorMockedStatic.when(()-> RateConfigurator.checkRateLimitUsage(anyString(),anyInt(),anyInt())).thenReturn(rateLimitInfo);

            Contact contact = new Contact();
            contact.setId("contactId");
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getContactByEmailID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            Map<String,Object> mockResp = Map.of(Commons.SUCCESS,true, ContactConstants.VERIFICATION_ID,"123");
            passwordDCMHelperMockedStatic.when(()-> PasswordDCMHelper.getVerificationID(anyString())).thenReturn(mockResp);

            PasswordEmailHelper passwordEmailHelper = Mockito.mock(PasswordEmailHelper.class);
            Mockito.doNothing().when(passwordEmailHelper).initiateResetPasswordInitMail(any(Contact.class),anyString());
            passwordEmailHelperMockedStatic.when(PasswordEmailHelper::getInstance).thenReturn(passwordEmailHelper);

            Map<String,Object> response = new PasswordService().forgotPassword("test@gmail.com");

            Assertions.assertEquals(mockResp,response);
            rateConfiguratorMockedStatic.verify(()-> RateConfigurator.checkRateLimitUsage("399893507_forgotPass",5,86400));
            passwordDCMHelperMockedStatic.verify(()-> PasswordDCMHelper.getVerificationID("contactId"));
            Mockito.verify(passwordEmailHelper).initiateResetPasswordInitMail(contact,"123");
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void resetPassword_invalid_payload_test(String testValue){
        try{
            new PasswordService().resetPassword("contactId",testValue);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void resetPassword_invalid_payloadJson_test(){
        try{
            new PasswordService().resetPassword("contactId","payload");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void resetPassword_noKey_verification_test(){
        try{
            new PasswordService().resetPassword("contactId",JsonUtil.getJson(Map.of("key","data")));
        }catch (Exception e){
            Assertions.assertEquals(CONTACT_ERROR_MESSAGE.INVALID_VERIFICATION_ID.value(),e.getMessage());
        }
    }

    @Test
    void resetPassword_invalid_verification_test(){
        try{
            new PasswordService().resetPassword("contactId",JsonUtil.getJson(Map.of(ContactConstants.VERIFICATION_ID,"")));
        }catch (Exception e){
            Assertions.assertEquals(CONTACT_ERROR_MESSAGE.INVALID_VERIFICATION_ID.value(),e.getMessage());
        }
    }

    @Test
    void resetPassword_noKey_newPassword_test(){
        try{
            new PasswordService().resetPassword("contactId",JsonUtil.getJson(Map.of(ContactConstants.VERIFICATION_ID,"123")));
        }catch (Exception e){
            Assertions.assertEquals(CONTACT_ERROR_MESSAGE.INVALID_PASSWORD.value(),e.getMessage());
        }
    }

    @Test
    void resetPassword_invalid_newPassword_test(){
        try{
            new PasswordService().resetPassword("contactId",JsonUtil.getJson(Map.of(ContactConstants.VERIFICATION_ID,"123","newPassword","")));
        }catch (Exception e){
            Assertions.assertEquals(CONTACT_ERROR_MESSAGE.INVALID_PASSWORD.value(),e.getMessage());
        }
    }

    @Test
    void resetPassword_no_contact_test(){
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getByID(anyString())).thenReturn(null);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            new PasswordService().resetPassword("contactId",JsonUtil.getJson(Map.of(ContactConstants.VERIFICATION_ID,"123","newPassword","password")));
        }catch (Exception e){
            Assertions.assertEquals(CONTACT_ERROR_MESSAGE.CONTACT_DOES_NOT_EXIST.value(),e.getMessage());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void resetPassword_emptyResponse_test(Map<String,Object> testMap) throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<PasswordDCMHelper> passwordDCMHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)){

            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact();
            contact.setId("contactId");
            Mockito.when(contactMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);

            passwordDCMHelperMockedStatic.when(()-> PasswordDCMHelper.resetPassword(anyString(),anyMap())).thenReturn(testMap);

            Assertions.assertEquals(testMap,new PasswordService().resetPassword("contactId",JsonUtil.getJson(Map.of(ContactConstants.VERIFICATION_ID,"123","newPassword","password"))));
        }
    }

    @Test
    void resetPassword_falseResponse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<PasswordDCMHelper> passwordDCMHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)){

            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact();
            contact.setId("contactId");
            Mockito.when(contactMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);

            passwordDCMHelperMockedStatic.when(()-> PasswordDCMHelper.resetPassword(anyString(),anyMap())).thenReturn(Map.of("success",false));

            Assertions.assertEquals(Map.of("success",false),new PasswordService().resetPassword("contactId",JsonUtil.getJson(Map.of(ContactConstants.VERIFICATION_ID,"123","newPassword","password"))));
        }
    }

    @Test
    void resetPassword_trueResponse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<PasswordDCMHelper> passwordDCMHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class);
            MockedStatic<AccessTokenCacheService> accessTokenCacheServiceMockedStatic = Mockito.mockStatic(AccessTokenCacheService.class);
            MockedStatic<PasswordEmailHelper> passwordEmailHelperMockedStatic = Mockito.mockStatic(PasswordEmailHelper.class)){

            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact();
            contact.setId("contactId");
            Mockito.when(contactMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);

            passwordDCMHelperMockedStatic.when(()-> PasswordDCMHelper.resetPassword(anyString(),anyMap())).thenReturn(Map.of("success",true));
            accessTokenCacheServiceMockedStatic.when(()-> AccessTokenCacheService.clearAccessTokensForUser(any(OauthAccessToken.class),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            PasswordEmailHelper passwordEmailHelperMock = Mockito.mock(PasswordEmailHelper.class);
            Mockito.doNothing().when(passwordEmailHelperMock).initiateResetPasswordConfirmationMail(any(Contact.class));
            passwordEmailHelperMockedStatic.when(PasswordEmailHelper::getInstance).thenReturn(passwordEmailHelperMock);

            Assertions.assertEquals(Map.of("success",true),new PasswordService().resetPassword("contactId",JsonUtil.getJson(Map.of(ContactConstants.VERIFICATION_ID,"123","newPassword","password"))));
        }
    }

}