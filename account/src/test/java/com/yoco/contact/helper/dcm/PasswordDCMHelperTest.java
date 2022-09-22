package com.yoco.contact.helper.dcm;

import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.enums.AppMode;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.contact.helper.password.PasswordDCMHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;

class PasswordDCMHelperTest {

    @Test
    void getVerificationID_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            PasswordDCMHelper.getVerificationID("contactId");
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendGetRequest(eq(DcmConstants.getInitResetPasswordUrl().replace("{$contactID}","contactId")),eq(new String[]{"Authorization","Bearer token"}),isNull()));
        }
    }

    @Test
    void validateVerificationId_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            PasswordDCMHelper.validateVerificationId("123");
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendGetRequestWithBooleanResp(eq(DcmConstants.getValidateVerificationIdUrl().replace("{$verificationID}","123")),eq(new String[]{"Authorization","Bearer token"}),isNull()));
        }
    }

    @Test
    void updatePassword_test() throws IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            Map<String,Object> payloadMap = new HashMap<>();
            PasswordDCMHelper.updatePassword("accountId","token",payloadMap);
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPutRequest(eq(DcmConstants.getUpdatePasswordUrl().replace("{$accountID}","accountId")),eq(payloadMap),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token"}),isNull()));
        }
    }

    @Test
    void resetPassword_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            Map<String,Object> payloadMap = new HashMap<>();
            PasswordDCMHelper.resetPassword("123",payloadMap);
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPutRequest(eq(DcmConstants.getResetPasswordUrl().replace("{$verificationID}","123")),eq(payloadMap),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token"}),isNull()));
        }
    }

    @Test
    void validatePassword_nullPassword_test(){
        try{
            PasswordDCMHelper.validatePassword(null);
        }catch (Exception e){
            Assertions.assertEquals("Password must contain 8 to 64 characters",e.getMessage());
        }
    }

    @Test
    void validatePassword_emptyPassword_test(){
        try{
            PasswordDCMHelper.validatePassword("");
        }catch (Exception e){
            Assertions.assertEquals("Password must contain 8 to 64 characters",e.getMessage());
        }
    }

    @Test
    void validatePassword_sizeLessThan8_test(){
        try{
            PasswordDCMHelper.validatePassword("asdasd");
        }catch (Exception e){
            Assertions.assertEquals("Password must contain 8 to 64 characters",e.getMessage());
        }
    }

    @Test
    void validatePassword_sizeGreaterThan64_test(){
        try{
            PasswordDCMHelper.validatePassword("asdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasd123123");
        }catch (Exception e){
            Assertions.assertEquals("Password must contain 8 to 64 characters",e.getMessage());
        }
    }

    @Test
    void validatePassword_leadingSpace_test(){
        try{
            PasswordDCMHelper.validatePassword(" asdasdasd");
        }catch (Exception e){
            Assertions.assertEquals("Password must not contain leading and trailing spaces",e.getMessage());
        }
    }

    @Test
    void validatePassword_trailingSpace_test(){
        try{
            PasswordDCMHelper.validatePassword("asdasdasd ");
        }catch (Exception e){
            Assertions.assertEquals("Password must not contain leading and trailing spaces",e.getMessage());
        }
    }

    @Test
    void validatePassword_noAlphabets_test(){
        try{
            PasswordDCMHelper.validatePassword("123123123");
        }catch (Exception e){
            Assertions.assertEquals("Password must contain atleast one number and one alphabet",e.getMessage());
        }
    }

    @Test
    void validatePassword_noNumbers_test(){
        try{
            PasswordDCMHelper.validatePassword("asdasdasd");
        }catch (Exception e){
            Assertions.assertEquals("Password must contain atleast one number and one alphabet",e.getMessage());
        }
    }

    @Test
    void validatePassword_valid_test(){
        Assertions.assertDoesNotThrow(()->PasswordDCMHelper.validatePassword("asdasd123123"));
    }
}