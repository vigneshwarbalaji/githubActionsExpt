package com.yoco.account.service;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.account.helper.*;
import com.yoco.account.modal.AccountCreationPayloadDTO;
import com.yoco.account.modal.AccountUpdatePayloadDTO;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.account.AccountDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.UserPROUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.mock.web.MockHttpServletRequest;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class AccountServiceTest {
    @Test
    void getAccount_UserNotFound_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","contactID")).thenReturn(null);
            new AccountService().getAccount("accID","contactID");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void getAccount_nullAccount_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class);){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","contactID")).thenReturn(new PeopleRelationJDO());
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getById("accID")).thenReturn(null);
            accountImplMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            new AccountService().getAccount("accID","contactID");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.ACCOUNT_NOT_FOUND.value(),e.getMessage());
        }
    }

    @Test
    void getAccount_validAccount_test(){
        SettingsJDO mockSettingsJDO = new SettingsJDO();
        mockSettingsJDO.setIsPayrollEnabled(true);
        mockSettingsJDO.setPayrollAccessSince(0L);
        mockSettingsJDO.setDateDeletedLongTime(0L);
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class)) {
            userPROUtilMockedStatic.when(() -> UserPROUtil.validateAndExtractUserPRO("accID", "contactID")).thenReturn(new PeopleRelationJDO());
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getById("accID")).thenReturn(mockSettingsJDO);
            accountImplMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            Assertions.assertEquals(new AccountDTO(mockSettingsJDO), new AccountService().getAccount("accID", "contactID"));
        }
    }

    @Test
    void getAllAccountsForUser_userNotAuthorized_Test(){
        try{
            new AccountService().getAllAccountsForUser("123","234");
        }catch (Exception e){
            Assertions.assertEquals(e.getMessage(),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        }
    }

    @Test
    void getAllAccountsForUser_nullActiveUserPros_Test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            Mockito.when(userImplMock.getAllUserProsForUser("123",false)).thenReturn(null);
            Assertions.assertEquals(List.of(),new AccountService().getAllAccountsForUser("123","123"));
        }
    }

    @Test
    void getAllAccountsForUser_valid_Test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            PeopleRelationJDO userPro1 = new PeopleRelationJDO();
            userPro1.setUniquepin("acc1");
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setUniquepin("acc2");
            Mockito.when(userImplMock.getAllUserProsForUser("123",false)).thenReturn(List.of(userPro1,userPro2));
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);

            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO account1 = new SettingsJDO();
            SettingsJDO account2 = new SettingsJDO();
            Mockito.when(accountImplMock.get(SettingsJDO.class,List.of("acc1","acc2"))).thenReturn(List.of(account1,account2));
            accountImplMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);

            Assertions.assertEquals(List.of(account1,account2),new AccountService().getAllAccountsForUser("123","123"));
        }
    }

    @Test
    void getDefaultAccountIDForUser_userAccessTokenNotAuthorized_test(){
        OauthAccessToken token = new OauthAccessToken();
        token.setType("user");
        token.setUserId("123");
        try{
            new AccountService().getDefaultAccountProForUser("234",token);
        }catch(Exception e){
            Assertions.assertEquals(e.getMessage(),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        }
    }

    @Test
    void getDefaultAccountIDForUser_userAccessTokenAuthorized_nullDefaultUserPro_test(){
        OauthAccessToken token = new OauthAccessToken();
        token.setType("user");
        token.setUserId("234");
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.getDefaultUserPro("234")).thenReturn(null);
            new AccountService().getDefaultAccountProForUser("234",token);
        }catch(Exception e){
            Assertions.assertEquals(e.getMessage(),COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());
        }
    }

    @Test
    void getDefaultAccountIDForUser_ServerAccessTokenAuthorized_nullDefaultUserPro_test(){
        OauthAccessToken token = new OauthAccessToken();
        token.setType("server");
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.getDefaultUserPro("234")).thenReturn(null);
            new AccountService().getDefaultAccountProForUser("234",token);
        }catch(Exception e){
            Assertions.assertEquals(e.getMessage(),COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());
        }
    }

    @Test
    void getDefaultAccountIDForUser_ServerAccessTokenAuthorized_ValidDefaultUserPro_test() throws NoSuchAlgorithmException, IOException {
        OauthAccessToken token = new OauthAccessToken();
        token.setType("server");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.getDefaultUserPro("234")).thenReturn(userPro);
            Assertions.assertEquals(new UserDTO(userPro,null),new AccountService().getDefaultAccountProForUser("234",token));
        }
    }

    @Test
    void getDefaultAccountIDForUser_UserAccessTokenAuthorized_ValidDefaultUserPro_test() throws NoSuchAlgorithmException, IOException {
        OauthAccessToken token = new OauthAccessToken();
        token.setType("user");
        token.setUserId("234");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.getDefaultUserPro("234")).thenReturn(userPro);
            Assertions.assertEquals(new UserDTO(userPro,null),new AccountService().getDefaultAccountProForUser("234",token));
        }
    }

    @Test
    void setDefaultAccountForUser_UserNotAuthorized_test(){
        try{
            new AccountService().setDefaultAccountForUser("123","234","accID");
        }catch(Exception e){
            Assertions.assertEquals(e.getMessage(),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        }
    }

    @Test
    void setDefaultAccountForUser_UserProNull_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","234")).thenReturn(null);
            new AccountService().setDefaultAccountForUser("234","234","accID");
        }catch(Exception e){
            Assertions.assertEquals(e.getMessage(),COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());
        }
    }

    @Test
    void setDefaultAccountForUser_UserProValid_test() throws NoSuchAlgorithmException, IOException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","234")).thenReturn(userPro);
            Assertions.assertEquals(new UserDTO(userPro,null),new AccountService().setDefaultAccountForUser("234","234","accID"));
            userPROUtilMockedStatic.verify(()->UserPROUtil.setUserProAsDefault(userPro));
            dcmUtilMockedStatic.verify(()->DcmUtil.updateDefaultAccountInDcm("accID","234"));
        }
    }

    @Test
    void deleteAccount_fullUS_accountID_test(){
        try{
           new AccountService().deleteAccount(new Contact(),"YH0D44","payload");
        }catch(Exception e){
            Assertions.assertEquals(e.getMessage(),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        }
    }

    @Test
    void deleteAccount_fullChennai_accountID_test(){
        try{
            new AccountService().deleteAccount(new Contact(),"91dfed2f-d29f-4302-89ee-341e9364b941","payload");
        }catch(Exception e){
            Assertions.assertEquals(e.getMessage(),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        }
    }

    @Test
    void deleteAccount_nullUserPro_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            Contact contact = new Contact();
            contact.setId("123");
            userPROUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO("accID","123")).thenReturn(null);
            new AccountService().deleteAccount(contact,"accID","payload");
        }catch(Exception e){
            Assertions.assertEquals(e.getMessage(),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        }
    }

    @Test
    void deleteAccount_notPrimaryAdmin_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            Contact contact = new Contact();
            contact.setId("123");
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO("accID","123")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isPrimaryAdmin(userPro)).thenReturn(false);
            new AccountService().deleteAccount(contact,"accID","payload");
        }catch(Exception e){
            Assertions.assertEquals(e.getMessage(),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        }
    }

    @Test
    void deleteAccount_accountNull_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class)
        ){
            Contact contact = new Contact();
            contact.setId("123");
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO("accID","123")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isPrimaryAdmin(userPro)).thenReturn(true);
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getById("accID")).thenReturn(null);
            accountImplMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            new AccountService().deleteAccount(contact,"accID","payload");
        }catch(Exception e){
            Assertions.assertEquals(e.getMessage(),COMMON_ERROR_RESPONSE.ACCOUNT_NOT_FOUND.value());
        }
    }

    @Test
    void deleteAccount_accountNotActive_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class)
        ){
            Contact contact = new Contact();
            contact.setId("123");
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()-> UserPROUtil.validateAndExtractUserPRO("accID","123")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isPrimaryAdmin(userPro)).thenReturn(true);
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getById("accID")).thenReturn(new SettingsJDO());
            accountImplMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            new AccountService().deleteAccount(contact,"accID","payload");
        }catch(Exception e){
            Assertions.assertEquals(e.getMessage(),COMMON_ERROR_RESPONSE.ACCOUNT_NOT_FOUND.value());
        }
    }

    @Test
    void deleteAccount_valid_test() throws IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<AccountDeleteHelper> accountDeleteHelperMockedStatic = Mockito.mockStatic(AccountDeleteHelper.class);
            MockedStatic<AccountTaskInitiator> accountTaskInitiatorMockedStatic = Mockito.mockStatic(AccountTaskInitiator.class)
        ) {
            Contact contact = new Contact();
            contact.setId("123");
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(() -> UserPROUtil.validateAndExtractUserPRO("accID", "123")).thenReturn(userPro);
            userPROUtilMockedStatic.when(() -> UserPROUtil.isPrimaryAdmin(userPro)).thenReturn(true);
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO account = new SettingsJDO();
            account.setStatus("ACTIVE");
            account.setIsPayrollEnabled(true);
            account.setPayrollAccessSince(0L);
            Mockito.when(accountImplMock.getById("accID")).thenReturn(account);
            accountImplMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            accountDeleteHelperMockedStatic.when(() -> AccountDeleteHelper.extractReasonFromPayload("payload")).thenReturn("reason");
            accountDeleteHelperMockedStatic.when(() -> AccountDeleteHelper.updateSettingsJdoEntryForDeletion(account)).thenCallRealMethod();
            AccountDTO actual = new AccountService().deleteAccount(contact, "accID", "payload");
            accountDeleteHelperMockedStatic.verify(()->AccountDeleteHelper.saveAccountDeletionActivity(contact,account,"reason"));
            accountTaskInitiatorMockedStatic.verify(()->AccountTaskInitiator.initiateAccountDeletionQueues(new AccountDTO(account),"123"));
            Assertions.assertFalse(actual.isActive());
            Assertions.assertTrue(actual.getDateDeletedTime() > 0);
        }
    }

    @Test
    void createAccount_rateLimitReached_test(){
        HttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<AccountCreationHelper> accountCreationHelperMockedStatic = Mockito.mockStatic(AccountCreationHelper.class)){
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.isSignUpRateLimitExceeded(request)).thenReturn(true);
            new AccountService().createAccount(request,"payload");
        }catch(Exception e){
            Assertions.assertEquals("Too many attempts, try again later",e.getMessage());
        }
    }

    @Test
    void createAccount_nullClient_test(){
        HttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<AccountCreationHelper> accountCreationHelperMockedStatic = Mockito.mockStatic(AccountCreationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.isSignUpRateLimitExceeded(request)).thenReturn(false);
            headerUtilMockedStatic.when(()->HeaderUtil.extractSrcClientFromRequest(request)).thenReturn(null);
            new AccountService().createAccount(request,"payload");
        }catch(Exception e){
            Assertions.assertEquals("User not authorized",e.getMessage());
        }
    }

    @Test
    void createAccount_valid_test() throws IOException {
        HttpServletRequest request = new MockHttpServletRequest();
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        payloadDTO.setPlan("free");
        Map<String,Object> signupPayload = Map.of("key","value");
        Map<String,Object> apiResponse = Map.of("success",true);
        SettingsJDO account = new SettingsJDO("id", "email",  "accountName", "Asia/Kolkata",  "source", Map.of(), "srcRef");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        userPro.setContactId("123");
        AccessPolicy policy = new AccessPolicy();
        policy.setPermissions(Set.of());
        try(MockedStatic<AccountCreationHelper> accountCreationHelperMockedStatic = Mockito.mockStatic(AccountCreationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<UniversalSignupHelper> universalSignupHelperMockedStatic = Mockito.mockStatic(UniversalSignupHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<AccountTaskInitiator> accountTaskInitiatorMockedStatic = Mockito.mockStatic(AccountTaskInitiator.class)){
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.isSignUpRateLimitExceeded(request)).thenReturn(false);
            headerUtilMockedStatic.when(()->HeaderUtil.extractSrcClientFromRequest(request)).thenReturn("web");
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.validatePayloadAndExtractDTO("payload",request,"web")).thenReturn(payloadDTO);
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.generateSignupPayload(payloadDTO)).thenReturn(signupPayload);
            universalSignupHelperMockedStatic.when(()->UniversalSignupHelper.createAccountAndValidateResponse(signupPayload)).thenReturn(apiResponse);
            universalSignupHelperMockedStatic.when(()->UniversalSignupHelper.extractAndSaveSettingsJdoFromResponse(apiResponse,payloadDTO)).thenReturn(account);
            universalSignupHelperMockedStatic.when(()->UniversalSignupHelper.extractAndSaveUserProAndContactFromResponse(apiResponse,payloadDTO,account)).thenReturn(userPro);
            AccessManager accessManagerMock = Mockito.mock(AccessManager.class);
            Mockito.when(accessManagerMock.createNewPolicyForOwner("accID","123","free")).thenReturn(policy);
            accessManagerMockedStatic.when(AccessManager::getInstance).thenReturn(accessManagerMock);
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.generateSsoSetupUrl(payloadDTO)).thenReturn("ssourl");
            Map<String,Object> actual = new AccountService().createAccount(request,"payload");
            Map<String,Object> expected = Map.of("account", new AccountDTO(account),"user",new UserDTO(userPro,Set.of()),"ssoUrl","ssourl");
            Assertions.assertEquals(actual,expected);
            accountTaskInitiatorMockedStatic.verify(()->AccountTaskInitiator.initiateAccountCreationQueue(account,userPro));
        }
    }

    @Test
    void processOneTapRequest_userAssociatedToYoCo_test() throws GeneralSecurityException, IOException {
        try(MockedStatic<OneTapHelper> oneTapHelperMockedStatic = Mockito.mockStatic(OneTapHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            oneTapHelperMockedStatic.when(()->OneTapHelper.validateAndExtractUserDetails("payload")).thenReturn(Map.of("emailID","email","credential","token","clientId","id"));
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAssociatedToYoco("email")).thenReturn(true);
            oneTapHelperMockedStatic.when(()->OneTapHelper.generateSsoSetupUrl("token","id")).thenReturn("ssoUrl");
            Assertions.assertEquals(Map.of("ssoUrl","ssoUrl"),new AccountService().processOneTapRequest(new MockHttpServletRequest(),"payload"));
        }
    }

    @Test
    void processOneTapRequest_userNotAssociatedToYoCo_test() throws GeneralSecurityException, IOException {
        HttpServletRequest request = new MockHttpServletRequest();
        Map<String,Object> signupPayload = Map.of("key","value");
        Map<String,Object> apiResponse = Map.of("success",true);
        SettingsJDO account = new SettingsJDO("id", "email",  "accountName", "Asia/Kolkata",  "source", Map.of(), "srcRef");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        userPro.setContactId("123");
        AccessPolicy policy = new AccessPolicy();
        policy.setPermissions(Set.of());
        try(MockedStatic<OneTapHelper> oneTapHelperMockedStatic = Mockito.mockStatic(OneTapHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedConstruction<AccountCreationPayloadDTO> mockedConstruction = Mockito.mockConstruction(AccountCreationPayloadDTO.class,(payloadMock,context)->{
                Mockito.when(payloadMock.getPlan()).thenReturn("free");
                Mockito.when(payloadMock.getOneTapToken()).thenReturn("token");
            });
            MockedStatic<AccountCreationHelper> accountCreationHelperMockedStatic = Mockito.mockStatic(AccountCreationHelper.class);
            MockedStatic<UniversalSignupHelper> universalSignupHelperMockedStatic = Mockito.mockStatic(UniversalSignupHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<AccountTaskInitiator> accountTaskInitiatorMockedStatic = Mockito.mockStatic(AccountTaskInitiator.class)){
            oneTapHelperMockedStatic.when(()->OneTapHelper.validateAndExtractUserDetails("payload")).thenReturn(Map.of("emailID","email","credential","token","clientId","id"));
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAssociatedToYoco("email")).thenReturn(false);
            accountCreationHelperMockedStatic.when(()->AccountCreationHelper.generateSignupPayload(any(AccountCreationPayloadDTO.class))).thenReturn(signupPayload);
            universalSignupHelperMockedStatic.when(()->UniversalSignupHelper.createAccountAndValidateResponse(signupPayload)).thenReturn(apiResponse);
            universalSignupHelperMockedStatic.when(()->UniversalSignupHelper.extractAndSaveSettingsJdoFromResponse(eq(apiResponse),any(AccountCreationPayloadDTO.class))).thenReturn(account);
            universalSignupHelperMockedStatic.when(()->UniversalSignupHelper.extractAndSaveUserProAndContactFromResponse(eq(apiResponse),any(AccountCreationPayloadDTO.class),eq(account))).thenReturn(userPro);
            AccessManager accessManagerMock = Mockito.mock(AccessManager.class);
            Mockito.when(accessManagerMock.createNewPolicyForOwner("accID","123","free")).thenReturn(policy);
            accessManagerMockedStatic.when(AccessManager::getInstance).thenReturn(accessManagerMock);
            oneTapHelperMockedStatic.when(()->OneTapHelper.generateSsoSetupUrl("token","id")).thenReturn("ssoUrl");
            Map<String,Object> actual = new AccountService().processOneTapRequest(request,"payload");
            Map<String,Object> expected = Map.of("account", new AccountDTO(account),"user",new UserDTO(userPro,Set.of()),"ssoUrl","ssoUrl");
            Assertions.assertEquals(actual,expected);
            accountTaskInitiatorMockedStatic.verify(()->AccountTaskInitiator.initiateAccountCreationQueue(account,userPro));
        }
    }

    @Test
    void updateAccount_nullPro_test(){
        Contact contact = new Contact();
        contact.setId("123");
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","123")).thenReturn(null);
            new AccountService().updateAccount(contact,"accID","payload");
        }catch (Exception e){
            Assertions.assertEquals("User not authorized",e.getMessage());
        }
    }

    @Test
    void updateAccount_ProNotPrimaryAdmin_test(){
        Contact contact = new Contact();
        contact.setId("123");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        SettingsJDO account = new SettingsJDO();
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","123")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()->UserPROUtil.isPrimaryAdmin(userPro)).thenReturn(false);
            new AccountService().updateAccount(contact,"accID","payload");

        }catch (Exception e){
            Assertions.assertEquals("User not authorized",e.getMessage());
        }
    }

    @Test
    void updateAccount_AccountNotActive_test(){
        Contact contact = new Contact();
        contact.setId("123");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","123")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()->UserPROUtil.isPrimaryAdmin(userPro)).thenReturn(true);
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getById("accID")).thenReturn(new SettingsJDO());
            accountImplMockedStatic.when(()->AccountImpl.getAccountImplInstance()).thenReturn(accountImplMock);
            new AccountService().updateAccount(contact,"accID","payload");
        }catch (Exception e){
            Assertions.assertEquals("account not found",e.getMessage());
        }
    }

    @Test
    void updateAccount_valid_test() throws IOException {
        Contact contact = new Contact();
        contact.setId("123");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        SettingsJDO account = new SettingsJDO();
        account.setPeopleUniquePin("accID");
        account.setStatus("ACTIVE");
        account.setIsPayrollEnabled(true);
        account.setPayrollAccessSince(0L);
        account.setDateDeletedLongTime(0L);
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<AccountUpdateHelper> accountUpdateHelperMockedStatic = Mockito.mockStatic(AccountUpdateHelper.class);
            MockedStatic<AccountTaskInitiator> accountTaskInitiatorMockedStatic = Mockito.mockStatic(AccountTaskInitiator.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("accID","123")).thenReturn(userPro);
            userPROUtilMockedStatic.when(()->UserPROUtil.isPrimaryAdmin(userPro)).thenReturn(true);
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getById("accID")).thenReturn(account);
            accountImplMockedStatic.when(()->AccountImpl.getAccountImplInstance()).thenReturn(accountImplMock);
            accountUpdateHelperMockedStatic.when(()->AccountUpdateHelper.validateAndExtractDTO("payload")).thenReturn(new AccountUpdatePayloadDTO());
            Map<String,Object> actual = new AccountService().updateAccount(contact,"accID","payload");
            Assertions.assertEquals(Map.of("account",new AccountDTO(account)),actual);
            accountUpdateHelperMockedStatic.verify(()->AccountUpdateHelper.updateAccountAndSetActivityMessage(account,new AccountUpdatePayloadDTO()));
            accountTaskInitiatorMockedStatic.verify(()->AccountTaskInitiator.initiateAccountUpdateQueue("accID",contact,new AccountUpdatePayloadDTO()));
        }
    }
}