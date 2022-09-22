package com.yoco.commons.utils;

import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.enums.AppMode;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.services.UrlFetcher;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.net.http.HttpClient;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class DcmUtilTest {
    @Test
    void getDefaultAccountIDForUser_nullDcmResponse_test()throws NoSuchAlgorithmException, IOException{
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenCallRealMethod();
            userPROUtilMockedStatic.when(()->UserPROUtil.getEmailIdForUser(anyString())).thenReturn("email");
            dcmUtilMockedStatic.when(()->DcmUtil.getDefaultAccountIDForUser("234")).thenCallRealMethod();
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("email")).thenReturn(null);
            Assertions.assertNull(DcmUtil.getDefaultAccountIDForUser("234"));
        }
    }

    @Test
    void getDefaultAccountIDForUser_emptyDcmResponse_test()throws NoSuchAlgorithmException, IOException{
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenCallRealMethod();
            userPROUtilMockedStatic.when(()->UserPROUtil.getEmailIdForUser(anyString())).thenReturn("email");
            dcmUtilMockedStatic.when(()->DcmUtil.getDefaultAccountIDForUser("234")).thenCallRealMethod();
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("email")).thenReturn(new HashMap<>());
            Assertions.assertNull(DcmUtil.getDefaultAccountIDForUser("234"));
        }
    }

    @Test
    void getDefaultAccountIDForUser_nullContactSkillset_test()throws NoSuchAlgorithmException, IOException{
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenCallRealMethod();
            userPROUtilMockedStatic.when(()->UserPROUtil.getEmailIdForUser(anyString())).thenReturn("email");
            dcmUtilMockedStatic.when(()->DcmUtil.getDefaultAccountIDForUser("234")).thenCallRealMethod();
            HashMap<String,Object> dcmResponse = new HashMap(){{
                put("contactSkillSet",null);
            }};
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("email")).thenReturn(dcmResponse);
            Assertions.assertNull(DcmUtil.getDefaultAccountIDForUser("234"));
        }
    }

    @Test
    void getDefaultAccountIDForUser_noDefaultSkillset_test()throws NoSuchAlgorithmException, IOException{
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenCallRealMethod();
            userPROUtilMockedStatic.when(()->UserPROUtil.getEmailIdForUser(anyString())).thenReturn("email");
            dcmUtilMockedStatic.when(()->DcmUtil.getDefaultAccountIDForUser("234")).thenCallRealMethod();
            HashMap<String,Object> dcmResponse = new HashMap(){{
                put("contactSkillSet",new ArrayList<HashMap<String,Object>>());
            }};
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.STAGING);
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("email")).thenReturn(dcmResponse);
            Assertions.assertNull(DcmUtil.getDefaultAccountIDForUser("234"));
        }
    }

    @Test
    void getDefaultAccountIDForUser_validDefaultSkillset_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.getEmailIdForUser(anyString())).thenReturn("email");
            dcmUtilMockedStatic.when(()->DcmUtil.getDefaultAccountIDWithEmailID(anyString())).thenCallRealMethod();
            dcmUtilMockedStatic.when(()->DcmUtil.getDefaultAccountIDForUser("234")).thenCallRealMethod();
            dcmUtilMockedStatic.when(()-> DcmUtil.extractDefaultDomainFromContactSkillSet(anyList())).thenCallRealMethod();
            HashMap<String,Object> dcmResponse = new HashMap(){{
                put("contactSkillSet",new ArrayList<HashMap<String,Object>>(){{
                    add(new HashMap(){{
                        put("skillSetID","98223b25-b41c-4c7e-bb91-569003f4cc45");
                        put("accountID","accID");
                    }});
                }});
            }};
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("email")).thenReturn(dcmResponse);
            Assertions.assertEquals("accID",DcmUtil.getDefaultAccountIDForUser("234"));
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getActiveDefaultAccountFromDcm_invalidContactResponse_test(Map<String,Object> testValue)throws NoSuchAlgorithmException, IOException{
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getActiveDefaultAccountFromDcm("test@gmail.com")).thenCallRealMethod();
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("test@gmail.com")).thenReturn(testValue);
            Assertions.assertNull(DcmUtil.getActiveDefaultAccountFromDcm("test@gmail.com"));
        }
    }

   @Test
    void getActiveDefaultAccountFromDcm_nullSkillSet_test()throws NoSuchAlgorithmException, IOException{
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getActiveDefaultAccountFromDcm("test@gmail.com")).thenCallRealMethod();
            Map<String,Object> skillSet = new HashMap<>();
            skillSet.put("skillSet",null);
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("test@gmail.com")).thenReturn(skillSet);
            Assertions.assertNull(DcmUtil.getActiveDefaultAccountFromDcm("test@gmail.com"));
        }
    }

    @Test
    void getActiveDefaultAccountFromDcm_emptySkillSet_test()throws NoSuchAlgorithmException, IOException{
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getActiveDefaultAccountFromDcm("test@gmail.com")).thenCallRealMethod();
            Map<String,Object> skillSet = new HashMap<>();
            skillSet.put("skillSet",new ArrayList<>());
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("test@gmail.com")).thenReturn(skillSet);
            Assertions.assertNull(DcmUtil.getActiveDefaultAccountFromDcm("test@gmail.com"));
        }
    }

    @Test
    void getActiveDefaultAccountFromDcm_InValidDefaultSkillset_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getActiveDefaultAccountFromDcm("test@gmail.com")).thenCallRealMethod();
            dcmUtilMockedStatic.when(()-> DcmUtil.extractDefaultDomainFromContactSkillSet(anyList())).thenCallRealMethod();
            HashMap<String,Object> dcmResponse = new HashMap(){{
                put("skillSet",new ArrayList<HashMap<String,Object>>(){{
                    add(new HashMap(){{
                        put("skillSetID","98223b25-b41c-4c7e-bb91-569003f4cc45");
                        put("accountID","accID");
                    }});
                }});
                put("contactSkillSet",null);
            }};
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("test@gmail.com")).thenReturn(dcmResponse);
            Assertions.assertNull(DcmUtil.getActiveDefaultAccountFromDcm("test@gmail.com"));
        }
    }

    @Test
    void getDcmContactSkillsetInformation_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            Assertions.assertTrue((DcmUtil.getDcmContactSkillsetInformation("email")).isEmpty());
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(eq(DcmConstants.getContactSkillsetInfoUrl()),eq(new HashMap(){{put("login","email");}}),eq(new String[]{"Authorization","Bearer token"}),isNull()));
        }
    }

    @Test
    void getIsPasswordPresentForUserEmailID_nullDcmResponse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getIsPasswordPresentForUserEmailID("email")).thenCallRealMethod();
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("email")).thenReturn(null);
            Assertions.assertFalse(DcmUtil.getIsPasswordPresentForUserEmailID("email"));
        }
    }

    @Test
    void getIsPasswordPresentForUserEmailID_emptyDcmResponse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getIsPasswordPresentForUserEmailID("email")).thenCallRealMethod();
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("email")).thenReturn(new HashMap<>());
            Assertions.assertFalse(DcmUtil.getIsPasswordPresentForUserEmailID("email"));
        }
    }

    @Test
    void getIsPasswordPresentForUserEmailID_nullContactData_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getIsPasswordPresentForUserEmailID("email")).thenCallRealMethod();
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("email")).thenReturn(new HashMap(){{put("key","value");}});
            Assertions.assertFalse(DcmUtil.getIsPasswordPresentForUserEmailID("email"));
        }
    }

    @Test
    void getIsPasswordPresentForUserEmailID_emptyContactData_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getIsPasswordPresentForUserEmailID("email")).thenCallRealMethod();
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("email")).thenReturn(new HashMap(){{put("contact",new HashMap<>());}});
            Assertions.assertFalse(DcmUtil.getIsPasswordPresentForUserEmailID("email"));
        }
    }

    @Test
    void getIsPasswordPresentForUserEmailID_nullIsPasswordPresent_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getIsPasswordPresentForUserEmailID("email")).thenCallRealMethod();
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("email")).thenReturn(new HashMap(){{put("contact",new HashMap(){{put("key","value");}});}});
            Assertions.assertFalse(DcmUtil.getIsPasswordPresentForUserEmailID("email"));
        }
    }

    @Test
    void getIsPasswordPresentForUserEmailID_IsPasswordPresentFalse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getIsPasswordPresentForUserEmailID("email")).thenCallRealMethod();
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("email")).thenReturn(new HashMap(){{put("contact",new HashMap(){{put("is_password_present",false);}});}});
            Assertions.assertFalse(DcmUtil.getIsPasswordPresentForUserEmailID("email"));
        }
    }

    @Test
    void getIsPasswordPresentForUserEmailID_IsPasswordPresentTrue_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.getIsPasswordPresentForUserEmailID("email")).thenCallRealMethod();
            dcmUtilMockedStatic.when(()->DcmUtil.getDcmContactSkillsetInformation("email")).thenReturn(new HashMap(){{put("contact",new HashMap(){{put("is_password_present",true);}});}});
            Assertions.assertTrue(DcmUtil.getIsPasswordPresentForUserEmailID("email"));
        }
    }

    @Test
    void fetchContactFromDcm_firstAttempt_test(){
        Map<String ,Object> mockObj = new HashMap<>();
        mockObj.put("dcmContact","dcm");
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(),anyMap(),any(String[].class),any())).thenReturn(mockObj);
            Assertions.assertEquals(mockObj,DcmUtil.fetchContactFromDcm("email"));
            Map<String, Object> requestBody = new HashMap<>();
            requestBody.put("login", "email" );
            requestBody.put("brandID", DcmConstants.YOCO_BRAND_ID);
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(eq(DcmConstants.getUserExistenceInDCMUrl()),eq(requestBody),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token"}),isNull()),times(1));
        }
    }
    @Test
    void fetchContactFromDcm_secondAttempt_test(){
        Map<String ,Object> mockObj = new HashMap<>();
        mockObj.put("dcmContact","dcm");
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(),anyMap(),any(String[].class),any())).thenReturn(null).thenReturn(mockObj);
            Assertions.assertEquals(mockObj, DcmUtil.fetchContactFromDcm("email"));
            Map<String, Object> requestBody = new HashMap<>();
            requestBody.put("login", "email" );
            requestBody.put("brandID", DcmConstants.YOCO_BRAND_ID);
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(eq(DcmConstants.getUserExistenceInDCMUrl()),eq(requestBody),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token"}),isNull()),times(2));
        }
    }

    @Test
    void getDcmContact_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            DcmUtil.getDcmContact("email");
            Map<String, Object> requestBody = new HashMap<>();
            requestBody.put("login", "email" );
            requestBody.put("brandID", DcmConstants.YOCO_BRAND_ID);
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(eq(DcmConstants.getUserExistenceInDCMUrl()),eq(requestBody),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token"}),isNull()));
        }
    }

    @Test
    void linkContactToAccount_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            DcmUtil.linkContactToAccount("accountId","contactId");
            Map<String, Object> requestBody = new HashMap<>();
            requestBody.put("contactID", "contactId" );
            requestBody.put("brandID", DcmConstants.YOCO_BRAND_ID);
            requestBody.put("skillSetID", DcmConstants.YOCO_SKILLSET_ID);
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPutRequest(eq(DcmConstants.getAddExistingContactToDCMUrl().replace("{$accountID}","accountId")),eq(requestBody),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token"}),isNull()));
        }
    }

    @Test
    void updateDefaultAccountInDcm_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            DcmUtil.updateDefaultAccountInDcm("accountId","contactId");
            Map<String, Object> requestBody = new HashMap<>();
            requestBody.put("contactID", "contactId" );
            requestBody.put("accountID", "accountId");
            requestBody.put("skillSetID", DcmConstants.DEFAULT_ACCOUNT_SKILLSET_ID);
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(eq(DcmConstants.getUpdateDcmDefaultDomainUrl()),eq(requestBody),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token"}),isNull()));
        }
    }

    @Test
    void createUserInDcm_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            DcmUtil.createUserInDcm("accountId","emailId","password","fName","lName","photoId");
            Map<String, Object> payload = new DcmContactDTO().createDcmContactDTO("emailId","password","fName","lName","photoId");
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(eq(DcmConstants.getCreateUserUnderDCMAccountUrl().replace("{$accountID}","accountId")),eq(payload),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token"}),isNull()));
        }
    }

    @Test
    void extractDefaultDomainFromContactSkillSet_defaultDomain_exists_test(){
        HashMap<String,Object> contactSkillSet = new HashMap<>();
        contactSkillSet.put(DcmUtil.SKILL_SET_ID,DcmConstants.DEFAULT_ACCOUNT_SKILLSET_ID);
        contactSkillSet.put("accountID","accountId");
        List<HashMap<String,Object>> contactsList = new ArrayList<>();
        contactsList.add(contactSkillSet);
        Assertions.assertEquals("accountId",DcmUtil.extractDefaultDomainFromContactSkillSet(contactsList));
    }

    @Test
    void extractDefaultDomainFromContactSkillSet_defaultDomain_doesNot_exists_test(){
        HashMap<String,Object> contactSkillSet = new HashMap<>();
        contactSkillSet.put(DcmUtil.SKILL_SET_ID,"skillSetId");
        contactSkillSet.put("accountID","accountId");
        List<HashMap<String,Object>> contactsList = new ArrayList<>();
        contactsList.add(contactSkillSet);
        Assertions.assertNull(DcmUtil.extractDefaultDomainFromContactSkillSet(contactsList));
    }

    @Test
    void extractDefaultDomainFromContactSkillSet_null_test(){
        Assertions.assertNull(DcmUtil.extractDefaultDomainFromContactSkillSet(null));
    }

    @Test
    void removeUserFromAccount_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenReturn(client);
            headerUtilMockedStatic.when(HeaderUtil::getJsonUtf8ContentTypedServerTokenAuthHeader).thenReturn(new String[]{});
            dcmUtilMockedStatic.when(()->DcmUtil.getRemoveUserFromAccountUrl("accID","contactID")).thenReturn("url");
            urlFetcherMockedStatic.when(()->UrlFetcher.sendDeleteRequest("url",new String[]{},client)).thenReturn(Map.of("success",true));
            dcmUtilMockedStatic.when(()->DcmUtil.removeUserFromAccount("accID","contactID")).thenCallRealMethod();
            DcmUtil.removeUserFromAccount("accID","contactID");
            urlFetcherMockedStatic.verify(()->UrlFetcher.sendDeleteRequest("url",new String[]{},client));
        }
    }

    @Test
    void getRemoveUserFromAccountUrl_valid_test(){
        DcmConstants.initializeDcmConstants(AppMode.STAGING);
        String actual = DcmUtil.getRemoveUserFromAccountUrl("accID","contactID");
        Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Account/accID/User/contactID/remove?apikey=SEN42&productID=d56194e1-b98b-4068-86f8-d442777d2a16",actual);
    }

    @Test
    void updateContactInDcm_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.STAGING);
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            Map<String,Object> payloadMap = Map.of("key","data");
            DcmUtil.updateContactInDcm("accId",payloadMap);
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPutRequest(eq(DcmConstants.getUpdateDcmContactUrl().replace("{$accountID}","accId")),eq(payloadMap),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token","User-Agent","AppEngine-Google; (+http://code.google.com/appengine; appid: s~staging-goclockin-dashboard)"}),isNull()));
        }
    }

    @Test
    void updateDcmAccount_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.STAGING);
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            Map<String,Object> payloadMap = Map.of("key","data");
            DcmUtil.updateDcmAccount("accId",payloadMap);
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPutRequest(eq(DcmConstants.getUpdateDcmAccountUrl().replace("{$accountID}","accId")),eq(payloadMap),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token","User-Agent","AppEngine-Google; (+http://code.google.com/appengine; appid: s~staging-goclockin-dashboard)"}),isNull()));
        }
    }

}
