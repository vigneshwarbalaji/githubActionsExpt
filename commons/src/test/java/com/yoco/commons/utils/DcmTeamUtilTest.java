package com.yoco.commons.utils;

import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.enums.AppMode;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.modal.dcm.TeamDTO;
import com.yoco.commons.services.UrlFetcher;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.net.http.HttpClient;
import java.security.NoSuchAlgorithmException;
import java.util.*;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;

class DcmTeamUtilTest {
    @Test
    void getTeamsForAnAccount_DcmResponseNull_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForAccountResponseFromDCM("accID",100,"")).thenReturn(null);
            dcmTeamUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForAnAccount("accID",100,"")).thenCallRealMethod();
            Map<String, Object> actual = DcmTeamUtil.getTeamsForAnAccount("accID",100,"");
            Assertions.assertTrue(((ArrayList<TeamDTO>)actual.get("teams")).isEmpty());
            Assertions.assertNull(actual.get("cursor"));
        }
    }

    @Test
    void getTeamsForAnAccount_DcmResponseSuccessFalse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForAccountResponseFromDCM("accID",100,"")).thenReturn(new HashMap(){{put("success",false);}});
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForAnAccount("accID",100,"")).thenCallRealMethod();
            Map<String, Object> actual = DcmTeamUtil.getTeamsForAnAccount("accID",100,"");
            Assertions.assertTrue(((ArrayList<TeamDTO>)actual.get("teams")).isEmpty());
            Assertions.assertNull(actual.get("cursor"));
        }
    }

    @Test
    void getTeamsForAnAccount_DcmResponseSuccessTrueNullResponse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForAccountResponseFromDCM("accID",100,"")).thenReturn(new HashMap(){{put("success",true);}});
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForAnAccount("accID",100,"")).thenCallRealMethod();
            Map<String, Object> actual = DcmTeamUtil.getTeamsForAnAccount("accID",100,"");
            Assertions.assertTrue(((ArrayList<TeamDTO>)actual.get("teams")).isEmpty());
            Assertions.assertNull(actual.get("cursor"));
        }
    }

    @Test
    void getTeamsForAnAccount_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForAccountResponseFromDCM("accID",100,"")).thenReturn(new HashMap(){{put("success",true);
                put("group", new ArrayList(){{add(new HashMap(){{
                    put("id","teamid");
                    put("ownerID","ownerid");
                    put("accountID","accID");
                    put("name","teamname");
                    put("linkedContacts",new ArrayList(){{add("contact1");add("contact2");}});
                }})
                ;}});
                put("cursor", "1234zxc");
            }});
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForAnAccount("accID",100,"")).thenCallRealMethod();
            Map<String, Object> actual = DcmTeamUtil.getTeamsForAnAccount("accID",100,"");
            List<TeamDTO> actualList = (ArrayList<TeamDTO>)actual.get("teams");
            Assertions.assertEquals(1,actualList.size());
            Assertions.assertEquals("teamid",actualList.get(0).getId());
            Assertions.assertEquals("ownerid",actualList.get(0).getOwnerContactID());
            Assertions.assertEquals("accID",actualList.get(0).getAccountID());
            Assertions.assertEquals("teamname",actualList.get(0).getName());
            Assertions.assertEquals(2,actualList.get(0).getMembers().size());
            Assertions.assertEquals("contact1",actualList.get(0).getMembers().get(0));
            Assertions.assertEquals("contact2",actualList.get(0).getMembers().get(1));
            Assertions.assertEquals("1234zxc",actual.get("cursor"));
        }
    }

    @Test
    void createFetchTeamsForAccountUrl_valid_test(){
        DcmConstants.initializeDcmConstants(AppMode.STAGING);
        String actual = DcmTeamUtil.createFetchTeamsForAccountUrl("accID",100);
        Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v3.0/objects/Contact/Group?apikey=accID&type=team&limit=100&productID=d56194e1-b98b-4068-86f8-d442777d2a16",actual);

    }

    @Test
    void getTeamsForAccountResponseFromDCM_nullCursor_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class);
            MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.createFetchTeamsForAccountUrl("accID",100)).thenReturn("url");
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForAccountResponseFromDCM("accID",100,null)).thenCallRealMethod();
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            HttpClient httpClient = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenReturn(httpClient);
            HashMap<String,Object> response = new HashMap(){{put("success",true);}};
            urlFetcherMockedStatic.when(()-> UrlFetcher.sendGetRequest("url",new String[]{"Authorization","Bearer token"},httpClient)).thenReturn(response);
            Assertions.assertEquals(response, DcmTeamUtil.getTeamsForAccountResponseFromDCM("accID",100,null));
        }
    }

    @Test
    void getTeamsForAccountResponseFromDCM_validCursor_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class);
            MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.createFetchTeamsForAccountUrl("accID",100)).thenReturn("url");
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForAccountResponseFromDCM("accID",100,"cursor")).thenCallRealMethod();
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            HttpClient httpClient = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenReturn(httpClient);
            HashMap<String,Object> response = new HashMap(){{put("success",true);}};
            urlFetcherMockedStatic.when(()-> UrlFetcher.sendGetRequest("url&cursor=cursor",new String[]{"Authorization","Bearer token"},httpClient)).thenReturn(response);
            Assertions.assertEquals(response, DcmTeamUtil.getTeamsForAccountResponseFromDCM("accID",100,"cursor"));
        }
    }

    @Test
    void getTeamsForUserInAnAccount_DcmResponseNull_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForUserInAnAccountResponseFromDCM("accID","123")).thenReturn(null);
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForUserInAnAccount("accID","123")).thenCallRealMethod();
            Map<String, Object> actual = DcmTeamUtil.getTeamsForUserInAnAccount("accID","123");
            Assertions.assertTrue(((ArrayList<TeamDTO>)actual.get("teams")).isEmpty());
        }
    }

    @Test
    void getTeamsForUserInAnAccount_DcmResponseEmpty_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForUserInAnAccountResponseFromDCM("accID","123")).thenReturn(new HashMap<>());
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForUserInAnAccount("accID","123")).thenCallRealMethod();
            Map<String, Object> actual = DcmTeamUtil.getTeamsForUserInAnAccount("accID","123");
            Assertions.assertTrue(((ArrayList<TeamDTO>)actual.get("teams")).isEmpty());
        }
    }

    @Test
    void getTeamsForUserInAnAccount_DcmResponseSuccessFalse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForUserInAnAccountResponseFromDCM("accID","123")).thenReturn(new HashMap(){{put("success",false);}});
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForUserInAnAccount("accID","123")).thenCallRealMethod();
            Map<String, Object> actual = DcmTeamUtil.getTeamsForUserInAnAccount("accID","123");
            Assertions.assertTrue(((ArrayList<TeamDTO>)actual.get("teams")).isEmpty());
        }
    }

    @Test
    void getTeamsForUserInAnAccount_DcmResponseValid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForUserInAnAccountResponseFromDCM("accID","123")).thenReturn(new HashMap(){{put("success",true);
                put("group", new HashMap(){{put("123",new ArrayList(){{add(new HashMap(){{
                    put("id","teamid");
                    put("ownerID","ownerid");
                    put("accountID","accID");
                    put("name","teamname");
                    put("linkedContacts",new ArrayList(){{add("contact1");add("contact2");}});
                }});}});}});
            }});
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForUserInAnAccount("accID","123")).thenCallRealMethod();
            Map<String, Object> actual = DcmTeamUtil.getTeamsForUserInAnAccount("accID","123");
            List<TeamDTO> actualList = (ArrayList<TeamDTO>)actual.get("teams");
            Assertions.assertEquals(1,actualList.size());
            Assertions.assertEquals("teamid",actualList.get(0).getId());
            Assertions.assertEquals("ownerid",actualList.get(0).getOwnerContactID());
            Assertions.assertEquals("accID",actualList.get(0).getAccountID());
            Assertions.assertEquals("teamname",actualList.get(0).getName());
            Assertions.assertEquals(2,actualList.get(0).getMembers().size());
            Assertions.assertEquals("contact1",actualList.get(0).getMembers().get(0));
            Assertions.assertEquals("contact2",actualList.get(0).getMembers().get(1));
        }
    }

    @Test
    void createFetchTeamsForUserInAnAccountUrl_valid_test(){
        DcmConstants.initializeDcmConstants(AppMode.STAGING);
        String actual = DcmTeamUtil.createFetchTeamsForUserInAnAccountUrl("accID");
        Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v3.0/objects/Contact/Group?apikey=accID&type=team&productID=d56194e1-b98b-4068-86f8-d442777d2a16",actual);
    }

    @Test
    void getTeamsForUserInAnAccountResponseFromDCM_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class);
            MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.createFetchTeamsForUserInAnAccountUrl("accID")).thenReturn("url");
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForUserInAnAccountResponseFromDCM("accID","123")).thenCallRealMethod();
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            HttpClient httpClient = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenReturn(httpClient);
            HashMap<String,Object> response = new HashMap(){{put("success",true);}};
            urlFetcherMockedStatic.when(()-> UrlFetcher.sendPostRequest("url",new HashMap(){{put("contact", new ArrayList(){{add("123");}});}},new String[]{"Authorization","Bearer token"},httpClient)).thenReturn(response);
            Assertions.assertEquals(response, DcmTeamUtil.getTeamsForUserInAnAccountResponseFromDCM("accID","123"));
        }
    }

    @Test
    void createFetchTeamInfoUrl_valid_test(){
        Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v3.0/objects/Group/teamID?apikey=accID&productID=d56194e1-b98b-4068-86f8-d442777d2a16",DcmTeamUtil.createFetchTeamInfoUrl("accID","teamID"));
    }

    @Test
    void getTeamInfo_nullDcmResponse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamInfoResponseFromDcm("accID","teamID")).thenReturn(null);
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamInfo("accID","teamID")).thenCallRealMethod();
            Assertions.assertNull(DcmTeamUtil.getTeamInfo("accID","teamID"));
        }
    }

    @Test
    void getTeamInfo_emptyDcmResponse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamInfoResponseFromDcm("accID","teamID")).thenReturn(new HashMap<>());
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamInfo("accID","teamID")).thenCallRealMethod();
            Assertions.assertNull(DcmTeamUtil.getTeamInfo("accID","teamID"));
        }
    }

    @Test
    void getTeamInfo_DcmResponseSuccessFalse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamInfoResponseFromDcm("accID","teamID")).thenReturn(Map.of("success",false));
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamInfo("accID","teamID")).thenCallRealMethod();
            Assertions.assertNull(DcmTeamUtil.getTeamInfo("accID","teamID"));
        }
    }

    @Test
    void getTeamInfo_DcmResponseSuccessTrue_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            Map<String,Object> response =new HashMap(){{
                put("id","teamid");
                put("ownerID","ownerid");
                put("accountID","accID");
                put("name","teamname");
                put("linkedContacts",new ArrayList(){{add("contact1");add("contact2");}});
            }};
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamInfoResponseFromDcm("accID","teamID")).thenReturn(Map.of("success",true,"group",response));
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamInfo("accID","teamID")).thenCallRealMethod();
            TeamDTO actual  = DcmTeamUtil.getTeamInfo("accID","teamID");
            Assertions.assertEquals("teamid",actual.getId());
            Assertions.assertEquals("ownerid",actual.getOwnerContactID());
            Assertions.assertEquals("accID",actual.getAccountID());
            Assertions.assertEquals("teamname",actual.getName());
            Assertions.assertEquals(2,actual.getMembers().size());
            Assertions.assertEquals("contact1",actual.getMembers().get(0));
            Assertions.assertEquals("contact2",actual.getMembers().get(1));
        }
    }

    @Test
    void getTeamInfoResponseFromDcm_Valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenReturn(client);
            headerUtilMockedStatic.when(HeaderUtil::getServerTokenAuthHeader).thenReturn(new String[]{});
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.createFetchTeamInfoUrl("accID","teamID")).thenReturn("url");
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamInfoResponseFromDcm("accID","teamID")).thenCallRealMethod();
            urlFetcherMockedStatic.when(()->UrlFetcher.sendGetRequest("url",new String[]{},client)).thenReturn(Map.of("success",true));
            Assertions.assertTrue((boolean)(DcmTeamUtil.getTeamInfoResponseFromDcm("accID","teamID").get("success")));
        }
    }

    @Test
    void deleteTeam_null_dcmResponse_test(){
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenReturn(client);
            headerUtilMockedStatic.when(HeaderUtil::getServerTokenAuthHeader).thenReturn(new String[]{});
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.createFetchTeamInfoUrl("accID","teamID")).thenReturn("url");
            urlFetcherMockedStatic.when(()->UrlFetcher.sendDeleteRequest("url",new String[]{},client)).thenReturn(null);
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.deleteTeam("accID","teamID")).thenCallRealMethod();
            DcmTeamUtil.deleteTeam("accID","teamID");
        }catch(Exception e){
            Assertions.assertEquals("Delete team request failed",e.getMessage());
        }
    }

    @Test
    void deleteTeam_empty_dcmResponse_test(){
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenReturn(client);
            headerUtilMockedStatic.when(HeaderUtil::getServerTokenAuthHeader).thenReturn(new String[]{});
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.createFetchTeamInfoUrl("accID","teamID")).thenReturn("url");
            urlFetcherMockedStatic.when(()->UrlFetcher.sendDeleteRequest("url",new String[]{},client)).thenReturn(new HashMap());
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.deleteTeam("accID","teamID")).thenCallRealMethod();
            DcmTeamUtil.deleteTeam("accID","teamID");
        }catch(Exception e){
            Assertions.assertEquals("Delete team request failed",e.getMessage());
        }
    }

    @Test
    void deleteTeam_successFalse_dcmResponse_test(){
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenReturn(client);
            headerUtilMockedStatic.when(HeaderUtil::getServerTokenAuthHeader).thenReturn(new String[]{});
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.createFetchTeamInfoUrl("accID","teamID")).thenReturn("url");
            urlFetcherMockedStatic.when(()->UrlFetcher.sendDeleteRequest("url",new String[]{},client)).thenReturn(Map.of("success",false));
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.deleteTeam("accID","teamID")).thenCallRealMethod();
            DcmTeamUtil.deleteTeam("accID","teamID");
        }catch(Exception e){
            Assertions.assertEquals("Delete team request failed",e.getMessage());
        }
    }

    @Test
    void deleteTeam_successTrue_dcmResponse_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenReturn(client);
            headerUtilMockedStatic.when(HeaderUtil::getServerTokenAuthHeader).thenReturn(new String[]{});
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.createFetchTeamInfoUrl("accID","teamID")).thenReturn("url");
            urlFetcherMockedStatic.when(()->UrlFetcher.sendDeleteRequest("url",new String[]{},client)).thenReturn(Map.of("success",true));
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.deleteTeam("accID","teamID")).thenCallRealMethod();
            DcmTeamUtil.deleteTeam("accID","teamID");
            urlFetcherMockedStatic.verify(()->UrlFetcher.sendDeleteRequest("url",new String[]{},client));
        }
    }

    @Test
    void createTeam_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenReturn(client);
            headerUtilMockedStatic.when(HeaderUtil::getJsonUtf8ContentTypedServerTokenAuthHeader).thenReturn(new String[]{});
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.createTeamCreationUrl("accID")).thenReturn("url");
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url",Map.of(),new String[]{},client)).thenReturn(Map.of("success",true));
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.createTeam("accID",Map.of())).thenCallRealMethod();
            DcmTeamUtil.createTeam("accID",Map.of());
            urlFetcherMockedStatic.verify(()->UrlFetcher.sendPostRequest("url",Map.of(),new String[]{},client));
        }
    }

    @Test
    void updateTeamInfo_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenReturn(client);
            headerUtilMockedStatic.when(HeaderUtil::getJsonUtf8ContentTypedServerTokenAuthHeader).thenReturn(new String[]{});
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.createFetchTeamInfoUrl("accID","teamID")).thenReturn("url");
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPutRequest("url",Map.of(),new String[]{},client)).thenReturn(Map.of("success",true));
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.updateTeamInfo("accID","teamID",Map.of())).thenCallRealMethod();
            DcmTeamUtil.updateTeamInfo("accID","teamID",Map.of());
            urlFetcherMockedStatic.verify(()->UrlFetcher.sendPutRequest("url",Map.of(),new String[]{},client));
        }
    }

    @Test
    void updateTeamContacts_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            HttpClient client = Mockito.mock(HttpClient.class);
            urlFetcherMockedStatic.when(UrlFetcher::getHttpClientInstance).thenReturn(client);
            headerUtilMockedStatic.when(HeaderUtil::getServerTokenAuthHeader).thenReturn(new String[]{});
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.createUpdateTeamContactsUrl("accID","teamID","add")).thenReturn("url");
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url",Map.of("type","add"),new String[]{},client)).thenReturn(Map.of("success",true));
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.updateTeamContacts("accID","teamID",Map.of("type","add"))).thenCallRealMethod();
            Assertions.assertEquals( Map.of("success",true),DcmTeamUtil.updateTeamContacts("accID","teamID",Map.of("type","add")));
        }
    }

    @Test
    void createTeamCreationUrl_valid_test(){
        DcmConstants.initializeDcmConstants(AppMode.STAGING);
        String actual = DcmTeamUtil.createTeamCreationUrl("accID");
        Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v3.0/objects/Group?apikey=accID&productID=d56194e1-b98b-4068-86f8-d442777d2a16",actual);
    }

    @Test
    void createUpdateTeamContactsUrl_valid_test(){
        DcmConstants.initializeDcmConstants(AppMode.STAGING);
        String actual = DcmTeamUtil.createUpdateTeamContactsUrl("accID","teamID","add");
        Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v3.0/objects/Group/teamID/Contact?apikey=accID&operation=add&productID=d56194e1-b98b-4068-86f8-d442777d2a16",actual);
    }

    @Test
    void getAllTeamsUnderAccount_nullAccountID_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getAllTeamsUnderAccount(null)).thenCallRealMethod();
            DcmTeamUtil.getAllTeamsUnderAccount(null);
            dcmTeamUtilMockedStatic.verify(()->DcmTeamUtil.getAllTeamsUnderAccount(null));
            dcmTeamUtilMockedStatic.verifyNoMoreInteractions();
        }
    }

    @Test
    void getAllTeamsUnderAccount_validAccountID_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getAllTeamsUnderAccount("accID")).thenCallRealMethod();
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamsForAnAccount("accID",100,"")).thenReturn(Map.of("teams",List.of(new TeamDTO(),new TeamDTO()),"cursor","cursor"));
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamsForAnAccount("accID",100,"cursor")).thenReturn(Map.of("teams",List.of(new TeamDTO(),new TeamDTO())));
            Assertions.assertEquals(4,DcmTeamUtil.getAllTeamsUnderAccount("accID").size());
        }
    }

    @Test
    void deleteAllTeamsUnderAccount_Exception_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.deleteAllTeamsUnderAccount("accID")).thenCallRealMethod();
            TeamDTO team1 = new TeamDTO();
            team1.setId("1");
            TeamDTO team2 = new TeamDTO();
            team2.setId("2");
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getAllTeamsUnderAccount("accID")).thenReturn(List.of(team1,team2));
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.deleteTeam("accID","1")).thenThrow(new IllegalArgumentException("exception"));
            DcmTeamUtil.deleteAllTeamsUnderAccount("accID");
            dcmTeamUtilMockedStatic.verify(()-> DcmTeamUtil.deleteTeam(eq("accID"),anyString()),Mockito.times(2));
        }
    }

    @Test
    void deleteAllTeamsUnderAccount_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.deleteAllTeamsUnderAccount("accID")).thenCallRealMethod();
            TeamDTO team1 = new TeamDTO();
            team1.setId("1");
            TeamDTO team2 = new TeamDTO();
            team2.setId("2");
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getAllTeamsUnderAccount("accID")).thenReturn(List.of(team1,team2));
            DcmTeamUtil.deleteAllTeamsUnderAccount("accID");
            dcmTeamUtilMockedStatic.verify(()-> DcmTeamUtil.deleteTeam(eq("accID"),anyString()),Mockito.times(2));
        }
    }

    @Test
    void getAllTeamMembersOfAnUserInAccount_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            TeamDTO teamDTO1 = new TeamDTO();
            teamDTO1.setMembers(List.of("1","2"));
            TeamDTO teamDTO2 = new TeamDTO();
            teamDTO2.setMembers(List.of("4","2"));
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamsForUserInAnAccount("accID","123")).thenReturn(Map.of("teams",List.of(teamDTO1,teamDTO2)));
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getAllTeamMembersOfAnUserInAccount("accID","123")).thenCallRealMethod();
            Assertions.assertEquals(Set.of("1","2","4"),DcmTeamUtil.getAllTeamMembersOfAnUserInAccount("accID","123"));
        }
    }
}
