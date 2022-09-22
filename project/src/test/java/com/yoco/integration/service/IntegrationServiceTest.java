package com.yoco.integration.service;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.google.api.client.json.Json;
import com.google.appengine.api.users.User;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.dataservices.impl.IntegrationsImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.IntegrationsJDO;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.ProjectUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.integration.enums.INTEGRATION_ERROR_RESPONSE;
import com.yoco.integration.helper.IntegrationHelper;
import com.yoco.integration.modal.IntegrationsEntityResponse;
import com.yoco.integration.modal.ZendeskInfoResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.test.web.servlet.MockMvc;

import java.io.IOException;
import java.net.http.HttpClient;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeoutException;

import static org.mockito.ArgumentMatchers.*;

class IntegrationServiceTest {
    @Test
    void enableIntegration_nullIntegrationType_test(){
        try{
            IntegrationService.enableIntegration("123","234",null,"payload",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.INVALID_INTEGRATION_TYPE.value(),e.getMessage());
        }
    }

    @Test
    void enableIntegration_emptyIntegrationType_test(){
        try{
            IntegrationService.enableIntegration("123","234","","payload",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.INVALID_INTEGRATION_TYPE.value(),e.getMessage());
        }
    }

    @Test
    void enableIntegration_nullPayload_test(){
        try{
            IntegrationService.enableIntegration("123","234","slack","payload",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void enableIntegration_emptyPayload_test(){
        try{
            IntegrationService.enableIntegration("123","234","slack","{}",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void enableIntegration_accessNotAllowed_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class)){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(false);
            IntegrationService.enableIntegration("123","234","slack","{\"details\":\"slack\"}",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.ACCESS_DENIED.value(),e.getMessage());
        }
    }

    @Test
    void enableIntegration_nullUserPro_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("234", "123")).thenReturn(null);
            IntegrationService.enableIntegration("123","234","slack","{\"details\":\"slack\"}",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void enableIntegration_createIntegrationNullResponse_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<IntegrationsImpl> integrationsImplMockedStatic = Mockito.mockStatic(IntegrationsImpl.class);){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            String integrationDetails = "{}";
            integrationHelperMockedStatic.when(()->IntegrationHelper.extractIntegrationDetails(any(Map.class))).thenReturn(integrationDetails);
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("123");
            userPRO.setContactId("234");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("123", "234")).thenReturn(userPRO);

            IntegrationsImpl integrationsImplMock = Mockito.mock(IntegrationsImpl.class);
            Mockito.when(integrationsImplMock.getIntegrationByType("123","234","slack")).thenReturn(null);
            integrationHelperMockedStatic.when(()->IntegrationHelper.createIntegration(integrationsImplMock,userPRO,integrationDetails,"slack")).thenReturn(null);
            integrationsImplMockedStatic.when(IntegrationsImpl::getIntegrationsImplInstance).thenReturn(integrationsImplMock);
            Assertions.assertTrue(IntegrationService.enableIntegration("123","234","slack","{\"details\":\"slack\"}",new OauthAccessToken()).isEmpty());
        }
    }

    @Test
    void enableIntegration_createIntegrationValidResponse_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<IntegrationsImpl> integrationsImplMockedStatic = Mockito.mockStatic(IntegrationsImpl.class);){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            String integrationDetails = "{}";
            integrationHelperMockedStatic.when(()->IntegrationHelper.extractIntegrationDetails(any(Map.class))).thenReturn(integrationDetails);
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("123");
            userPRO.setContactId("234");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("123", "234")).thenReturn(userPRO);

            IntegrationsImpl integrationsImplMock = Mockito.mock(IntegrationsImpl.class);
            Mockito.when(integrationsImplMock.getIntegrationByType("123","234","slack")).thenReturn(null);
            IntegrationsJDO result = new IntegrationsJDO();
            integrationHelperMockedStatic.when(()->IntegrationHelper.createIntegration(integrationsImplMock,userPRO,integrationDetails,"slack")).thenReturn(result);
            integrationsImplMockedStatic.when(IntegrationsImpl::getIntegrationsImplInstance).thenReturn(integrationsImplMock);
            Map<String,Object> actual = IntegrationService.enableIntegration("123","234","slack","{\"details\":\"slack\"}",new OauthAccessToken());
            Assertions.assertEquals(new IntegrationsEntityResponse(result),actual.get("integration"));
        }
    }

    @Test
    void enableIntegration_updateIntegrationValidResponse_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<IntegrationsImpl> integrationsImplMockedStatic = Mockito.mockStatic(IntegrationsImpl.class);){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            String integrationDetails = "{}";
            integrationHelperMockedStatic.when(()->IntegrationHelper.extractIntegrationDetails(any(Map.class))).thenReturn(integrationDetails);
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("123");
            userPRO.setContactId("234");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("123", "234")).thenReturn(userPRO);

            IntegrationsJDO result = new IntegrationsJDO();
            IntegrationsImpl integrationsImplMock = Mockito.mock(IntegrationsImpl.class);
            Mockito.when(integrationsImplMock.getIntegrationByType("123","234","slack")).thenReturn(result);
            integrationHelperMockedStatic.when(()->IntegrationHelper.updateIntegration(integrationsImplMock,result,integrationDetails)).thenReturn(result);
            integrationsImplMockedStatic.when(IntegrationsImpl::getIntegrationsImplInstance).thenReturn(integrationsImplMock);
            Map<String,Object> actual = IntegrationService.enableIntegration("123","234","slack","{\"details\":\"slack\"}",new OauthAccessToken());
            Assertions.assertEquals(new IntegrationsEntityResponse(result),actual.get("integration"));
        }
    }

    @Test
    void disableIntegration_nullIntegrationType_test(){
        try{
            IntegrationService.disableIntegration("123","234",null,new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.INVALID_INTEGRATION_TYPE.value(),e.getMessage());
        }
    }

    @Test
    void disableIntegration_emptyIntegrationType_test(){
        try{
            IntegrationService.disableIntegration("123","234","",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.INVALID_INTEGRATION_TYPE.value(),e.getMessage());
        }
    }

    @Test
    void disableIntegration_accessNotAllowed_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class)){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(false);
            IntegrationService.disableIntegration("123","234","slack",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.ACCESS_DENIED.value(),e.getMessage());
        }
    }

    @Test
    void disableIntegration_nullUserPro_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("234", "123")).thenReturn(null);
            IntegrationService.disableIntegration("123","234","slack",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void disableIntegration_nullDisableResponse_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<IntegrationsImpl> integrationsImplMockedStatic = Mockito.mockStatic(IntegrationsImpl.class)){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("123");
            userPRO.setContactId("234");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("123", "234")).thenReturn(userPRO);
            IntegrationsImpl integrationsImplMock = Mockito.mock(IntegrationsImpl.class);
            Mockito.when(integrationsImplMock.getIntegrationByType("123","234","slack")).thenReturn(null);
            integrationsImplMockedStatic.when(IntegrationsImpl::getIntegrationsImplInstance).thenReturn(integrationsImplMock);
            integrationHelperMockedStatic.when(()->IntegrationHelper.disableIntegration(eq(integrationsImplMock),isNull())).thenReturn(null);
            Assertions.assertTrue(IntegrationService.disableIntegration("123","234","slack",new OauthAccessToken()).isEmpty());
        }
    }

    @Test
    void disableIntegration_validDisableResponse_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<IntegrationsImpl> integrationsImplMockedStatic = Mockito.mockStatic(IntegrationsImpl.class)){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("123");
            userPRO.setContactId("234");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("123", "234")).thenReturn(userPRO);
            IntegrationsImpl integrationsImplMock = Mockito.mock(IntegrationsImpl.class);
            Mockito.when(integrationsImplMock.getIntegrationByType("123","234","slack")).thenReturn(null);
            integrationsImplMockedStatic.when(IntegrationsImpl::getIntegrationsImplInstance).thenReturn(integrationsImplMock);
            IntegrationsJDO result = new IntegrationsJDO();
            integrationHelperMockedStatic.when(()->IntegrationHelper.disableIntegration(eq(integrationsImplMock),isNull())).thenReturn(result);
            Assertions.assertEquals(new IntegrationsEntityResponse(result),IntegrationService.disableIntegration("123","234","slack",new OauthAccessToken()).get("integration"));
        }
    }

    @Test
    void getIntegration_nullIntegrationType_test(){
        try{
            IntegrationService.getIntegration("123","234",null,new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.INVALID_INTEGRATION_TYPE.value(),e.getMessage());
        }
    }

    @Test
    void getIntegration_emptyIntegrationType_test(){
        try{
            IntegrationService.getIntegration("123","234","",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.INVALID_INTEGRATION_TYPE.value(),e.getMessage());
        }
    }

    @Test
    void getIntegration_accessNotAllowed_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class)){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(false);
            IntegrationService.getIntegration("123","234","slack",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.ACCESS_DENIED.value(),e.getMessage());
        }
    }

    @Test
    void getAllIntegrations_accessNotAllowed_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class)){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(false);
            IntegrationService.getAllIntegrations("123","234",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.ACCESS_DENIED.value(),e.getMessage());
        }
    }

    @Test
    void getIntegration_nullUserPro_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("234", "123")).thenReturn(null);
            IntegrationService.getIntegration("123","234","slack",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getAllIntegrations_nullUserPro_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("234", "123")).thenReturn(null);
            IntegrationService.getAllIntegrations("123","234",new OauthAccessToken());
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getIntegration_nullResponse_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<IntegrationsImpl> integrationsImplMockedStatic = Mockito.mockStatic(IntegrationsImpl.class)){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("123");
            userPRO.setContactId("234");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("123", "234")).thenReturn(userPRO);
            IntegrationsImpl integrationsImplMock = Mockito.mock(IntegrationsImpl.class);
            Mockito.when(integrationsImplMock.getIntegrationByType("123","234","slack")).thenReturn(null);
            integrationsImplMockedStatic.when(IntegrationsImpl::getIntegrationsImplInstance).thenReturn(integrationsImplMock);
            Assertions.assertNull(IntegrationService.getIntegration("123","234","slack",new OauthAccessToken()).get("integration"));
        }
    }

    @Test
    void getAllIntegrations_nullResponse_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<IntegrationsImpl> integrationsImplMockedStatic = Mockito.mockStatic(IntegrationsImpl.class)){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("123");
            userPRO.setContactId("234");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("123", "234")).thenReturn(userPRO);
            IntegrationsImpl integrationsImplMock = Mockito.mock(IntegrationsImpl.class);
            Mockito.when(integrationsImplMock.getAllIntegrations("123","234")).thenReturn(null);
            integrationsImplMockedStatic.when(IntegrationsImpl::getIntegrationsImplInstance).thenReturn(integrationsImplMock);
            Assertions.assertTrue(((ArrayList)IntegrationService.getAllIntegrations("123","234",new OauthAccessToken()).get("integrations")).isEmpty());
        }
    }

    @Test
    void getIntegration_validResponse_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<IntegrationsImpl> integrationsImplMockedStatic = Mockito.mockStatic(IntegrationsImpl.class)){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("123");
            userPRO.setContactId("234");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("123", "234")).thenReturn(userPRO);
            IntegrationsImpl integrationsImplMock = Mockito.mock(IntegrationsImpl.class);
            IntegrationsJDO result = new IntegrationsJDO();
            Mockito.when(integrationsImplMock.getIntegrationByType("123","234","slack")).thenReturn(result);
            integrationsImplMockedStatic.when(IntegrationsImpl::getIntegrationsImplInstance).thenReturn(integrationsImplMock);
            Assertions.assertEquals(new IntegrationsEntityResponse(result),IntegrationService.getIntegration("123","234","slack",new OauthAccessToken()).get("integration"));
        }
    }

    @Test
    void getAllIntegrations_validResponse_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<IntegrationsImpl> integrationsImplMockedStatic = Mockito.mockStatic(IntegrationsImpl.class)){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("234"))).thenReturn(true);
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("123");
            userPRO.setContactId("234");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("123", "234")).thenReturn(userPRO);
            IntegrationsImpl integrationsImplMock = Mockito.mock(IntegrationsImpl.class);
            IntegrationsJDO result = new IntegrationsJDO();
            Mockito.when(integrationsImplMock.getAllIntegrations("123","234")).thenReturn(new ArrayList(){{add(result);}});
            integrationsImplMockedStatic.when(IntegrationsImpl::getIntegrationsImplInstance).thenReturn(integrationsImplMock);
            Assertions.assertEquals(new ArrayList(){{add(new IntegrationsEntityResponse(result));}},IntegrationService.getAllIntegrations("123","234",new OauthAccessToken()).get("integrations"));
        }
    }

    @Test
    void getZendeskInfo_nullTaskId_test(){
        try{
            IntegrationService.getZendeskInfo(null,new OauthAccessToken(),new Contact());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.INVALID_TASKID.value(),e.getMessage());
        }
    }

    @Test
    void getZendeskInfo_emptyTaskId_test(){
        try{
            IntegrationService.getZendeskInfo("",new OauthAccessToken(),new Contact());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.INVALID_TASKID.value(),e.getMessage());
        }
    }

    @Test
    void getZendeskInfo_accessNotAllowed_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class)){
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),isNull())).thenReturn(false);
            IntegrationService.getZendeskInfo("123",new OauthAccessToken(),new Contact());
        }catch (Exception e){
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.ACCESS_DENIED.value(),e.getMessage());
        }
    }

    @Test
    void getZendeskInfo_nullUserPRO_test(){
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractDefaultUserPRO(isNull())).thenReturn(null);
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),isNull())).thenReturn(true);
            IntegrationService.getZendeskInfo("123",new OauthAccessToken(),new Contact());
        }catch (Exception e){
            Assertions.assertEquals("Invalid accountID.",e.getMessage());
        }
    }

    @Test
    void getZendeskInfo_valid_test() throws NoSuchAlgorithmException, IOException, TimeoutException {
        try(MockedStatic<IntegrationHelper> integrationHelperMockedStatic = Mockito.mockStatic(IntegrationHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);
            MockedStatic<ReportImpl> reportsImplMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setContactId("contact");
            userPRO.setUniquepin("account");
            userPRO.setTimeZone("timezone");
            userPRO.setTimeZoneDisplayName("timezoneDisplayName");
            List<ProjectJDO> projectJDOList = new ArrayList<>();
            Map<String,Object> entriesMap = new HashMap<>();
            List<Map<String,Object>> entriesList = new ArrayList<>();
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectsForUserInAnAccount("account","contact")).thenReturn(projectJDOList);
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getUserEntriesByTaskID("account","contact","123")).thenReturn(entriesList);
            reportsImplMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);
            integrationHelperMockedStatic.when(()->IntegrationHelper.getEntriesMapForTaskID(entriesList,"timezone")).thenReturn(entriesMap);
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractDefaultUserPRO(isNull())).thenReturn(userPRO);
            integrationHelperMockedStatic.when(()->IntegrationHelper.isAccessAllowed(any(OauthAccessToken.class),eq("contact"))).thenReturn(true);
            Contact contact = new Contact();
            contact.setId("contact");
            contact.setFirstName("first");
            contact.setLastName("last");
            ZendeskInfoResponse response = IntegrationService.getZendeskInfo("123",new OauthAccessToken(),contact);
            Assertions.assertEquals("account",response.getAccountID());
            Assertions.assertEquals(entriesMap,response.getEntries());
            Assertions.assertEquals("contact",response.getContactID());
            Assertions.assertEquals(projectJDOList,response.getProjects());
            Assertions.assertEquals("timezoneDisplayName",response.getTimezone());
            Assertions.assertEquals("first last",response.getName());
        }
    }

    @Test
    void saveJiraInfo_null_payload_test(){
        try{
            IntegrationService.saveJiraInfo(null);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void saveJiraInfo_empty_payload_test(){
        try{
            IntegrationService.saveJiraInfo("");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void saveJiraInfo_null_emailID_payload_test(){
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("emailID", null);
            IntegrationService.saveJiraInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.toString(),e.getMessage());
        }
    }

    @Test
    void saveJiraInfo_empty_emailID_payload_test(){
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("emailID", "");
            IntegrationService.saveJiraInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.toString(),e.getMessage());
        }
    }

    @Test
    void saveJiraInfo_null_integration_type_test(){

        HashMap<String, Object> payload = new HashMap<>();
        payload.put("emailID", "abc@gmail.com");
        payload.put("info", new HashMap<>());

        PeopleRelationJDO objPro = new PeopleRelationJDO();
        objPro.setContactId("1234");
        objPro.setUniquepin("5678");

        try(MockedConstruction<UserImpl> mock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
                Mockito.when(userImplMock.getDefaultUserProWithEmail(anyString())).thenReturn(objPro);
            });
            MockedConstruction<IntegrationsImpl> objIntegrationsImplMock = Mockito.mockConstruction(IntegrationsImpl.class, (integrationsImplMock, context) -> {
                Mockito.when(integrationsImplMock.getIntegrationByType(anyString(), anyString(), anyString())).thenReturn(null);
            })){

           IntegrationsJDO resp =  IntegrationService.saveJiraInfo(JsonUtil.getJson(payload));
           Assertions.assertTrue(objPro.getUniquepin().equalsIgnoreCase(resp.getUniquePin()));
        }
    }

    @Test
    void saveJiraInfo_existing_integration_type_test(){

        HashMap<String, Object> payload = new HashMap<>();
        payload.put("emailID", "abc@gmail.com");
        payload.put("info", new HashMap<>());

        PeopleRelationJDO objPro = new PeopleRelationJDO();
        objPro.setContactId("1234");
        objPro.setUniquepin("5678");

        IntegrationsJDO objIntegration = new IntegrationsJDO();
        objIntegration.setIntegrationDetails(JsonUtil.getJson(payload.get("info")));

        try(MockedConstruction<UserImpl> mock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
            Mockito.when(userImplMock.getDefaultUserProWithEmail(anyString())).thenReturn(objPro);
        });
            MockedConstruction<IntegrationsImpl> objIntegrationsImplMock = Mockito.mockConstruction(IntegrationsImpl.class, (integrationsImplMock, context) -> {
                Mockito.when(integrationsImplMock.getIntegrationByType(anyString(), anyString(), anyString())).thenReturn(objIntegration);
            })){

            IntegrationsJDO resp =  IntegrationService.saveJiraInfo(JsonUtil.getJson(payload));
            Assertions.assertTrue(objIntegration.getIntegrationDetails().equalsIgnoreCase(resp.getIntegrationDetails()));
        }
    }

    @Test
    void getJiraInfo_user_not_found_test() {
        try(MockedConstruction<UserImpl> mock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(null);
            })){
            Map<String, Object> response = IntegrationService.getJiraInfo("1234");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_FOUND.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraInfo_empty_integration_type_test() {
        PeopleRelationJDO objPro = new PeopleRelationJDO();
        objPro.setContactId("1234");
        try(MockedConstruction<UserImpl> mock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(objPro);
        });
            MockedConstruction<IntegrationsImpl> integrationsImplmock = Mockito.mockConstruction(IntegrationsImpl.class, (integrationsImplMock, context) -> {
            Mockito.when(integrationsImplMock.getIntegrationByType(anyString(),anyString(), anyString())).thenReturn(null);
        })){
            Map<String, Object> response = IntegrationService.getJiraInfo("1234");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        }
    }

    @Test
    void getJiraInfo_valid_integration_type_test() {
        PeopleRelationJDO objPro = new PeopleRelationJDO();
        objPro.setContactId("1234");
        objPro.setUniquepin("1234");
        IntegrationsJDO objIntegration = new IntegrationsJDO();
        objIntegration.setIntegrationID("1234");
        try(MockedConstruction<UserImpl> mock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
            Mockito.when(userImplMock.getDefaultUserPro(anyString())).thenReturn(objPro);
            });
            MockedConstruction<IntegrationsImpl> integrationsImplmock = Mockito.mockConstruction(IntegrationsImpl.class, (integrationsImplMock, context) -> {
                Mockito.when(integrationsImplMock.getIntegrationByType(anyString(),anyString(), anyString())).thenReturn(objIntegration);
            })){
            Map<String, Object> response = IntegrationService.getJiraInfo("1234");
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(response));
        }
    }

    @Test
    void getJiraTenantInfo_null_payload_test() {
        try{
            IntegrationService.getJiraTenantInfo(null);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraTenantInfo_empty_payload_test() {
        try{
            IntegrationService.getJiraTenantInfo("");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraTenantInfo_no_token_key_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            IntegrationService.getJiraTenantInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraTenantInfo_null_token_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("token", null);
            IntegrationService.getJiraTenantInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraTenantInfo_empty_token_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("token", "");
            IntegrationService.getJiraTenantInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraTenantInfo_valid_test() throws IOException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesmock = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<UrlFetcher> urlFetcherMock = Mockito.mockStatic(UrlFetcher.class);){
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("token", "1234");
            commonAppPropertiesmock.when(()->CommonAppProperties.getJiraUrl()).thenReturn("url");
            urlFetcherMock.when(()-> UrlFetcher.sendGetRequest(anyString(), any(String[].class), any(HttpClient.class))).thenReturn(new HashMap<String, Object>());

            Assertions.assertTrue(ObjUtils.isNullOrEmpty(IntegrationService.getJiraTenantInfo(JsonUtil.getJson(payload))));
        }
    }

    @Test
    void getJiraCardInfo_null_payload_test() {
        try{
            IntegrationService.getJiraCardInfo(null);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraCardInfo_empty_payload_test() {
        try{
            IntegrationService.getJiraCardInfo("");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraCardInfo_no_token_key_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            IntegrationService.getJiraCardInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraCardInfo_null_token_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("token", null);
            IntegrationService.getJiraCardInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraCardInfo_empty_token_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("token", "");
            IntegrationService.getJiraCardInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraCardInfo_no_jiraID_key_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("token", 1234);
            IntegrationService.getJiraCardInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraCardInfo_null_jiraID_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("token", 1234);
            payload.put("jiraID", null);
            IntegrationService.getJiraCardInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraCardInfo_empty_jiraID_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("token", 1234);
            payload.put("jiraID", "");
            IntegrationService.getJiraCardInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraCardInfo_no_cardID_key_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("token", "1234");
            payload.put("jiraID", "5678");
            IntegrationService.getJiraCardInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraCardInfo_null_cardID_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("token", "1234");
            payload.put("jiraID", "5678");
            payload.put("cardID", null);
            IntegrationService.getJiraCardInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraCardInfo_empty_cardID_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("token", "1234");
            payload.put("jiraID", "5678");
            payload.put("cardID", "");
            IntegrationService.getJiraCardInfo(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void getJiraCardInfo_valid_test() throws IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMock = Mockito.mockStatic(UrlFetcher.class);){
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("token", "1234");
            payload.put("jiraID", "5678");
            payload.put("cardID", "9876");

            urlFetcherMock.when(()-> UrlFetcher.sendGetRequest(anyString(), any(String[].class), any(HttpClient.class))).thenReturn(new HashMap<String, Object>());
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(IntegrationService.getJiraCardInfo(JsonUtil.getJson(payload))));
        }
    }

    @Test
    void updateNewToken_null_payload_test() {
        try{
            IntegrationService.updateNewToken(null);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_empty_payload_test() {
        try{
            IntegrationService.updateNewToken("");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_no_token_key_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_null_token_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", null);
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_empty_token_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", "");
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_no_accountID_key_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", "1234");
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_null_accountID_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", "1234");
            payload.put("accountID", null);
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_empty_accountID_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", "1234");
            payload.put("accountID", "");
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_no_contactID_key_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", "1234");
            payload.put("accountID", "5678");
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_null_contactID_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", "1234");
            payload.put("accountID", "5678");
            payload.put("contactID", null);
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_empty_contactID_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", "1234");
            payload.put("accountID", "5678");
            payload.put("contactID", "");
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_no_refreshToken_key_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", "1234");
            payload.put("accountID", "5678");
            payload.put("contactID", "9876");
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_null_refreshToken_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", "1234");
            payload.put("accountID", "5678");
            payload.put("contactID", "9876");
            payload.put("refreshToken", null);
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_empty_refreshToken_in_payload_test() {
        try{
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", "1234");
            payload.put("accountID", "5678");
            payload.put("contactID", "9876");
            payload.put("refreshToken", "");
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_user_not_found_test() {
        try(MockedConstruction<UserImpl> userImplMock = Mockito.mockConstruction(UserImpl.class, (userImplmock, context)->{
            Mockito.when(userImplmock.getUserWithoutContact(anyString(), anyString())).thenReturn(null);
        })){
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", "1234");
            payload.put("accountID", "5678");
            payload.put("contactID", "9876");
            payload.put("refreshToken", "refresh_token");
            IntegrationService.updateNewToken(JsonUtil.getJson(payload));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_FOUND.toString(),e.getMessage());
        }
    }

    @Test
    void updateNewToken_no_jira_type_exists_test() {
        PeopleRelationJDO objPro = new PeopleRelationJDO();
        objPro.setUniquepin("1234");
        objPro.setContactId("5678");
        try(MockedConstruction<UserImpl> userImplMock = Mockito.mockConstruction(UserImpl.class, (userImplmock, context)->{
                Mockito.when(userImplmock.getUserWithoutContact(anyString(), anyString())).thenReturn(objPro);
            });
            MockedConstruction<IntegrationsImpl> integrationsMockedConstruction = Mockito.mockConstruction(IntegrationsImpl.class, (integrationImplMock, context)->{
                Mockito.when(integrationImplMock.getIntegrationByType(anyString(), anyString(), anyString())).thenReturn(null);
            })){
            HashMap<String, Object> payload = new HashMap<>();
            payload.put("accessToken", "1234");
            payload.put("accountID", "5678");
            payload.put("contactID", "9876");
            payload.put("refreshToken", "refresh_token");
            Map<String, Object> resp = IntegrationService.updateNewToken(JsonUtil.getJson(payload));
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
        }
    }

    @Test
    void updateNewToken_valid_test() {

        HashMap<String, Object> payload = new HashMap<>();
        payload.put("accessToken", "1234");
        payload.put("accountID", "5678");
        payload.put("contactID", "9876");
        payload.put("refreshToken", "refresh_token");

        PeopleRelationJDO objPro = new PeopleRelationJDO();
        objPro.setUniquepin("1234");
        objPro.setContactId("5678");

        IntegrationsJDO objIntegration = new IntegrationsJDO();
        objIntegration.setIntegrationDetails(JsonUtil.getJson(payload));

        try(MockedConstruction<UserImpl> userImplMock = Mockito.mockConstruction(UserImpl.class, (userImplmock, context)->{
            Mockito.when(userImplmock.getUserWithoutContact(anyString(), anyString())).thenReturn(objPro);
        });
            MockedConstruction<IntegrationsImpl> integrationsMockedConstruction = Mockito.mockConstruction(IntegrationsImpl.class, (integrationImplMock, context)->{
                Mockito.when(integrationImplMock.getIntegrationByType(anyString(), anyString(), anyString())).thenReturn(objIntegration);
            })){

            Map<String, Object> resp = IntegrationService.updateNewToken(JsonUtil.getJson(payload));
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));
        }
    }

    @Test
    void deleteJiraInfo_null_accountID_test() {
        try{
            IntegrationService.deleteJiraInfo(null, "1234");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.toString(),e.getMessage());
        }
    }

    @Test
    void deleteJiraInfo_empty_accountID_test() {
        try{
            IntegrationService.deleteJiraInfo("", "1234");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.toString(),e.getMessage());
        }
    }

    @Test
    void deleteJiraInfo_null_contactID_test() {
        try{
            IntegrationService.deleteJiraInfo("1234", null);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.toString(),e.getMessage());
        }
    }

    @Test
    void deleteJiraInfo_empty_contactID_test() {
        try{
            IntegrationService.deleteJiraInfo("1234", "");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.toString(),e.getMessage());
        }
    }

    @Test
    void deleteJiraInfo_null_integration_type_test() {
        try (MockedConstruction<IntegrationsImpl> mock = Mockito.mockConstruction(IntegrationsImpl.class, (integrationImplMock, context)->{
            Mockito.when(integrationImplMock.getIntegrationByType(anyString(), anyString(), anyString())).thenReturn(null);
        })) {
            Assertions.assertFalse(IntegrationService.deleteJiraInfo("1234", "5678"));
        }
    }

    @Test
    void deleteJiraInfo_valid_integration_type_test() {
        try (MockedConstruction<IntegrationsImpl> mock = Mockito.mockConstruction(IntegrationsImpl.class, (integrationImplMock, context)->{
            Mockito.when(integrationImplMock.getIntegrationByType(anyString(), anyString(), anyString())).thenReturn(new IntegrationsJDO());
        })) {
            Assertions.assertTrue(IntegrationService.deleteJiraInfo("1234", "5678"));
        }
    }


}
