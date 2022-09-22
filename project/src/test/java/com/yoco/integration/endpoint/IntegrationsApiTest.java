package com.yoco.integration.endpoint;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.integration.enums.INTEGRATION_ERROR_RESPONSE;
import com.yoco.integration.modal.ZendeskInfoResponse;
import com.yoco.integration.service.IntegrationService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.MockitoRule;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import javax.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.HashMap;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mockStatic;

class IntegrationsApiTest {
    @Test
    void enableIntegration_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Test Error"));
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().enableIntegration("123","123","slack","payload",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertFalse(responseEntity.getBody().isSuccess());
            Assertions.assertEquals("Test Error",responseEntity.getBody().getErrorMessage());
        }
    }

    @Test
    void enableIntegration_nullResponseMap_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedStatic<IntegrationService> integrationServiceMockedStatic = mockStatic(IntegrationService.class);){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(new OauthAccessToken());
            integrationServiceMockedStatic.when(()-> IntegrationService.enableIntegration(eq("123"),eq("123"),eq("slack"),eq("payload"),any(OauthAccessToken.class))).thenReturn(null);
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().enableIntegration("123","123","slack","payload",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertFalse(responseEntity.getBody().isSuccess());
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.UNABLE_TO_UPDATE_INTEGRATION.value(),responseEntity.getBody().getErrorMessage());
        }
    }

    @Test
    void enableIntegration_emptyResponseMap_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedStatic<IntegrationService> integrationServiceMockedStatic = mockStatic(IntegrationService.class);){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(new OauthAccessToken());
            integrationServiceMockedStatic.when(()-> IntegrationService.enableIntegration(eq("123"),eq("123"),eq("slack"),eq("payload"),any(OauthAccessToken.class))).thenReturn(new HashMap<>());
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().enableIntegration("123","123","slack","payload",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertFalse(responseEntity.getBody().isSuccess());
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.UNABLE_TO_UPDATE_INTEGRATION.value(),responseEntity.getBody().getErrorMessage());
        }
    }

    @Test
    void enableIntegration_validResponseMap_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedStatic<IntegrationService> integrationServiceMockedStatic = mockStatic(IntegrationService.class);){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(new OauthAccessToken());
            integrationServiceMockedStatic.when(()-> IntegrationService.enableIntegration(eq("123"),eq("123"),eq("slack"),eq("payload"),any(OauthAccessToken.class))).thenReturn(new HashMap(){{put("integration","integration");}});
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().enableIntegration("123","123","slack","payload",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertTrue(responseEntity.getBody().isSuccess());
            Assertions.assertEquals(new HashMap<>(){{put("integration","integration");}},responseEntity.getBody().getData());
        }
    }

    @Test
    void disableIntegration_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Test Error"));
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().disableIntegration("123","123","slack",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertFalse(responseEntity.getBody().isSuccess());
            Assertions.assertEquals("Test Error",responseEntity.getBody().getErrorMessage());
        }
    }

    @Test
    void disableIntegration_nullResponseMap_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedStatic<IntegrationService> integrationServiceMockedStatic = mockStatic(IntegrationService.class);){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(new OauthAccessToken());
            integrationServiceMockedStatic.when(()-> IntegrationService.disableIntegration(eq("123"),eq("123"),eq("slack"),any(OauthAccessToken.class))).thenReturn(null);
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().disableIntegration("123","123","slack",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertFalse(responseEntity.getBody().isSuccess());
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.UNABLE_TO_UPDATE_INTEGRATION.value(),responseEntity.getBody().getErrorMessage());
        }
    }

    @Test
    void disableIntegration_emptyResponseMap_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedStatic<IntegrationService> integrationServiceMockedStatic = mockStatic(IntegrationService.class);){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(new OauthAccessToken());
            integrationServiceMockedStatic.when(()-> IntegrationService.disableIntegration(eq("123"),eq("123"),eq("slack"),any(OauthAccessToken.class))).thenReturn(new HashMap<>());
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().disableIntegration("123","123","slack",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertFalse(responseEntity.getBody().isSuccess());
            Assertions.assertEquals(INTEGRATION_ERROR_RESPONSE.UNABLE_TO_UPDATE_INTEGRATION.value(),responseEntity.getBody().getErrorMessage());
        }
    }

    @Test
    void disableIntegration_validResponseMap_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedStatic<IntegrationService> integrationServiceMockedStatic = mockStatic(IntegrationService.class);){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(new OauthAccessToken());
            integrationServiceMockedStatic.when(()-> IntegrationService.disableIntegration(eq("123"),eq("123"),eq("slack"),any(OauthAccessToken.class))).thenReturn(new HashMap(){{put("integration","integration");}});
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().disableIntegration("123","123","slack",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertTrue(responseEntity.getBody().isSuccess());
            Assertions.assertEquals(new HashMap<>(){{put("integration","integration");}},responseEntity.getBody().getData());
        }
    }

    @Test
    void getIntegration_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Test Error"));
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().getIntegration("123","123","slack",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertFalse(responseEntity.getBody().isSuccess());
            Assertions.assertEquals("Test Error",responseEntity.getBody().getErrorMessage());
        }
    }

    @Test
    void getIntegration_validResponseMap_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedStatic<IntegrationService> integrationServiceMockedStatic = mockStatic(IntegrationService.class);){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(new OauthAccessToken());
            integrationServiceMockedStatic.when(()-> IntegrationService.getIntegration(eq("123"),eq("123"),eq("slack"),any(OauthAccessToken.class))).thenReturn(new HashMap(){{put("integration","integration");}});
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().getIntegration("123","123","slack",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertTrue(responseEntity.getBody().isSuccess());
            Assertions.assertEquals(new HashMap<>(){{put("integration","integration");}},responseEntity.getBody().getData());
        }
    }

    @Test
    void getAllIntegrations_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Test Error"));
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().getAllIntegrations("123","123",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertFalse(responseEntity.getBody().isSuccess());
            Assertions.assertEquals("Test Error",responseEntity.getBody().getErrorMessage());
        }
    }

    @Test
    void getAllIntegrations_validResponseMap_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedStatic<IntegrationService> integrationServiceMockedStatic = mockStatic(IntegrationService.class);){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(new OauthAccessToken());
            integrationServiceMockedStatic.when(()-> IntegrationService.getAllIntegrations(eq("123"),eq("123"),any(OauthAccessToken.class))).thenReturn(new HashMap(){{put("integration","integration");}});
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().getAllIntegrations("123","123",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertTrue(responseEntity.getBody().isSuccess());
            Assertions.assertEquals(new HashMap<>(){{put("integration","integration");}},responseEntity.getBody().getData());
        }
    }

    @Test
    void getZendeskInfo_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Test Error"));
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().getZendeskInfo("123",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertFalse(responseEntity.getBody().isSuccess());
            Assertions.assertEquals("Test Error",responseEntity.getBody().getErrorMessage());
        }
    }

    @Test
    void getZendeskInfo_validResponseMap_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedStatic<IntegrationService> integrationServiceMockedStatic = mockStatic(IntegrationService.class);){
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(new OauthAccessToken());
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(new Contact());
            ZendeskInfoResponse zendeskInfoResponse = new ZendeskInfoResponse();
            zendeskInfoResponse.setAccountID("123");
            zendeskInfoResponse.setName("user name");
            zendeskInfoResponse.setContactID("234");
            zendeskInfoResponse.setTimezone("timezone");
            zendeskInfoResponse.setProjects(new ArrayList<>());
            zendeskInfoResponse.setEntries(new HashMap<>());
            integrationServiceMockedStatic.when(()-> IntegrationService.getZendeskInfo(eq("123"),any(OauthAccessToken.class), any(Contact.class))).thenReturn(zendeskInfoResponse);
            ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().getZendeskInfo("123",new MockHttpServletRequest());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertTrue(responseEntity.getBody().isSuccess());
            Assertions.assertEquals("123",responseEntity.getBody().getData().get("accountID"));
            Assertions.assertEquals("234",responseEntity.getBody().getData().get("contactID"));
            Assertions.assertEquals("user name",responseEntity.getBody().getData().get("name"));
            Assertions.assertEquals("timezone",responseEntity.getBody().getData().get("timezone"));
            Assertions.assertTrue(((ArrayList)responseEntity.getBody().getData().get("projects")).isEmpty());
            Assertions.assertTrue(((HashMap)responseEntity.getBody().getData().get("entries")).isEmpty());
        }
    }

    @Test
    void saveJiraInfo_exception_test() {
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()->IntegrationService.saveJiraInfo(anyString())).thenThrow(new IllegalArgumentException("Exception"));
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().saveJiraInfo("payload");
            var genericResponse = new GenericResponse(false, null, "Exception");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void saveJiraInfo_valid_test() {
       try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
           ResponseEntity<GenericResponse> responseEntity = new IntegrationsApi().saveJiraInfo("payload");
           Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
           Assertions.assertTrue(responseEntity.getBody().isSuccess());
       }
    }

    @Test
    void getJiraInfo_service_method_exception_test(){
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()->IntegrationService.getJiraInfo(anyString())).thenThrow(new IllegalArgumentException("Exception"));
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().getJiraInfo("contactID");
            var genericResponse = new GenericResponse(false, null, "Exception");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }


    @Test
    void getJiraInfo_emptyResp_test(){
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()->IntegrationService.getJiraInfo(anyString())).thenReturn(new HashMap<>());
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().getJiraInfo("contactID");
            Assertions.assertFalse(resp.getBody().isSuccess());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void getJiraInfo_validResp_test(){
        HashMap<String, Object> responseMap = new HashMap<>();
        responseMap.put("info", "jira");
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()->IntegrationService.getJiraInfo(anyString())).thenReturn(responseMap);
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().getJiraInfo("contactID");
            Assertions.assertTrue(resp.getBody().isSuccess());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void accessJiraTenantInfo_service_method_exception_test(){
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
           integrationServiceMockedStatic.when(()-> IntegrationService.getJiraTenantInfo(anyString())).thenThrow(new IllegalArgumentException("Exception"));
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().accessJiraTenantInfo("contactID");
            var genericResponse = new GenericResponse(false, null, "Exception");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }


    @Test
    void accessJiraTenantInfo_emptyResp_test(){
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()->IntegrationService.getJiraTenantInfo(anyString())).thenReturn(new HashMap<>());
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().accessJiraTenantInfo("payload");
            Assertions.assertFalse(resp.getBody().isSuccess());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void accessJiraTenantInfo_validResp_test(){
        HashMap<String, Object> responseMap = new HashMap<>();
        responseMap.put("info", "jira");
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()->IntegrationService.getJiraTenantInfo(anyString())).thenReturn(responseMap);
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().accessJiraTenantInfo("payload");
            Assertions.assertTrue(resp.getBody().isSuccess());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void jiraCardInfo_service_method_exception_test(){
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()-> IntegrationService.getJiraCardInfo(anyString())).thenThrow(new IllegalArgumentException("Exception"));
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().jiraCardInfo("contactID");
            var genericResponse = new GenericResponse(false, null, "Exception");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }


    @Test
    void jiraCardInfo_emptyResp_test(){
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()->IntegrationService.getJiraCardInfo(anyString())).thenReturn(new HashMap<>());
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().jiraCardInfo("payload");
            Assertions.assertFalse(resp.getBody().isSuccess());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void jiraCardInfo_validResp_test(){
        HashMap<String, Object> responseMap = new HashMap<>();
        responseMap.put("info", "jira");
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()->IntegrationService.getJiraCardInfo(anyString())).thenReturn(responseMap);
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().jiraCardInfo("payload");
            Assertions.assertTrue(resp.getBody().isSuccess());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void updateAccessToken_service_method_exception_test(){
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()-> IntegrationService.updateNewToken(anyString())).thenThrow(new IllegalArgumentException("Exception"));
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().updateAccessToken("contactID");
            var genericResponse = new GenericResponse(false, null, "Exception");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }


    @Test
    void updateAccessToken_emptyResp_test(){
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()->IntegrationService.updateNewToken(anyString())).thenReturn(new HashMap<>());
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().updateAccessToken("payload");
            Assertions.assertFalse(resp.getBody().isSuccess());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void updateAccessToken_validResp_test(){
        HashMap<String, Object> responseMap = new HashMap<>();
        responseMap.put("info", "jira");
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()->IntegrationService.updateNewToken(anyString())).thenReturn(responseMap);
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().updateAccessToken("payload");
            Assertions.assertTrue(resp.getBody().isSuccess());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void deleteJiraInfo_service_method_exception_test(){
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()-> IntegrationService.deleteJiraInfo(anyString(),anyString())).thenThrow(new IllegalArgumentException("Exception"));
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().deleteJiraInfo("accountID", "contactID");
            var genericResponse = new GenericResponse(false, null, "Exception");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }


    @Test
    void deleteJiraInfo_success_false_test(){
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()->IntegrationService.deleteJiraInfo(anyString(), anyString())).thenReturn(false);
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().deleteJiraInfo("accountID", "contactID");
            Assertions.assertFalse(resp.getBody().isSuccess());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void deleteJiraInfo_true_test(){
        try(MockedStatic<IntegrationService> integrationServiceMockedStatic = Mockito.mockStatic(IntegrationService.class)){
            integrationServiceMockedStatic.when(()->IntegrationService.deleteJiraInfo(anyString(), anyString())).thenReturn(true);
            ResponseEntity<GenericResponse> resp = new IntegrationsApi().deleteJiraInfo("accountID", "contactID");
            Assertions.assertTrue(resp.getBody().isSuccess());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

}
