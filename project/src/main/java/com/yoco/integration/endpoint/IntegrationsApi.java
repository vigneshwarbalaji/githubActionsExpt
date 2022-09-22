package com.yoco.integration.endpoint;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.integration.enums.INTEGRATION_ERROR_RESPONSE;
import com.yoco.integration.service.IntegrationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;
import javax.websocket.server.PathParam;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class IntegrationsApi {

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PostMapping("/account/{accountID}/contact/{contactID}/integration/{integrationType}/enable")
    public ResponseEntity<GenericResponse> enableIntegration (@PathVariable("accountID") String  accountID,@PathVariable("contactID") String  contactID, @PathVariable("integrationType") String  integrationType, @RequestBody(required=false) String payload, HttpServletRequest req){
        var genericResponse = new GenericResponse();
        try {
            OauthAccessToken accessToken = AnnotationHelper.extractCurrentUserAccessTokenFromRequest(req);
            Map<String, Object> responseMap =  IntegrationService.enableIntegration(accountID, contactID, integrationType, payload, accessToken);
            if(ObjUtils.isNullOrEmpty(responseMap)){
                genericResponse.setErrorMessage(INTEGRATION_ERROR_RESPONSE.UNABLE_TO_UPDATE_INTEGRATION.value());
                genericResponse.setSuccess(false);
            } else {
                genericResponse.setData(responseMap);
                genericResponse.setSuccess(true);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info( "error on saving integration: "+e.getMessage());
            genericResponse.setSuccess(false);
            genericResponse.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping("/account/{accountID}/contact/{contactID}/integration/{integrationType}/disable")
    public ResponseEntity<GenericResponse> disableIntegration (@PathVariable("accountID") String  accountID,@PathVariable("contactID") String  contactID, @PathVariable("integrationType") String  integrationType, HttpServletRequest req){
        var genericResponse = new GenericResponse();
        try {
            OauthAccessToken accessToken = AnnotationHelper.extractCurrentUserAccessTokenFromRequest(req);
            Map<String, Object> responseMap =  IntegrationService.disableIntegration(accountID, contactID,integrationType, accessToken);
            if(ObjUtils.isNullOrEmpty(responseMap)){
                genericResponse.setSuccess(false);
                genericResponse.setErrorMessage(INTEGRATION_ERROR_RESPONSE.UNABLE_TO_UPDATE_INTEGRATION.value());
            } else {
                genericResponse.setSuccess(true);
                genericResponse.setData(responseMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info( "error on disabling integration: "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/contact/{contactID}/integration")
    public ResponseEntity<GenericResponse> getAllIntegrations (@PathVariable("accountID") String  accountID,@PathVariable("contactID") String  contactID, HttpServletRequest req){
        var genericResponse = new GenericResponse();
        try {
            OauthAccessToken accessToken = AnnotationHelper.extractCurrentUserAccessTokenFromRequest(req);
            genericResponse.setSuccess(true);
            genericResponse.setData(IntegrationService.getAllIntegrations(accountID, contactID, accessToken));
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info( "error on fetching integration: "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/contact/{contactID}/integration/{integrationType}")
    public ResponseEntity<GenericResponse> getIntegration (@PathVariable("accountID") String  accountID,@PathVariable("contactID") String  contactID, @PathVariable("integrationType") String  integrationType, HttpServletRequest req){
        var genericResponse = new GenericResponse();
        try {
            OauthAccessToken accessToken = AnnotationHelper.extractCurrentUserAccessTokenFromRequest(req);
            genericResponse.setSuccess(true);
            genericResponse.setData(IntegrationService.getIntegration(accountID, contactID,integrationType, accessToken));
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info( "error on fetching integration: "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/account/default/zendesk/info")
    public ResponseEntity<GenericResponse> getZendeskInfo (@RequestParam(defaultValue = "", name = "taskID") String taskID,HttpServletRequest req) {
        var genericResponse = new GenericResponse();
        try {
            var accessToken = AnnotationHelper.extractCurrentUserAccessTokenFromRequest(req);
            var userContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            var zendeskInfoResponse = IntegrationService.getZendeskInfo( taskID,accessToken,userContact);
            genericResponse.setSuccess( true );
            genericResponse.add("name", zendeskInfoResponse.getName());
            genericResponse.add("accountID", zendeskInfoResponse.getAccountID());
            genericResponse.add("contactID", zendeskInfoResponse.getContactID());
            genericResponse.add("projects", zendeskInfoResponse.getProjects());
            genericResponse.add("entries", zendeskInfoResponse.getEntries());
            genericResponse.add("timezone", zendeskInfoResponse.getTimezone());
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info( "error on fetching zendesk info: "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PostMapping("/jira")
    public ResponseEntity<GenericResponse> saveJiraInfo (@RequestBody String payload) {

        var genericResponse = new GenericResponse();
        try {
            IntegrationService.saveJiraInfo( payload );
            genericResponse.setSuccess( true );
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info("exception on creating jira integration " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/jira/contact/{contactID}")
    public ResponseEntity<GenericResponse> getJiraInfo ( @PathVariable("contactID") String contactID ) {

        var genericResponse = new GenericResponse();
        try {
            Map<String, Object> responseMap = IntegrationService.getJiraInfo(contactID);

            if(ObjUtils.isNullOrEmpty(responseMap)) {
                genericResponse.setSuccess(false);
            } else {
                genericResponse.setSuccess( true );
                genericResponse.add("info", responseMap.get("info"));
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info(" exception on getting the jira info " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PostMapping("/jira/tenantInfo") // tenant is the jira term where they consider each one as their tenant
    public ResponseEntity<GenericResponse> accessJiraTenantInfo ( @RequestBody String payload) { // this is mapped as post instead of get is because
                                                                                // we will be sending the access token so sending it in payload rather than query param
        GenericResponse genericResponse = new GenericResponse();
        try {
            Map<String, Object> responseMap = IntegrationService.getJiraTenantInfo( payload );
            if(!ObjUtils.isNullOrEmpty(responseMap)) {
                genericResponse.setSuccess( true );
                genericResponse.setData(responseMap);
            } else {
                genericResponse.setSuccess(false);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info("exception caused in accessJiraTenantInfo " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PostMapping("/jira/cardInfo")
    public ResponseEntity<GenericResponse> jiraCardInfo ( @RequestBody String payload) {
        GenericResponse genericResponse = new GenericResponse();
        try {
            Map<String, Object> responseMap = IntegrationService.getJiraCardInfo( payload );
            if(ObjUtils.isNullOrEmpty(responseMap)) {
                genericResponse.setSuccess(false);
            } else {
                genericResponse.setData(responseMap);
                genericResponse.setSuccess( true );
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            genericResponse = new GenericResponse(false, null, e.getMessage());
            log.info("exception caused in jiraCardInfo " + e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping("/jira/token")
    public ResponseEntity<GenericResponse> updateAccessToken (@RequestBody String payload) {
        GenericResponse genericResponse = new GenericResponse();
        try {
            Map<String, Object> responseMap = IntegrationService.updateNewToken( payload );

            if(ObjUtils.isNullOrEmpty(responseMap)) {
                genericResponse.setSuccess(false);
            } else {
                genericResponse.setSuccess( true );
                genericResponse.setData(responseMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info("exception caused in updateAccessToken "  +e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS})
    @DeleteMapping("/account/{accountID}/contact/{contactID}")
    public ResponseEntity<GenericResponse> deleteJiraInfo (@PathParam("accountID") String accountID,
                                            @PathParam("contactID") String contactID) {
        GenericResponse genericResponse = new GenericResponse();
        try {
            boolean deleted = IntegrationService.deleteJiraInfo( accountID, contactID );
            genericResponse.setSuccess(deleted);
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info("exception caused in delete jira api " + e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

}
