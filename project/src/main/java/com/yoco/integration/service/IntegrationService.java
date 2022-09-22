package com.yoco.integration.service;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.SchedulingEngineUrlConstants;
import com.yoco.commons.dataservices.impl.IntegrationsImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.IntegrationsJDO;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.*;
import com.yoco.commons.validations.Validator;
import com.yoco.integration.enums.INTEGRATION_ERROR_RESPONSE;
import com.yoco.integration.helper.IntegrationHelper;
import com.yoco.integration.modal.IntegrationsEntityResponse;
import com.yoco.integration.modal.ZendeskInfoResponse;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.concurrent.TimeoutException;

import static java.util.stream.Collectors.toList;

@Slf4j
public class IntegrationService {
    public static final String INTEGRATION_KEY = "integration";
    public static final String INTEGRATIONS_KEY = "integrations";
    public static final String JIRA = "jira";
    public static final String TOKEN = "token";

    private IntegrationService(){}
    public static Map<String,Object> enableIntegration(String accountID, String userContactID, String integrationType, String payload, OauthAccessToken requesterAccessToken){
        Map<String,Object> responseMap = new HashMap<>();
        Validator.checkArgument(ObjUtils.isNullOrEmpty(integrationType),INTEGRATION_ERROR_RESPONSE.INVALID_INTEGRATION_TYPE.value());
        Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payloadMap),COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        Validator.checkArgument(!IntegrationHelper.isAccessAllowed(requesterAccessToken,userContactID), INTEGRATION_ERROR_RESPONSE.ACCESS_DENIED.value());
        PeopleRelationJDO userPro = UserPROUtil.validateAndExtractUserPRO(accountID, userContactID);
        Validator.checkArgument(ObjUtils.isNull(userPro), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());

        var integrationsImpl = IntegrationsImpl.getIntegrationsImplInstance();
        IntegrationsJDO userIntegration = integrationsImpl.getIntegrationByType(userPro.getUniquepin(),userPro.getContactId(),integrationType);
        String newIntegrationDetails = IntegrationHelper.extractIntegrationDetails(payloadMap);
        if(ObjUtils.isNull(userIntegration)){
            userIntegration = IntegrationHelper.createIntegration(integrationsImpl, userPro, newIntegrationDetails, integrationType);
        }else{
            userIntegration = IntegrationHelper.updateIntegration(integrationsImpl,userIntegration,newIntegrationDetails);
        }
        if(!ObjUtils.isNull(userIntegration)){
            responseMap.put(INTEGRATION_KEY,new IntegrationsEntityResponse(userIntegration));
        }
        return responseMap;
    }

    public static Map<String,Object> disableIntegration(String accountID, String userContactID, String integrationType, OauthAccessToken requesterAccessToken){
        Map<String,Object> responseMap = new HashMap<>();
        Validator.checkArgument(ObjUtils.isNullOrEmpty(integrationType),INTEGRATION_ERROR_RESPONSE.INVALID_INTEGRATION_TYPE.value());

        Validator.checkArgument(!IntegrationHelper.isAccessAllowed(requesterAccessToken,userContactID), INTEGRATION_ERROR_RESPONSE.ACCESS_DENIED.value());

        PeopleRelationJDO userPro = UserPROUtil.validateAndExtractUserPRO(accountID, userContactID);
        Validator.checkArgument(ObjUtils.isNull(userPro), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        var integrationsImpl = IntegrationsImpl.getIntegrationsImplInstance();
        IntegrationsJDO userIntegration = integrationsImpl.getIntegrationByType(userPro.getUniquepin(),userPro.getContactId(),integrationType);
        userIntegration = IntegrationHelper.disableIntegration(integrationsImpl,userIntegration);
        if(!ObjUtils.isNull(userIntegration)){
            responseMap.put(INTEGRATION_KEY,new IntegrationsEntityResponse(userIntegration));
        }
        return responseMap;
    }

    public static Map<String,Object> getIntegration(String accountID, String userContactID, String integrationType, OauthAccessToken requesterAccessToken){
        Map<String,Object> responseMap = new HashMap<>();
        Validator.checkArgument(ObjUtils.isNullOrEmpty(integrationType),INTEGRATION_ERROR_RESPONSE.INVALID_INTEGRATION_TYPE.value());
        Validator.checkArgument(!IntegrationHelper.isAccessAllowed(requesterAccessToken,userContactID), INTEGRATION_ERROR_RESPONSE.ACCESS_DENIED.value());
        PeopleRelationJDO userPro = UserPROUtil.validateAndExtractUserPRO(accountID, userContactID);
        Validator.checkArgument(ObjUtils.isNull(userPro), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        var integrationsImpl = IntegrationsImpl.getIntegrationsImplInstance();
        IntegrationsJDO userIntegration = integrationsImpl.getIntegrationByType(userPro.getUniquepin(),userPro.getContactId(),integrationType);
        responseMap.put(INTEGRATION_KEY, ObjUtils.isNull(userIntegration) ? null : new IntegrationsEntityResponse(userIntegration));
        return responseMap;
    }

    public static Map<String,Object> getAllIntegrations(String accountID, String userContactID, OauthAccessToken requesterAccessToken){
        Map<String,Object> responseMap = new HashMap<>();
        Validator.checkArgument(!IntegrationHelper.isAccessAllowed(requesterAccessToken,userContactID), INTEGRATION_ERROR_RESPONSE.ACCESS_DENIED.value());
        PeopleRelationJDO userPro = UserPROUtil.validateAndExtractUserPRO(accountID, userContactID);
        Validator.checkArgument(ObjUtils.isNull(userPro), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        var integrationsImpl = IntegrationsImpl.getIntegrationsImplInstance();
        List<IntegrationsJDO> userIntegrations = integrationsImpl.getAllIntegrations(userPro.getUniquepin(),userPro.getContactId());
        responseMap.put(INTEGRATIONS_KEY, ObjUtils.isNullOrEmpty(userIntegrations) ? new ArrayList<>() : userIntegrations.stream().map(IntegrationsEntityResponse::new).collect(toList()));
        return responseMap;
    }

    public static ZendeskInfoResponse getZendeskInfo(String taskId, OauthAccessToken requesterAccessToken, Contact userContact) throws NoSuchAlgorithmException, IOException, TimeoutException {
        Validator.checkArgument(ObjUtils.isNullOrEmpty(taskId), INTEGRATION_ERROR_RESPONSE.INVALID_TASKID.value());
        Validator.checkArgument(!IntegrationHelper.isAccessAllowed(requesterAccessToken,userContact.getId()), INTEGRATION_ERROR_RESPONSE.ACCESS_DENIED.value());
        PeopleRelationJDO userPro = UserPROUtil.validateAndExtractDefaultUserPRO(requesterAccessToken.getUserId());
        Validator.checkArgument(ObjUtils.isNull(userPro), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        userPro.setContact(userContact);
        List<ProjectJDO> userProjects = ProjectUtil.getAllProjectsForUserInAnAccount(userPro.getUniquepin(),userPro.getContactId());
        List<Map<String,Object>> userEntriesList = ReportImpl.getReportImplInstance().getUserEntriesByTaskID(userPro.getUniquepin(),userPro.getContactId(), taskId);
        Map<String,Object> entriesMap = IntegrationHelper.getEntriesMapForTaskID(userEntriesList, userPro.getTimeZone());
        return new ZendeskInfoResponse(userPro,userProjects,entriesMap);
    }

    public static IntegrationsJDO saveJiraInfo( String payload ) {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString());

        HashMap<String, Object> payloadMap = (HashMap<String, Object>) JsonUtil.convertJsonToMap(payload);
        String emailID = payloadMap.containsKey("emailID") ? payloadMap.get("emailID").toString() : "";

        Validator.checkArgument(ObjUtils.isNullOrEmpty(emailID), COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.toString());
        HashMap<String, Object> info = payloadMap.containsKey("info") ? (HashMap<String, Object>) payloadMap.get("info") : new HashMap<>();

        PeopleRelationJDO userPRO = new UserImpl().getDefaultUserProWithEmail(emailID);

        IntegrationsJDO objJira = null;

        if(!ObjUtils.isNull(userPRO)) {
            objJira = new IntegrationsImpl().getIntegrationByType(userPRO.getUniquepin(), userPRO.getContactId(), JIRA);
            if(ObjUtils.isNull(objJira)){
                objJira =  new IntegrationsJDO(userPRO.getUniquepin(), userPRO.getContactId(), JIRA , JsonUtil.getJson(info), true );
            } else {
                objJira.setIntegrationDetails(JsonUtil.getJson(info));
                objJira.setDateAddedLongTime(DateUtil.getCurrentTime());
            }
            new IntegrationsImpl().saveIntegration(objJira);
        }
        return objJira;
    }

    public static Map<String, Object> getJiraInfo( String contactID ) {

        PeopleRelationJDO userPRO = new UserImpl().getDefaultUserPro(contactID);
        Validator.checkArgument(ObjUtils.isNull(userPRO), COMMON_ERROR_RESPONSE.USER_NOT_FOUND.toString());

        IntegrationsJDO objJira = new IntegrationsImpl().getIntegrationByType(userPRO.getUniquepin(), userPRO.getContactId(), JIRA);
        HashMap<String, Object> response = new HashMap<>();
        if(!ObjUtils.isNull(objJira)){
            response.put("info", objJira);
        }
        return response;
    }

    public static Map<String, Object> getJiraTenantInfo( String payload ) throws IOException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString());
        HashMap<String, Object> payloadMap = (HashMap<String, Object>) JsonUtil.convertJsonToMap(payload);
        String accessToken = payloadMap.containsKey(TOKEN)? payloadMap.get(TOKEN).toString() : "";
        Validator.checkArgument(ObjUtils.isNullOrEmpty(accessToken), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString());

        String url = CommonAppProperties.getJiraUrl();
        log.info(url);

        var headers = HeaderUtil.getContentTypedAccessTokenAuthHeader(accessToken);
        return UrlFetcher.sendGetRequest(url,headers, UrlFetcher.getHttpClientInstance());
    }

    public static Map<String, Object> getJiraCardInfo( String payload ) throws IOException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString());
        HashMap<String, Object> payloadMap = (HashMap<String, Object>) JsonUtil.convertJsonToMap(payload);
        String accessToken = payloadMap.containsKey(TOKEN)? payloadMap.get(TOKEN).toString() : "";
        Validator.checkArgument(ObjUtils.isNullOrEmpty(accessToken), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString());
        String jiraDomainID = payloadMap.containsKey("jiraID")? payloadMap.get("jiraID").toString() : "";
        Validator.checkArgument(ObjUtils.isNullOrEmpty(jiraDomainID), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString());
        String cardID = payloadMap.containsKey("cardID")? payloadMap.get("cardID").toString() : "";
        Validator.checkArgument(ObjUtils.isNullOrEmpty(cardID), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString());


        String url = "https://api.atlassian.com/ex/jira/"+ jiraDomainID +"/rest/api/3/issue/" + cardID;
        log.info(url);
        var headers = HeaderUtil.getContentTypedAccessTokenAuthHeader(accessToken);
        return UrlFetcher.sendGetRequest(url,headers, UrlFetcher.getHttpClientInstance());
    }

    public static Map<String, Object> updateNewToken( String payload ) {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString());
        HashMap<String, Object> payloadMap = (HashMap<String, Object>) JsonUtil.convertJsonToMap(payload);
        String accessToken = payloadMap.containsKey("accessToken")? payloadMap.get("accessToken").toString() : "";
        Validator.checkArgument(ObjUtils.isNullOrEmpty(accessToken), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString());
        String accountID = payloadMap.containsKey("accountID")? payloadMap.get("accountID").toString() : "";
        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.toString());
        String contactID = payloadMap.containsKey("contactID")? payloadMap.get("contactID").toString() : "";
        Validator.checkArgument(ObjUtils.isNullOrEmpty(contactID), COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.toString());
        String refreshToken = payloadMap.containsKey("refreshToken")? payloadMap.get("refreshToken").toString() : "";
        Validator.checkArgument(ObjUtils.isNullOrEmpty(refreshToken), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.toString());

        PeopleRelationJDO userPRO = new UserImpl().getUserWithoutContact(accountID, contactID);
        Validator.checkArgument(ObjUtils.isNull(userPRO), COMMON_ERROR_RESPONSE.USER_NOT_FOUND.toString());

        IntegrationsJDO jira = new IntegrationsImpl().getIntegrationByType(userPRO.getUniquepin(), userPRO.getContactId(), JIRA);

        if(ObjUtils.isNull(jira)){
            return new HashMap<>();
        } else {

            HashMap<String, Object> infoMap = (HashMap<String, Object>) JsonUtil.convertJsonToMap(jira.getIntegrationDetails());
            infoMap.put("access_token", accessToken);
            infoMap.put("refresh_token", refreshToken);

            jira.setIntegrationDetails(JsonUtil.getJson(infoMap));
            new IntegrationsImpl().saveIntegration(jira);

            HashMap<String, Object> responseMap = new HashMap<>();
            responseMap.put("info", jira);
            return responseMap;
        }

    }

    public static boolean deleteJiraInfo( String accountID, String contactID ) {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.toString());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(contactID), COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.toString());

        IntegrationsJDO jira = new IntegrationsImpl().getIntegrationByType(accountID, contactID, JIRA);

        if(!ObjUtils.isNull(jira)){
            new IntegrationsImpl().delete(jira);
            return true;
        }

        return false;
    }
}
