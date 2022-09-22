package com.yoco.commons.utils;

import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.services.UrlFetcher;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

@Slf4j
public class DcmUtil {

    public static final String TEAMS_KEY = "teams";
    public static final String SKILL_SET_ID = "skillSetID";
    public static final String ACCOUNT_ID_QPARAM = "{$accountID}";
    public static final String CONTACT_ID_QPARAM = "{$contactID}";

    private static final String LOGIN_KEY = "login";
    private static final String BRAND_ID = "brandID";
    private static final String CONTACT_SKILL_SET = "contactSkillSet";
    private static final String ACCOUNT_ID = "accountID";
    private static final String CONTACT_ID = "contactID";

    private DcmUtil(){}

    public static String getDefaultAccountIDForUser(String contactID) throws NoSuchAlgorithmException, IOException {
        String userEmailID = UserPROUtil.getEmailIdForUser(contactID);
        return getDefaultAccountIDWithEmailID(userEmailID);
    }

    public static String getDefaultAccountIDWithEmailID(String userEmailID) throws NoSuchAlgorithmException, IOException {
        Map<String,Object> dcmResponse = getDcmContactSkillsetInformation(userEmailID);
        if(ObjUtils.isNullOrEmpty(dcmResponse)){
            return null;
        }
        ArrayList<HashMap<String,Object>> contactSkillSetList = (ArrayList<HashMap<String,Object>>)dcmResponse.get(CONTACT_SKILL_SET);
        return extractDefaultDomainFromContactSkillSet(contactSkillSetList);
    }

    public static String getActiveDefaultAccountFromDcm(String userEmailID) throws NoSuchAlgorithmException, IOException {
        Map<String,Object> dcmResponse = getDcmContactSkillsetInformation(userEmailID);
        if(ObjUtils.isNullOrEmpty(dcmResponse)){
            return null;
        }
        List<HashMap<String,Object>> resp = (List) dcmResponse.get("skillSet");
        if(ObjUtils.isNullOrEmpty(resp)){
            log.info(" skill set is empty ");
            return null;
        }
        ArrayList<HashMap<String,Object>> contactSkillSetList = (ArrayList<HashMap<String,Object>>)dcmResponse.get(CONTACT_SKILL_SET);
        return extractDefaultDomainFromContactSkillSet(contactSkillSetList);
    }

    public static String extractDefaultDomainFromContactSkillSet(List<HashMap<String, Object>> contactSkillSetList) {
        HashMap<String,Object> defaultAccountSkillSet = ObjUtils.isNull(contactSkillSetList) ? new HashMap<>() :
                 contactSkillSetList.stream()
                .filter(accountSkillSet -> Objects.equals(accountSkillSet.get(SKILL_SET_ID), DcmConstants.DEFAULT_ACCOUNT_SKILLSET_ID))
                .findFirst().orElse(new HashMap<>());
        return ObjUtils.isNullOrEmpty(defaultAccountSkillSet) ? null : (String) defaultAccountSkillSet.get(ACCOUNT_ID);
    }

    public static boolean getIsPasswordPresentForUserEmailID(String emailID) throws NoSuchAlgorithmException, IOException {
        Map<String,Object> dcmResponse = getDcmContactSkillsetInformation(emailID);
        if(ObjUtils.isNullOrEmpty(dcmResponse)){
            return false;
        }
        HashMap<String,Object> contactData = (HashMap<String,Object>)dcmResponse.get(ContactConstants.CONTACT);
        return !ObjUtils.isNullOrEmpty(contactData) && !ObjUtils.isNull(contactData.get(ContactConstants.IS_PASSWORD_PRESENT)) && (boolean) contactData.get(ContactConstants.IS_PASSWORD_PRESENT);
    }

    public static Map<String, Object> getDcmContactSkillsetInformation (String emailID) throws NoSuchAlgorithmException, IOException {
        Map<String, Object> requestBody = new HashMap<>();
        requestBody.put(LOGIN_KEY, emailID );
        return UrlFetcher.sendPostRequest(DcmConstants.getContactSkillsetInfoUrl(),requestBody,HeaderUtil.getServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static Map<String,Object> fetchContactFromDcm(String emailID){
        Map<String, Object> dcmRespContact = null;
        try {
            dcmRespContact = getDcmContact(emailID);
            if(ObjUtils.isNullOrEmpty(dcmRespContact)){  // this condition is added because at time when we hit the dcm api several times the url fetch exceeds the 60sec so re-trying it again
                dcmRespContact = getDcmContact(emailID);
            }
        } catch (NoSuchAlgorithmException|IOException e) {
            log.info(" exception in fetching contactExists info from Dcm " + e.getMessage());
        }
        return dcmRespContact;
    }

    public static Map<String,Object> getDcmContact(String emailID) throws NoSuchAlgorithmException, IOException {
        Map<String, Object> request = new HashMap<>();
        request.put(LOGIN_KEY, emailID);
        request.put(BRAND_ID, DcmConstants.YOCO_BRAND_ID);
        return UrlFetcher.sendPostRequest(DcmConstants.getUserExistenceInDCMUrl(),request,HeaderUtil.getContentTypedServerTokenAuthHeader(),
                UrlFetcher.getHttpClientInstance());
    }

    public static Map<String,Object> linkContactToAccount(String accountID,String contactID) throws NoSuchAlgorithmException, IOException {
        Map<String, Object> requestPayLoad = new HashMap<>();
        requestPayLoad.put(CONTACT_ID, contactID);
        requestPayLoad.put(BRAND_ID, DcmConstants.YOCO_BRAND_ID);
        requestPayLoad.put(SKILL_SET_ID, DcmConstants.YOCO_SKILLSET_ID);
        String url = DcmConstants.getAddExistingContactToDCMUrl().replace(ACCOUNT_ID_QPARAM,accountID);
        return UrlFetcher.sendPutRequest(url,requestPayLoad,HeaderUtil.getContentTypedServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static Map<String,Object> updateDefaultAccountInDcm(String accountID, String contactID) throws NoSuchAlgorithmException, IOException {
        Map<String, Object> requestPayLoad = new HashMap<>();
        requestPayLoad.put(ACCOUNT_ID, accountID);
        requestPayLoad.put(CONTACT_ID, contactID);
        requestPayLoad.put(SKILL_SET_ID, DcmConstants.DEFAULT_ACCOUNT_SKILLSET_ID);
        Map<String,Object> resp =  UrlFetcher.sendPostRequest(DcmConstants.getUpdateDcmDefaultDomainUrl(),requestPayLoad,
                HeaderUtil.getContentTypedServerTokenAuthHeader(), UrlFetcher.getHttpClientInstance());
        log.info(" updated default domain resp : " + resp);
        return resp;
    }

    public static Map<String,Object> createUserInDcm(String accountID,String emailID,String password,String firstName,
                                                     String lastName,String photoID) throws NoSuchAlgorithmException, IOException {
        Map<String, Object> payload = new DcmContactDTO().createDcmContactDTO(emailID,password,firstName,lastName,photoID);
        String url = DcmConstants.getCreateUserUnderDCMAccountUrl().replace(ACCOUNT_ID_QPARAM,accountID);
        return UrlFetcher.sendPostRequest(url,payload,HeaderUtil.getContentTypedServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static Map<String,Object> removeUserFromAccount(String accountID, String contactID) throws NoSuchAlgorithmException, IOException {
        return UrlFetcher.sendDeleteRequest(getRemoveUserFromAccountUrl(accountID,contactID),HeaderUtil.getJsonUtf8ContentTypedServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static String getRemoveUserFromAccountUrl(String accountID, String contactID){
        return DcmConstants.getRemoveUserFromAccountUrl().replace(ACCOUNT_ID_QPARAM,accountID).replace(CONTACT_ID_QPARAM,contactID);
    }

    public static Map<String,Object> updateContactInDcm(String accountID, Map<String,Object> payloadMap) throws NoSuchAlgorithmException, IOException {
        String updateUrl = DcmConstants.getUpdateDcmContactUrl().replace(ACCOUNT_ID_QPARAM, accountID);
        return UrlFetcher.sendPutRequest(updateUrl,payloadMap,HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static Map<String,Object> updateDcmAccount(String accountID, Map<String,Object> payloadMap) throws NoSuchAlgorithmException, IOException {
        String updateUrl = DcmConstants.getUpdateDcmAccountUrl().replace(ACCOUNT_ID_QPARAM, accountID);
        return UrlFetcher.sendPutRequest(updateUrl,payloadMap,HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(),UrlFetcher.getHttpClientInstance());
    }

}