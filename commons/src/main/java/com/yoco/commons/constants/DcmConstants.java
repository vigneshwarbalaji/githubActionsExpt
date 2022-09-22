package com.yoco.commons.constants;

import com.yoco.commons.enums.AppMode;
import com.yoco.commons.utils.GaeUtils;

public class DcmConstants {
    private DcmConstants(){}

    public static final AppMode APP_MODE = GaeUtils.getAppMode();
    public static final String DEFAULT_ACCOUNT_SKILLSET_ID = "98223b25-b41c-4c7e-bb91-569003f4cc45";
    public static final String YOCO_BRAND_ID = "d56194e1-b98b-4068-86f8-d442777d2a16";
    public static final String YOCO_SKILLSET_ID = "459b5f5d-7010-410a-8e93-a56ef9a67ce4";
    public static final String API_KEY = "SEN42";
    private static String baseUrl = "";
    private static String contactSkillsetInfoUrl = "";
    private static String fetchTeamsForAccountUrl = "";
    private static String fetchTeamsForUserUrl = "";
    private static String fetchTeamInfoUrl = "";
    private static String userExistenceInDCMUrl = "";
    private static String addExistingContactToDCMUrl = "";
    private static String updateDcmDefaultDomainUrl = "";
    private static String createUserUnderDCMAccountUrl = "";
    private static String createTeamUrl = "";
    private static String initResetPasswordUrl = "";
    private static String updateTeamContactsUrl = "";
    private static String removeUserFromAccountUrl = "";
    private static String validateVerificationIdUrl = "";
    private static String updatePasswordUrl = "";
    private static String resetPasswordUrl = "";
    private static String fetchProfileImageUrl = "";
    private static String updateProfileImageUrl = "";
    private static String updateDcmContactUrl = "";
    private static String updateDcmAccountUrl = "";

    static{
        initializeDcmConstants(APP_MODE);
    }

    public static void initializeDcmConstants(AppMode appMode){
        if(AppMode.LIVE.equals(appMode)){
            setupDcmConstantsForLive();
        }else{
            setupDcmConstantsForStaging();
        }
        setupDcmUrls();
    }

    public static void setupDcmConstantsForLive(){
        baseUrl = "https://contacts.anywhere.co";
    }

    public static void setupDcmConstantsForStaging(){
        baseUrl = "https://staging.contacts.anywhere.co";
    }

    public static void setupDcmUrls(){
        contactSkillsetInfoUrl = baseUrl + "/services/signin/v2.0/Account/authenticate_v2?apikey=SEN42";
        String teamsContactUrl = baseUrl + "/services/data/v3.0/objects/Contact/Group";
        String teamsUrl = baseUrl + "/services/data/v3.0/objects/Group";
        createTeamUrl = teamsUrl + "?apikey={$accountID}&productID=" + YOCO_BRAND_ID;
        fetchTeamsForAccountUrl = teamsContactUrl + "?apikey={$accountID}&type=team&limit={$limit}&productID=" + YOCO_BRAND_ID;
        fetchTeamsForUserUrl = teamsContactUrl + "?apikey={$accountID}&type=team&productID=" + YOCO_BRAND_ID;
        fetchTeamInfoUrl = teamsUrl + "/{$teamID}?apikey={$accountID}&productID=" + YOCO_BRAND_ID;
        updateTeamContactsUrl = teamsUrl + "/{$teamID}/Contact?apikey={$accountID}&operation={$operation}&productID=" + YOCO_BRAND_ID;
        userExistenceInDCMUrl = baseUrl + "/services/data/v2.0/objects/Account/checkIfUserExists?apikey=" + API_KEY;
        addExistingContactToDCMUrl = baseUrl + "/services/data/v2.0/objects/Account/{$accountID}/linkContactToAccount/?apikey=" + API_KEY;
        updateDcmDefaultDomainUrl = baseUrl + "/services/data/v2.0/objects/Skills/goClockDefaultAccount?apikey=" + API_KEY;
        createUserUnderDCMAccountUrl = baseUrl + "/services/data/v2.0/objects/Account/{$accountID}/createUser?apikey=" + API_KEY;
        initResetPasswordUrl = baseUrl + "/services/data/v2.0/objects/Account/{$contactID}/initiateResetPassword?apikey=" + API_KEY;
        removeUserFromAccountUrl = baseUrl + "/services/data/v2.0/objects/Account/{$accountID}/User/{$contactID}/remove?apikey="
                + API_KEY + "&productID=" + YOCO_BRAND_ID;
        validateVerificationIdUrl = baseUrl + "/services/data/v2.0/objects/Account/{$verificationID}/checkIfVerificationValid?apikey=" + API_KEY;
        updatePasswordUrl = baseUrl + "/services/data/v2.0/objects/Account/{$accountID}/updatePassword?apikey=" + API_KEY;
        resetPasswordUrl = baseUrl + "/services/data/v2.0/objects/Account/{$verificationID}/resetPassword?apikey=" + API_KEY;
        fetchProfileImageUrl = baseUrl + "/services/data/v2.0/objects/Contact/getProfilePicUrl?apikey=" + API_KEY;
        updateProfileImageUrl = baseUrl + "/services/data/v2.0/objects/Contact/profilePicture?apikey=" + API_KEY;
        updateDcmContactUrl = baseUrl + "/services/data/v2.0/objects/Account/{$accountID}/updateUser?apikey=" + API_KEY;
        updateDcmAccountUrl = baseUrl + "/services/data/v2.0/objects/Account/{$accountID}?apikey=" + API_KEY;
    }

    public static String getContactSkillsetInfoUrl() { return contactSkillsetInfoUrl; }

    public static String getFetchTeamsForAccountUrl() { return fetchTeamsForAccountUrl; }

    public static String getUserExistenceInDCMUrl() {
        return userExistenceInDCMUrl;
    }

    public static String getAddExistingContactToDCMUrl() {
        return addExistingContactToDCMUrl;
    }

    public static String getUpdateDcmDefaultDomainUrl() {
        return updateDcmDefaultDomainUrl;
    }

    public static String getCreateUserUnderDCMAccountUrl() {
        return createUserUnderDCMAccountUrl;
    }

    public static String getFetchTeamsForUserUrl() { return fetchTeamsForUserUrl; }

    public static String getFetchTeamInfoUrl() { return fetchTeamInfoUrl; }

    public static String getCreateTeamUrl() { return createTeamUrl; }

    public static String getInitResetPasswordUrl() {
        return initResetPasswordUrl;
    }

    public static String getUpdateTeamContactsUrl(){ return updateTeamContactsUrl; }

    public static String getRemoveUserFromAccountUrl(){ return removeUserFromAccountUrl; }

    public static String getValidateVerificationIdUrl() {
        return validateVerificationIdUrl;
    }

    public static String getUpdatePasswordUrl() {
        return updatePasswordUrl;
    }

    public static String getResetPasswordUrl() {
        return resetPasswordUrl;
    }

    public static String getFetchProfileImageUrl() {
        return fetchProfileImageUrl;
    }

    public static String getUpdateProfileImageUrl() {
        return updateProfileImageUrl;
    }

    public static String getUpdateDcmContactUrl() {
        return updateDcmContactUrl;
    }

    public static String getUpdateDcmAccountUrl() { return updateDcmAccountUrl;}
}

