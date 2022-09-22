package com.yoco.account.service;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.account.helper.*;
import com.yoco.account.modal.AccountCreationPayloadDTO;
import com.yoco.account.modal.AccountUpdatePayloadDTO;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.account.AccountDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.*;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.CommonConstants;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import static com.yoco.account.helper.OneTapHelper.CLIENT_ID_KEY;
import static com.yoco.account.helper.OneTapHelper.CREDENTIAL_KEY;
import static com.yoco.account.modal.AccountCreationPayloadDTO.EMAIL_KEY;

public class AccountService {
    public static final String SSO_URL_KEY = "ssoUrl";
    public AccountDTO getAccount(String accountID, String requesterContactID){
        Validator.checkArgument(ObjUtils.isNull(UserPROUtil.validateAndExtractUserPRO(accountID,requesterContactID)), COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        var account = AccountImpl.getAccountImplInstance().getById(accountID);
        Validator.checkArgument(ObjUtils.isNull(account), COMMON_ERROR_RESPONSE.ACCOUNT_NOT_FOUND.value());
        return new AccountDTO(account);
    }

    public List<SettingsJDO> getAllAccountsForUser(String contactID, String requesterContactID) {
        Validator.checkArgument(!requesterContactID.equalsIgnoreCase(contactID), COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        List<PeopleRelationJDO> activeUserPros = UserImpl.getUserImplInstance().getAllUserProsForUser(contactID,false);
        if(ObjUtils.isNullOrEmpty(activeUserPros)){
            return List.of();
        }
        List<String> accountIdKeys = activeUserPros.stream().map(PeopleRelationJDO::getUniquepin).collect(Collectors.toList());
        return AccountImpl.getAccountImplInstance().get(SettingsJDO.class,accountIdKeys);
    }

    public UserDTO getDefaultAccountProForUser(String contactID, OauthAccessToken accessToken) throws NoSuchAlgorithmException, IOException {
        Validator.checkArgument(AnnotationHelper.isAccessTokenTypeUser(accessToken) && !accessToken.getUserId().equalsIgnoreCase(contactID) , COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        PeopleRelationJDO defaultUserPro = UserPROUtil.getDefaultUserPro(contactID);
        Validator.checkArgument(ObjUtils.isNull(defaultUserPro),COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());
        return new UserDTO(defaultUserPro,null);
    }

    public UserDTO setDefaultAccountForUser(String contactID, String requesterContactID, String accountID) throws NoSuchAlgorithmException, IOException {
        Validator.checkArgument(!requesterContactID.equalsIgnoreCase(contactID), COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        PeopleRelationJDO userPro = UserPROUtil.validateAndExtractUserPRO(accountID,contactID);
        Validator.checkArgument(ObjUtils.isNull(userPro),COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());
        DcmUtil.updateDefaultAccountInDcm(accountID,contactID);
        UserPROUtil.setUserProAsDefault(userPro);
        return new UserDTO(userPro,null);
    }

    public AccountDTO deleteAccount(Contact requesterContact, String accountID, String payload) throws IOException{
        Validator.checkArgument(InternalUsage.FULL.equalsIgnoreCase(accountID) || InternalUsage.ADAPTAVANT.equalsIgnoreCase(accountID), COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        PeopleRelationJDO requesterUserPro = UserPROUtil.validateAndExtractUserPRO(accountID,requesterContact.getId());
        Validator.checkArgument(ObjUtils.isNull(requesterUserPro) || !UserPROUtil.isPrimaryAdmin(requesterUserPro),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        SettingsJDO account = AccountImpl.getAccountImplInstance().getById(accountID);
        Validator.checkArgument(!AccountUtil.isAccountActive(account),COMMON_ERROR_RESPONSE.ACCOUNT_NOT_FOUND.value());
        String reasonForDeletion = AccountDeleteHelper.extractReasonFromPayload(payload);
        AccountDeleteHelper.updateSettingsJdoEntryForDeletion(account);
        AccountDeleteHelper.saveAccountDeletionActivity(requesterContact,account,reasonForDeletion);
        var accountDTO = new AccountDTO(account);
        AccountTaskInitiator.initiateAccountDeletionQueues(accountDTO,requesterContact.getId());
        return accountDTO;
    }

    public Map<String, Object> createAccount(HttpServletRequest request, String payload)throws IOException {
        Validator.checkArgument(AccountCreationHelper.isSignUpRateLimitExceeded(request),COMMON_ERROR_RESPONSE.REQUEST_LIMIT_REACHED.value());
        var srcClient = HeaderUtil.extractSrcClientFromRequest(request);
        Validator.checkArgument(ObjUtils.isNullOrEmpty(srcClient), COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        AccountCreationPayloadDTO payloadDTO = AccountCreationHelper.validatePayloadAndExtractDTO(payload,request,srcClient);
        Map<String,Object> universalSignupPayload = AccountCreationHelper.generateSignupPayload(payloadDTO);
        Map<String,Object> apiResponse = UniversalSignupHelper.createAccountAndValidateResponse(universalSignupPayload);
        var account = UniversalSignupHelper.extractAndSaveSettingsJdoFromResponse(apiResponse,payloadDTO);
        var userPro = UniversalSignupHelper.extractAndSaveUserProAndContactFromResponse(apiResponse,payloadDTO,account);
        var accessPolicy = AccessManager.getInstance().createNewPolicyForOwner(userPro.getUniquepin(),userPro.getContactId(),payloadDTO.getPlan());
        AccountTaskInitiator.initiateAccountCreationQueue(account,userPro);
        return Map.of(CommonConstants.ACCOUNT_KEY,new AccountDTO(account), CommonConstants.USER_KEY, new UserDTO(userPro,accessPolicy.getPermissions()),
                SSO_URL_KEY,AccountCreationHelper.generateSsoSetupUrl(payloadDTO));
    }

    public Map<String, Object> processOneTapRequest(HttpServletRequest request, String payload) throws GeneralSecurityException, IOException {
        Map<String,Object> userDetails = OneTapHelper.validateAndExtractUserDetails(payload);
        if(UserPROUtil.isUserAssociatedToYoco((String)userDetails.get(EMAIL_KEY))){
            return Map.of(SSO_URL_KEY,OneTapHelper.generateSsoSetupUrl((String)userDetails.get(CREDENTIAL_KEY),(String)userDetails.get(CLIENT_ID_KEY)));
        }else{
            var accountCreationPayloadDTO = new AccountCreationPayloadDTO(userDetails, request, ClientSource.CLIENT_SOURCE_CONSTANTS.ONE_TAP.value());
            Map<String,Object> universalSignupPayload = AccountCreationHelper.generateSignupPayload(accountCreationPayloadDTO);
            Map<String,Object> apiResponse = UniversalSignupHelper.createAccountAndValidateResponse(universalSignupPayload);
            var account = UniversalSignupHelper.extractAndSaveSettingsJdoFromResponse(apiResponse,accountCreationPayloadDTO);
            var userPro = UniversalSignupHelper.extractAndSaveUserProAndContactFromResponse(apiResponse,accountCreationPayloadDTO,account);
            var accessPolicy = AccessManager.getInstance().createNewPolicyForOwner(userPro.getUniquepin(),userPro.getContactId(),accountCreationPayloadDTO.getPlan());
            AccountTaskInitiator.initiateAccountCreationQueue(account,userPro);
            return Map.of(CommonConstants.ACCOUNT_KEY,new AccountDTO(account), CommonConstants.USER_KEY, new UserDTO(userPro,accessPolicy.getPermissions()),
                    SSO_URL_KEY,OneTapHelper.generateSsoSetupUrl(accountCreationPayloadDTO.getOneTapToken(),(String)userDetails.get(CLIENT_ID_KEY)));
        }
    }

    public Map<String, Object> updateAccount(Contact requesterContact, String accountID, String payload) throws IOException {
        PeopleRelationJDO requesterUserPro = UserPROUtil.validateAndExtractUserPRO(accountID,requesterContact.getId());
        Validator.checkArgument(ObjUtils.isNull(requesterUserPro) || !UserPROUtil.isPrimaryAdmin(requesterUserPro),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        SettingsJDO account = AccountImpl.getAccountImplInstance().getById(accountID);
        Validator.checkArgument(!AccountUtil.isAccountActive(account),COMMON_ERROR_RESPONSE.ACCOUNT_NOT_FOUND.value());
        AccountUpdatePayloadDTO accountUpdatePayloadDTO = AccountUpdateHelper.validateAndExtractDTO(payload);
        AccountUpdateHelper.updateAccountAndSetActivityMessage(account,accountUpdatePayloadDTO);
        AccountTaskInitiator.initiateAccountUpdateQueue(account.getPeopleUniquePin(),requesterContact,accountUpdatePayloadDTO);
        return Map.of(CommonConstants.ACCOUNT_KEY,new AccountDTO(account));
    }
}
