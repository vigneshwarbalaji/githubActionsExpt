package com.yoco.contact.service;

import com.yoco.commons.annotation.helper.AccessTokenCacheService;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.RateConfigurator;
import com.yoco.commons.utils.HashUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import com.yoco.contact.enums.CONTACT_ERROR_MESSAGE;
import com.yoco.contact.helper.password.PasswordDCMHelper;
import com.yoco.contact.helper.password.PasswordEmailHelper;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@Service
public class PasswordService {

    @NoArgsConstructor
    private enum CONTACT_PASSWORD_SERVICE_CONSTANTS {

        OLD_PASSWORD("oldPassword"),
        NEW_PASSWORD("newPassword");

        private String value;

        CONTACT_PASSWORD_SERVICE_CONSTANTS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    public Map<String,Object> updatePassword(String accountID, String contactID, String payload, String token) throws NoSuchAlgorithmException, IOException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload) || !JsonUtil.isValidJson(payload),COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());

        Map<String,Object> payloadMap =  JsonUtil.convertJsonToMap(payload);

        String oldPassword = payloadMap.containsKey(CONTACT_PASSWORD_SERVICE_CONSTANTS.OLD_PASSWORD.value()) ? (String) payloadMap.get(CONTACT_PASSWORD_SERVICE_CONSTANTS.OLD_PASSWORD.value()) : "";
        String newPassword = payloadMap.containsKey(CONTACT_PASSWORD_SERVICE_CONSTANTS.NEW_PASSWORD.value()) ? (String) payloadMap.get(CONTACT_PASSWORD_SERVICE_CONSTANTS.NEW_PASSWORD.value()) : "";

        Validator.checkArgument(ObjUtils.isNullOrEmpty(oldPassword) || ObjUtils.isNullOrEmpty(newPassword),COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value());

        Map<String, Object> requestPayload = new HashMap<>();
        requestPayload.put("id", contactID);
        requestPayload.put(CONTACT_PASSWORD_SERVICE_CONSTANTS.OLD_PASSWORD.value(), HashUtil.generateSHA1(oldPassword));
        requestPayload.put(CONTACT_PASSWORD_SERVICE_CONSTANTS.NEW_PASSWORD.value(), HashUtil.generateSHA1(newPassword));

        return PasswordDCMHelper.updatePassword(accountID,token,requestPayload);
    }

    private Boolean isForgotPasswordRequestLimitExceeded(String email){
        final var requestLimit = 5;
        final var expirationTime = 24 * 60 * 60;
        String key = email.hashCode() + "_forgotPass";
        var rateLimitInfo = RateConfigurator.checkRateLimitUsage(key,requestLimit,expirationTime);
        if(rateLimitInfo == null){
            log.info("Rate limiting api call failure");
            return false;
        }
        return !rateLimitInfo.isAllow();
    }


    public Map<String,Object> forgotPassword(String emailId) throws NoSuchAlgorithmException, IOException {
        Validator.checkArgument(Boolean.FALSE.equals(Validator.isValidEmail(emailId)), COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.value());
        Validator.checkArgument(Boolean.TRUE.equals(this.isForgotPasswordRequestLimitExceeded(emailId)), COMMON_ERROR_RESPONSE.REQUEST_LIMIT_REACHED.value());

        var contactObj = ContactImpl.getContactImplInstance().getContactByEmailID(emailId);

        Validator.checkArgument(ObjUtils.isNull(contactObj), CONTACT_ERROR_MESSAGE.MAIL_DOES_NOT_EXIST.value());

        Map<String,Object> response = PasswordDCMHelper.getVerificationID(contactObj.getId());

        if(!ObjUtils.isNullOrEmpty(response) && Boolean.TRUE.equals(response.get(Commons.SUCCESS))){
            PasswordEmailHelper.getInstance().initiateResetPasswordInitMail(contactObj,response.get(ContactConstants.VERIFICATION_ID).toString());
        }

        return response;
    }

    public Map<String,Object> resetPassword(String contactID,String payload) throws NoSuchAlgorithmException, IOException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload) || !JsonUtil.isValidJson(payload),COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        Map<String,Object> payloadMap =  JsonUtil.convertJsonToMap(payload);

        String verificationId = payloadMap.containsKey(ContactConstants.VERIFICATION_ID) ?
                payloadMap.get(ContactConstants.VERIFICATION_ID).toString() : "";

        Validator.checkArgument( ObjUtils.isNullOrEmpty(verificationId),CONTACT_ERROR_MESSAGE.INVALID_VERIFICATION_ID.value());

        String newPassword = payloadMap.containsKey(CONTACT_PASSWORD_SERVICE_CONSTANTS.NEW_PASSWORD.value()) ?
                payloadMap.get(CONTACT_PASSWORD_SERVICE_CONSTANTS.NEW_PASSWORD.value()).toString() : "";

        Validator.checkArgument( ObjUtils.isNullOrEmpty(newPassword),CONTACT_ERROR_MESSAGE.INVALID_PASSWORD.value());

        var contactObj = ContactImpl.getContactImplInstance().getByID(contactID);
        Validator.checkArgument( ObjUtils.isNull(contactObj), CONTACT_ERROR_MESSAGE.CONTACT_DOES_NOT_EXIST.value());

        Map<String, Object> requestPayload = new HashMap<>();
        requestPayload.put("id",contactID);
        requestPayload.put(CONTACT_PASSWORD_SERVICE_CONSTANTS.NEW_PASSWORD.value(),HashUtil.generateSHA1(newPassword));

        Map<String,Object> contactResponse = PasswordDCMHelper.resetPassword(verificationId,requestPayload);

        if(!ObjUtils.isNullOrEmpty(contactResponse) && Boolean.TRUE.equals(contactResponse.get(Commons.SUCCESS))){
            AccessTokenCacheService.clearAccessTokensForUser(null,contactID);
            PasswordEmailHelper.getInstance().initiateResetPasswordConfirmationMail(contactObj);
        }

        return contactResponse;
    }

}
