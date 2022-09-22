package com.yoco.contact.helper.password;

import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import com.yoco.contact.enums.CONTACT_ERROR_MESSAGE;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

public class PasswordDCMHelper {

    private PasswordDCMHelper(){}

    private static final String ACCOUNT_ID_Q_PARAM = "{$accountID}";
    private static final String CONTACT_ID_Q_PARAM = "{$contactID}";
    private static final String VERIFICATION_ID_Q_PARAM = "{$verificationID}";

    public static Map<String,Object> getVerificationID(String contactId) throws NoSuchAlgorithmException, IOException {
        String url = DcmConstants.getInitResetPasswordUrl().replace(CONTACT_ID_Q_PARAM,contactId);
        return UrlFetcher.sendGetRequest(url, HeaderUtil.getServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static boolean validateVerificationId(String verificationId) throws NoSuchAlgorithmException, IOException {
        String url = DcmConstants.getValidateVerificationIdUrl().replace(VERIFICATION_ID_Q_PARAM,verificationId);
        return UrlFetcher.sendGetRequestWithBooleanResp(url,HeaderUtil.getServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static Map<String,Object> updatePassword(String accountID,String token,Map<String,Object> payloadMap) throws IOException {
        String url = DcmConstants.getUpdatePasswordUrl().replace(ACCOUNT_ID_Q_PARAM,accountID);
        return UrlFetcher.sendPutRequest(url,payloadMap,HeaderUtil.getContentTypedAccessTokenAuthHeader(token),UrlFetcher.getHttpClientInstance());
    }

    public static Map<String,Object> resetPassword(String verificationId,Map<String,Object> payloadMap) throws IOException, NoSuchAlgorithmException {
        String url = DcmConstants.getResetPasswordUrl().replace(VERIFICATION_ID_Q_PARAM,verificationId);
        return UrlFetcher.sendPutRequest(url,payloadMap,HeaderUtil.getContentTypedServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static void validatePassword(String password){
        Validator.checkArgument(ObjUtils.isNullOrEmpty(password) || password.length() < 8 || password.length() > 64, CONTACT_ERROR_MESSAGE.INVALID_PASSWORD_LENGTH.value());
        Validator.checkArgument(!(password.trim().equals(password)),CONTACT_ERROR_MESSAGE.INVALID_PASSWORD_TRAILING_SPACE.value());
        Validator.checkArgument(!password.matches(".*[a-zA-Z]+.*") || !password.matches(".*[0-9]+.*"), CONTACT_ERROR_MESSAGE.INVALID_PASSWORD_COMBINATION.value());
    }
}
