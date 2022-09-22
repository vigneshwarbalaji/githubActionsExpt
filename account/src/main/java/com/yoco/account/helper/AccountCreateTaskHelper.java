package com.yoco.account.helper;

import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.constants.StoragePublicUrl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.user.helper.staff.UserStaffFullMetricHelper;
import freemarker.template.TemplateException;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;
import static com.yoco.commons.fullservices.FullAuthService.AW_ACCOUNT_CREATION_TOKEN_KEY;

@Slf4j
public class AccountCreateTaskHelper {
    private AccountCreateTaskHelper(){}

    public static void handleAccountCreation(SettingsJDO account, PeopleRelationJDO userPro) throws NoSuchAlgorithmException, IOException, TemplateException {
        var accountID = userPro.getUniquepin();
        var contactID = userPro.getContactId();
        var contact = userPro.getContact();
        DcmUtil.updateDefaultAccountInDcm(accountID,contactID);
        String activity = "Source - "+account.getSource()+", emailID - "+userPro.getEmailID()+", contactID - "+contactID+", UserName - "+contact.getFullName();
        ActivityUtil.saveActivity(accountID, contactID, "Signup_" +account.getSource(), userPro.getEmailID(), activity, ActivityUtil.ACTIVITIES.DUMMY.value(), account.getDateAddedLongTime());
        setupAwAccount(accountID,contactID);
        AccountFullMetricHelper.getInstance().updateAccountCreationMetric(accountID,account.getSource());
        UserStaffFullMetricHelper.getInstance().userCreationMetric(accountID,"web_force");
        updatePhotoForDcmContact(contact);
        AccountEmailHelper.sendAccountRegistrationEmail(contact);
    }

    public static void updatePhotoForDcmContact(Contact contact) throws NoSuchAlgorithmException, IOException {
        if(!ObjUtils.isNullOrEmpty(contact.getPhotoID()) && !StoragePublicUrl.DUMMY_USER_PIC_URL.equals(contact.getPhotoID())){
            Map<String,Object> payload = Map.of("contactID",contact.getId(),"operation","add","profile_pic_url",contact.getPhotoID());
            UrlFetcher.sendPostRequest(DcmConstants.getUpdateProfileImageUrl(),payload,
                    HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(), UrlFetcher.getHttpClientInstance());
        }
    }

    public static void setupAwAccount(String accountID, String contactID){
        try{
            String awAccountSetupURL = CommonAppProperties.getAwHostUrl() + "api/v1/admin/account/"+accountID+"/owner/"+contactID+"/setup";
            UrlFetcher.sendPostRequest(awAccountSetupURL,(Map<String, Object>) null, HeaderUtil.getContentTypedServerTokenAuthHeader(FullAuthService.getServerAccessToken(AW_ACCOUNT_CREATION_TOKEN_KEY, ApiScope.getAwAccountCreationServiceTokenScopes())),UrlFetcher.getHttpClientInstance());
        }catch(Exception e){
            log.warn("Error occurred while setting up aw account :: " + e.getMessage());
        }
    }

}
