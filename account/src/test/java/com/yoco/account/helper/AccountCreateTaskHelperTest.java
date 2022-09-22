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
import com.yoco.user.helper.staff.UserStaffFullMetricHelper;
import freemarker.template.TemplateException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;
import java.util.Set;
import static com.yoco.commons.fullservices.FullAuthService.AW_ACCOUNT_CREATION_TOKEN_KEY;
import static org.mockito.ArgumentMatchers.eq;

class AccountCreateTaskHelperTest {
    @Test
    void handleAccountCreation_valid_test() throws TemplateException, NoSuchAlgorithmException, IOException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        userPro.setContactId("123");
        userPro.setEmailID("email");
        Contact contact = new Contact();
        contact.setFirstName("name");
        contact.setLastName("second");
        userPro.setContact(contact);
        SettingsJDO account = new SettingsJDO();
        account.setSource("web");
        account.setDateAddedLongTime(1L);
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<AccountCreateTaskHelper> accountCreateTaskHelperMockedStatic = Mockito.mockStatic(AccountCreateTaskHelper.class);
            MockedStatic<AccountFullMetricHelper> accountFullMetricHelperMockedStatic = Mockito.mockStatic(AccountFullMetricHelper.class);
            MockedStatic<UserStaffFullMetricHelper> userStaffFullMetricHelperMockedStatic = Mockito.mockStatic(UserStaffFullMetricHelper.class);
            MockedStatic<AccountEmailHelper> accountEmailHelperMockedStatic = Mockito.mockStatic(AccountEmailHelper.class)){
                accountCreateTaskHelperMockedStatic.when(()->AccountCreateTaskHelper.handleAccountCreation(account,userPro)).thenCallRealMethod();
                AccountFullMetricHelper accountFullMetricHelperMock = Mockito.mock(AccountFullMetricHelper.class);
                accountFullMetricHelperMockedStatic.when(AccountFullMetricHelper::getInstance).thenReturn(accountFullMetricHelperMock);
                UserStaffFullMetricHelper userStaffFullMetricHelperMock = Mockito.mock(UserStaffFullMetricHelper.class);
                userStaffFullMetricHelperMockedStatic.when(UserStaffFullMetricHelper::getInstance).thenReturn(userStaffFullMetricHelperMock);
                AccountCreateTaskHelper.handleAccountCreation(account,userPro);
                dcmUtilMockedStatic.verify(()->DcmUtil.updateDefaultAccountInDcm("accID","123"));
                accountCreateTaskHelperMockedStatic.verify(()->AccountCreateTaskHelper.setupAwAccount("accID","123"));
                accountCreateTaskHelperMockedStatic.verify(()->AccountCreateTaskHelper.updatePhotoForDcmContact(contact));
                Mockito.verify(accountFullMetricHelperMock).updateAccountCreationMetric("accID","web");
                Mockito.verify(userStaffFullMetricHelperMock).userCreationMetric("accID","web_force");
                accountEmailHelperMockedStatic.verify(()->AccountEmailHelper.sendAccountRegistrationEmail(contact));
                activityUtilMockedStatic.verify(()->ActivityUtil.saveActivity("accID","123","Signup_web","email","Source - web, emailID - email, contactID - 123, UserName - name second","DUMMY",1L));
        }
    }

    @Test
    void updatePhotoForDcmContact_photoID_Null_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            AccountCreateTaskHelper.updatePhotoForDcmContact(new Contact());
            urlFetcherMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void updatePhotoForDcmContact_photoID_DummyUserPic_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)){
            Contact contact = new Contact();
            contact.setPhotoID(StoragePublicUrl.DUMMY_USER_PIC_URL);
            AccountCreateTaskHelper.updatePhotoForDcmContact(contact);
            urlFetcherMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void updatePhotoForDcmContact_photoID_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<DcmConstants> dcmConstantsMockedStatic = Mockito.mockStatic(DcmConstants.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class)){
            Contact contact = new Contact();
            contact.setId("123");
            contact.setPhotoID("photoID");
            dcmConstantsMockedStatic.when(DcmConstants::getUpdateProfileImageUrl).thenReturn("url");
            headerUtilMockedStatic.when(HeaderUtil::getContentTypedServerTokenWithUserAgentHeader).thenReturn(new String[]{});
            AccountCreateTaskHelper.updatePhotoForDcmContact(contact);
            urlFetcherMockedStatic.verify(()->UrlFetcher.sendPostRequest(eq("url"), eq(Map.of("contactID","123","operation","add","profile_pic_url","photoID")),eq(new String[]{}),eq(null)));
        }
    }

    @Test
    void setupAwAccount_exception_test(){
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){
            Contact contact = new Contact();
            contact.setId("123");
            contact.setPhotoID("photoID");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAwHostUrl).thenThrow(new IllegalArgumentException("test"));
            AccountCreateTaskHelper.setupAwAccount("accID","123");
        }catch (Exception e){
            Assertions.assertEquals("test",e.getMessage());
        }
    }

    @Test
    void setupAwAccount_valid_test(){
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){
            Contact contact = new Contact();
            contact.setId("123");
            contact.setPhotoID("photoID");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAwHostUrl).thenReturn("url/");
            headerUtilMockedStatic.when(()->HeaderUtil.getContentTypedServerTokenAuthHeader("token")).thenReturn(new String[]{});
            fullAuthServiceMockedStatic.when(()->FullAuthService.getServerAccessToken(AW_ACCOUNT_CREATION_TOKEN_KEY, Set.of("awapis.account.create"))).thenReturn("token");
            AccountCreateTaskHelper.setupAwAccount("accID","123");
            urlFetcherMockedStatic.verify(()->UrlFetcher.sendPostRequest("url/api/v1/admin/account/accID/owner/123/setup", (Map<String, Object>) null,new String[]{},null));
        }
    }
}
