package com.yoco.user.helper.staff;

import com.yoco.commons.config.TemplateConfig;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.EmailKeys;
import com.yoco.commons.modal.contact.ContactDTO;
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.services.EmailService;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.EmailUtil;
import com.yoco.commons.utils.GaeUtils;
import com.yoco.contact.helper.password.PasswordDCMHelper;
import com.yoco.user.helper.UserDownloadHelper;
import freemarker.template.TemplateException;
import freemarker.template.TemplateNotFoundException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;

class UserStaffEmailHelperTest {

    UserStaffEmailHelper userStaffEmailHelper = UserStaffEmailHelper.getInstance();

    @Test
    void sendWelcomeMailForNewUser() throws TemplateException, IOException, NoSuchAlgorithmException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            Map<String,Object> dcmResp = new HashMap<>();
            dcmResp.put(ContactConstants.VERIFICATION_ID,"verificationId");
            passwordHelperMockedStatic.when(()-> PasswordDCMHelper.getVerificationID(anyString())).thenReturn(dcmResp);

            ContactDTO userContactDTO = new ContactDTO();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("userName");
            userContactDTO.setEmailID("test@gmail.com");

            Contact adminContactDTO = new Contact();
            adminContactDTO.setFirstName("adminName");

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Map<String,Object> userMap = new HashMap<>();
            userMap.put("isNewContact",true);
            userMap.put("accountName","Account");
            userStaffEmailHelper.addStaffEmailHandler(userMap,"accountId",adminContactDTO,userContactDTO);

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(), CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.USER_NAME.toString(),"userName");
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"adminName");
            modalMap.put(EmailKeys.REDIRECT_URL.toString(),"https://yocoDomain/createnewpassword?code=verificationId&Id=contactId&type=new");
            modalMap.put(EmailKeys.COMPANY_NAME.toString(),"Account");
            modalMap.put(EmailKeys.LOGIN_MAIL.toString(),"test@gmail.com");
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("welcomeUserMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"You have been invited to be a part of Account (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
            passwordHelperMockedStatic.verify(()-> PasswordDCMHelper.getVerificationID("contactId"));
        }
    }

    @Test
    void sendWelcomeMailForNewUser_sendAddUserMail_exception_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class);
            MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            templateConfigMockedStatic.when(()-> TemplateConfig.processAndExtractTemplate(anyString(),anyMap())).thenThrow(new TemplateNotFoundException("exception",null,"templateNotFound"));

            EmailService emailService  = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            Map<String,Object> dcmResp = new HashMap<>();
            dcmResp.put(ContactConstants.VERIFICATION_ID,"verificationId");
            passwordHelperMockedStatic.when(()-> PasswordDCMHelper.getVerificationID(anyString())).thenReturn(dcmResp);

            ContactDTO userContactDTO = new ContactDTO();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("userName");
            userContactDTO.setEmailID("test@gmail.com");

            Contact adminContactDTO = new Contact();
            adminContactDTO.setFirstName("adminName");

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Map<String,Object> userMap = new HashMap<>();
            userMap.put("isNewContact",true);
            userMap.put("accountName","Account");
            userStaffEmailHelper.addStaffEmailHandler(userMap,"accountId",adminContactDTO,userContactDTO);

            Mockito.verify(emailService,Mockito.times(0)).sendHtmlTemplatedMail(any());
            passwordHelperMockedStatic.verify(()-> PasswordDCMHelper.getVerificationID("contactId"));
        }
    }


    @Test
    void sendWelcomeMailForNewUser_exception_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            EmailService emailService  = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            Map<String,Object> dcmResp = new HashMap<>();
            dcmResp.put(ContactConstants.VERIFICATION_ID,"verificationId");
            passwordHelperMockedStatic.when(()-> PasswordDCMHelper.getVerificationID(anyString())).thenReturn(dcmResp);

            ContactDTO userContactDTO = new ContactDTO();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("userName");
            userContactDTO.setEmailID("test@gmail.com");

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Map<String,Object> userMap = new HashMap<>();
            userMap.put("isNewContact",true);
            userMap.put("accountName","Account");
            userStaffEmailHelper.addStaffEmailHandler(userMap,"accountId",null,userContactDTO);

            Mockito.verifyNoInteractions(emailService);
            passwordHelperMockedStatic.verify(()-> PasswordDCMHelper.getVerificationID("contactId"));
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void sendWelcomeMailForNewUser_dcm_emptyResp_test(Map<String,Object> testValue) throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            EmailService emailService  = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            passwordHelperMockedStatic.when(()-> PasswordDCMHelper.getVerificationID(anyString())).thenReturn(testValue);

            ContactDTO userContactDTO = new ContactDTO();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("userName");
            userContactDTO.setEmailID("test@gmail.com");

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Map<String,Object> userMap = new HashMap<>();
            userMap.put("isNewContact",true);
            userMap.put("accountName","Account");
            userStaffEmailHelper.addStaffEmailHandler(userMap,"accountId",null,userContactDTO);

            Mockito.verifyNoInteractions(emailService);
            passwordHelperMockedStatic.verify(()-> PasswordDCMHelper.getVerificationID("contactId"));
        }
    }

    @Test
    void sendWelcomeMailForNewUser_dcm_invalidVerificationID_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<PasswordDCMHelper> passwordHelperMockedStatic = Mockito.mockStatic(PasswordDCMHelper.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            EmailService emailService  = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            Map<String,Object> dcmResp = new HashMap<>();
            dcmResp.put(Commons.SUCCESS,false);
            passwordHelperMockedStatic.when(()-> PasswordDCMHelper.getVerificationID(anyString())).thenReturn(dcmResp);

            ContactDTO userContactDTO = new ContactDTO();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("userName");
            userContactDTO.setEmailID("test@gmail.com");

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Map<String,Object> userMap = new HashMap<>();
            userMap.put("isNewContact",true);
            userMap.put("accountName","Account");
            userStaffEmailHelper.addStaffEmailHandler(userMap,"accountId",null,userContactDTO);

            Mockito.verifyNoInteractions(emailService);
            passwordHelperMockedStatic.verify(()-> PasswordDCMHelper.getVerificationID("contactId"));
        }
    }

    @Test
    void sendWelcomeMailForUserToNewWorkSpace() throws TemplateException, IOException, NoSuchAlgorithmException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            ContactDTO userContactDTO = new ContactDTO();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("userName");
            userContactDTO.setEmailID("test@gmail.com");

            Contact adminContactDTO = new Contact();
            adminContactDTO.setFirstName("adminName");

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Map<String,Object> userMap = new HashMap<>();
            userMap.put("isNewContact",false);
            userMap.put("accountName","Account");
            userStaffEmailHelper.addStaffEmailHandler(userMap,"accountId",adminContactDTO,userContactDTO);

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(), CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.USER_NAME.toString(),"userName");
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"adminName");
            modalMap.put(EmailKeys.REDIRECT_URL.toString(),"https://yocoDomain/auth/company?accountID=accountId&contactID=contactId");
            modalMap.put(EmailKeys.COMPANY_NAME.toString(),"Account");
            modalMap.put(EmailKeys.LOGIN_MAIL.toString(),"test@gmail.com");
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("welcomeUserMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"You have been invited to be a part of Account (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void sendWelcomeMailForUserToNewWorkSpace_sendAddUserMail_exception_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            templateConfigMockedStatic.when(()-> TemplateConfig.processAndExtractTemplate(anyString(),anyMap())).thenThrow(new IOException("exception"));

            EmailService emailService  = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            ContactDTO userContactDTO = new ContactDTO();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("userName");
            userContactDTO.setEmailID("test@gmail.com");

            Contact adminContactDTO = new Contact();
            adminContactDTO.setFirstName("adminName");

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Map<String,Object> userMap = new HashMap<>();
            userMap.put("isNewContact",false);
            userMap.put("accountName","Account");
            userStaffEmailHelper.addStaffEmailHandler(userMap,"accountId",adminContactDTO,userContactDTO);

            Mockito.verify(emailService,Mockito.times(0)).sendHtmlTemplatedMail(any());
        }
    }

    @Test
    void sendWelcomeMailForUserToNewWorkSpace_exception_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            EmailService emailService  = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            ContactDTO userContactDTO = new ContactDTO();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("userName");
            userContactDTO.setEmailID("test@gmail.com");

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Map<String,Object> userMap = new HashMap<>();
            userMap.put("isNewContact",false);
            userMap.put("accountName","Account");
            userStaffEmailHelper.addStaffEmailHandler(userMap,"accountId",null,userContactDTO);

            Mockito.verifyNoInteractions(emailService);
        }
    }

    @Test
    void importStaffEmailHandler_test() throws TemplateException, IOException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            LinkedHashMap<String, Object> failedUsersList = new LinkedHashMap<>();
            failedUsersList.put("test1@gmail.com", "EmailId already exists");
            failedUsersList.put("test2@gmail.com", "Invalid user");

            Contact userContactDTO = new Contact();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("userName");
            userContactDTO.setLastName("test");
            userContactDTO.setEmailID("test@gmail.com");

            userStaffEmailHelper.importStaffEmailHandler(failedUsersList, userContactDTO, 3);

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(), CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"userName test");
            modalMap.put(EmailKeys.TOTAL_COUNT.toString(),3);
            modalMap.put(EmailKeys.SUCCESS_COUNT.toString(),1);
            modalMap.put(EmailKeys.FAILED_COUNT_WITH_MSG.toString(),2 + " &#38; their email addresses are attached below ");
            modalMap.put(EmailKeys.SUPPORT_MAIL.toString(),EmailUtil.MAIL_SENDER);
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("staffImportAdminMail.ftl",modalMap);

            var failedUsersBuilder = new StringBuilder();
            failedUsersBuilder.append("test1@gmail.com,");
            failedUsersBuilder.append("EmailId already exists");
            failedUsersBuilder.append(UserDownloadHelper.CSV_TERMINATOR);
            failedUsersBuilder.append("test2@gmail.com,");
            failedUsersBuilder.append("Invalid user");
            failedUsersBuilder.append(UserDownloadHelper.CSV_TERMINATOR);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Summary of Invites Sent (staging-goclockin-dashboard) ","Failed_User_report.csv",failedUsersBuilder.toString());
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void importStaffEmailHandler_no_failedUsers_test() throws TemplateException, IOException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            LinkedHashMap<String, Object> failedUsersList = new LinkedHashMap<>();

            Contact userContactDTO = new Contact();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("userName");
            userContactDTO.setLastName("test");
            userContactDTO.setEmailID("test@gmail.com");

            userStaffEmailHelper.importStaffEmailHandler(failedUsersList, userContactDTO, 3);

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(), CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"userName test");
            modalMap.put(EmailKeys.TOTAL_COUNT.toString(),3);
            modalMap.put(EmailKeys.SUCCESS_COUNT.toString(),3);
            modalMap.put(EmailKeys.FAILED_COUNT_WITH_MSG.toString(),0 + " ");
            modalMap.put(EmailKeys.SUPPORT_MAIL.toString(),EmailUtil.MAIL_SENDER);
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("staffImportAdminMail.ftl",modalMap);

            var failedUsersBuilder = new StringBuilder();

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Summary of Invites Sent (staging-goclockin-dashboard) ","Failed_User_report.csv",failedUsersBuilder.toString());
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void importStaffEmailHandler_exception_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            EmailService emailService  = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            templateConfigMockedStatic.when(()-> TemplateConfig.processAndExtractTemplate(anyString(),anyMap())).thenThrow(new TemplateNotFoundException("exception",null,"templateNotFound"));

            LinkedHashMap<String, Object> failedUsersList = new LinkedHashMap<>();
            failedUsersList.put("test1@gmail.com", "EmailId already exists");
            failedUsersList.put("test2@gmail.com", "Invalid user");

            Contact userContactDTO = new Contact();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("userName");
            userContactDTO.setLastName("test");
            userContactDTO.setEmailID("test@gmail.com");

            userStaffEmailHelper.importStaffEmailHandler(failedUsersList, userContactDTO, 3);

            Mockito.verify(emailService,Mockito.times(0)).sendHtmlTemplatedMail(any());
        }
    }


}