package com.yoco.contact.helper.password;

import com.yoco.commons.config.TemplateConfig;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.EmailKeys;
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.services.EmailService;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.EmailUtil;
import com.yoco.commons.utils.GaeUtils;
import freemarker.template.TemplateException;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;

class PasswordEmailHelperTest {

    PasswordEmailHelper passwordEmailHelper = PasswordEmailHelper.getInstance();

    @Test
    void initiateResetPasswordInitMail_valid_test() throws TemplateException, IOException {

        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            Contact userContactDTO = new Contact();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("user");
            userContactDTO.setLastName("Name");
            userContactDTO.setEmailID("test@gmail.com");

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            passwordEmailHelper.initiateResetPasswordInitMail(userContactDTO,"verificationId");

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(), CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.USER_NAME.toString(),"user Name");
            modalMap.put(EmailKeys.REDIRECT_URL.toString(),"https://yocoDomain/createnewpassword?code=verificationId&Id=contactId");
            modalMap.put(EmailKeys.SUPPORT_MAIL.toString(),EmailUtil.MAIL_SENDER);
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("resetPasswordInitiationMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Reset your YoCoBoard account password (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void initiateResetPasswordInitMail_exception_test(){

        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            templateConfigMockedStatic.when(()-> TemplateConfig.processAndExtractTemplate(anyString(),anyMap())).thenThrow(new IOException("exception"));

            EmailService emailService  = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            Contact userContactDTO = new Contact();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("user");
            userContactDTO.setLastName("Name");
            userContactDTO.setEmailID("test@gmail.com");

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            passwordEmailHelper.initiateResetPasswordInitMail(userContactDTO,"verificationId");

            Mockito.verify(emailService,Mockito.times(0)).sendHtmlTemplatedMail(any());
        }
    }


    @Test
    void initiateResetPasswordConfirmationMail_valid_test() throws TemplateException, IOException {

        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            Contact userContactDTO = new Contact();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("user");
            userContactDTO.setLastName("Name");
            userContactDTO.setEmailID("test@gmail.com");

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            passwordEmailHelper.initiateResetPasswordConfirmationMail(userContactDTO);

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(), CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.USER_NAME.toString(),"user Name");
            modalMap.put(EmailKeys.REDIRECT_URL.toString(),"https://yocoDomain/");
            modalMap.put(EmailKeys.SUPPORT_MAIL.toString(),EmailUtil.MAIL_SENDER);
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("resetPasswordConfirmationMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Your YoCoBoard password was updated successfully (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void initiateResetPasswordConfirmationMail_exception_test(){

        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class)) {

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);

            templateConfigMockedStatic.when(()-> TemplateConfig.processAndExtractTemplate(anyString(),anyMap())).thenThrow(new IOException("exception"));

            EmailService emailService  = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            Contact userContactDTO = new Contact();
            userContactDTO.setId("contactId");
            userContactDTO.setFirstName("user");
            userContactDTO.setLastName("Name");
            userContactDTO.setEmailID("test@gmail.com");

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            passwordEmailHelper.initiateResetPasswordConfirmationMail(userContactDTO);

            Mockito.verify(emailService,Mockito.times(0)).sendHtmlTemplatedMail(any());
        }
    }


}