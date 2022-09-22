package com.yoco.account.helper;

import com.yoco.commons.config.TemplateConfig;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
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
import java.util.Map;

class AccountEmailHelperTest {
    @Test
    void sendAccountDeletionEmailForPrimaryAdmin_valid_test() throws TemplateException, IOException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)) {
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(true);
            EmailService emailServiceMock = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailServiceMock);
            AccountEmailHelper.sendAccountDeletionEmailForPrimaryAdmin("email","name");
            MailAddressDTO expectedAddressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "email", "","","");
            String expectedTemplate = TemplateConfig.processAndExtractTemplate("accountDeletionPrimaryAdminMail.ftl", Map.of(EmailKeys.USER_NAME.toString(),"name",EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear()));
            Mockito.verify(emailServiceMock).sendHtmlTemplatedMail(new MailDTO(expectedAddressDTO,expectedTemplate,"Thank you for using YoCoBoard."));
        }
    }

    @Test
    void sendAccountDeletionEmailForStaff_valid_test() throws TemplateException, IOException {
        EmailService emailServiceMock = Mockito.mock(EmailService.class);
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setEmailID("email");
        Contact contact = new Contact();
        contact.setFirstName("name");
        userPro.setContact(contact);
        userPro.setTimeZone("Asia/Kolkata");
        userPro.setDateAddedLongTime(1652380200000L);
        AccountEmailHelper.sendAccountDeletionEmailForStaff("subject",userPro,"account","adminemail",emailServiceMock);
        MailAddressDTO expectedAddressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "email", "","","");
        String expectedTemplate = TemplateConfig.processAndExtractTemplate("accountDeletionStaffMail.ftl", Map.of(EmailKeys.USER_NAME.toString(),"name",EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear(), EmailKeys.COMPANY_NAME.toString(),"account",
                EmailKeys.ADMIN_MAIL.toString(),"adminemail",
                EmailKeys.START_DATE.toString(),"13-May-2022"));
        Mockito.verify(emailServiceMock).sendHtmlTemplatedMail(new MailDTO(expectedAddressDTO,expectedTemplate,"subject"));
    }

    @Test
    void sendAccountRegistrationEmail_valid_test() throws TemplateException, IOException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)) {
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(true);
            EmailService emailServiceMock = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailServiceMock);
            Contact contact = new Contact();
            contact.setEmailID("email");
            contact.setFirstName("name");
            AccountEmailHelper.sendAccountRegistrationEmail(contact);
            MailAddressDTO expectedAddressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "email", "","","");
            String expectedTemplate = TemplateConfig.processAndExtractTemplate("accountRegistrationMail.ftl", Map.of(EmailKeys.USER_NAME.toString(),"name",EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear()));
            Mockito.verify(emailServiceMock).sendHtmlTemplatedMail(new MailDTO(expectedAddressDTO,expectedTemplate,"Welcome to YoCoBoard - Your Account is Ready"));
        }
    }
}
