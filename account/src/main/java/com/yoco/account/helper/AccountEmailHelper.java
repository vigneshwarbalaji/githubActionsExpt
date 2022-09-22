package com.yoco.account.helper;

import com.yoco.commons.config.TemplateConfig;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.EmailKeys;
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.services.EmailService;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.EmailUtil;
import freemarker.template.TemplateException;
import java.io.IOException;
import java.util.Map;

public class AccountEmailHelper {
    private AccountEmailHelper(){}
    private static final String ACCOUNT_DELETE_ADMIN_TEMPLATE = "accountDeletionPrimaryAdminMail.ftl";
    private static final String ACCOUNT_DELETE_STAFF_TEMPLATE = "accountDeletionStaffMail.ftl";
    private static final String ACCOUNT_CREATE_TEMPLATE = "accountRegistrationMail.ftl";

    public static void sendAccountDeletionEmailForPrimaryAdmin(String emailID, String name) throws TemplateException, IOException {
        var subject = EmailUtil.getSubject("Thank you for using YoCoBoard.");
        var addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, emailID, "","","");
        Map<String,Object> modalMap = Map.of(EmailKeys.USER_NAME.toString(),name,EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear());
        String template = TemplateConfig.processAndExtractTemplate(ACCOUNT_DELETE_ADMIN_TEMPLATE,modalMap);
        EmailService.getInstance().sendHtmlTemplatedMail(new MailDTO(addressDTO,template,subject));
    }

    public static void sendAccountDeletionEmailForStaff(String subject, PeopleRelationJDO staffPro, String accountDisplayName, String primaryAdminEmail, EmailService emailService) throws TemplateException, IOException {
        var addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, staffPro.getEmailID(), "","","");
        Map<String,Object> modalMap = Map.of(EmailKeys.USER_NAME.toString(),staffPro.getContact().getFirstName(),
                EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear(),
                EmailKeys.COMPANY_NAME.toString(),accountDisplayName,
                EmailKeys.ADMIN_MAIL.toString(),primaryAdminEmail,
                EmailKeys.START_DATE.toString(),DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY,staffPro.getDateAddedLongTime(),staffPro.getTimeZone()));
        String template = TemplateConfig.processAndExtractTemplate(ACCOUNT_DELETE_STAFF_TEMPLATE,modalMap);
        emailService.sendHtmlTemplatedMail(new MailDTO(addressDTO,template,subject));
    }

    public static void sendAccountRegistrationEmail(Contact contact) throws TemplateException, IOException {
            var subject = EmailUtil.getSubject("Welcome to YoCoBoard - Your Account is Ready");
            var addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, contact.getEmailID(), "","","");
            Map<String,Object> modalMap = Map.of(EmailKeys.USER_NAME.toString(),contact.getFirstName(),
                    EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear());
            String template = TemplateConfig.processAndExtractTemplate(ACCOUNT_CREATE_TEMPLATE,modalMap);
            EmailService.getInstance().sendHtmlTemplatedMail(new MailDTO(addressDTO,template,subject));
    }
}
