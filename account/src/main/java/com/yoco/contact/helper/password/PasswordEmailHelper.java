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
import freemarker.template.TemplateException;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public class PasswordEmailHelper {

    public static PasswordEmailHelper getInstance(){
        return new PasswordEmailHelper();
    }

    private static final String RESET_PASS_INIT_MAIL_FTL = "resetPasswordInitiationMail.ftl";
    private static final String RESET_PASS_CONFIRMATION_MAIL_FTL = "resetPasswordConfirmationMail.ftl";


    private String getRedirectUrlForResetPassInitMail(String verificationID,String contactID){
        return CommonAppProperties.getYoCoDashboardUrl() + "createnewpassword?code=" + verificationID + "&Id=" + contactID;
    }

    private MailAddressDTO getMailAddressesForResetPasswordMail(String toEmailID){
        return new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, toEmailID, "","","");
    }

    private Map<String,Object> getModalForResetPasswordMail(String userName,String redirectUrl,String supportMail){
        Map<String,Object> modalMap = new HashMap<>();
        modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(), CommonAppProperties.getYoCoDashboardUrl());
        modalMap.put(EmailKeys.USER_NAME.toString(),userName);
        modalMap.put(EmailKeys.REDIRECT_URL.toString(),redirectUrl);
        modalMap.put(EmailKeys.SUPPORT_MAIL.toString(),supportMail);
        modalMap.put(EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear());
        return modalMap;
    }

    private void sendResetPassInitMail(Map<String,Object> modalMap, MailAddressDTO addressDTO){
        try {
            String subject = EmailUtil.getSubject("Reset your YoCoBoard account password");

            String template = TemplateConfig.processAndExtractTemplate(RESET_PASS_INIT_MAIL_FTL,modalMap);

            log.info("Initiating mail on reset password initiation ... ");
            EmailService.getInstance().sendHtmlTemplatedMail(new MailDTO(addressDTO,template,subject));

        } catch (TemplateException | IOException e) {
            log.info(" exception in sending rest password initiation flow : " + e.getMessage());
        }
    }


    public void initiateResetPasswordInitMail(Contact userContact, String verificationID){

        MailAddressDTO addressDTO = this.getMailAddressesForResetPasswordMail(userContact.getEmailID());

        String redirectUri = this.getRedirectUrlForResetPassInitMail(verificationID,userContact.getId());

        Map<String, Object> modalMap = this.getModalForResetPasswordMail(userContact.getFullName(),redirectUri,EmailUtil.MAIL_SENDER);

        this.sendResetPassInitMail(modalMap, addressDTO);
    }


    private void sendResetPassConfirmationMail(Map<String,Object> modalMap, MailAddressDTO addressDTO){
        try {
            String subject = EmailUtil.getSubject("Your YoCoBoard password was updated successfully");

            String template = TemplateConfig.processAndExtractTemplate(RESET_PASS_CONFIRMATION_MAIL_FTL,modalMap);

            log.info("Initiating mail on reset password confirmation ... ");
            EmailService.getInstance().sendHtmlTemplatedMail(new MailDTO(addressDTO,template,subject));

        } catch (TemplateException | IOException e) {
            log.info(" exception in sending rest password confirmation flow : " + e.getMessage());
        }
    }

    public void initiateResetPasswordConfirmationMail(Contact userContact){

        MailAddressDTO addressDTO = this.getMailAddressesForResetPasswordMail(userContact.getEmailID());

        Map<String, Object> modalMap = this.getModalForResetPasswordMail(userContact.getFullName(),CommonAppProperties.getYoCoDashboardUrl(),EmailUtil.MAIL_SENDER);

        this.sendResetPassConfirmationMail(modalMap, addressDTO);
    }


}
