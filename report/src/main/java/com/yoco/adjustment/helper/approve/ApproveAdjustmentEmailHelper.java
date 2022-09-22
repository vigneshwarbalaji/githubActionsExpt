package com.yoco.adjustment.helper.approve;

import com.yoco.adjustment.modal.AdjustmentMailDTO;
import com.yoco.adjustment.modal.ContactMailDTO;
import com.yoco.commons.config.TemplateConfig;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.EmailKeys;
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.services.EmailService;
import com.yoco.commons.utils.EmailUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import freemarker.template.TemplateException;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public class ApproveAdjustmentEmailHelper {

    public static ApproveAdjustmentEmailHelper getInstance(){
        return new ApproveAdjustmentEmailHelper();
    }

    private static final String ADJUSTMENT_APPROVED_MAIL = "adjustmentApprovedMail.ftl";
    private static final String ADJUSTMENT_EDIT_APPROVED_MAIL = "adjustmentEditApprovedMail.ftl";

    private void sendAdjustmentApprovedMail(Map<String,Object> modalMap,String toEmailID){
        try {
            String subject = EmailUtil.getSubject("Your adjustment is approved.");
            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, toEmailID, "","","");
            String template = TemplateConfig.processAndExtractTemplate(ADJUSTMENT_APPROVED_MAIL,modalMap);
            EmailService.getInstance().sendHtmlTemplatedMail(new MailDTO(addressDTO,template,subject));
        } catch (TemplateException |IOException e) {
            log.info(" exception in sending approved adjustment mail : " + e.getMessage());
        }
    }

    private Map<String,Object> getModalForAdjustmentApprovedMail(AdjustmentMailDTO adjustmentMailDTO, ContactMailDTO contactMailDTO){
        Map<String,Object> modalMap = new HashMap<>();
        modalMap.put(EmailKeys.USER_NAME.toString(),contactMailDTO.getUserName());
        modalMap.put(EmailKeys.ADMIN_NAME.toString(),contactMailDTO.getAdminName());
        modalMap.put(EmailKeys.ADMIN_MAIL.toString(),contactMailDTO.getAdminMailID());
        modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(), adjustmentMailDTO.getDomainImage());
        modalMap.put(EmailKeys.MESSAGE.toString(),adjustmentMailDTO.getReason());
        modalMap.put(EmailKeys.IN_DATE.toString(),adjustmentMailDTO.getInDate());
        modalMap.put(EmailKeys.IN_TIME.toString(),adjustmentMailDTO.getInTime());
        modalMap.put(EmailKeys.OUT_TIME.toString(),adjustmentMailDTO.getOutTime());
        modalMap.put(EmailKeys.DURATION.toString(),adjustmentMailDTO.getDuration());
        modalMap.put(EmailKeys.REDIRECT_URL.toString(),adjustmentMailDTO.getRedirectUrl());
        modalMap.put(EmailKeys.CURRENT_YEAR.toString(),adjustmentMailDTO.getCurrentYear());
        return modalMap;
    }


    public void adjustmentApprovedMailHandler(Contact userContact, Contact adminContact,String zone,
                                              AdjustmentDTO adjustmentDTO, SettingsJDO account){
        try{
            String accountID = account.getPeopleUniquePin();
            AdjustmentMailDTO adjustmentMailDTO = new AdjustmentMailDTO(adjustmentDTO,zone,account.getDisplayTimeFormat(),adjustmentDTO.getRequestMessage());

            if(InternalUsage.isFullUsAccount(accountID)){
                PeopleRelationJDO primaryPro = UserPROUtil.getPrimaryAdmin(accountID);
                adminContact = ObjUtils.isNull(primaryPro) ? adminContact : primaryPro.getContact();
            }

            ContactMailDTO contactMailDTO = new ContactMailDTO(userContact,adminContact,adminContact);
            Map<String, Object> modalMap= this.getModalForAdjustmentApprovedMail(adjustmentMailDTO,contactMailDTO);
            this.sendAdjustmentApprovedMail(modalMap,userContact.getEmailID());
        }catch (Exception e){
            log.info(" exception in sending adjustmentApprovedMailHandler : " + e.getMessage());
        }
    }

    private void sendAdjustmentEditApprovedMail(Map<String,Object> modalMap,String toEmailID){
        try {
            String subject = EmailUtil.getSubject("Hours Adjustment Request Status.");
            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, toEmailID, "","","");
            String template = TemplateConfig.processAndExtractTemplate(ADJUSTMENT_EDIT_APPROVED_MAIL,modalMap);
            EmailService.getInstance().sendHtmlTemplatedMail(new MailDTO(addressDTO,template,subject));
        } catch (TemplateException |IOException e) {
            log.info(" exception in sending edit approved adjustment mail : " + e.getMessage());
        }
    }

    private Map<String,Object> getModalForAdjustmentEditApprovedMail(AdjustmentMailDTO adjustmentMailDTO, ContactMailDTO contactMailDTO){
        Map<String,Object> modalMap = new HashMap<>();
        modalMap.put(EmailKeys.USER_NAME.toString(),contactMailDTO.getUserName());
        modalMap.put(EmailKeys.ADMIN_NAME.toString(),contactMailDTO.getAdminName());
        modalMap.put(EmailKeys.ADMIN_MAIL.toString(),contactMailDTO.getAdminMailID());
        modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(), adjustmentMailDTO.getDomainImage());
        modalMap.put(EmailKeys.COMPANY_NAME.toString(),adjustmentMailDTO.getCompanyName());
        modalMap.put(EmailKeys.MESSAGE.toString(),adjustmentMailDTO.getReason());
        modalMap.put(EmailKeys.IN_DATE.toString(),adjustmentMailDTO.getInDate());
        modalMap.put(EmailKeys.IN_TIME.toString(),adjustmentMailDTO.getInTime());
        modalMap.put(EmailKeys.OUT_TIME.toString(),adjustmentMailDTO.getOutTime());
        modalMap.put(EmailKeys.DURATION.toString(),adjustmentMailDTO.getDuration());
        modalMap.put(EmailKeys.DURATION_DIFF.toString(),adjustmentMailDTO.getDurationDiff());
        modalMap.put(EmailKeys.REDIRECT_URL.toString(),adjustmentMailDTO.getRedirectUrl());
        modalMap.put(EmailKeys.CURRENT_YEAR.toString(),adjustmentMailDTO.getCurrentYear());
        return modalMap;
    }

    public void adjustmentEditApprovedMailHandler(Contact userContact, Contact adminContact,String zone,
                                                  AdjustmentDTO adjustmentDTO, SettingsJDO account){
        try{
            String accountID = account.getPeopleUniquePin();
            AdjustmentMailDTO adjustmentMailDTO = new AdjustmentMailDTO(adjustmentDTO,zone,account,adjustmentDTO.getApprovedMessage());

            if(InternalUsage.isFullUsAccount(accountID)){
                PeopleRelationJDO primaryPro = UserPROUtil.getPrimaryAdmin(accountID);
                adminContact = ObjUtils.isNull(primaryPro) ? adminContact : primaryPro.getContact();
            }

            ContactMailDTO contactMailDTO = new ContactMailDTO(userContact,adminContact,adminContact);
            Map<String, Object> modalMap= this.getModalForAdjustmentEditApprovedMail(adjustmentMailDTO,contactMailDTO);
            this.sendAdjustmentEditApprovedMail(modalMap,userContact.getEmailID());
        }catch (Exception e){
            log.info(" exception in sending adjustmentEditApprovedMailHandler : " + e.getMessage());
        }
    }

}
