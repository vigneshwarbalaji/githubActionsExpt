package com.yoco.payroll.helper;

import com.yoco.adjustment.modal.ContactMailDTO;
import com.yoco.commons.config.TemplateConfig;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.EmailKeys;
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.services.EmailService;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.EmailUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import freemarker.template.TemplateException;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;


@Slf4j
public class PayrollEmailHelper {

    public static PayrollEmailHelper getInstance(){
        return new PayrollEmailHelper();
    }

    private static final String PAYROLL_APPROVAL_MAIL_FTL = "payrollApprovalMail.ftl";
    private static final String SESSION_CONTENT_TEMPLATE = "sessionContentTemplate.ftl";

    private Long calculateSessionDuration(Long startMillis,Long endMillis){
        return endMillis - startMillis;
    }

    private String constructSessionDetails(Long startMillis,Long endMillis,String timeFormat,String zone) throws TemplateException, IOException {
        Map<String,Object> modalMap = new HashMap<>();
        modalMap.put(EmailKeys.IN_DATE.toString(),DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY,startMillis,zone));
        modalMap.put(EmailKeys.IN_TIME.toString(),DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A,startMillis, zone));
        modalMap.put(EmailKeys.OUT_TIME.toString(),DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A,endMillis,zone));
        modalMap.put(EmailKeys.DURATION.toString(),DateUtil.getTimeFormatAsPerCompany(calculateSessionDuration(startMillis,endMillis),timeFormat));
        return TemplateConfig.processAndExtractTemplate(SESSION_CONTENT_TEMPLATE, modalMap);
    }

    public Map<String,Object> buildSessionContentForMail(Map<String, Object> event,String timeFormat,String zone) {
        Map<String,Object> sessionMap = new HashMap<>();
        try {
            Long startMillis = ReportsUtil.getStartMillisOfEvent(event);
            Long endMillis = ReportsUtil.getEndMillisOfEvent(event);
            sessionMap.put("session",this.constructSessionDetails(startMillis,endMillis,timeFormat,zone));
            sessionMap.put("duration",this.calculateSessionDuration(startMillis,endMillis));
        } catch (TemplateException|IOException e) {
            log.info(" exception in buildSessionContentForMail : " + e.getMessage());
        }
        return sessionMap;
    }

    private Map<String,Object> getModalForPayrollApprovalMail(ContactMailDTO contactMailDTO,String sessionDetails,String totalHours){
        Map<String,Object> modalMap = new HashMap<>();
        modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(),CommonAppProperties.getYoCoDashboardUrl());
        modalMap.put(EmailKeys.USER_NAME.toString(),contactMailDTO.getUserName());
        modalMap.put(EmailKeys.ADMIN_NAME.toString(),contactMailDTO.getAdminFirstName());
        modalMap.put(EmailKeys.SESSION_DETAILS.toString(),sessionDetails);
        modalMap.put(EmailKeys.TOTAL_HOURS.toString(),totalHours);
        modalMap.put(EmailKeys.SUPPORT_MAIL.toString(),contactMailDTO.getPrimaryAdminMailID());
        modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());
        return modalMap;
    }

    private void sendPayrollApprovalMail(Map<String,Object> modalMap, String toEmailID){
        try {
            String subject = EmailUtil.getSubject("Payroll request approved");
            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, toEmailID, "","","");
            String template = TemplateConfig.processAndExtractTemplate(PAYROLL_APPROVAL_MAIL_FTL,modalMap);
            EmailService.getInstance().sendHtmlTemplatedMail(new MailDTO(addressDTO,template,subject));
        } catch (TemplateException|IOException e) {
            log.info(" exception in sendPayrollApprovalMail : " + e.getMessage());
        }
    }

    public void sendApprovalMail(String accountID,Contact userContact, Contact adminContact,String sessionDetails,String totalHours){
        if(InternalUsage.isFullUsAccount(accountID)){
            PeopleRelationJDO primaryPro = UserPROUtil.getPrimaryAdmin(accountID);
            adminContact = ObjUtils.isNull(primaryPro) ? adminContact : primaryPro.getContact();
        }
        ContactMailDTO contactMailDTO = new ContactMailDTO(userContact,adminContact,adminContact);
        Map<String, Object> modalMap= this.getModalForPayrollApprovalMail(contactMailDTO,sessionDetails,totalHours);
        this.sendPayrollApprovalMail(modalMap,userContact.getEmailID());
    }

}