package com.yoco.payroll.helper.adminupdate;

import com.yoco.commons.config.TemplateConfig;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.EmailKeys;
import com.yoco.commons.enums.PayrollStatus;
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.services.EmailService;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.EmailUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.InAppReminderUtil;
import com.yoco.payroll.helper.PayrollTaskHelper;
import freemarker.template.TemplateException;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public class PayrollAdminUpdateTaskHelper {
    private static final String ADMIN_UPDATE_MAIL_TEMPLATE = "adminUpdateMail.ftl";

    public void handleAdminUpdate(PeopleRelationJDO adminPro, PeopleRelationJDO userPro, AdjustmentDTO adjustmentDTO) throws TemplateException, IOException {
        saveAdminUpdatedActivity(adminPro,userPro,adjustmentDTO);
        if(UserPROUtil.isUserProActive(userPro)){
            new InAppReminderUtil().handleInAppNotificationForAdjustment(adjustmentDTO,userPro, userPro.getTimeZone());
            SettingsJDO account = new AccountImpl().getById(userPro.getUniquepin());
            Contact adminContact = adminPro.getContact();
            if(InternalUsage.isFullUsAccount(userPro.getUniquepin())){
                adminContact = new ContactImpl().getByID(InternalUsage.FULL_US_ADMIN_CONTACT_ID);
            }
            sendAdminUpdateEmail(userPro.getEmailID(),generateEmailTemplate(userPro,adminContact,adjustmentDTO, account));
        }
    }

    public void saveAdminUpdatedActivity(PeopleRelationJDO adminPro,PeopleRelationJDO userPro,AdjustmentDTO adjustmentDTO){
        String activityMessage = "Admin : " + adminPro.getContactId() + ", status : " + PayrollStatus.ADMIN_UPDATED + ", user : " + userPro.getEmailID() + ", entryId : " + adjustmentDTO.getEntryID();
        ActivityUtil.saveActivity(adminPro.getUniquepin(), userPro.getContactId(), PayrollTaskHelper.PAYROLL_ACTIVITY, userPro.getEmailID(), activityMessage, ActivityUtil.ACTIVITIES.DUMMY.value(), DateUtil.getCurrentTime());
    }

    public String generateEmailTemplate(PeopleRelationJDO userPro, Contact adminContact, AdjustmentDTO adjustment, SettingsJDO account) throws TemplateException, IOException {
        Map<String,Object> modalMap = new HashMap<>();
        modalMap.put(EmailKeys.USER_NAME.toString(),userPro.getContact().getFirstName());
        modalMap.put(EmailKeys.ADMIN_NAME.toString(),adminContact.getFirstName());
        modalMap.put(EmailKeys.REDIRECT_URL.toString(), CommonAppProperties.getYoCoDashboardUrl());
        modalMap.put(EmailKeys.ADMIN_MAIL.toString(),adminContact.getEmailID());
        modalMap.put(EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear());
        modalMap.put(EmailKeys.MESSAGE.toString(),adjustment.getRequestMessage());
        setAdjustmentTimeDetailsToModalMap(adjustment, userPro.getTimeZone(), account.getDisplayTimeFormat(), modalMap);
        return TemplateConfig.processAndExtractTemplate(ADMIN_UPDATE_MAIL_TEMPLATE,modalMap);
    }

    public void setAdjustmentTimeDetailsToModalMap(AdjustmentDTO adjustment, String timeZone, String displayTimeFormat, Map<String,Object> modalMap){
        String inDate = DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY,adjustment.getAdjustedInTime(),timeZone);
        modalMap.put(EmailKeys.IN_DATE.toString(),inDate);
        modalMap.put(EmailKeys.IN_TIME.toString(),DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A,adjustment.getAdjustedInTime(),timeZone));
        String outTime = DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A,adjustment.getAdjustedOutTime(),timeZone);
        String outDate = DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY,adjustment.getAdjustedOutTime(),timeZone);
        if(!outDate.equalsIgnoreCase(inDate)){
            outTime = outTime.concat("("+outDate+")");
        }
        modalMap.put(EmailKeys.OUT_TIME.toString(),outTime);
        modalMap.put(EmailKeys.DURATION.toString(),DateUtil.getTimeFormatAsPerCompany(adjustment.getAdjustedOutTime() - adjustment.getAdjustedInTime(),displayTimeFormat));
    }

    public void sendAdminUpdateEmail(String userEmail, String template){
        try {
            String subject = EmailUtil.getSubject( "YoCo - Payroll Confirmed Record Modified");
            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, userEmail, "","","");
            new EmailService().sendHtmlTemplatedMail(new MailDTO(addressDTO,template,subject));
        } catch (Exception e) {
            log.info("Error sending adjustment deleted email :: " + e.getMessage());
        }
    }
}