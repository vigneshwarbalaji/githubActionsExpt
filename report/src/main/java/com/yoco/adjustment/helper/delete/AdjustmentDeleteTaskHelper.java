package com.yoco.adjustment.helper.delete;

import com.yoco.adjustment.util.AdjustmentUtil;
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
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.EmailService;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.*;
import com.yoco.commons.utils.events.InAppReminderUtil;
import freemarker.template.TemplateException;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static com.yoco.hours.helper.EntryDeleteHelper.ENTRY_DELETE_CHANNEL_KEY;
import static com.yoco.hours.helper.EntryDeleteHelper.USER_CLOCK_IN_JDO_KEY;

@Slf4j
public class AdjustmentDeleteTaskHelper {
    private AdjustmentDeleteTaskHelper(){}

    private static final String ADJUSTMENT_JDO_KEY = "adjustmentJDO";
    private static final String CONTACT_ID_CHANNEL_KEY = "contactId";
    private static final String DELETE_ADJUSTMENT_KEY = "DELETE_ADJUSTMENT";
    private static final String DELETE_ADJUSTMENT_MAIL_TEMPLATE = "adjustmentDeletedMail.ftl";
    private static final String DELETE_ADJUSTMENT_CONTENT_TEMPLATE = "sessionContentTemplate.ftl";
    public static final String ADJUSTMENT_REJECTED_KEY = "adj_rej";

    public static void handleAdjustmentDeletion(List<ReportsDTO> reportsDTOS, List<AdjustmentDTO> adjustmentDTOS, PeopleRelationJDO loggedInUserPro, PeopleRelationJDO entryContactPro) throws IOException, NoSuchAlgorithmException, TemplateException {
        publishToChannel(reportsDTOS,adjustmentDTOS,entryContactPro);
        if(AdjustmentUtil.isAdjustmentMadeForToday(adjustmentDTOS,entryContactPro.getTimeZone())){
            RTMService.publishToAW(entryContactPro.getUniquepin(),entryContactPro.getContactId(),DELETE_ADJUSTMENT_KEY,null,null);
            new InAppReminderUtil().handleInAppNotificationForReminderTimeChange(new UserDTO(entryContactPro,null), UserPROUtil.extractPROSkills(entryContactPro));
        }
        if(!entryContactPro.getContactId().equals(loggedInUserPro.getContactId()) && UserPROUtil.isUserProActive(entryContactPro)){
            SettingsJDO account = AccountImpl.getAccountImplInstance().getById(entryContactPro.getUniquepin());
            Contact adminContact = loggedInUserPro.getContact();
            if(InternalUsage.isFullUsAccount(loggedInUserPro.getUniquepin())){
                adminContact = ContactImpl.getContactImplInstance().getByID(InternalUsage.FULL_US_ADMIN_CONTACT_ID);
            }
            boolean isRejectAdjustmentRequest = adjustmentDTOS.get(0).hasAdjustmentBeenRejected();
            String emailTemplate = generateEmailTemplate(adjustmentDTOS,entryContactPro,adminContact,account.getDisplayTimeFormat(),isRejectAdjustmentRequest);
            sendAdjustmentDeletedMail(entryContactPro.getEmailID(),emailTemplate,isRejectAdjustmentRequest);
        }
    }

    public static void publishToChannel(List<ReportsDTO> reportsDTOS, List<AdjustmentDTO> adjustmentDTOS, PeopleRelationJDO userPro){
        Map<String,String> channelPublishData = new HashMap<>();
        channelPublishData.put(CONTACT_ID_CHANNEL_KEY,userPro.getContactId());
        for(AdjustmentDTO adjustmentDTO : adjustmentDTOS){
            channelPublishData.put(RTMService.STATUS, Boolean.TRUE.equals(adjustmentDTO.getIsNew()) ? ENTRY_DELETE_CHANNEL_KEY
                    : ADJUSTMENT_REJECTED_KEY);
            ReportsDTO entry = reportsDTOS.stream().filter(reportsDTO -> reportsDTO.getId().equals(adjustmentDTO.getId())).findFirst().orElse(new ReportsDTO());
            if(entry.getId() != null){
                channelPublishData.put(USER_CLOCK_IN_JDO_KEY, JsonUtil.getJson(entry));
            }
            channelPublishData.put(ADJUSTMENT_JDO_KEY, JsonUtil.getJson(adjustmentDTO));
            RTMService.publishToChannel(userPro.getUniquepin(),channelPublishData);
        }
    }

    public static void sendAdjustmentDeletedMail(String userEmail, String template, boolean isRejectAdjustmentRequest){
        try {
            String subject = EmailUtil.getSubject(isRejectAdjustmentRequest ? "Your adjustment has been rejected." : "Your adjustment has been deleted.");
            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, userEmail, "","","");
            EmailService.getInstance().sendHtmlTemplatedMail(new MailDTO(addressDTO,template,subject));
        } catch (Exception e) {
            log.info("Error sending adjustment deleted email :: " + e.getMessage());
        }
    }

    public static String generateEmailTemplate(List<AdjustmentDTO> adjustmentDTOS, PeopleRelationJDO userPro, Contact adminContact, String displayTimeFormat, boolean isRejectAdjustmentRequest) throws TemplateException, IOException {
        Map<String,Object> modalMap = new HashMap<>();
        modalMap.put(EmailKeys.USER_NAME.toString(),userPro.getContact().getFirstName());
        modalMap.put(EmailKeys.ADMIN_NAME.toString(),adminContact.getFirstName());
        modalMap.put(EmailKeys.REDIRECT_URL.toString(), CommonAppProperties.getYoCoDashboardUrl());
        modalMap.put(EmailKeys.ADMIN_MAIL.toString(),adminContact.getEmailID());
        modalMap.put(EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear());
        modalMap.put(EmailKeys.ADJUSTMENTS_DELETED_CONTENT.toString(),generateAdjustmentsDeletedContentTemplate(adjustmentDTOS,userPro.getTimeZone(),displayTimeFormat));
        if(isRejectAdjustmentRequest){
            modalMap.put(EmailKeys.ACTION.toString(),"rejected");
            modalMap.put(EmailKeys.MESSAGE.toString()," for \"" +adjustmentDTOS.get(0).getRejectMessage() + "\" as reason");
        }else{
            modalMap.put(EmailKeys.ACTION.toString(),"deleted");
            modalMap.put(EmailKeys.MESSAGE.toString(),"");
        }
        return TemplateConfig.processAndExtractTemplate(DELETE_ADJUSTMENT_MAIL_TEMPLATE,modalMap);
    }

    public static String generateAdjustmentsDeletedContentTemplate(List<AdjustmentDTO> adjustmentDTOS, String timeZone, String displayTimeFormat) throws TemplateException, IOException {
        StringBuilder builder = new StringBuilder();
        HashMap<String,Object> modalMap = new HashMap<>();
        for(AdjustmentDTO adjustmentDTO : adjustmentDTOS){
            String inDate = DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY,adjustmentDTO.getAdjustedInTime(),timeZone);
            modalMap.put(EmailKeys.IN_DATE.toString(),inDate);
            modalMap.put(EmailKeys.IN_TIME.toString(),DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A,adjustmentDTO.getAdjustedInTime(),timeZone));
            String outTime = DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A,adjustmentDTO.getAdjustedOutTime(),timeZone);
            String outDate = DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY,adjustmentDTO.getAdjustedOutTime(),timeZone);
            if(!outDate.equalsIgnoreCase(inDate)){
                outTime = outTime.concat("("+outDate+")");
            }
            modalMap.put(EmailKeys.OUT_TIME.toString(),outTime);
            modalMap.put(EmailKeys.DURATION.toString(),DateUtil.getTimeFormatAsPerCompany(adjustmentDTO.getAdjustedOutTime() - adjustmentDTO.getAdjustedInTime(),displayTimeFormat));
            builder.append(TemplateConfig.processAndExtractTemplate(DELETE_ADJUSTMENT_CONTENT_TEMPLATE,modalMap));
        }
        return builder.toString();
    }
}
