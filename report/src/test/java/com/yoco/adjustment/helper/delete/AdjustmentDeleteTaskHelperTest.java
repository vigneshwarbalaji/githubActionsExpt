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
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.EmailService;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.EmailUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.events.InAppReminderUtil;
import freemarker.template.TemplateException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;

class AdjustmentDeleteTaskHelperTest {
    @Test
    void handleAdjustmentDeletion_adjustmentNotMadeForToday_adjustmentDeletedBySameUser_onlyChannelPublishShouldBeDone() throws TemplateException, IOException, NoSuchAlgorithmException {
        PeopleRelationJDO entryContactPro = new PeopleRelationJDO();
        entryContactPro.setTimeZone("Asia/Kolkata");
        entryContactPro.setContactId("234");
        PeopleRelationJDO loggedInUserPro = new PeopleRelationJDO();
        loggedInUserPro.setContactId("234");
        try(MockedStatic<AdjustmentDeleteTaskHelper> adjustmentDeleteTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteTaskHelper.class);
            MockedStatic<AdjustmentUtil> adjustmentUtilMockedStatic = Mockito.mockStatic(AdjustmentUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            adjustmentUtilMockedStatic.when(()->AdjustmentUtil.isAdjustmentMadeForToday(List.of(),"Asia/Kolkata")).thenReturn(false);
            adjustmentDeleteTaskHelperMockedStatic.when(()->AdjustmentDeleteTaskHelper.handleAdjustmentDeletion(List.of(),List.of(),loggedInUserPro,entryContactPro)).thenCallRealMethod();
            AdjustmentDeleteTaskHelper.handleAdjustmentDeletion(List.of(),List.of(),loggedInUserPro,entryContactPro);
            adjustmentDeleteTaskHelperMockedStatic.verify(()->AdjustmentDeleteTaskHelper.publishToChannel(List.of(),List.of(),entryContactPro));
            rtmServiceMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleAdjustmentDeletion_adjustmentMadeForToday_adjustmentDeletedBySameUser_ChannelPublishAwPublishAndReminderResetShouldBeDone() throws TemplateException, IOException, NoSuchAlgorithmException {
        PeopleRelationJDO entryContactPro = new PeopleRelationJDO();
        entryContactPro.setTimeZone("Asia/Kolkata");
        entryContactPro.setContactId("234");
        entryContactPro.setUniquepin("accID");
        PeopleRelationJDO loggedInUserPro = new PeopleRelationJDO();
        loggedInUserPro.setContactId("234");
        try(MockedStatic<AdjustmentDeleteTaskHelper> adjustmentDeleteTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteTaskHelper.class);
            MockedStatic<AdjustmentUtil> adjustmentUtilMockedStatic = Mockito.mockStatic(AdjustmentUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedConstruction<InAppReminderUtil> inAppReminderUtilMockedConstruction = Mockito.mockConstruction(InAppReminderUtil.class);
            MockedConstruction<UserDTO> userDTOMockedConstruction = Mockito.mockConstruction(UserDTO.class);){
            adjustmentUtilMockedStatic.when(()->AdjustmentUtil.isAdjustmentMadeForToday(List.of(),"Asia/Kolkata")).thenReturn(true);
            adjustmentDeleteTaskHelperMockedStatic.when(()->AdjustmentDeleteTaskHelper.handleAdjustmentDeletion(List.of(),List.of(),loggedInUserPro,entryContactPro)).thenCallRealMethod();
            AdjustmentDeleteTaskHelper.handleAdjustmentDeletion(List.of(),List.of(),loggedInUserPro,entryContactPro);
            adjustmentDeleteTaskHelperMockedStatic.verify(()->AdjustmentDeleteTaskHelper.publishToChannel(List.of(),List.of(),entryContactPro));
            rtmServiceMockedStatic.verify(()->RTMService.publishToAW("accID","234","DELETE_ADJUSTMENT",null,null));
            Mockito.verify(inAppReminderUtilMockedConstruction.constructed().get(0)).handleInAppNotificationForReminderTimeChange(userDTOMockedConstruction.constructed().get(0),null );
        }
    }

    @Test
    void handleAdjustmentDeletion_adjustmentNotMadeForToday_userProNotActive_adjustmentDeletedByDifferentUser_ChannelPublishShouldBeDone() throws TemplateException, IOException, NoSuchAlgorithmException {
        PeopleRelationJDO entryContactPro = new PeopleRelationJDO();
        entryContactPro.setTimeZone("Asia/Kolkata");
        entryContactPro.setContactId("234");
        entryContactPro.setDelete(true);
        PeopleRelationJDO loggedInUserPro = new PeopleRelationJDO();
        loggedInUserPro.setContactId("123");
        try(MockedStatic<AdjustmentDeleteTaskHelper> adjustmentDeleteTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteTaskHelper.class);
            MockedStatic<AdjustmentUtil> adjustmentUtilMockedStatic = Mockito.mockStatic(AdjustmentUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            adjustmentUtilMockedStatic.when(()->AdjustmentUtil.isAdjustmentMadeForToday(List.of(),"Asia/Kolkata")).thenReturn(false);
            adjustmentDeleteTaskHelperMockedStatic.when(()->AdjustmentDeleteTaskHelper.handleAdjustmentDeletion(List.of(),List.of(),loggedInUserPro,entryContactPro)).thenCallRealMethod();
            AdjustmentDeleteTaskHelper.handleAdjustmentDeletion(List.of(),List.of(),loggedInUserPro,entryContactPro);
            adjustmentDeleteTaskHelperMockedStatic.verify(()->AdjustmentDeleteTaskHelper.publishToChannel(List.of(),List.of(),entryContactPro));
            rtmServiceMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleAdjustmentDeletion_adjustmentNotMadeForToday_userProActive_adjustmentDeletedByDifferentUser_ChannelPublishEmailSendingShouldBeDone() throws TemplateException, IOException, NoSuchAlgorithmException {
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        adjustmentDTO.setStatus("REJECTED");
        PeopleRelationJDO entryContactPro = new PeopleRelationJDO();
        entryContactPro.setTimeZone("Asia/Kolkata");
        entryContactPro.setContactId("234");
        entryContactPro.setDelete(false);
        entryContactPro.setUniquepin("accID");
        entryContactPro.setEmailID("email");
        PeopleRelationJDO loggedInUserPro = new PeopleRelationJDO();
        loggedInUserPro.setContactId("123");
        loggedInUserPro.setContact(new Contact());
        try(MockedStatic<AdjustmentDeleteTaskHelper> adjustmentDeleteTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteTaskHelper.class);
            MockedStatic<AdjustmentUtil> adjustmentUtilMockedStatic = Mockito.mockStatic(AdjustmentUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO account = new SettingsJDO();
            account.setDisplayTimeFormat("HHMM");
            Mockito.when(accountImplMock.getById("accID")).thenReturn(account);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            adjustmentUtilMockedStatic.when(()->AdjustmentUtil.isAdjustmentMadeForToday(List.of(adjustmentDTO),"Asia/Kolkata")).thenReturn(false);
            adjustmentDeleteTaskHelperMockedStatic.when(()->AdjustmentDeleteTaskHelper.handleAdjustmentDeletion(List.of(),List.of(adjustmentDTO),loggedInUserPro,entryContactPro)).thenCallRealMethod();
            adjustmentDeleteTaskHelperMockedStatic.when(()->AdjustmentDeleteTaskHelper.generateEmailTemplate(List.of(adjustmentDTO),entryContactPro,new Contact(),"HHMM",true)).thenReturn("template");
            AdjustmentDeleteTaskHelper.handleAdjustmentDeletion(List.of(),List.of(adjustmentDTO),loggedInUserPro,entryContactPro);
            adjustmentDeleteTaskHelperMockedStatic.verify(()->AdjustmentDeleteTaskHelper.publishToChannel(List.of(),List.of(adjustmentDTO),entryContactPro));
            adjustmentDeleteTaskHelperMockedStatic.verify(()->AdjustmentDeleteTaskHelper.sendAdjustmentDeletedMail("email","template",true));
            rtmServiceMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleAdjustmentDeletion_adjustmentNotMadeForToday_AccountFullUs_userProActive_adjustmentDeletedByDifferentUser_ChannelPublishEmailSendingWithPrimaryAdminProShouldBeDone() throws TemplateException, IOException, NoSuchAlgorithmException {
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        adjustmentDTO.setStatus("REJECTED");
        PeopleRelationJDO entryContactPro = new PeopleRelationJDO();
        entryContactPro.setTimeZone("Asia/Kolkata");
        entryContactPro.setContactId("234");
        entryContactPro.setDelete(false);
        entryContactPro.setUniquepin("YH0D44");
        entryContactPro.setEmailID("email");
        PeopleRelationJDO loggedInUserPro = new PeopleRelationJDO();
        loggedInUserPro.setContactId("123");
        loggedInUserPro.setUniquepin("YH0D44");
        try(MockedStatic<AdjustmentDeleteTaskHelper> adjustmentDeleteTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteTaskHelper.class);
            MockedStatic<AdjustmentUtil> adjustmentUtilMockedStatic = Mockito.mockStatic(AdjustmentUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getByID(InternalUsage.FULL_US_ADMIN_CONTACT_ID)).thenReturn(new Contact());
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO account = new SettingsJDO();
            account.setDisplayTimeFormat("HHMM");
            Mockito.when(accountImplMock.getById("YH0D44")).thenReturn(account);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            adjustmentUtilMockedStatic.when(()->AdjustmentUtil.isAdjustmentMadeForToday(List.of(adjustmentDTO),"Asia/Kolkata")).thenReturn(false);
            adjustmentDeleteTaskHelperMockedStatic.when(()->AdjustmentDeleteTaskHelper.handleAdjustmentDeletion(List.of(),List.of(adjustmentDTO),loggedInUserPro,entryContactPro)).thenCallRealMethod();
            adjustmentDeleteTaskHelperMockedStatic.when(()->AdjustmentDeleteTaskHelper.generateEmailTemplate(List.of(adjustmentDTO),entryContactPro,new Contact(),"HHMM",true)).thenReturn("template");
            AdjustmentDeleteTaskHelper.handleAdjustmentDeletion(List.of(),List.of(adjustmentDTO),loggedInUserPro,entryContactPro);
            adjustmentDeleteTaskHelperMockedStatic.verify(()->AdjustmentDeleteTaskHelper.publishToChannel(List.of(),List.of(adjustmentDTO),entryContactPro));
            adjustmentDeleteTaskHelperMockedStatic.verify(()->AdjustmentDeleteTaskHelper.sendAdjustmentDeletedMail("email","template",true));
            rtmServiceMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void publishToChannel_oneNewAdjustment_shouldCallPublishMethodOnceWithOneAdjustmentDtoInData(){
        AdjustmentDTO adjustmentDTO1 = new AdjustmentDTO();
        adjustmentDTO1.setIsNew(true);
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        userPro.setContactId("123");
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            AdjustmentDeleteTaskHelper.publishToChannel(List.of(),List.of(adjustmentDTO1),userPro);
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID", Map.of("contactId","123","status","originalEntryOrNewAdjDel","adjustmentJDO", JsonUtil.getJson(adjustmentDTO1))));
        }
    }

    @Test
    void publishToChannel_OneEditAdjustment_shouldCallPublishMethodOnceWithOneAdjustmentDtoAndOneReportDtoInData(){
        ReportsDTO reportsDTO = new ReportsDTO();
        reportsDTO.setId("pID");
        AdjustmentDTO adjustmentDTO2 = new AdjustmentDTO();
        adjustmentDTO2.setIsNew(false);
        adjustmentDTO2.setId("pID");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        userPro.setContactId("123");
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            AdjustmentDeleteTaskHelper.publishToChannel(List.of(reportsDTO),List.of(adjustmentDTO2),userPro);
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID", Map.of("contactId","123","status","adj_rej","userClockInJDO",JsonUtil.getJson(reportsDTO),"adjustmentJDO", JsonUtil.getJson(adjustmentDTO2))));
        }
    }

    @Test
    void sendAdjustmentDeletedMail_exceptionThrown_exceptionMustBeCaught(){
        try(MockedStatic<EmailUtil> emailUtilMockedStatic = Mockito.mockStatic(EmailUtil.class)){
            emailUtilMockedStatic.when(()->EmailUtil.getSubject("Your adjustment has been deleted.")).thenThrow(new IllegalArgumentException("test"));
            Assertions.assertDoesNotThrow(()->AdjustmentDeleteTaskHelper.sendAdjustmentDeletedMail("email","template",false));
        }
    }

    @Test
    void sendAdjustmentDeletedMail_rejectAdjustmentTrue_sendHtmlTemplatedMailShouldBeCalledWithRejectedSubject(){
        try(MockedStatic<EmailUtil> emailUtilMockedStatic = Mockito.mockStatic(EmailUtil.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)){
            EmailService emailServiceMock = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailServiceMock);
            emailUtilMockedStatic.when(()->EmailUtil.getSubject("Your adjustment has been rejected.")).thenReturn("subject");
            Assertions.assertDoesNotThrow(()->AdjustmentDeleteTaskHelper.sendAdjustmentDeletedMail("email","template",true));
            Mockito.verify(emailServiceMock).sendHtmlTemplatedMail(new MailDTO(new MailAddressDTO("support@yocoboard.com","YoCoBoard","email","","",""),"template","subject"));
        }
    }

    @Test
    void sendAdjustmentDeletedMail_rejectAdjustmentFalse_sendHtmlTemplatedMailShouldBeCalledWithRejectedSubject(){
        try(MockedStatic<EmailUtil> emailUtilMockedStatic = Mockito.mockStatic(EmailUtil.class);
            MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)){
            EmailService emailServiceMock = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailServiceMock);
            emailUtilMockedStatic.when(()->EmailUtil.getSubject("Your adjustment has been deleted.")).thenReturn("subject");
            Assertions.assertDoesNotThrow(()->AdjustmentDeleteTaskHelper.sendAdjustmentDeletedMail("email","template",false));
            Mockito.verify(emailServiceMock).sendHtmlTemplatedMail(new MailDTO(new MailAddressDTO("support@yocoboard.com","YoCoBoard","email","","",""),"template","subject"));
        }
    }

    @Test
    void generateEmailTemplate_IsRejectRequestFalse_shouldCallprocessAndExtractTemplateMethod() throws TemplateException, IOException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        Contact contact = new Contact();
        contact.setFirstName("name");
        Contact adminContact = new Contact();
        adminContact.setFirstName("admin");
        adminContact.setEmailID("email");
        userPro.setContact(contact);
        userPro.setTimeZone("Asia/Kolkata");
        try(MockedStatic<AdjustmentDeleteTaskHelper> adjustmentDeleteTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteTaskHelper.class);
            MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getYoCoDashboardUrl).thenReturn("url");
            adjustmentDeleteTaskHelperMockedStatic.when(()->AdjustmentDeleteTaskHelper.generateEmailTemplate(List.of(),userPro,adminContact,"HHMM",false)).thenCallRealMethod();
            adjustmentDeleteTaskHelperMockedStatic.when(()->AdjustmentDeleteTaskHelper.generateAdjustmentsDeletedContentTemplate(List.of(),"Asia/Kolkata","HHMM")).thenReturn("content");
            AdjustmentDeleteTaskHelper.generateEmailTemplate(List.of(),userPro,adminContact,"HHMM",false);
            templateConfigMockedStatic.verify(()->TemplateConfig.processAndExtractTemplate("adjustmentDeletedMail.ftl",Map.of("USER_NAME","name","ADMIN_NAME","admin","REDIRECT_URL","url","ADMIN_MAIL","email","CURRENT_YEAR", DateUtil.getCurrentYear(),"ADJUSTMENTS_DELETED_CONTENT","content","ACTION","deleted","MESSAGE","")));
        }
    }

    @Test
    void generateEmailTemplate_IsRejectRequestTrue_shouldCallProcessAndExtractTemplateMethodWithReasonText() throws TemplateException, IOException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        Contact contact = new Contact();
        contact.setFirstName("name");
        Contact adminContact = new Contact();
        adminContact.setFirstName("admin");
        adminContact.setEmailID("email");
        userPro.setContact(contact);
        userPro.setTimeZone("Asia/Kolkata");
        try(MockedStatic<AdjustmentDeleteTaskHelper> adjustmentDeleteTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteTaskHelper.class);
            MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getYoCoDashboardUrl).thenReturn("url");
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setRejectMessage("test");
            adjustmentDeleteTaskHelperMockedStatic.when(()->AdjustmentDeleteTaskHelper.generateEmailTemplate(List.of(adjustmentDTO),userPro,adminContact,"HHMM",true)).thenCallRealMethod();
            adjustmentDeleteTaskHelperMockedStatic.when(()->AdjustmentDeleteTaskHelper.generateAdjustmentsDeletedContentTemplate(List.of(adjustmentDTO),"Asia/Kolkata","HHMM")).thenReturn("content");
            AdjustmentDeleteTaskHelper.generateEmailTemplate(List.of(adjustmentDTO),userPro,adminContact,"HHMM",true);
            templateConfigMockedStatic.verify(()->TemplateConfig.processAndExtractTemplate("adjustmentDeletedMail.ftl",Map.of("USER_NAME","name","ADMIN_NAME","admin","REDIRECT_URL","url","ADMIN_MAIL","email","CURRENT_YEAR", DateUtil.getCurrentYear(),"ADJUSTMENTS_DELETED_CONTENT","content","ACTION","rejected","MESSAGE"," for \"test\" as reason")));
        }
    }


    @Test
    void generateAdjustmentsDeletedContentTemplate_adjustmentWithSameDateInTimeAndOutTime_shouldCallProcessAndExtractTemplateOnce() throws TemplateException, IOException {
        try(MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class);){
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1658847600000L);
            adjustmentDTO.setAdjustedOutTime(1658851200000L);
            AdjustmentDeleteTaskHelper.generateAdjustmentsDeletedContentTemplate(List.of(adjustmentDTO),"Asia/Kolkata","HH MM");
            templateConfigMockedStatic.verify(()->TemplateConfig.processAndExtractTemplate("sessionContentTemplate.ftl",Map.of("IN_DATE","26-Jul-2022","IN_TIME","08:30:00 PM","OUT_TIME","09:30:00 PM","DURATION","1h 00m")));
        }
    }

    @Test
    void generateAdjustmentsDeletedContentTemplate_adjustmentWithDifferentDateInTimeAndOutTime_shouldCallProcessAndExtractTemplateOnce() throws TemplateException, IOException {
        try(MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class);){
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1658847600000L);
            adjustmentDTO.setAdjustedOutTime(1658860200000L);
            AdjustmentDeleteTaskHelper.generateAdjustmentsDeletedContentTemplate(List.of(adjustmentDTO),"Asia/Kolkata","HH MM");
            templateConfigMockedStatic.verify(()->TemplateConfig.processAndExtractTemplate("sessionContentTemplate.ftl",Map.of("IN_DATE","26-Jul-2022","IN_TIME","08:30:00 PM","OUT_TIME","12:00:00 AM(27-Jul-2022)","DURATION","3h 30m")));
        }
    }
}
