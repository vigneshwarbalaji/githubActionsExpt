package com.yoco.payroll.helper.adminupdate;

import com.yoco.commons.config.TemplateConfig;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.EmailKeys;
import com.yoco.commons.enums.PayrollStatus;
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.services.EmailService;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.EmailUtil;
import com.yoco.commons.utils.events.InAppReminderUtil;
import freemarker.template.TemplateException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

class PayrollAdminUpdateTaskHelperTest {
    @Test
    void handleAdminUpdate_UserProInactive_shouldOnlyCallsaveAdminUpdatedActivityMethod() throws TemplateException, IOException {
        PeopleRelationJDO adminPro = new PeopleRelationJDO();
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setDelete(true);
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        try(MockedConstruction<InAppReminderUtil> inAppReminderUtilMockedConstruction = Mockito.mockConstruction(InAppReminderUtil.class)){
            PayrollAdminUpdateTaskHelper payrollAdminUpdateTaskHelper = Mockito.mock(PayrollAdminUpdateTaskHelper.class);
            Mockito.doCallRealMethod().when(payrollAdminUpdateTaskHelper).handleAdminUpdate(adminPro,userPro,adjustmentDTO);
            payrollAdminUpdateTaskHelper.handleAdminUpdate(adminPro,userPro,adjustmentDTO);
            Assertions.assertEquals(0, inAppReminderUtilMockedConstruction.constructed().size());
            Mockito.verify(payrollAdminUpdateTaskHelper).saveAdminUpdatedActivity(adminPro,userPro,adjustmentDTO);
        }
    }

    @Test
    void handleAdminUpdate_UserProactive_NotFullUsAccount_test() throws TemplateException, IOException {
        PeopleRelationJDO adminPro = new PeopleRelationJDO();
        adminPro.setContact(new Contact());
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("123");
        userPro.setTimeZone("Asia/Kolkata");
        userPro.setDelete(false);
        userPro.setEmailID("email");
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        try(MockedConstruction<InAppReminderUtil> inAppReminderUtilMockedConstruction = Mockito.mockConstruction(InAppReminderUtil.class);
        MockedConstruction<AccountImpl> accountMockedConstruction = Mockito.mockConstruction(AccountImpl.class,(mock,context)->{
            Mockito.when(mock.getById("123")).thenReturn(new SettingsJDO());
        })){
            PayrollAdminUpdateTaskHelper payrollAdminUpdateTaskHelper = Mockito.mock(PayrollAdminUpdateTaskHelper.class);
            Mockito.doCallRealMethod().when(payrollAdminUpdateTaskHelper).handleAdminUpdate(adminPro,userPro,adjustmentDTO);
            Mockito.when(payrollAdminUpdateTaskHelper.generateEmailTemplate(userPro,new Contact(),adjustmentDTO,new SettingsJDO())).thenReturn("template");
            payrollAdminUpdateTaskHelper.handleAdminUpdate(adminPro,userPro,adjustmentDTO);
            Mockito.verify(inAppReminderUtilMockedConstruction.constructed().get(0)).handleInAppNotificationForAdjustment(adjustmentDTO,userPro,"Asia/Kolkata");
            Mockito.verify(payrollAdminUpdateTaskHelper).saveAdminUpdatedActivity(adminPro,userPro,adjustmentDTO);
            Mockito.verify(payrollAdminUpdateTaskHelper).sendAdminUpdateEmail("email","template");
        }
    }

    @Test
    void handleAdminUpdate_UserProactive_FullUsAccount_test() throws TemplateException, IOException {
        PeopleRelationJDO adminPro = new PeopleRelationJDO();
        adminPro.setContact(new Contact());
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("YH0D44");
        userPro.setTimeZone("Asia/Kolkata");
        userPro.setDelete(false);
        userPro.setEmailID("email");
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        try(MockedConstruction<InAppReminderUtil> inAppReminderUtilMockedConstruction = Mockito.mockConstruction(InAppReminderUtil.class);
            MockedConstruction<AccountImpl> accountMockedConstruction = Mockito.mockConstruction(AccountImpl.class,(mock,context)->{
                Mockito.when(mock.getById("YH0D44")).thenReturn(new SettingsJDO());
            });
            MockedConstruction<ContactImpl> contactMockedConstruction = Mockito.mockConstruction(ContactImpl.class,(mock,context)->{
                Mockito.when(mock.getByID(InternalUsage.FULL_US_ADMIN_CONTACT_ID)).thenReturn(new Contact());
            })){
            PayrollAdminUpdateTaskHelper payrollAdminUpdateTaskHelper = Mockito.mock(PayrollAdminUpdateTaskHelper.class);
            Mockito.doCallRealMethod().when(payrollAdminUpdateTaskHelper).handleAdminUpdate(adminPro,userPro,adjustmentDTO);
            Mockito.when(payrollAdminUpdateTaskHelper.generateEmailTemplate(userPro,new Contact(),adjustmentDTO,new SettingsJDO())).thenReturn("template");
            payrollAdminUpdateTaskHelper.handleAdminUpdate(adminPro,userPro,adjustmentDTO);
            Mockito.verify(payrollAdminUpdateTaskHelper).saveAdminUpdatedActivity(adminPro,userPro,adjustmentDTO);
            Mockito.verify(inAppReminderUtilMockedConstruction.constructed().get(0)).handleInAppNotificationForAdjustment(adjustmentDTO,userPro,"Asia/Kolkata");
            Mockito.verify(payrollAdminUpdateTaskHelper).sendAdminUpdateEmail("email","template");
        }
    }

    @Test
    void saveAdminUpdatedActivity_valid_shouldCallSaveActivityMethod(){
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)){
            dateUtilMockedStatic.when(DateUtil::getCurrentTime).thenReturn(1L);
            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            adminPro.setContactId("123");
            adminPro.setUniquepin("accID");
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setContactId("234");
            userPro.setEmailID("email");
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setEntryID("eID");
            new PayrollAdminUpdateTaskHelper().saveAdminUpdatedActivity(adminPro,userPro,adjustmentDTO);
            activityUtilMockedStatic.verify(()->ActivityUtil.saveActivity("accID","234","payroll_activity","email","Admin : 123, status : " + PayrollStatus.ADMIN_UPDATED + ", user : email, entryId : eID","DUMMY",1L));
        }
    }

    @Test
    void generateEmailTemplate_test() throws TemplateException, IOException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        Contact userContact= new Contact();
        userContact.setFirstName("name");
        userPro.setContact(userContact);
        userPro.setTimeZone("Asia/Kolkata");
        Contact adminContact =  new Contact();
        adminContact.setFirstName("adminName");
        adminContact.setEmailID("email");
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        adjustmentDTO.setRequestMessage("message");
        SettingsJDO account = new SettingsJDO();
        account.setDisplayTimeFormat("HHMMSS");
        PayrollAdminUpdateTaskHelper payrollAdminUpdateTaskHelper = Mockito.mock(PayrollAdminUpdateTaskHelper.class);
        Mockito.when(payrollAdminUpdateTaskHelper.generateEmailTemplate(userPro,adminContact,adjustmentDTO,account)).thenCallRealMethod();
        try(MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class);
        MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getYoCoDashboardUrl).thenReturn("url");
            Map<String,Object> expectedModalMap = new HashMap<>();
            expectedModalMap.put(EmailKeys.USER_NAME.toString(),"name");
            expectedModalMap.put(EmailKeys.ADMIN_NAME.toString(),"adminName");
            expectedModalMap.put(EmailKeys.REDIRECT_URL.toString(), "url");
            expectedModalMap.put(EmailKeys.ADMIN_MAIL.toString(),"email");
            expectedModalMap.put(EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear());
            expectedModalMap.put(EmailKeys.MESSAGE.toString(),"message");
            templateConfigMockedStatic.when(()->TemplateConfig.processAndExtractTemplate("adminUpdateMail.ftl",expectedModalMap)).thenReturn("template");
            Assertions.assertEquals("template",payrollAdminUpdateTaskHelper.generateEmailTemplate(userPro,adminContact,adjustmentDTO,account));
            Mockito.verify(payrollAdminUpdateTaskHelper).setAdjustmentTimeDetailsToModalMap(adjustmentDTO,"Asia/Kolkata","HHMMSS",expectedModalMap);
        }
    }

    @Test
    void setAdjustmentTimeDetailsToModalMap_SameInDateOutDate_test(){
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        adjustmentDTO.setAdjustedInTime(1663007400000L);
        adjustmentDTO.setAdjustedOutTime(1663093800000L);
        Map<String,Object> actual = new HashMap<>();
        new PayrollAdminUpdateTaskHelper().setAdjustmentTimeDetailsToModalMap(adjustmentDTO,"Asia/Kolkata","HHMM",actual);
        Assertions.assertEquals(Map.of("IN_DATE","13-Sep-2022","IN_TIME","12:00:00 AM","OUT_TIME","12:00:00 AM(14-Sep-2022)","DURATION","24h 00m"),actual);
    }

    @Test
    void sendAdminUpdateEmail_ExceptionThrown_shouldNotCallsendHtmlTemplatedMailMethod(){
        try(MockedStatic<EmailUtil> emailUtilMockedStatic = Mockito.mockStatic(EmailUtil.class);
            MockedConstruction<MailAddressDTO> mockedConstruction = Mockito.mockConstruction(MailAddressDTO.class)){
            emailUtilMockedStatic.when(()->EmailUtil.getSubject("YoCo - Payroll Confirmed Record Modified")).thenThrow(new IllegalArgumentException("test"));
            new PayrollAdminUpdateTaskHelper().sendAdminUpdateEmail("email","template");
            Assertions.assertEquals(0,mockedConstruction.constructed().size());
        }
    }

    @Test
    void sendAdminUpdateEmail_valid_shouldCallsendHtmlTemplatedMailMethod(){
        try(MockedStatic<EmailUtil> emailUtilMockedStatic = Mockito.mockStatic(EmailUtil.class);
            MockedConstruction<EmailService> emailServiceMockedConstruction = Mockito.mockConstruction(EmailService.class)){
            emailUtilMockedStatic.when(()->EmailUtil.getSubject("YoCo - Payroll Confirmed Record Modified")).thenReturn("subject");
            new PayrollAdminUpdateTaskHelper().sendAdminUpdateEmail("email","template");
            Mockito.verify(emailServiceMockedConstruction.constructed().get(0)).sendHtmlTemplatedMail(new MailDTO(new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "email", "","",""),"template","subject"));
        }
    }
}
