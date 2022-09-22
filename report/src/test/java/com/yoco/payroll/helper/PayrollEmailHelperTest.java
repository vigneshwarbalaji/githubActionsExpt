package com.yoco.payroll.helper;

import com.yoco.commons.config.TemplateConfig;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.DateConstants;
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
import freemarker.template.TemplateException;
import freemarker.template.TemplateNotFoundException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static com.yoco.commons.constants.SchedulingKeys.END_TIME;
import static com.yoco.commons.constants.SchedulingKeys.START_TIME;
import static org.mockito.ArgumentMatchers.*;

class PayrollEmailHelperTest {

    PayrollEmailHelper payrollEmailHelper = PayrollEmailHelper.getInstance();

    @Test
    void buildSessionContentForMail_valid_test() throws TemplateException, IOException {
        Map<String, Object> event = new HashMap<>();
        event.put(START_TIME,1660220974000l);
        event.put(END_TIME,1660222783000l);
        Map<String,Object> response = payrollEmailHelper.buildSessionContentForMail(event,DateConstants.DEFAULT_TIME_FORMAT,DateConstants.ZONE_ID_IST);
        Map<String,Object> modalMap = new HashMap<>();
        modalMap.put(EmailKeys.IN_DATE.toString(), DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY,1660220974000l,DateConstants.ZONE_ID_IST));
        modalMap.put(EmailKeys.IN_TIME.toString(),DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A,1660220974000l, DateConstants.ZONE_ID_IST));
        modalMap.put(EmailKeys.OUT_TIME.toString(),DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A,1660222783000l,DateConstants.ZONE_ID_IST));
        modalMap.put(EmailKeys.DURATION.toString(),DateUtil.getTimeFormatAsPerCompany(1660222783000l-1660220974000l,DateConstants.DEFAULT_TIME_FORMAT));
        Assertions.assertEquals(TemplateConfig.processAndExtractTemplate("sessionContentTemplate.ftl", modalMap),response.get("session"));
        Assertions.assertEquals(1660222783000l-1660220974000l,response.get("duration"));
    }

    @Test
    void buildSessionContentForMail_templateException_test(){
        try(MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class)){
            templateConfigMockedStatic.when(()-> TemplateConfig.processAndExtractTemplate(anyString(),anyMap())).thenThrow(new TemplateNotFoundException("exception",null,"templateNotFound"));
            Map<String, Object> event = new HashMap<>();
            event.put(START_TIME,1660220974000l);
            event.put(END_TIME,1660222783000l);
            Map<String,Object> response = payrollEmailHelper.buildSessionContentForMail(event,DateConstants.DEFAULT_TIME_FORMAT,DateConstants.ZONE_ID_IST);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        }
    }

    @Test
    void buildSessionContentForMail_IOException_test(){
        try(MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class)){
            templateConfigMockedStatic.when(()-> TemplateConfig.processAndExtractTemplate(anyString(),anyMap())).thenThrow(new IOException("exception"));
            Map<String, Object> event = new HashMap<>();
            event.put(START_TIME,1660220974000l);
            event.put(END_TIME,1660222783000l);
            Map<String,Object> response = payrollEmailHelper.buildSessionContentForMail(event,DateConstants.DEFAULT_TIME_FORMAT,DateConstants.ZONE_ID_IST);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        }
    }

    @Test
    void sendApprovalMail_valid_test() throws TemplateException, IOException {
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");
            payrollEmailHelper.sendApprovalMail("accId",contact,contact,"<tr>sessionTimeDetails</tr>","3 hours 00 minutes");

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(),CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.USER_NAME.toString(), "fName lName");
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"fName");
            modalMap.put(EmailKeys.SESSION_DETAILS.toString(),"<tr>sessionTimeDetails</tr>");
            modalMap.put(EmailKeys.TOTAL_HOURS.toString(),"3 hours 00 minutes");
            modalMap.put(EmailKeys.SUPPORT_MAIL.toString(), "test@gmail.com");
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("payrollApprovalMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Payroll request approved (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void sendApprovalMail_full_test() throws TemplateException, IOException {
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");

            PeopleRelationJDO primaryAdminPro = new PeopleRelationJDO();
            primaryAdminPro.setContact(contact);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(primaryAdminPro);

            payrollEmailHelper.sendApprovalMail(InternalUsage.FULL,contact,contact,"<tr>sessionTimeDetails</tr>","3 hours 00 minutes");

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(),CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.USER_NAME.toString(), "fName lName");
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"fName");
            modalMap.put(EmailKeys.SESSION_DETAILS.toString(),"<tr>sessionTimeDetails</tr>");
            modalMap.put(EmailKeys.TOTAL_HOURS.toString(),"3 hours 00 minutes");
            modalMap.put(EmailKeys.SUPPORT_MAIL.toString(), "test@gmail.com");
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("payrollApprovalMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Payroll request approved (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void sendApprovalMail_full_null_test() throws TemplateException, IOException {
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");

            userPROUtilMockedStatic.when(()-> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(null);

            payrollEmailHelper.sendApprovalMail(InternalUsage.FULL,contact,contact,"<tr>sessionTimeDetails</tr>","3 hours 00 minutes");

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(),CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.USER_NAME.toString(), "fName lName");
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"fName");
            modalMap.put(EmailKeys.SESSION_DETAILS.toString(),"<tr>sessionTimeDetails</tr>");
            modalMap.put(EmailKeys.TOTAL_HOURS.toString(),"3 hours 00 minutes");
            modalMap.put(EmailKeys.SUPPORT_MAIL.toString(), "test@gmail.com");
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("payrollApprovalMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Payroll request approved (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void sendApprovalMail_TemplateException_test(){
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            templateConfigMockedStatic.when(()-> TemplateConfig.processAndExtractTemplate(anyString(),anyMap())).thenThrow(new TemplateNotFoundException("exception",null,"templateNotFound"));

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");

            payrollEmailHelper.sendApprovalMail("accId",contact,contact,"<tr>sessionTimeDetails</tr>","3 hours 00 minutes");

            Mockito.verify(emailService,Mockito.times(0)).sendHtmlTemplatedMail(any());
        }
    }

    @Test
    void sendApprovalMail_IOException_test(){
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            templateConfigMockedStatic.when(()-> TemplateConfig.processAndExtractTemplate(anyString(),anyMap())).thenThrow(new IOException("exception"));

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");

            payrollEmailHelper.sendApprovalMail("accId",contact,contact,"<tr>sessionTimeDetails</tr>","3 hours 00 minutes");

            Mockito.verify(emailService,Mockito.times(0)).sendHtmlTemplatedMail(any());
        }
    }

}