package com.yoco.adjustment.helper.approve;

import com.yoco.commons.config.TemplateConfig;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.EmailKeys;
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.services.EmailService;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.EmailUtil;
import com.yoco.commons.utils.UserPROUtil;
import freemarker.template.TemplateException;
import freemarker.template.TemplateNotFoundException;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;

class ApproveAdjustmentEmailHelperTest {

    ApproveAdjustmentEmailHelper approveAdjustmentEmailHelper = ApproveAdjustmentEmailHelper.getInstance();

    @Test
    void adjustmentApprovedMailHandler_test() throws TemplateException, IOException {
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1657789367000L);
            adjustmentDTO.setAdjustedOutTime(1657798478000L);
            adjustmentDTO.setOriginalInTime(1657789367000L);
            adjustmentDTO.setOriginalOutTime(1657789368000L);
            adjustmentDTO.setRequestMessage("Others - test");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            account.setDisplayTimeFormat(DateConstants.DEFAULT_TIME_FORMAT);
            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");
            approveAdjustmentEmailHelper.adjustmentApprovedMailHandler(contact,contact,DateConstants.ZONE_ID_IST,adjustmentDTO,account);

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.USER_NAME.toString(), "fName lName");
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"fName lName");
            modalMap.put(EmailKeys.ADMIN_MAIL.toString(),"test@gmail.com");
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(),CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.MESSAGE.toString(),"test");
            modalMap.put(EmailKeys.IN_DATE.toString(),"14-Jul-2022");
            modalMap.put(EmailKeys.IN_TIME.toString(), "02:32:47 PM");
            modalMap.put(EmailKeys.OUT_TIME.toString(), "05:04:38 PM");
            modalMap.put(EmailKeys.DURATION.toString(),"2h 31m");
            modalMap.put(EmailKeys.REDIRECT_URL.toString(), CommonAppProperties.getYoCoDashboardUrl() + "hours");
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("adjustmentApprovedMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Your adjustment is approved. (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void adjustmentApprovedMailHandler_full_test() throws TemplateException, IOException {
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1657789367000L);
            adjustmentDTO.setAdjustedOutTime(1657798478000L);
            adjustmentDTO.setOriginalInTime(1657789367000L);
            adjustmentDTO.setOriginalOutTime(1657789368000L);
            adjustmentDTO.setRequestMessage("Others - test");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin(InternalUsage.FULL);
            account.setDisplayTimeFormat(DateConstants.DEFAULT_TIME_FORMAT);
            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");

            PeopleRelationJDO primaryAdminPro = new PeopleRelationJDO();
            primaryAdminPro.setContact(contact);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(primaryAdminPro);

            approveAdjustmentEmailHelper.adjustmentApprovedMailHandler(contact,contact,DateConstants.ZONE_ID_IST,adjustmentDTO,account);

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.USER_NAME.toString(), "fName lName");
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"fName lName");
            modalMap.put(EmailKeys.ADMIN_MAIL.toString(),"test@gmail.com");
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(),CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.MESSAGE.toString(),"test");
            modalMap.put(EmailKeys.IN_DATE.toString(),"14-Jul-2022");
            modalMap.put(EmailKeys.IN_TIME.toString(), "02:32:47 PM");
            modalMap.put(EmailKeys.OUT_TIME.toString(), "05:04:38 PM");
            modalMap.put(EmailKeys.DURATION.toString(),"2h 31m");
            modalMap.put(EmailKeys.REDIRECT_URL.toString(), CommonAppProperties.getYoCoDashboardUrl() + "hours");
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("adjustmentApprovedMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Your adjustment is approved. (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void adjustmentApprovedMailHandler_full_null_test() throws TemplateException, IOException {
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1657789367000L);
            adjustmentDTO.setAdjustedOutTime(1657798478000L);
            adjustmentDTO.setOriginalInTime(1657789367000L);
            adjustmentDTO.setOriginalOutTime(1657789368000L);
            adjustmentDTO.setRequestMessage("Others - test");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin(InternalUsage.FULL);
            account.setDisplayTimeFormat(DateConstants.DEFAULT_TIME_FORMAT);
            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");

            userPROUtilMockedStatic.when(()-> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(null);

            approveAdjustmentEmailHelper.adjustmentApprovedMailHandler(contact,contact,DateConstants.ZONE_ID_IST,adjustmentDTO,account);

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.USER_NAME.toString(), "fName lName");
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"fName lName");
            modalMap.put(EmailKeys.ADMIN_MAIL.toString(),"test@gmail.com");
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(),CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.MESSAGE.toString(),"test");
            modalMap.put(EmailKeys.IN_DATE.toString(),"14-Jul-2022");
            modalMap.put(EmailKeys.IN_TIME.toString(), "02:32:47 PM");
            modalMap.put(EmailKeys.OUT_TIME.toString(), "05:04:38 PM");
            modalMap.put(EmailKeys.DURATION.toString(),"2h 31m");
            modalMap.put(EmailKeys.REDIRECT_URL.toString(), CommonAppProperties.getYoCoDashboardUrl() + "hours");
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("adjustmentApprovedMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Your adjustment is approved. (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }


    @Test
    void adjustmentApprovedMailHandler_exception_test(){
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            templateConfigMockedStatic.when(()-> TemplateConfig.processAndExtractTemplate(anyString(),anyMap())).thenThrow(new TemplateNotFoundException("exception",null,"templateNotFound"));

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1657789367000L);
            adjustmentDTO.setAdjustedOutTime(1657798478000L);
            adjustmentDTO.setOriginalInTime(1657789367000L);
            adjustmentDTO.setOriginalOutTime(1657789368000L);
            adjustmentDTO.setRequestMessage("Others - test");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            account.setDisplayTimeFormat(DateConstants.DEFAULT_TIME_FORMAT);
            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");
            approveAdjustmentEmailHelper.adjustmentApprovedMailHandler(contact,contact,DateConstants.ZONE_ID_IST,adjustmentDTO,account);

            Mockito.verify(emailService,Mockito.times(0)).sendHtmlTemplatedMail(any());
        }
    }

    @Test
    void adjustmentApprovedMailHandler_exception_test2(){
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1657789367000L);
            adjustmentDTO.setAdjustedOutTime(1657798478000L);
            adjustmentDTO.setOriginalInTime(1657789367000L);
            adjustmentDTO.setOriginalOutTime(1657789368000L);
            adjustmentDTO.setRequestMessage("Others - test");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            account.setDisplayTimeFormat(DateConstants.DEFAULT_TIME_FORMAT);
            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");
            approveAdjustmentEmailHelper.adjustmentApprovedMailHandler(contact,contact,DateConstants.ZONE_ID_IST,adjustmentDTO,null);

            Mockito.verifyNoInteractions(emailService);
        }
    }

    @Test
    void adjustmentEditApprovedMailHandler_test() throws TemplateException, IOException {
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1657789367000L);
            adjustmentDTO.setAdjustedOutTime(1657798478000L);
            adjustmentDTO.setOriginalInTime(1657789367000L);
            adjustmentDTO.setOriginalOutTime(1657789368000L);
            adjustmentDTO.setApprovedMessage("Others - test");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            account.setDisplayTimeFormat(DateConstants.DEFAULT_TIME_FORMAT);
            account.setDisplayDomainName("TEST");
            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");
            approveAdjustmentEmailHelper.adjustmentEditApprovedMailHandler(contact,contact,DateConstants.ZONE_ID_IST,adjustmentDTO,account);

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.USER_NAME.toString(), "fName lName");
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"fName lName");
            modalMap.put(EmailKeys.ADMIN_MAIL.toString(),"test@gmail.com");
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(),CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.COMPANY_NAME.toString(),"TEST");
            modalMap.put(EmailKeys.MESSAGE.toString(),"test");
            modalMap.put(EmailKeys.IN_DATE.toString(),"14-Jul-2022");
            modalMap.put(EmailKeys.IN_TIME.toString(), "02:32:47 PM");
            modalMap.put(EmailKeys.OUT_TIME.toString(), "05:04:38 PM");
            modalMap.put(EmailKeys.DURATION.toString(),"2h 31m");
            modalMap.put(EmailKeys.DURATION_DIFF.toString(),"2 hours and 32 minutes (+)");
            modalMap.put(EmailKeys.REDIRECT_URL.toString(), CommonAppProperties.getYoCoDashboardUrl() + "hours");
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("adjustmentEditApprovedMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Hours Adjustment Request Status. (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void adjustmentEditApprovedMailHandler_full_test() throws TemplateException, IOException {
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1657789367000L);
            adjustmentDTO.setAdjustedOutTime(1657798478000L);
            adjustmentDTO.setOriginalInTime(1657789367000L);
            adjustmentDTO.setOriginalOutTime(1657789368000L);
            adjustmentDTO.setApprovedMessage("Others - test");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin(InternalUsage.FULL);
            account.setDisplayTimeFormat(DateConstants.DEFAULT_TIME_FORMAT);
            account.setDisplayDomainName("TEST");
            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");

            PeopleRelationJDO primaryAdminPro = new PeopleRelationJDO();
            primaryAdminPro.setContact(contact);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(primaryAdminPro);

            approveAdjustmentEmailHelper.adjustmentEditApprovedMailHandler(contact,contact,DateConstants.ZONE_ID_IST,adjustmentDTO,account);

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.USER_NAME.toString(), "fName lName");
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"fName lName");
            modalMap.put(EmailKeys.ADMIN_MAIL.toString(),"test@gmail.com");
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(),CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.COMPANY_NAME.toString(),"TEST");
            modalMap.put(EmailKeys.MESSAGE.toString(),"test");
            modalMap.put(EmailKeys.IN_DATE.toString(),"14-Jul-2022");
            modalMap.put(EmailKeys.IN_TIME.toString(), "02:32:47 PM");
            modalMap.put(EmailKeys.OUT_TIME.toString(), "05:04:38 PM");
            modalMap.put(EmailKeys.DURATION.toString(),"2h 31m");
            modalMap.put(EmailKeys.DURATION_DIFF.toString(),"2 hours and 32 minutes (+)");
            modalMap.put(EmailKeys.REDIRECT_URL.toString(), CommonAppProperties.getYoCoDashboardUrl() + "hours");
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("adjustmentEditApprovedMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Hours Adjustment Request Status. (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void adjustmentEditApprovedMailHandler_full_null_test() throws TemplateException, IOException {
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1657789367000L);
            adjustmentDTO.setAdjustedOutTime(1657798478000L);
            adjustmentDTO.setOriginalInTime(1657789367000L);
            adjustmentDTO.setOriginalOutTime(1657789368000L);
            adjustmentDTO.setApprovedMessage("Others - test");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin(InternalUsage.FULL);
            account.setDisplayTimeFormat(DateConstants.DEFAULT_TIME_FORMAT);
            account.setDisplayDomainName("TEST");
            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");

            userPROUtilMockedStatic.when(()-> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(null);

            approveAdjustmentEmailHelper.adjustmentEditApprovedMailHandler(contact,contact,DateConstants.ZONE_ID_IST,adjustmentDTO,account);

            MailAddressDTO addressDTO = new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, "test@gmail.com", "","","");
            Map<String,Object> modalMap = new HashMap<>();
            modalMap.put(EmailKeys.USER_NAME.toString(), "fName lName");
            modalMap.put(EmailKeys.ADMIN_NAME.toString(),"fName lName");
            modalMap.put(EmailKeys.ADMIN_MAIL.toString(),"test@gmail.com");
            modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(),CommonAppProperties.getYoCoDashboardUrl());
            modalMap.put(EmailKeys.COMPANY_NAME.toString(),"TEST");
            modalMap.put(EmailKeys.MESSAGE.toString(),"test");
            modalMap.put(EmailKeys.IN_DATE.toString(),"14-Jul-2022");
            modalMap.put(EmailKeys.IN_TIME.toString(), "02:32:47 PM");
            modalMap.put(EmailKeys.OUT_TIME.toString(), "05:04:38 PM");
            modalMap.put(EmailKeys.DURATION.toString(),"2h 31m");
            modalMap.put(EmailKeys.DURATION_DIFF.toString(),"2 hours and 32 minutes (+)");
            modalMap.put(EmailKeys.REDIRECT_URL.toString(), CommonAppProperties.getYoCoDashboardUrl() + "hours");
            modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());

            String template = TemplateConfig.processAndExtractTemplate("adjustmentEditApprovedMail.ftl",modalMap);

            MailDTO mailDTO = new MailDTO(addressDTO,template,"Hours Adjustment Request Status. (staging-goclockin-dashboard) ");
            Mockito.verify(emailService).sendHtmlTemplatedMail(mailDTO);
        }
    }

    @Test
    void adjustmentEditApprovedMailHandler_exception_test(){
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class);
            MockedStatic<TemplateConfig> templateConfigMockedStatic = Mockito.mockStatic(TemplateConfig.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            Mockito.when(emailService.sendHtmlTemplatedMail(any(MailDTO.class))).thenReturn(new HashMap<>());
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            templateConfigMockedStatic.when(()-> TemplateConfig.processAndExtractTemplate(anyString(),anyMap())).thenThrow(new TemplateNotFoundException("exception",null,"templateNotFound"));

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1657789367000L);
            adjustmentDTO.setAdjustedOutTime(1657798478000L);
            adjustmentDTO.setOriginalInTime(1657789367000L);
            adjustmentDTO.setOriginalOutTime(1657789368000L);
            adjustmentDTO.setRequestMessage("Others - test");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            account.setDisplayTimeFormat(DateConstants.DEFAULT_TIME_FORMAT);
            account.setDisplayDomainName("TEST");
            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");
            approveAdjustmentEmailHelper.adjustmentEditApprovedMailHandler(contact,contact,DateConstants.ZONE_ID_IST,adjustmentDTO,account);

            Mockito.verify(emailService,Mockito.times(0)).sendHtmlTemplatedMail(any());
        }
    }

    @Test
    void adjustmentEditApprovedMailHandler_exception_test2(){
        try(MockedStatic<EmailService> emailServiceMockedStatic = Mockito.mockStatic(EmailService.class)){

            EmailService emailService  = Mockito.mock(EmailService.class);
            emailServiceMockedStatic.when(EmailService::getInstance).thenReturn(emailService);

            CommonAppProperties.setYoCoDashboardUrl("https://yocoDomain/");

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1657789367000L);
            adjustmentDTO.setAdjustedOutTime(1657798478000L);
            adjustmentDTO.setOriginalInTime(1657789367000L);
            adjustmentDTO.setOriginalOutTime(1657789368000L);
            adjustmentDTO.setRequestMessage("Others - test");
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accountId");
            account.setDisplayTimeFormat(DateConstants.DEFAULT_TIME_FORMAT);
            account.setDisplayDomainName("TEST");
            Contact contact = new Contact();
            contact.setEmailID("test@gmail.com");
            contact.setFirstName("fName");
            contact.setLastName("lName");
            approveAdjustmentEmailHelper.adjustmentEditApprovedMailHandler(contact,contact,DateConstants.ZONE_ID_IST,adjustmentDTO,null);

            Mockito.verifyNoInteractions(emailService);
        }
    }


}