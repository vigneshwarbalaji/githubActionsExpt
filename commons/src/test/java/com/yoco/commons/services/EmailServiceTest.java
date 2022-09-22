package com.yoco.commons.services;

import com.sendgrid.Method;
import com.sendgrid.Request;
import com.sendgrid.Response;
import com.sendgrid.SendGrid;
import com.sendgrid.helpers.mail.Mail;
import com.sendgrid.helpers.mail.objects.Attachments;
import com.sendgrid.helpers.mail.objects.Content;
import com.sendgrid.helpers.mail.objects.Email;
import com.sendgrid.helpers.mail.objects.Personalization;
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.utils.HashUtil;
import com.yoco.commons.utils.ObjUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;

class EmailServiceTest {

    EmailService emailService = EmailService.getInstance();

    @Test
    void buildRequest_test(){
        Request resp = emailService.buildRequest("mailBody");
        Assertions.assertEquals(Method.POST,resp.getMethod());
        Assertions.assertEquals("mail/send",resp.getEndpoint());
        Assertions.assertEquals("mailBody",resp.getBody());
    }

    @Test
    void buildMail_test(){
        MailAddressDTO mailAddressDTO = new MailAddressDTO("from@gmail.com","senderName",
                "to@gmail.com","cc@gmail.com", "bcc@gmail.com","replyTo@gmail.com");
        Mail resp = emailService.buildMail(new MailDTO(mailAddressDTO,"htmlCode","Test mail"));
        Assertions.assertEquals(new Email("from@gmail.com","senderName"),resp.getFrom());
        Assertions.assertEquals("Test mail",resp.getSubject());
        Assertions.assertEquals(new Email("replyTo@gmail.com"),resp.getReplyto());
        Personalization personalization = resp.getPersonalization().get(0);
        Assertions.assertEquals(new Email("to@gmail.com"),personalization.getTos().get(0));
        Assertions.assertEquals(new Email("cc@gmail.com"),personalization.getCcs().get(0));
        Assertions.assertEquals(new Email("bcc@gmail.com"),personalization.getBccs().get(0));
    }

    @Test
    void buildMail_noReplyMail_test(){
        MailAddressDTO mailAddressDTO = new MailAddressDTO("from@gmail.com","senderName", "","", "","");
        Mail resp = emailService.buildMail(new MailDTO(mailAddressDTO,"htmlCode","Test mail"));
        Assertions.assertEquals(new Email("from@gmail.com","senderName"),resp.getFrom());
        Assertions.assertEquals("Test mail",resp.getSubject());
        Assertions.assertNull(resp.getReplyto());
        Personalization personalization = resp.getPersonalization().get(0);
        Assertions.assertTrue(personalization.getTos().isEmpty());
        Assertions.assertTrue(personalization.getCcs().isEmpty());
        Assertions.assertTrue(personalization.getBccs().isEmpty());
    }

    @Test
    void buildMail_attachment_test(){
        MailAddressDTO mailAddressDTO = new MailAddressDTO("from@gmail.com","senderName",
                "to@gmail.com","cc@gmail.com", "bcc@gmail.com","replyTo@gmail.com");
        Mail resp = emailService.buildMail(new MailDTO(mailAddressDTO,"htmlCode","Test mail","fileName.txt","attachmentContent"));
        Assertions.assertEquals(new Email("from@gmail.com","senderName"),resp.getFrom());
        Assertions.assertEquals("Test mail",resp.getSubject());
        Assertions.assertEquals(new Email("replyTo@gmail.com"),resp.getReplyto());
        Personalization personalization = resp.getPersonalization().get(0);
        Assertions.assertEquals(new Email("to@gmail.com"),personalization.getTos().get(0));
        Assertions.assertEquals(new Email("cc@gmail.com"),personalization.getCcs().get(0));
        Assertions.assertEquals(new Email("bcc@gmail.com"),personalization.getBccs().get(0));
        Attachments attachmentObj = new Attachments();
        attachmentObj.setFilename("fileName.txt");
        attachmentObj.setContent(HashUtil.encodeToBase64("attachmentContent".getBytes()));
        Assertions.assertEquals(attachmentObj,resp.getAttachments().get(0));
    }

    @Test
    void buildPersonalization_multipleMails_test(){
        MailAddressDTO mailAddressDTO = new MailAddressDTO("from@gmail.com","senderName",
                new ArrayList<>(){{add("to1@gmail.com");add("to2@gmail.com");}},
                new ArrayList<>(){{add("cc1@gmail.com");add("cc2@gmail.com");}},
                new ArrayList<>(){{add("bcc1@gmail.com");add("bcc2@gmail.com");}},
                "");
        Personalization resp = emailService.buildPersonalization(mailAddressDTO);
        Assertions.assertEquals(2,resp.getTos().size());
        Assertions.assertEquals(new Email("to1@gmail.com"),resp.getTos().get(0));
        Assertions.assertEquals(new Email("to2@gmail.com"),resp.getTos().get(1));
        Assertions.assertEquals(2,resp.getCcs().size());
        Assertions.assertEquals(new Email("cc1@gmail.com"),resp.getCcs().get(0));
        Assertions.assertEquals(new Email("cc2@gmail.com"),resp.getCcs().get(1));
        Assertions.assertEquals(2,resp.getBccs().size());
        Assertions.assertEquals(new Email("bcc1@gmail.com"),resp.getBccs().get(0));
        Assertions.assertEquals(new Email("bcc2@gmail.com"),resp.getBccs().get(1));
    }

    @Test
    void buildHtmlMailBody_test() throws IOException {
        MailAddressDTO mailAddressDTO = new MailAddressDTO("from@gmail.com","senderName",
                new ArrayList<>(){{add("to@gmail.com");}},new ArrayList<>(){{add("cc@gmail.com");}},
                new ArrayList<>(){{add("bcc@gmail.com");}},"replyTo@gmail.com");
        String mailContent = emailService.buildHtmlMailBody(new MailDTO(mailAddressDTO,"htmlCode","Test mail"));

        Mail mail = new Mail();
        mail.setFrom(new Email("from@gmail.com","senderName"));
        mail.setSubject("Test mail");
        mail.setReplyTo(new Email("replyTo@gmail.com"));
        Personalization personalization = new Personalization();
        personalization.addTo(new Email("to@gmail.com"));
        personalization.addCc(new Email("cc@gmail.com"));
        personalization.addBcc(new Email("bcc@gmail.com"));
        mail.addPersonalization(personalization);
        mail.addContent(new Content("text/html","htmlCode"));

        Assertions.assertEquals(mail.build(),mailContent);
    }

    @Test
    void sendApiRequest_valid_test(){
        Response response = new Response();
        response.setStatusCode(200);
        try(MockedConstruction<SendGrid> sendGridMockedConstruction = Mockito.mockConstruction(SendGrid.class,(mock,context) -> {
            Mockito.when(mock.api(any(Request.class))).thenReturn(response);
        })){
            Request request = new Request();
            Map<String,Object> actualResp = emailService.makeApiRequest(request);
            Assertions.assertFalse(ObjUtils.isNullOrEmpty(actualResp));
            Assertions.assertEquals(response,actualResp.get("sendGridResponse"));
        }
    }

    @Test
    void sendApiRequest_exception_test(){
        try(MockedConstruction<SendGrid> sendGridMockedConstruction = Mockito.mockConstruction(SendGrid.class,(mock,context) -> {
            Mockito.when(mock.api(any(Request.class))).thenThrow(new IllegalArgumentException());
        })){
            Request request = new Request();
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(emailService.makeApiRequest(request)));
        }
    }

    @Test
    void sendHtmlTemplatedMail_valid_test(){
        Response response = new Response();
        response.setStatusCode(200);
        try(MockedConstruction sendGridMockedConstruction = Mockito.mockConstruction(SendGrid.class,(sendGridMock, context) -> {
            Mockito.when(sendGridMock.api(any(Request.class))).thenReturn(response);
        })){
            MailDTO mailDTO = new MailDTO();
            MailAddressDTO mailAddressDTO = new MailAddressDTO("from@gmail.com","senderName",
                    new ArrayList<>(){{add("to@gmail.com");}},new ArrayList<>(){{add("cc@gmail.com");}},
                    new ArrayList<>(){{add("bcc@gmail.com");}},"replyTo@gmail.com");
            Map<String,Object> resp = emailService.sendHtmlTemplatedMail(new MailDTO(mailAddressDTO,"htmlCode","Test mail"));
            Assertions.assertNotNull(resp);
        }
    }

    @Test
    void sendHtmlTemplatedMail_exception_test(){
        MailDTO mailDTO = new MailDTO();
        mailDTO.setHtmlTemplate("htmlCode");
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(emailService.sendHtmlTemplatedMail(mailDTO)));
    }

    @Test
    void sendHtmlTemplatedMail_invalid_htmlTemplate_test(){
        MailDTO mailDTO = new MailDTO();
        mailDTO.setHtmlTemplate("");
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(emailService.sendHtmlTemplatedMail(mailDTO)));
    }

    @Test
    void buildAttachments_valid_test(){
        MailDTO mailDTO = new MailDTO();
        mailDTO.setAttachment("attachment");
        mailDTO.setFileName("fileName.txt");
        Attachments attachments = emailService.buildAttachments(mailDTO);
        Assertions.assertNotNull(attachments);
        Assertions.assertEquals("fileName.txt",attachments.getFilename());
        Assertions.assertEquals(HashUtil.encodeToBase64(mailDTO.getAttachment().getBytes()),attachments.getContent());
    }

    @ParameterizedTest
    @NullAndEmptySource
    void buildAttachments_Invalid_fileName_test(String testValue){
        MailDTO mailDTO = new MailDTO();
        mailDTO.setAttachment("attachment");
        mailDTO.setFileName(testValue);
        Attachments attachments = emailService.buildAttachments(mailDTO);
        Assertions.assertNull(attachments);
    }

    @ParameterizedTest
    @NullAndEmptySource
    void buildAttachments_Invalid_attachment_test(String testValue){
        MailDTO mailDTO = new MailDTO();
        mailDTO.setAttachment(testValue);
        mailDTO.setFileName("fileName.txt");
        Attachments attachments = emailService.buildAttachments(mailDTO);
        Assertions.assertNull(attachments);
    }

}