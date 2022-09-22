package com.yoco.commons.services;

import com.sendgrid.Method;
import com.sendgrid.Request;
import com.sendgrid.SendGrid;
import com.sendgrid.helpers.mail.Mail;
import com.sendgrid.helpers.mail.objects.Attachments;
import com.sendgrid.helpers.mail.objects.Content;
import com.sendgrid.helpers.mail.objects.Email;
import com.sendgrid.helpers.mail.objects.Personalization;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.utils.HashUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public class EmailService {

    private static final String SEND_GRID_MAIL_END_POINT = "mail/send";

    public static EmailService getInstance(){
        return new EmailService();
    }

    public Mail buildMail(MailDTO mailDTO){

        var addressDTO = mailDTO.getMailAddressDTO();

        var mail = new Mail();
        mail.setFrom(new Email(addressDTO.getFrom(),addressDTO.getFromName()));
        mail.setSubject(mailDTO.getSubject());

        if(!ObjUtils.isNullOrEmpty(addressDTO.getReplyTo())){
            mail.setReplyTo(new Email(addressDTO.getReplyTo()));
        }

        mail.addPersonalization(this.buildPersonalization(addressDTO));

        var attachments = this.buildAttachments(mailDTO);
        if(!ObjUtils.isNull(attachments)){
            mail.addAttachments(attachments);
        }

        return mail;
    }

    public Personalization buildPersonalization(MailAddressDTO addressDTO){

        var personalization = new Personalization();

        addressDTO.getTo().stream().forEach(address -> personalization.addTo(new Email(address)));

        if(!ObjUtils.isNullOrEmpty(addressDTO.getCc()))
            addressDTO.getCc().stream().forEach(address -> personalization.addCc(new Email(address)));

        if(!ObjUtils.isNullOrEmpty(addressDTO.getBcc()))
            addressDTO.getBcc().stream().forEach(address -> personalization.addBcc(new Email(address)));

        return personalization;
    }

    public Attachments buildAttachments(MailDTO mailDTO){
        Attachments attachmentObj = null;
        if(!ObjUtils.isNullOrEmpty(mailDTO.getFileName()) && !ObjUtils.isNullOrEmpty(mailDTO.getAttachment())){
            attachmentObj = new Attachments();
            attachmentObj.setFilename(mailDTO.getFileName());
            attachmentObj.setContent(HashUtil.encodeToBase64(mailDTO.getAttachment().getBytes()));
        }
        return attachmentObj;
    }

    public String buildHtmlMailBody(MailDTO mailDTO) throws IOException {
        var mail = this.buildMail(mailDTO);
        mail.addContent(new Content("text/html",mailDTO.getHtmlTemplate()));
        return mail.build();
    }

    public Request buildRequest(String bodyContent){
        var request = new Request();
        request.setMethod(Method.POST);
        request.setEndpoint(SEND_GRID_MAIL_END_POINT);
        request.setBody(bodyContent);
        return request;
    }

    public Map<String,Object> makeApiRequest(Request request){
        Map<String,Object> resp = new HashMap<>();
        try{
            var sendGrid = new SendGrid(CommonAppProperties.getSendGridApiKey());
            var response = sendGrid.api(request);
            log.info("status code :: " + response.getStatusCode());
            resp.put("sendGridResponse",response);
        }catch (Exception e){
            log.info(" Exception in sending mail ... " + e.getMessage());
        }
        return resp;
    }

    public Map<String,Object> sendHtmlTemplatedMail(MailDTO mailDTO) {
        Map<String,Object> resp = new HashMap<>();
        try {
            if(!ObjUtils.isNullOrEmpty(mailDTO.getHtmlTemplate())){
                String mailBody = this.buildHtmlMailBody(mailDTO);
                var request = this.buildRequest(mailBody);
                return this.makeApiRequest(request);
            }
        } catch (Exception e) {
            log.info(" exception in building mail... " + e.getMessage());
        }
        return resp;
    }

}
