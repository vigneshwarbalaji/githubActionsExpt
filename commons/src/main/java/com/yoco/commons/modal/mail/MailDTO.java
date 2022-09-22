package com.yoco.commons.modal.mail;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@NoArgsConstructor
@Data
public class MailDTO implements Serializable {

    private String subject;
    private String htmlTemplate;
    private String textTemplate;
    private String fileName;
    private String attachment;
    MailAddressDTO mailAddressDTO;

    public MailDTO(MailAddressDTO mailAddress,String htmlTemplate,String subject){
      this.setMailAddressDTO(mailAddress);
      this.setHtmlTemplate(htmlTemplate);
      this.setSubject(subject);
    }

    public MailDTO(MailAddressDTO mailAddress,String htmlTemplate,String subject,String fileName,String attachment){
        this.setMailAddressDTO(mailAddress);
        this.setHtmlTemplate(htmlTemplate);
        this.setSubject(subject);
        this.setFileName(fileName);
        this.setAttachment(attachment);
    }

}
