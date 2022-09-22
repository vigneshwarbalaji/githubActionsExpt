package com.yoco.commons.modal.mail;

import com.yoco.commons.utils.ObjUtils;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class MailAddressDTO implements Serializable {

    private String from;
    private String fromName;
    private List<String> to = new ArrayList<>();
    private List<String> cc = new ArrayList<>();
    private List<String> bcc = new ArrayList<>();
    private String replyTo;

    public MailAddressDTO(String sender,String senderName,List<String> to, List<String> cc, List<String> bcc, String replyTo){
        this.setFrom(sender);
        this.setFromName(senderName);
        this.setTo(to);
        this.setCc(cc);
        this.setBcc(bcc);
        this.setReplyTo(replyTo);
    }

    public MailAddressDTO(String sender,String senderName,String to,String cc, String bcc, String replyTo){
        this.setFrom(sender);
        this.setFromName(senderName);

        if(!ObjUtils.isNullOrEmpty(to)){
            List<String> toAddress = new ArrayList<>();
            toAddress.add(to);
            this.setTo(toAddress);
        }

        if(!ObjUtils.isNullOrEmpty(cc)){
            List<String> ccAddress = new ArrayList<>();
            ccAddress.add(cc);
            this.setCc(ccAddress);
        }

        if(!ObjUtils.isNullOrEmpty(bcc)){
            List<String> bccAddress = new ArrayList<>();
            bccAddress.add(bcc);
            this.setBcc(bccAddress);
        }

        this.setReplyTo(replyTo);
    }

}
