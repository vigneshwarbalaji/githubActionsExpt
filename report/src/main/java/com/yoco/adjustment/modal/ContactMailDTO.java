package com.yoco.adjustment.modal;

import com.yoco.commons.entity.Contact;
import lombok.Data;

@Data
public class ContactMailDTO {

    private String userName = "";
    private String userMailID = "";
    private String adminName = "";
    private String adminMailID = "";
    private String primaryAdminName = "";
    private String primaryAdminMailID = "";
    private String adminFirstName = "";

    public ContactMailDTO(Contact userContact, Contact adminContact, Contact primaryContact){
        this.setUserName(userContact.getFullName());
        this.setUserMailID(userContact.getEmailID());
        this.setAdminName(adminContact.getFullName());
        this.setAdminMailID(adminContact.getEmailID());
        this.setPrimaryAdminName(primaryContact.getFullName());
        this.setPrimaryAdminMailID(primaryContact.getEmailID());
        this.setAdminFirstName(adminContact.getFirstName());
    }
}
