package com.yoco.commons.modal.contact;

import com.yoco.commons.entity.Contact;
import com.yoco.commons.utils.ObjUtils;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@NoArgsConstructor
@Data
public class ContactDTO implements Serializable {

    private String id;
    private String emailID;
    private String firstName;
    private String lastName;
    private String photoID;
    private Boolean is_password_present;
    private String name;

    public ContactDTO(Contact contact){
        this.setId(contact.getId());
        this.setEmailID(contact.getEmailID());
        this.setFirstName(contact.getFirstName());
        this.setLastName(contact.getLastName());
        this.setPhotoID(contact.getPhotoID());
        this.setIs_password_present( ObjUtils.isNull(contact.getIsPasswordPresent()) ? null :contact.getIsPasswordPresent() );
        this.setName(contact.getFullName());
    }

}
