package com.yoco.commons.utils.account;

import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.utils.ObjUtils;

public class ContactUtil {
    private ContactUtil(){}
    public static String getEmailID(String contactID){
        return getEmailID(new ContactImpl().getByID(contactID));
    }

    public static String getEmailID(Contact contact){
        if(ObjUtils.isNull(contact) || ObjUtils.isNullOrEmpty(contact.getEmailID())){
            return "";
        }
        return contact.getEmailID().trim();
    }
}
