package com.yoco.contact.helper;

import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ContactHelper {

    public static ContactHelper getInstance(){
        return new ContactHelper();
    }

    public Contact createContact(DcmContactDTO dcmContact,ContactImpl contactImpl){
        String firstName = ObjUtils.isNullOrEmpty(dcmContact.getFirstName()) ? "" : dcmContact.getFirstName().trim();
        String lastName = ObjUtils.isNullOrEmpty(dcmContact.getLastName()) ? "" : dcmContact.getLastName().trim();
        var contact = new Contact(dcmContact.getId(), dcmContact.getLogin(), firstName, lastName, dcmContact.getPhotoID(), 0l);
        return contactImpl.saveContact(contact);
    }

    public Contact validateAndCreateContact(DcmContactDTO dcmContact){
        ContactImpl contactImpl = ContactImpl.getContactImplInstance();
        Contact contactObj = contactImpl.getByID(dcmContact.getId());
        return ObjUtils.isNull(contactObj) ? this.createContact(dcmContact,contactImpl) : contactObj;
    }


}