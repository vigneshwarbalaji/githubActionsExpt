package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.dao.ContactDao;
import com.yoco.commons.dataservices.objectify.OfyService;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.utils.ObjUtils;

import java.util.List;


public class ContactImpl extends OfyService implements ContactDao {

    public static ContactImpl getContactImplInstance(){
        return new ContactImpl();
    }

    @Override
    public Contact getByID(String id){
        return get(Contact.class, id);
    }

    @Override
    public Contact saveContact(Contact contact) {

        if( !ObjUtils.isNull(contact)) {
            ofy().save().entity(contact).now();
            return contact;
        } else {
            return new Contact();
        }
    }

    @Override
    public Contact getContactByEmailID(String emailId) {
        return  ofy().load().type(Contact.class)
                .filter( "emailID", emailId)
                .first().now();
    }

    public List<Contact> getContacts(List<String> contactIds){
        return get(Contact.class,contactIds);
    }

}
