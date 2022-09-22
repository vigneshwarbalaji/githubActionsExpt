package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.objectify.LocalDatastoreExtension;
import com.yoco.commons.dataservices.objectify.ObjectifyExtension;
import com.yoco.commons.entity.Contact;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.Arrays;
import java.util.List;

import static com.yoco.commons.dataservices.objectify.OfyService.ofy;

@ExtendWith({
        LocalDatastoreExtension.class,
        ObjectifyExtension.class})
class ContactImplTest {

    ContactImpl contactImpl = ContactImpl.getContactImplInstance();


    @Test
    void getByID_nullEntity_test(){
        Assertions.assertNull(contactImpl.getByID("123"));
    }

    @Test
    void getByID_ValidEntity_test(){
        Contact contact = new Contact("123","email","fname","lname","photoID",0L);
        contactImpl.saveContact(contact);
        Assertions.assertEquals(contact,contactImpl.getByID("123"));
        ofy().delete().entity(contact).now();
    }

    @Test
    void saveContact_Null_test(){
        Assertions.assertEquals(null,contactImpl.saveContact(null).getId());
    }

    @Test
    void getContactByEmailID_test(){
        Contact contact = new Contact("123","test@gmail.com","fname","lname","photoID",0L);
        contactImpl.saveContact(contact);
        Assertions.assertEquals(contact,contactImpl.getContactByEmailID("test@gmail.com"));
        ofy().delete().entity(contact).now();
    }

    @Test
    void getContacts_test(){
        Contact contact = new Contact("123","test@gmail.com","fname","lname","photoID",0L);
        contactImpl.saveContact(contact);
        Contact contact2 = new Contact("1234","test02@gmail.com","fname01","","photoId",0L);
        contactImpl.saveContact(contact2);
        List<Contact> resp = contactImpl.getContacts(Arrays.asList("123","1234"));
        Assertions.assertNotNull(resp);
        Assertions.assertEquals(2,resp.size());
        Assertions.assertEquals(contact,resp.get(0));
        Assertions.assertEquals(contact2,resp.get(1));
        ofy().delete().entities(Arrays.asList(contact,contact2)).now();
    }

}
