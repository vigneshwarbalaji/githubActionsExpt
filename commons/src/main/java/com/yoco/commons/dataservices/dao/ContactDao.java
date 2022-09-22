package com.yoco.commons.dataservices.dao;

import com.yoco.commons.entity.Contact;

import java.util.List;

public interface ContactDao {
    Contact getByID(String id);
    Contact saveContact(Contact contact);
    Contact getContactByEmailID(String emailId);
    List<Contact> getContacts(List<String> contactIds);
}
