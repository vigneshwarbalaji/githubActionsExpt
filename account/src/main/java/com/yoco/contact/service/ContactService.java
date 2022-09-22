package com.yoco.contact.service;

import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Slf4j
@Service
public class ContactService {

    public Map<String,Object> getContacts(String contacts){

        Validator.checkArgument(ObjUtils.isNullOrEmpty(contacts),COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value());

        List<String> contactList = Arrays.asList(contacts.split(","));

        contactList = contactList.stream().filter(contact -> !ObjUtils.isNullOrEmpty(contact)).collect(Collectors.toList());

        List<Contact> contactsInfo = ContactImpl.getContactImplInstance().getContacts(contactList);

        if(ObjUtils.isNullOrEmpty(contactsInfo)){
            return Map.of();
        }
        return Map.of(ContactConstants.CONTACTS,contactsInfo);
    }

}
