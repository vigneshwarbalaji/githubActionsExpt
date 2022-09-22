package com.yoco.commons.modal.contact;

import com.yoco.commons.constants.StoragePublicUrl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.contact.ContactDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class ContactDTOTest {

    @Test
    void contactDTO_no_password_test(){
        Contact contact = new Contact();
        contact.setId("contactId");
        contact.setEmailID("test@gmail.com");
        contact.setFirstName("userFirstName");
        contact.setLastName("userLastName");
        contact.setPhotoID(StoragePublicUrl.DUMMY_USER_PIC_URL);
        ContactDTO contactDTO = new ContactDTO(contact);
        Assertions.assertNotNull(contactDTO);
        Assertions.assertEquals("contactId",contactDTO.getId());
        Assertions.assertEquals("test@gmail.com",contactDTO.getEmailID());
        Assertions.assertEquals("userFirstName",contactDTO.getFirstName());
        Assertions.assertEquals("userLastName",contactDTO.getLastName());
        Assertions.assertEquals(StoragePublicUrl.DUMMY_USER_PIC_URL,contactDTO.getPhotoID());
        Assertions.assertNull(contactDTO.getIs_password_present());
        Assertions.assertEquals(contact.getFullName(),contactDTO.getName());
    }

    @Test
    void contactDTO_valid_password_test(){
        Contact contact = new Contact();
        contact.setId("contactId");
        contact.setEmailID("test@gmail.com");
        contact.setFirstName("userFirstName");
        contact.setLastName("userLastName");
        contact.setPhotoID(StoragePublicUrl.DUMMY_USER_PIC_URL);
        contact.setIsPasswordPresent(true);
        ContactDTO contactDTO = new ContactDTO(contact);
        Assertions.assertNotNull(contactDTO);
        Assertions.assertEquals("contactId",contactDTO.getId());
        Assertions.assertEquals("test@gmail.com",contactDTO.getEmailID());
        Assertions.assertEquals("userFirstName",contactDTO.getFirstName());
        Assertions.assertEquals("userLastName",contactDTO.getLastName());
        Assertions.assertEquals(StoragePublicUrl.DUMMY_USER_PIC_URL,contactDTO.getPhotoID());
        Assertions.assertTrue(contactDTO.getIs_password_present());
        Assertions.assertEquals(contact.getFullName(),contactDTO.getName());
    }
}
