package com.yoco.commons.entity;

import com.yoco.commons.constants.StoragePublicUrl;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class ContactTest {
    @Test
    void constructor_nullId_0LModifiedTime_nullPhotoID_test(){
        Contact contact = new Contact(null,"email","fname","lname",null,0L);
        Assertions.assertNotNull(contact.getId());
        Assertions.assertEquals("email",contact.getEmailID());
        Assertions.assertEquals("fname lname",contact.getFullName());
        Assertions.assertEquals(StoragePublicUrl.DUMMY_USER_PIC_URL, contact.getPhotoID());
        Assertions.assertNotNull(contact.getDateAddedLongTime());
        Assertions.assertNotNull(contact.getDateModifiedLongTime());
    }

    @Test
    void constructor_validId_ModifiedTime_PhotoID_test(){
        Contact contact = new Contact("id","email","fname","lname","photoID",123L);
        Assertions.assertEquals("id",contact.getId());
        Assertions.assertEquals("email",contact.getEmailID());
        Assertions.assertEquals("fname lname",contact.getFullName());
        Assertions.assertEquals("photoID", contact.getPhotoID());
        Assertions.assertNotNull(contact.getDateAddedLongTime());
        Assertions.assertEquals(123L,contact.getDateModifiedLongTime());
    }
}
