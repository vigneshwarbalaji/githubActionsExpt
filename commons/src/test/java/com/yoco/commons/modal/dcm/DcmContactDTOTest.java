package com.yoco.commons.modal.dcm;

import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.commons.utils.HashUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class DcmContactDTOTest {

    @Test
    void DcmContactDTO_firstName_xss_photoID_invalid_test(){
        Map<String, Object> contactMap = new HashMap<>();
        contactMap.put("id","dcmContactId");
        contactMap.put("firstName","fName<img src ='asd' onerror='asd'>");
        contactMap.put("lastName","lName");
        contactMap.put("deleted",false);
        contactMap.put("login","test@gmail.com");
        contactMap.put("brandID","brandId");
        contactMap.put("photoID","photoId");
        contactMap.put("accountID","accountId");
        contactMap.put("linkedContacts",null);
        contactMap.put("linkedAccounts",null);
        contactMap.put("is_password_present",false);

        DcmContactDTO dcmContactDTO = new DcmContactDTO(contactMap);

        Assertions.assertEquals("dcmContactId",dcmContactDTO.getId());
        Assertions.assertEquals("fName",dcmContactDTO.getFirstName());
        Assertions.assertEquals("lName",dcmContactDTO.getLastName());
        Assertions.assertFalse(dcmContactDTO.getDeleted());
        Assertions.assertEquals("test@gmail.com",dcmContactDTO.getLogin());
        Assertions.assertEquals("brandId",dcmContactDTO.getBrandID());
        Assertions.assertEquals("",dcmContactDTO.getPhotoID());
        Assertions.assertEquals("accountId",dcmContactDTO.getAccountID());
        Assertions.assertNull(dcmContactDTO.getLinkedContacts());
        Assertions.assertNull(dcmContactDTO.getLinkedAccounts());
        Assertions.assertFalse(dcmContactDTO.isPasswordPresent());
    }

    @Test
    void createDcmContactDTO_test() throws NoSuchAlgorithmException {
        Map<String,Object> response  = new DcmContactDTO().createDcmContactDTO("test@gmail.com","password","fName","lName","photo");

        Map<String, Object> expectedMap = new HashMap<>();
        expectedMap.put("brandID", DcmConstants.YOCO_BRAND_ID);
        expectedMap.put(DcmUtil.SKILL_SET_ID, DcmConstants.YOCO_SKILLSET_ID);
        expectedMap.put("login", "test@gmail.com");
        expectedMap.put("firstName", "fName");
        expectedMap.put("lastName", "lName");
        expectedMap.put("password", HashUtil.generateSHA256("password"));
        expectedMap.put("photoID", "photo");
        expectedMap.put("type", "contact");
        expectedMap.put("category", "person");

        Map<String, String> contactDetails = new HashMap<>();
        contactDetails.put("type", "Email");
        contactDetails.put("title", "Office");
        contactDetails.put("value", "test@gmail.com");
        List<Map<String, String>> linkedContactMethods = new ArrayList<>();
        linkedContactMethods.add(contactDetails);

        expectedMap.put("linkedContactMethods", linkedContactMethods);

        Assertions.assertEquals(expectedMap,response);
    }

    @Test
    void createDcmContactDTO_empty_lastName_test() throws NoSuchAlgorithmException {
        Map<String,Object> response  = new DcmContactDTO().createDcmContactDTO("test@gmail.com","","fName","","");

        Map<String, Object> expectedMap = new HashMap<>();
        expectedMap.put("brandID", DcmConstants.YOCO_BRAND_ID);
        expectedMap.put(DcmUtil.SKILL_SET_ID, DcmConstants.YOCO_SKILLSET_ID);
        expectedMap.put("login", "test@gmail.com");
        expectedMap.put("firstName", "fName");
        expectedMap.put("lastName", "");
        expectedMap.put("type", "contact");
        expectedMap.put("category", "person");

        Map<String, String> contactDetails = new HashMap<>();
        contactDetails.put("type", "Email");
        contactDetails.put("title", "Office");
        contactDetails.put("value", "test@gmail.com");
        List<Map<String, String>> linkedContactMethods = new ArrayList<>();
        linkedContactMethods.add(contactDetails);

        expectedMap.put("linkedContactMethods", linkedContactMethods);

        Assertions.assertEquals(expectedMap,response);
    }

}