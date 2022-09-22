package com.yoco.commons.modal.dcm;

import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.commons.utils.HashUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static com.yoco.commons.constants.ContactConstants.PHOTO_ID;

@Data
@NoArgsConstructor
public class DcmContactDTO implements Serializable {
    private static final long serialVersionUID = -47940791545922350L;
    private static final String PERSON_CATEGORY = "person";
    private static final String CONTACT = "contact";

    private String id;
    private String firstName;
    private String lastName;
    private Boolean deleted;
    private String login;
    private String brandID;
    private String photoID;
    private String accountID;
    private String password;
    private String type;
    private String category;
    private List<String> linkedContacts;
    private List<String> linkedAccounts;
    private boolean isPasswordPresent;


    public DcmContactDTO(Map<String, Object> contactMap){
        this.id = (String) contactMap.get("id");
        this.firstName = Validator.sanitizeText((String) contactMap.get("firstName"));
        this.lastName = Validator.sanitizeText((String) contactMap.get("lastName"));
        this.deleted = (Boolean) contactMap.get("deleted");
        this.login = Validator.sanitizeEmail((String) contactMap.get("login"));
        this.brandID = (String) contactMap.get("brandID");
        this.photoID = Validator.sanitizeUrl((String) contactMap.get(PHOTO_ID));
        this.accountID = (String) contactMap.get("accountID");
        this.linkedContacts = (List<String>) contactMap.get("linkedContacts");
        this.linkedAccounts = (List<String>) contactMap.get("linkedAccounts");
        this.isPasswordPresent = (boolean) contactMap.get("is_password_present");
    }

    public Map<String,Object> createDcmContactDTO(String emailID,String password,String firsName,
                                                  String lastName,String photoID) throws NoSuchAlgorithmException {
        Map<String, Object> accountMap = new HashMap<>();
        accountMap.put("brandID", DcmConstants.YOCO_BRAND_ID);
        accountMap.put(DcmUtil.SKILL_SET_ID, DcmConstants.YOCO_SKILLSET_ID);
        accountMap.put("login", emailID.toLowerCase());
        accountMap.put("firstName", firsName);
        accountMap.put("lastName", ObjUtils.isNullOrEmpty(lastName) ? "" : lastName);

        if(!ObjUtils.isNullOrEmpty(password)){
            accountMap.put("password", HashUtil.generateSHA256(password));
        }
        if (!ObjUtils.isNullOrEmpty(photoID)) {
            accountMap.put(PHOTO_ID, photoID);
        }
        accountMap.put("type", CONTACT);
        accountMap.put("category", PERSON_CATEGORY);
        accountMap.put("linkedContactMethods", this.generateLinkedContactMethods(emailID));
        return accountMap;
    }

    private List<Map<String, String>> generateLinkedContactMethods(String emailID){
        Map<String, String> contactDetails = new HashMap<>();
        contactDetails.put("type", "Email");
        contactDetails.put("title", "Office");
        contactDetails.put("value", emailID);
        List<Map<String, String>> linkedContactMethods = new ArrayList<>();
        linkedContactMethods.add(contactDetails);
        return linkedContactMethods;
    }

}