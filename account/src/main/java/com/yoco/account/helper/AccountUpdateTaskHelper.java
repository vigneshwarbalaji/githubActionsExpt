package com.yoco.account.helper;

import com.yoco.account.modal.AccountUpdatePayloadDTO;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.account.PhoneNumberDetailsDTO;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.constants.CommonConstants;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static com.yoco.commons.constants.ContactConstants.ID;
import static com.yoco.commons.constants.UniversalSignupConstants.PAYLOAD_COUNTRY_CODE_KEY;
import static com.yoco.commons.constants.UniversalSignupConstants.PAYLOAD_PHONE_NUMBER_KEY;
import static com.yoco.commons.utils.AccountUtil.PHONE_KEY;

public class AccountUpdateTaskHelper {

    private AccountUpdateTaskHelper(){}

    private static final String ACCOUNT_UPDATE_ACTIVITY_KEY = "CompanyProfileUpdate";
    public static final String LINKED_CONTACT_METHODS_KEY = "linkedContactMethods";

    public static void handleAccountUpdate(String accountID, Contact adminContact, AccountUpdatePayloadDTO payloadDTO) throws NoSuchAlgorithmException, IOException {
        saveAccountUpdateActivity(accountID,adminContact,payloadDTO);
        updateDcmAccountTimeZone(accountID,payloadDTO);
        updateDcmContactPhoneNumber(accountID,adminContact,payloadDTO);
    }

    public static void updateDcmAccountTimeZone(String accountID, AccountUpdatePayloadDTO payloadDTO) throws NoSuchAlgorithmException, IOException {
        if(payloadDTO.getTimeZone() != null){
            Map<String,Object> payload = Map.of("timeZone", payloadDTO.getTimeZone());
            DcmUtil.updateDcmAccount(accountID,payload);
        }
    }

    public static void updateDcmContactPhoneNumber(String accountID, Contact adminContact, AccountUpdatePayloadDTO payloadDTO) throws NoSuchAlgorithmException, IOException {
        if(payloadDTO.getPhoneNumber() != null){
            PhoneNumberDetailsDTO phoneNumberDetailsDTO = new PhoneNumberDetailsDTO(payloadDTO.getPhoneNumberCountry(),payloadDTO.getPhoneNumber());
            Map<String,Object> payload = new HashMap<>();
            payload.put("value",payloadDTO.getPhoneNumber());
            payload.put("type",PHONE_KEY);
            payload.put(PAYLOAD_COUNTRY_CODE_KEY,phoneNumberDetailsDTO.getCountryPhoneCode());
            payload.put(PAYLOAD_PHONE_NUMBER_KEY,phoneNumberDetailsDTO.getNationalNumber());
            String contactMethodID = getExistingContactMethodIdForOldPhoneNumber(adminContact.getEmailID(), payloadDTO.getOldPhoneNumber());
            if(!ObjUtils.isNullOrEmpty(contactMethodID)){
                payload.put(ID,contactMethodID);
            }
            DcmUtil.updateContactInDcm(accountID,Map.of(ID,adminContact.getId(),LINKED_CONTACT_METHODS_KEY,List.of(payload)));
        }
    }

    public static String getExistingContactMethodIdForOldPhoneNumber(String email, String oldPhoneNumber) {
        try{
            if(ObjUtils.isNullOrEmpty(oldPhoneNumber)){
                return "";
            }
            Map<String,Object> dcmContact = DcmUtil.fetchContactFromDcm(email);
            Map<String,Object> contactMap = (Map<String, Object>) dcmContact.get(CommonConstants.CONTACT_KEY);
            List<Map<String,Object>> contactMethods = (List<Map<String, Object>>) contactMap.get(LINKED_CONTACT_METHODS_KEY);
            Map<String,Object> existingContactMethod = contactMethods.stream().filter(method -> ((String)method.get("value")).contains(oldPhoneNumber)).findFirst().orElse(Map.of());
            return (String)existingContactMethod.get(ID);
        }catch (Exception e){
            return "";
        }
    }

    public static void saveAccountUpdateActivity(String accountID, Contact adminContact, AccountUpdatePayloadDTO payloadDTO) {
        if(!ObjUtils.isNullOrEmpty(payloadDTO.getActivityMessage())){
            String activity = adminContact.getEmailID() + " has updated account : " + payloadDTO.getActivityMessage();
            ActivityUtil.saveActivity(accountID,adminContact.getId(),ACCOUNT_UPDATE_ACTIVITY_KEY,adminContact.getEmailID(),activity,ActivityUtil.ACTIVITIES.DUMMY.value(), DateUtil.getCurrentTime());
        }
    }
}
