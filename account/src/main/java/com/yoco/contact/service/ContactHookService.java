package com.yoco.contact.service;

import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.utils.GaeUtils;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import com.yoco.contact.helper.hook.CreateContactHookHelper;
import com.yoco.contact.helper.hook.DeleteContactHookHelper;
import com.yoco.contact.helper.hook.UpdateContactHookHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
public class ContactHookService {

    private static final String FULL_HOOKS_UPDATE = "contact_updated";
    private static final String FULL_HOOKS_DELETE = "contact_deleted";
    private static final String LOGIN = "login";
    private static final String EVENT = "event";
    private static final String APP_ID = "appID";
    private static final String DATA = "data";

    private boolean isValidEventType(String eventType){
        return FULL_HOOKS_UPDATE.equalsIgnoreCase(eventType) ||  FULL_HOOKS_DELETE.equalsIgnoreCase(eventType);
    }

    private boolean isValidRequestSource(String appID){
        return !(appID.equalsIgnoreCase(GaeUtils.APP_ID_STAGING) ||  appID.equalsIgnoreCase(GaeUtils.APP_ID_LIVE));
    }

    private Map<String,Object> validateContactData(Map<Object,Object> dataMap){

        List<Object> contactList  = (List<Object>) dataMap.get(ContactConstants.CONTACT);

        if(!ObjUtils.isNullOrEmpty(contactList)){

            Map<String,Object> contactMap = (Map<String, Object>) contactList.get(0);

            if(!ObjUtils.isNullOrEmpty(contactMap) && !ObjUtils.isNullOrEmpty((String) contactMap.get(ContactConstants.ID))
                    && Boolean.TRUE.equals(Validator.isValidEmail((String)contactMap.get(LOGIN)))){

                contactMap.put(LOGIN,contactMap.get(LOGIN).toString().toLowerCase());
                return contactMap;
            }
        }

       return Map.of();
    }


    public DcmContactDTO validateAndExtractDTOFromRequest(Map<String,Object> payloadMap){

        String eventType = (String) payloadMap.get(EVENT);

        Validator.checkArgument(Boolean.FALSE.equals(this.isValidEventType(eventType))," Not processing for event type : " + eventType);

        Map<Object,Object> dataMap = (Map<Object, Object>) payloadMap.get(DATA);

        Validator.checkArgument(Boolean.FALSE.equals(this.isValidRequestSource((String) dataMap.get(APP_ID)))," Not processing for appID : " + dataMap.get(APP_ID));

        Map<String,Object> contactMap = this.validateContactData(dataMap);

        Validator.checkArgument(ObjUtils.isNullOrEmpty(contactMap)," Not processing due to invalid id/login ");

        return new DcmContactDTO(contactMap);
    }


    public void processFullHook(String payload){
        try{
            Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);
            log.info(" received data : " + payloadMap);

            DcmContactDTO dcmContactDTO = this.validateAndExtractDTOFromRequest(payloadMap);

            Contact contact = ContactImpl.getContactImplInstance().getByID(dcmContactDTO.getId());

            boolean isNewContact = ObjUtils.isNull(contact);
            log.info(" isNewContact ? " + isNewContact);

            if(FULL_HOOKS_UPDATE.equalsIgnoreCase(payloadMap.get(EVENT).toString())){
                if(isNewContact)
                    CreateContactHookHelper.getInstance().processContactCreation(dcmContactDTO);
                else
                    UpdateContactHookHelper.getInstance().processContactUpdation(dcmContactDTO,contact);
            }

            if(Boolean.FALSE.equals(isNewContact) && Boolean.TRUE.equals(dcmContactDTO.getDeleted()))
                DeleteContactHookHelper.getInstance().processContactDeletion(dcmContactDTO);

        }catch (Exception e){
            log.info("Exception in processFullHook : " + e.getMessage());
        }
    }

}



