package com.yoco.contact.helper;

import com.yoco.commons.modal.contact.ContactDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.FCMService;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.constants.CommonConstants;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.Set;

@Slf4j
public class ContactTaskHandler {

    public static ContactTaskHandler getInstance(){
        return new ContactTaskHandler();
    }

    public void handleContactTaskHandler(Map<String, Object> payloadMap){
        if("profileUpdate".equalsIgnoreCase((String) payloadMap.get(CommonConstants.ACTION))){
            this.handleProfileUpdate((UserDTO) payloadMap.get(CommonConstants.USER_KEY),(String) payloadMap.get(CommonConstants.ACTIVITY));
        }
    }

    private void handleProfileUpdate(UserDTO userDTO, String activity){
        ContactDTO contact = userDTO.getContact();
        String accountId = userDTO.getAccountID();
        ActivityUtil.saveActivity(accountId,contact.getId(),"PROFILE_UPDATE",contact.getEmailID(),activity,ActivityUtil.ACTIVITIES.DUMMY.value());
        RTMService.publishToChannel(accountId,"profile_update",CommonConstants.USER_KEY, userDTO);
        FCMService.getFCMService().notifyFCM(accountId,Set.of(contact.getId()),"profile",true, contact.getName(), null);
    }

}
