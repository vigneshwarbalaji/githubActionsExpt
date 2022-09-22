package com.yoco.user.helper.staff;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.constants.CommonConstants;

import java.util.HashMap;
import java.util.Map;

public class UserStaffChannelPublishHelper {

    public static UserStaffChannelPublishHelper getInstance(){
        return new UserStaffChannelPublishHelper();
    }

    private Map<String,String> extractPublishData(AccessPolicy accessPolicy, UserDTO user, String action){
        String accountID = user.getAccountID();
        String contactID = user.getContactID();
        Map<String, String> dataToPublish = new HashMap<>();
        dataToPublish.put(RTMService.STATUS, action);
        dataToPublish.put(CommonConstants.ACCOUNT_ID_KEY, accountID);
        dataToPublish.put(CommonConstants.CONTACT_ID_KEY, contactID);
        if(!ObjUtils.isNull(accessPolicy)){
            dataToPublish.put(AccessManager.ACCESS_POLICY, JsonUtil.getJson(accessPolicy) );
        }
        dataToPublish.put(CommonConstants.USER_KEY,JsonUtil.getJson(user));
        return dataToPublish;
    }

    public void publishToAdminChannelOnStaffOperations(AccessPolicy accessPolicy, UserDTO user, String action){
        String accountID = user.getAccountID();
        RTMService.publishToAdminChannel(accountID,extractPublishData(accessPolicy,user,action));
    }

    // NOTE: Make use of this method to publish to both user and admin channels on staff enable/disable events
    public void publishToAdminAndUserChannelOnStaffOperations(AccessPolicy accessPolicy, UserDTO user, String action){
        String accountID = user.getAccountID();
        String contactID = user.getContactID();
        Map<String,String> publishMap = extractPublishData(accessPolicy,user,action);
        RTMService.publishToAdminChannel(accountID,publishMap);
        RTMService.publishToUserChannel(accountID,contactID,publishMap);
    }

}
