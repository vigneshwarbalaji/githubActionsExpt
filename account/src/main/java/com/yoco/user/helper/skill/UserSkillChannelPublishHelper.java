package com.yoco.user.helper.skill;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.constants.CommonConstants;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.Map;

@NoArgsConstructor
public class UserSkillChannelPublishHelper {

    public static UserSkillChannelPublishHelper getInstance(){
        return new UserSkillChannelPublishHelper();
    }

    public void publishToChannelWithUpdatedPolicy(AccessPolicy accessPolicy, UserDTO user, Map<String,Object> permissionType){
        String accountID = user.getAccountID();
        String contactID = user.getContactID();

        Map<String, String> dataToPublish = new HashMap<>();
        dataToPublish.put(RTMService.STATUS, "permissions_update");
        dataToPublish.put(CommonConstants.ACCOUNT_ID_KEY, accountID);
        dataToPublish.put(CommonConstants.CONTACT_ID_KEY, contactID);

        if(!ObjUtils.isNullOrEmpty(permissionType)){
            dataToPublish.put("permissionType", JsonUtil.getJson(permissionType));
        }

        dataToPublish.put(AccessManager.ACCESS_POLICY, JsonUtil.getJson(accessPolicy) );
        dataToPublish.put(CommonConstants.USER_KEY,JsonUtil.getJson(user));

        RTMService.publishToAdminChannel(accountID,dataToPublish);
        RTMService.publishToUserChannel(accountID,contactID,dataToPublish);
    }

    public void publishToChannelOnSkillSetOperation(UserDTO user){
        RTMService.publishToChannel(user.getAccountID(), UserSkillHelper.SKILLSETS_OPERATION,CommonConstants.USER_KEY, user);
    }
}