package com.yoco.user.helper.staff;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.constants.CommonConstants;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;

class UserStaffChannelPublishHelperTest {

    UserStaffChannelPublishHelper userStaffChannelPublishHelper = UserStaffChannelPublishHelper.getInstance();

    @Test
    void publishToAdminChannelOnStaffOperations_valid_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            rtmServiceMockedStatic.when(() -> RTMService.publishToAdminChannel(anyString(),anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            UserDTO userDTOObj = new UserDTO();
            userDTOObj.setAccountID("accountId");
            userDTOObj.setContactID("contactId");
            AccessPolicy accessPolicy = new AccessPolicy();
            userStaffChannelPublishHelper.publishToAdminChannelOnStaffOperations(accessPolicy,userDTOObj,"staff_added");

            Map<String, String> dataToPublish = new HashMap<>();
            dataToPublish.put(RTMService.STATUS, "staff_added");
            dataToPublish.put(CommonConstants.ACCOUNT_ID_KEY, "accountId");
            dataToPublish.put(CommonConstants.CONTACT_ID_KEY, "contactId");
            dataToPublish.put(AccessManager.ACCESS_POLICY, JsonUtil.getJson(accessPolicy) );
            dataToPublish.put(CommonConstants.USER_KEY,JsonUtil.getJson(userDTOObj));

            rtmServiceMockedStatic.verify(() -> RTMService.publishToAdminChannel("accountId",dataToPublish));
        }
    }

    @Test
    void publishToAdminAndUserChannelOnStaffOperations_valid_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            rtmServiceMockedStatic.when(() -> RTMService.publishToAdminChannel(anyString(),anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            rtmServiceMockedStatic.when(() -> RTMService.publishToUserChannel(anyString(),anyString(),anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            UserDTO userDTOObj = new UserDTO();
            userDTOObj.setAccountID("accountId");
            userDTOObj.setContactID("contactId");
            AccessPolicy accessPolicy = new AccessPolicy();
            userStaffChannelPublishHelper.publishToAdminAndUserChannelOnStaffOperations(accessPolicy,userDTOObj,"staff_deleted");

            Map<String, String> dataToPublish = new HashMap<>();
            dataToPublish.put(RTMService.STATUS, "staff_deleted");
            dataToPublish.put(CommonConstants.ACCOUNT_ID_KEY, "accountId");
            dataToPublish.put(CommonConstants.CONTACT_ID_KEY, "contactId");
            dataToPublish.put(AccessManager.ACCESS_POLICY, JsonUtil.getJson(accessPolicy) );
            dataToPublish.put(CommonConstants.USER_KEY,JsonUtil.getJson(userDTOObj));

            rtmServiceMockedStatic.verify(() -> RTMService.publishToAdminChannel("accountId",dataToPublish));
            rtmServiceMockedStatic.verify(() -> RTMService.publishToUserChannel("accountId","contactId",dataToPublish));
        }
    }

}