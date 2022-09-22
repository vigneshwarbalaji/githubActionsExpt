package com.yoco.user.helper.skill;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.constants.CommonConstants;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;

class UserSkillChannelPublishHelperTest {

    UserSkillChannelPublishHelper userSkillChannelPublishHelper = UserSkillChannelPublishHelper.getInstance();

    @Test
    void publishToChannelWithUpdatedPolicy_valid_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            rtmServiceMockedStatic.when(() -> RTMService.publishToAdminChannel(anyString(),anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            rtmServiceMockedStatic.when(() -> RTMService.publishToUserChannel(anyString(),anyString(),anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            UserDTO userDTOObj = new UserDTO();
            userDTOObj.setAccountID("accountId");
            userDTOObj.setContactID("contactId");
            AccessPolicy accessPolicy = new AccessPolicy();
            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("data","dummy");
            userSkillChannelPublishHelper.publishToChannelWithUpdatedPolicy(accessPolicy,userDTOObj,permissionType);

            Map<String, String> dataToPublish = new HashMap<>();
            dataToPublish.put(RTMService.STATUS, "permissions_update");
            dataToPublish.put(CommonConstants.ACCOUNT_ID_KEY, "accountId");
            dataToPublish.put(CommonConstants.CONTACT_ID_KEY, "contactId");
            dataToPublish.put("permissionType", JsonUtil.getJson(permissionType));
            dataToPublish.put(AccessManager.ACCESS_POLICY, JsonUtil.getJson(accessPolicy) );
            dataToPublish.put(CommonConstants.USER_KEY,JsonUtil.getJson(userDTOObj));

            rtmServiceMockedStatic.verify(() -> RTMService.publishToAdminChannel("accountId",dataToPublish));
            rtmServiceMockedStatic.verify(() -> RTMService.publishToUserChannel("accountId","contactId",dataToPublish));
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void publishToChannelWithUpdatedPolicy_nullPermission_test(Map<String,Object> testValue){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            rtmServiceMockedStatic.when(() -> RTMService.publishToAdminChannel(anyString(),anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            rtmServiceMockedStatic.when(() -> RTMService.publishToUserChannel(anyString(),anyString(),anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            UserDTO userDTOObj = new UserDTO();
            userDTOObj.setAccountID("accountId");
            userDTOObj.setContactID("contactId");
            AccessPolicy accessPolicy = new AccessPolicy();
            userSkillChannelPublishHelper.publishToChannelWithUpdatedPolicy(accessPolicy,userDTOObj,testValue);

            Map<String, String> dataToPublish = new HashMap<>();
            dataToPublish.put(RTMService.STATUS, "permissions_update");
            dataToPublish.put(CommonConstants.ACCOUNT_ID_KEY, "accountId");
            dataToPublish.put(CommonConstants.CONTACT_ID_KEY, "contactId");
            dataToPublish.put(AccessManager.ACCESS_POLICY, JsonUtil.getJson(accessPolicy) );
            dataToPublish.put(CommonConstants.USER_KEY,JsonUtil.getJson(userDTOObj));

            rtmServiceMockedStatic.verify(() -> RTMService.publishToAdminChannel("accountId",dataToPublish));
            rtmServiceMockedStatic.verify(() -> RTMService.publishToUserChannel("accountId","contactId",dataToPublish));
        }
    }

    @Test
    void publishToChannelOnSkillSetOperation_valid_test() {
        try (MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)) {
            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(), anyString(), anyString(), any())).thenAnswer((Answer<Void>) invocation -> null);

            UserDTO userDTOObj = new UserDTO();
            userDTOObj.setAccountID("accountId");
            userDTOObj.setContactID("contactId");
            userSkillChannelPublishHelper.publishToChannelOnSkillSetOperation(userDTOObj);
            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("accountId", UserSkillHelper.SKILLSETS_OPERATION, CommonConstants.USER_KEY, userDTOObj));
        }
    }
}