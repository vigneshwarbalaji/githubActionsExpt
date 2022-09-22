package com.yoco.commons.fullservices.iammanagement;

import com.fullauth.api.manage.enums.StatusType;
import com.fullauth.api.manage.exception.FullAuthApiException;
import com.fullauth.api.manage.iam.*;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.utils.GaeUtils;
import com.yoco.commons.utils.ObjUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.util.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;

class AccessManagerTest {

    AccessManager accessManager = AccessManager.getInstance();

    @Test
    void getAccessPolicyByMemberInfo_valid_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                    Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(new AccessPolicy());
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            Assertions.assertNotNull(new AccessManager().getAccessPolicyByMemberInfo("accountId","contactId","type"));
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void getAccessPolicyByMemberInfo_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenThrow(new FullAuthApiException("fullAuthApiException"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            Assertions.assertNull(new AccessManager().getAccessPolicyByMemberInfo("accountId","contactId","type"));
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void getAccessPolicyByMemberInfo_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenThrow(new IOException("IOException"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            Assertions.assertNull(new AccessManager().getAccessPolicyByMemberInfo("accountId","contactId","type"));
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void getPolicy_valid_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.addPermission(IAMPermission.ACTIVITY.toString());
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("accountId");
            userPRO.setContactId("contactId");
            Map<String,Object> response = new AccessManager().getPolicy(userPRO);
            Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
            Assertions.assertTrue(response.containsKey(AccessManager.ACCESS_POLICY));
            Assertions.assertNotNull(response.get(AccessManager.ACCESS_POLICY));
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void getPolicy_null_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(null);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("accountId");
            userPRO.setContactId("contactId");
            Map<String,Object> response = new AccessManager().getPolicy(userPRO);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
            Assertions.assertFalse(response.containsKey(AccessManager.ACCESS_POLICY));
            Assertions.assertNull(response.get(AccessManager.ACCESS_POLICY));
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void getPolicy_nullPermissions_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.setPermissions(null);
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            PeopleRelationJDO userPRO = new PeopleRelationJDO();
            userPRO.setUniquepin("accountId");
            userPRO.setContactId("contactId");
            Map<String,Object> response = new AccessManager().getPolicy(userPRO);
            Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
            Assertions.assertTrue(response.containsKey(AccessManager.ACCESS_POLICY));
            Assertions.assertNotNull(response.get(AccessManager.ACCESS_POLICY));
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addPermission_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.addPermission("clock");
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addPermission("accountId","contactId","clock");
            Assertions.assertNotNull(policyResponse);
            Assertions.assertTrue(policyResponse.getPermissions().contains("clock"));
            Assertions.assertEquals(accessPolicy,policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addPermission_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new FullAuthApiException("Exception from FullAuthApi"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addPermission("accountId","contactId","clock");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addPermission_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new IOException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addPermission("accountId","contactId","clock");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void removePermission_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.addPermission("report");
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().removePermission("accountId","contactId","clock");
            Assertions.assertNotNull(policyResponse);
            Assertions.assertFalse(policyResponse.getPermissions().contains("clock"));
            Assertions.assertEquals(accessPolicy,policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void removePermission_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new FullAuthApiException("Exception from FullAuthApi"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().removePermission("accountId","contactId","clock");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void removePermission_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new IOException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().removePermission("accountId","contactId","clock");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void hasReminderPermission_empty_test(){
        Set<String> permissions = new HashSet<>();
        try(MockedConstruction<PermissionManager> mock = Mockito.mockConstruction(PermissionManager.class, (permissionManagerMock, context) -> {
                Mockito.when(permissionManagerMock.getUserPermissions(any(PeopleRelationJDO.class))).thenReturn(permissions);
            })){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            Assertions.assertFalse(new AccessManager().hasReminderPermission(userPro));
        }
    }

    @Test
    void hasReminderPermission_false_test(){
        Set<String> permissions = new HashSet<>();
        permissions.add("clock");
        try(MockedConstruction<PermissionManager> mock = Mockito.mockConstruction(PermissionManager.class, (permissionManagerMock, context) -> {
            Mockito.when(permissionManagerMock.getUserPermissions(any(PeopleRelationJDO.class))).thenReturn(permissions);
        })){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            Assertions.assertFalse(new AccessManager().hasReminderPermission(userPro));
        }
    }

    @Test
    void hasReminderPermission_true_test(){
        Set<String> permissions = new HashSet<>();
        permissions.add("clock");
        permissions.add(IAMPermission.REMINDER.toString());
        try(MockedConstruction<PermissionManager> mock = Mockito.mockConstruction(PermissionManager.class, (permissionManagerMock, context) -> {
            Mockito.when(permissionManagerMock.getUserPermissions(any(PeopleRelationJDO.class))).thenReturn(permissions);
        })){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            Assertions.assertTrue(new AccessManager().hasReminderPermission(userPro));
        }
    }

    @Test
    void getSuperAdmin_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        List<AccessPolicy> accessPolicyList = new ArrayList<>();
        accessPolicyList.add(accessPolicy);
        AccessPolicyListApiResponse response = Mockito.mock(AccessPolicyListApiResponse.class);
        Mockito.when(response.getAccessPolicies()).thenReturn(accessPolicyList);

        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicies(any(AccessPolicyFetchParams.class))).thenReturn(response);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().getSuperAdmin("accountId");
            Assertions.assertEquals(accessPolicy,policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void getSuperAdmin_FullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicies(any(AccessPolicyFetchParams.class))).thenThrow(new FullAuthApiException("fullAuthApiException"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().getSuperAdmin("accountId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void getSuperAdmin_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicies(any(AccessPolicyFetchParams.class))).thenThrow(new IOException("IOException"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().getSuperAdmin("accountId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void getSuperAdmin_Exception_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicies(any(AccessPolicyFetchParams.class))).thenThrow(new IllegalArgumentException("IOException"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().getSuperAdmin("accountId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void createAccountLevelPolicyForMember_null_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(null);
                Mockito.when(accessPolicyApiMock.createAccessPolicy(any(AccessPolicy.class))).thenReturn(null);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy response = accessManager.createAccountLevelPolicyForUser("accountId","contactId", IAMDefinedRoleType.MEMBER);
            Assertions.assertNull(response);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(2));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(2));
        }
    }

    @Test
    void createAccountLevelPolicyForMember_newPolicy_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(null);
                Mockito.when(accessPolicyApiMock.createAccessPolicy(any(AccessPolicy.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy response = accessManager.createAccountLevelPolicyForUser("accountId","contactId",IAMDefinedRoleType.MEMBER);
            Assertions.assertEquals(accessPolicy,response);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(2));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(2));
        }
    }

    @Test
    void createAccountLevelPolicyForMember_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(null);
                Mockito.when(accessPolicyApiMock.createAccessPolicy(any(AccessPolicy.class))).thenThrow(new FullAuthApiException("exception from fullAuthApi"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy response = accessManager.createAccountLevelPolicyForUser("accountId","contactId",IAMDefinedRoleType.MEMBER);
            Assertions.assertNull(response);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(2));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(2));
        }
    }

    @Test
    void createAccountLevelPolicyForMember_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(null);
                Mockito.when(accessPolicyApiMock.createAccessPolicy(any(AccessPolicy.class))).thenThrow(new IOException("IOException"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy response = accessManager.createAccountLevelPolicyForUser("accountId","contactId",IAMDefinedRoleType.MEMBER);
            Assertions.assertNull(response);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(2));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(2));
        }
    }

    @Test
    void createAccountLevelPolicyForMember_Exception_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(null);
                Mockito.when(accessPolicyApiMock.createAccessPolicy(any(AccessPolicy.class))).thenThrow(new IllegalArgumentException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy response = accessManager.createAccountLevelPolicyForUser("accountId","contactId",IAMDefinedRoleType.MEMBER);
            Assertions.assertNull(response);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(2));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(2));
        }
    }

    @Test
    void createYoCoLevelPolicyForMember_noPolicy_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.setPermissions(PermissionManager.getMemberPermissions(false,"accountId"));
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(null);
                Mockito.when(accessPolicyApiMock.createAccessPolicy(any(AccessPolicy.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy response = accessManager.createYoCoLevelPolicyForMember("accountId","contactId",false);
            Assertions.assertNotNull(response);
            Assertions.assertEquals(accessPolicy,response);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(2));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(2));
        }
    }

    @Test
    void createYoCoLevelPolicyForMember_policyExists_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.setPermissions(PermissionManager.getMemberPermissions(false,"accountId"));
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(accessPolicy);
                Mockito.when(accessPolicyApiMock.createAccessPolicy(any(AccessPolicy.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy response = accessManager.createYoCoLevelPolicyForMember("accountId","contactId",false);
            Assertions.assertNotNull(response);
            Assertions.assertEquals(accessPolicy,response);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void createYoCoLevelPolicyForMember_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(null);
                Mockito.when(accessPolicyApiMock.createAccessPolicy(any(AccessPolicy.class))).thenThrow(new FullAuthApiException("full exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy response = accessManager.createYoCoLevelPolicyForMember("accountId","contactId",false);
            Assertions.assertNull(response);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(2));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(2));
        }
    }

    @Test
    void createYoCoLevelPolicyForMember_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(null);
                Mockito.when(accessPolicyApiMock.createAccessPolicy(any(AccessPolicy.class))).thenThrow(new IOException("IO exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy response = accessManager.createYoCoLevelPolicyForMember("accountId","contactId",false);
            Assertions.assertNull(response);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(2));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(2));
        }
    }

    @Test
    void createYoCoLevelPolicyForMember_Exception_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(null);
                Mockito.when(accessPolicyApiMock.createAccessPolicy(any(AccessPolicy.class))).thenThrow(new IllegalArgumentException("IO exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy response = accessManager.createYoCoLevelPolicyForMember("accountId","contactId",false);
            Assertions.assertNull(response);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(2));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(2));
        }
    }

    @Test
    void createNewPolicyForMember_policy_exists_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.setStatus(StatusType.ACTIVE);
        accessPolicy.setPermissions(PermissionManager.getMemberPermissions(false,"accountId"));
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(accessPolicy).thenReturn(null);
                Mockito.when(accessPolicyApiMock.createAccessPolicy(any(AccessPolicy.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy response = accessManager.createNewPolicyForMember("accountId","contactId",false);
            Assertions.assertEquals(accessPolicy,response);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(2));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(2));
        }
    }

    @Test
    void createNewPolicyForMember_newPolicy_exists_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicy(any(PolicyID.class))).thenReturn(null);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy response = accessManager.createNewPolicyForMember("accountId","contactId",false);
            Assertions.assertNull(response);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(2));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(2));
        }
    }

    @Test
    void addEditPermissionToReports_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new IOException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addEditPermissionToReports("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addEditPermissionToReports_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new FullAuthApiException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addEditPermissionToReports("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addEditPermissionToReports_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.setId("123");
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addEditPermissionToReports("accountId","contactId");
            Assertions.assertNotNull(policyResponse);
            Assertions.assertEquals(accessPolicy,policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void removeReportsPermission_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new IOException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().removeReportsPermission("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void removeReportsPermission_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new FullAuthApiException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().removeReportsPermission("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void removeReportsPermission_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.setId("123");
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().removeReportsPermission("accountId","contactId");
            Assertions.assertNotNull(policyResponse);
            Assertions.assertEquals(accessPolicy,policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addEditAllPermissionToReports_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new IOException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addEditAllPermissionToReports("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addEditAllPermissionToReports_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new FullAuthApiException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addEditAllPermissionToReports("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addEditAllPermissionToReports_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.setId("123");
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addEditAllPermissionToReports("accountId","contactId");
            Assertions.assertNotNull(policyResponse);
            Assertions.assertEquals(accessPolicy,policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addViewPermissionToReports_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new IOException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addViewPermissionToReports("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addViewPermissionToReports_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new FullAuthApiException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addViewPermissionToReports("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addViewPermissionToReports_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.setId("123");
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addViewPermissionToReports("accountId","contactId");
            Assertions.assertNotNull(policyResponse);
            Assertions.assertEquals(accessPolicy,policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addViewAllPermissionToReports_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new IOException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addViewAllPermissionToReports("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addViewAllPermissionToReports_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new FullAuthApiException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addViewAllPermissionToReports("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addViewAllPermissionToReports_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.setId("123");
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addViewAllPermissionToReports("accountId","contactId");
            Assertions.assertNotNull(policyResponse);
            Assertions.assertEquals(accessPolicy,policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addAdjustmentEditPermission_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new IOException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addAdjustmentEditPermission("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addAdjustmentEditPermission_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new FullAuthApiException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addAdjustmentEditPermission("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addAdjustmentEditPermission_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.setId("123");
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addAdjustmentEditPermission("accountId","contactId");
            Assertions.assertNotNull(policyResponse);
            Assertions.assertEquals(accessPolicy,policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addAdjustmentViewPermission_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new IOException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addAdjustmentViewPermission("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addAdjustmentViewPermission_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new FullAuthApiException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addAdjustmentViewPermission("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void addAdjustmentViewPermission_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.setId("123");
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().addAdjustmentViewPermission("accountId","contactId");
            Assertions.assertNotNull(policyResponse);
            Assertions.assertEquals(accessPolicy,policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void removeAdjustmentPermission_IOException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new IOException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().removeAdjustmentPermission("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void removeAdjustmentPermission_fullAuthApiException_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenThrow(new FullAuthApiException("Exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().removeAdjustmentPermission("accountId","contactId");
            Assertions.assertNull(policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void removeAdjustmentPermission_test(){
        AccessPolicy accessPolicy = new AccessPolicy();
        accessPolicy.setId("123");
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.updateAccessPolicy(any(PolicyID.class),any(PolicyUpdateParams.class))).thenReturn(accessPolicy);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");

            AccessPolicy policyResponse = new AccessManager().removeAdjustmentPermission("accountId","contactId");
            Assertions.assertNotNull(policyResponse);
            Assertions.assertEquals(accessPolicy,policyResponse);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullAuthApiKey(),times(1));
        }
    }

    @Test
    void deleteYoCoLevelPolicyForUser_Exception_test(){
        try(MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
            Mockito.when(accessPolicyApiMock.deleteAccessPolicy(any(PolicyID.class))).thenThrow(new FullAuthApiException("Exception"));
        })){
            Assertions.assertFalse(AccessManager.deleteYoCoLevelPolicyForUser("accountID","contactID"));
        }
    }

    @Test
    void deleteYoCoLevelPolicyForUser_valid_test(){
        try(MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
            Mockito.when(accessPolicyApiMock.deleteAccessPolicy(any(PolicyID.class))).thenReturn(true);
        })){
            Assertions.assertTrue(AccessManager.deleteYoCoLevelPolicyForUser("accountID","contactID"));
        }
    }

    @Test
    void getPolicyID_valid_test(){
        Assertions.assertEquals("35af03dd496e9c8d8b10",AccessManager.getPolicyID("accID","123"));
    }

    @Test
    void deleteAccessPolicies_valid_test() throws FullAuthApiException, IOException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");
            AccessManager.deleteAccessPolicies(Set.of("1","2"));
            Mockito.verify(mock.constructed().get(0)).deleteAccessPolicies(Set.of("1","2"));
        }
    }

    @Test
    void getAllActiveAccessPoliciesOfUser_test() throws FullAuthApiException, IOException {
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                Mockito.when(accessPolicyApiMock.getAccessPolicies(any(AccessPolicyFetchParams.class))).thenReturn(new AccessPolicyListApiResponse());
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");
            AccessManager.getAllActiveAccessPoliciesOfUser("contactId");
            Mockito.verify(mock.constructed().get(0)).getAccessPolicies(AccessPolicyFetchParams.builder()
                    .type("yoco").member("user:contactId").status(StatusType.ACTIVE).build());
        }
    }

    @Test
    void createNewPolicyForOwner_exception_test(){
        AccessManager accessManagerMock = Mockito.mock(AccessManager.class);
        Mockito.when(accessManagerMock.createAccountLevelPolicyForUser("accID","123",IAMDefinedRoleType.OWNER)).thenThrow(new IllegalArgumentException("test"));
        Mockito.when(accessManagerMock.createNewPolicyForOwner("accID","123","free")).thenCallRealMethod();
        Assertions.assertNull(accessManagerMock.createNewPolicyForOwner("accID","123","free"));
    }

    @Test
    void createNewPolicyForOwner_nullGlobalPolicy_test(){
        AccessManager accessManagerMock = Mockito.mock(AccessManager.class);
        Mockito.when(accessManagerMock.createAccountLevelPolicyForUser("accID","123",IAMDefinedRoleType.OWNER)).thenReturn(null);
        Mockito.when(accessManagerMock.createNewPolicyForOwner("accID","123","free")).thenCallRealMethod();
        Assertions.assertNull(accessManagerMock.createNewPolicyForOwner("accID","123","free"));
    }

    @Test
    void createNewPolicyForOwner_valid_test(){
        AccessPolicy mockResponse = new AccessPolicy();
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<AccessPolicyApi> mock = Mockito.mockConstruction(AccessPolicyApi.class, (accessPolicyApiMock, context) -> {
                AccessPolicy expectedAccessPolicy = new AccessPolicy("yoco", "account/accID", AccessPolicyMemberType.USER, "123");
                expectedAccessPolicy.setRole(IAMDefinedRoleType.OWNER);
                expectedAccessPolicy.setStatus(StatusType.ACTIVE);
                expectedAccessPolicy.setPermissions(new PermissionManager().getDefaultPermissions());
                Mockito.when(accessPolicyApiMock.createAccessPolicy(expectedAccessPolicy)).thenReturn(mockResponse);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullAuthApiKey).thenReturn("fullAuthApiKey");
            AccessManager accessManagerMock = Mockito.mock(AccessManager.class);
            Mockito.when(accessManagerMock.createAccountLevelPolicyForUser("accID","123",IAMDefinedRoleType.OWNER)).thenReturn(new AccessPolicy());
            Mockito.when(accessManagerMock.createNewPolicyForOwner("accID","123","free")).thenCallRealMethod();
            Assertions.assertEquals(mockResponse,accessManagerMock.createNewPolicyForOwner("accID","123","free"));
        }
    }

}
