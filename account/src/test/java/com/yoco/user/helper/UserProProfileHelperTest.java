package com.yoco.user.helper;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.MockPRO;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.constants.CommonConstants;
import com.yoco.user.enums.USER_ERROR_MESSAGE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class UserProProfileHelperTest {

    UserProProfileHelper userService = UserProProfileHelper.getInstance();

    @Test
    void validateRole_admin_test(){
        Assertions.assertDoesNotThrow(() -> userService.validateRole(Skillset.ROLE_ADMIN));
    }

    @Test
    void validateRole_staff_test(){
        Assertions.assertDoesNotThrow(() -> userService.validateRole(Skillset.ROLE_STAFF));
    }

    @Test
    void validateRole_invalid_test(){
        try{
            userService.validateRole("roles");
        }catch (Exception e){
            Assertions.assertEquals(USER_ERROR_MESSAGE.INVALID_ROLE.value(),e.getMessage());
        }
    }

    @Test
    void generateUpdateRoleTaskQueueMap_test(){
        UserDTO userDTO = new UserDTO();
        Map<String,Object> proMap = new HashMap<>();
        Map<String,Object> resp = userService.generateUpdateProProfileTaskQueueMap(userDTO,proMap);

        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put(CommonConstants.ACTION,"updateProProfile");
        queueMap.put("updatedProMap",proMap);

        Assertions.assertEquals(queueMap,resp);
    }

    @Test
    void updateProfilePRO_updateRole_staff_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.addPermission(IAMPermission.ACTIVITY.toString());
            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            mockedStatic.when(() -> UserPROUtil.updatePRO(any(PeopleRelationJDO.class))).thenReturn(userPRO);

            userTaskInitiatorMockedStatic.when(() -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> proMap = new HashMap<>();
            proMap.put(UserProProfileHelper.ROLE,Skillset.ROLE_STAFF);
            UserDTO rep = userService.updateProfilePRO(proMap,userPRO);

            UserDTO userDTO = new UserDTO(userPRO,accessPolicy.getPermissions());

            Assertions.assertNotNull(rep);
            Assertions.assertEquals(userDTO,rep);
            mockedStatic.verify( () -> UserPROUtil.updatePRO(userPRO),times(1));
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId",IAMPermission.ADMIN.toString()),times(1));

            Map<String,Object> updatedProMap = new HashMap<>();
            updatedProMap.put(UserProProfileHelper.ROLE,Skillset.ROLE_STAFF);
            updatedProMap.put(CommonConstants.USER_KEY,userPRO);
            updatedProMap.put("isRoleUpdated",true);
            updatedProMap.put(CommonConstants.ACTIVITY,"");
            updatedProMap.put(CommonConstants.POLICY,accessPolicy);

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put(CommonConstants.USER_KEY,userDTO);
            queueMap.put(CommonConstants.ACTION,"updateProProfile");
            queueMap.put("updatedProMap",updatedProMap);

            userTaskInitiatorMockedStatic.verify(() ->  UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void updateProfilePRO_updateRole_admin_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.addPermission(IAMPermission.SUPER_ADMIN.toString());
            accessManagerMockedStatic.when(() -> AccessManager.addPermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            mockedStatic.when(() -> UserPROUtil.updatePRO(any(PeopleRelationJDO.class))).thenReturn(userPRO);

            userTaskInitiatorMockedStatic.when(() -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> proMap = new HashMap<>();
            proMap.put(UserProProfileHelper.ROLE,Skillset.ROLE_ADMIN);
            UserDTO rep = userService.updateProfilePRO(proMap,userPRO);

            UserDTO userDTO = new UserDTO(userPRO,accessPolicy.getPermissions());

            Assertions.assertNotNull(rep);
            Assertions.assertEquals(userDTO,rep);
            mockedStatic.verify( () -> UserPROUtil.updatePRO(userPRO),times(1));
            accessManagerMockedStatic.verify(() -> AccessManager.addPermission("accountId","contactId",IAMPermission.ADMIN.toString()),times(1));

            Map<String,Object> updatedProMap = new HashMap<>();
            updatedProMap.put(UserProProfileHelper.ROLE,Skillset.ROLE_ADMIN);
            updatedProMap.put(CommonConstants.USER_KEY,userPRO);
            updatedProMap.put("isRoleUpdated",true);
            updatedProMap.put(CommonConstants.ACTIVITY,"");
            updatedProMap.put(CommonConstants.POLICY,accessPolicy);

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put(CommonConstants.USER_KEY,userDTO);
            queueMap.put(CommonConstants.ACTION,"updateProProfile");
            queueMap.put("updatedProMap",updatedProMap);

            userTaskInitiatorMockedStatic.verify(() ->  UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void updateProfilePRO_updateRole_nullPolicy_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            accessManagerMockedStatic.when(() -> AccessManager.addPermission(anyString(),anyString(),anyString())).thenReturn(null);

            Map<String,Object> proMap = new HashMap<>();
            proMap.put(UserProProfileHelper.ROLE,Skillset.ROLE_ADMIN);
            UserDTO rep = userService.updateProfilePRO(proMap,userPRO);

            Assertions.assertNull(rep);

            mockedStatic.verify( () -> UserPROUtil.updatePRO(userPRO),times(0));
            accessManagerMockedStatic.verify(() -> AccessManager.addPermission("accountId","contactId",IAMPermission.ADMIN.toString()),times(1));
            userTaskInitiatorMockedStatic.verify(() ->  UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap()),times(0));
        }
    }

    @Test
    void updateProfilePRO_updateRole_nullPolicyPermissions_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setPermissions(null);
            accessManagerMockedStatic.when(() -> AccessManager.addPermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            Map<String,Object> proMap = new HashMap<>();
            proMap.put(UserProProfileHelper.ROLE,Skillset.ROLE_ADMIN);
            UserDTO rep = userService.updateProfilePRO(proMap,userPRO);

            Assertions.assertNull(rep);

            mockedStatic.verify( () -> UserPROUtil.updatePRO(userPRO),times(0));
            accessManagerMockedStatic.verify(() -> AccessManager.addPermission("accountId","contactId",IAMPermission.ADMIN.toString()),times(1));
            userTaskInitiatorMockedStatic.verify(() ->  UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap()),times(0));
        }
    }

    @Test
    void updateProfilePRO_updateEmployeeId_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        userPRO.setLogin("login");
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            mockedStatic.when(() -> UserPROUtil.updatePRO(any(PeopleRelationJDO.class))).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when(() -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> proMap = new HashMap<>();
            proMap.put("employeeID","empId");
            UserDTO rep = userService.updateProfilePRO(proMap,userPRO);

            UserDTO userDTO = new UserDTO(userPRO,null);

            Assertions.assertNotNull(rep);
            Assertions.assertEquals(userDTO,rep);
            mockedStatic.verify( () -> UserPROUtil.updatePRO(userPRO),times(1));

            Map<String,Object> updatedProMap = new HashMap<>();
            updatedProMap.put(CommonConstants.USER_KEY,userPRO);
            updatedProMap.put("isRoleUpdated",false);
            updatedProMap.put(CommonConstants.ACTIVITY,"contactId: EmployeeID has been changed from login to empId");

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put(CommonConstants.USER_KEY,userDTO);
            queueMap.put(CommonConstants.ACTION,"updateProProfile");
            queueMap.put("updatedProMap",updatedProMap);

            userTaskInitiatorMockedStatic.verify(() ->  UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void updateProfilePRO_updateRfId_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        userPRO.setRfId("rfid");
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            mockedStatic.when(() -> UserPROUtil.updatePRO(any(PeopleRelationJDO.class))).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when(() -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> proMap = new HashMap<>();
            proMap.put("nfcID","nfcId");
            UserDTO rep = userService.updateProfilePRO(proMap,userPRO);

            UserDTO userDTO = new UserDTO(userPRO,null);

            Assertions.assertNotNull(rep);
            Assertions.assertEquals(userDTO,rep);
            mockedStatic.verify( () -> UserPROUtil.updatePRO(userPRO),times(1));

            Map<String,Object> updatedProMap = new HashMap<>();
            updatedProMap.put(CommonConstants.USER_KEY,userPRO);
            updatedProMap.put("isRoleUpdated",false);
            updatedProMap.put(CommonConstants.ACTIVITY,"contactId: NfcRFID has been changed from rfid to nfcId");

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put(CommonConstants.USER_KEY,userDTO);
            queueMap.put(CommonConstants.ACTION,"updateProProfile");
            queueMap.put("updatedProMap",updatedProMap);

            userTaskInitiatorMockedStatic.verify(() ->  UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void updateProfilePRO_updateEmployeeId_and_updateRfId_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        userPRO.setRfId("rfid");
        userPRO.setLogin("login");
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            mockedStatic.when(() -> UserPROUtil.updatePRO(any(PeopleRelationJDO.class))).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when(() -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> proMap = new HashMap<>();
            proMap.put("nfcID","nfcId");
            proMap.put("employeeID","empId");
            UserDTO rep = userService.updateProfilePRO(proMap,userPRO);

            UserDTO userDTO = new UserDTO(userPRO,null);

            Assertions.assertNotNull(rep);
            Assertions.assertEquals(userDTO,rep);
            mockedStatic.verify( () -> UserPROUtil.updatePRO(userPRO),times(1));

            Map<String,Object> updatedProMap = new HashMap<>();
            updatedProMap.put(CommonConstants.USER_KEY,userPRO);
            updatedProMap.put("isRoleUpdated",false);
            updatedProMap.put(CommonConstants.ACTIVITY,"contactId: EmployeeID has been changed from login to empId; NfcRFID has been changed from rfid to nfcId");

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put(CommonConstants.USER_KEY,userDTO);
            queueMap.put(CommonConstants.ACTION,"updateProProfile");
            queueMap.put("updatedProMap",updatedProMap);

            userTaskInitiatorMockedStatic.verify(() ->  UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }


}