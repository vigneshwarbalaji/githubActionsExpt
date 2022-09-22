package com.yoco.user.service;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.MockPRO;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.constants.CommonConstants;
import com.yoco.user.helper.skill.UserSkillHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class UserSkillServiceTest {

    UserSkillService userSkillService = new UserSkillService();

    @Test
    void modifyWeeklySkill_addPermission_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,true);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            Mockito.when(userSkillHelper.validateEnterpriseAccountCheck(anyString())).thenReturn(mockResp);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.addPermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            UserDTO userDTO = new UserDTO();
            Mockito.when(userSkillHelper.modifyWeeklySkillHelper(any(PeopleRelationJDO.class),any(AccessPolicy.class),anyBoolean(),anyString())).thenReturn(userDTO);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyWeeklySkill("accountId","contactId","payload");

            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).validateEnterpriseAccountCheck("accountId");
            Mockito.verify(userSkillHelper, times(1)).modifyWeeklySkillHelper(MockPRO.getMockPRO(),accessPolicy,true,"weekly reports mail skillset has been enabled");
            accessManagerMockedStatic.verify(() -> AccessManager.addPermission("accountId","contactId", IAMPermission.WEEKLY_DIGEST.toString()),times(1));
        }
    }

    @Test
    void modifyWeeklySkill_removePermission_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            Mockito.when(userSkillHelper.validateEnterpriseAccountCheck(anyString())).thenReturn(mockResp);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            UserDTO userDTO = new UserDTO();
            Mockito.when(userSkillHelper.modifyWeeklySkillHelper(any(PeopleRelationJDO.class),any(AccessPolicy.class),anyBoolean(),anyString())).thenReturn(userDTO);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyWeeklySkill("accountId","contactId","payload");

            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).validateEnterpriseAccountCheck("accountId");
            Mockito.verify(userSkillHelper, times(1)).modifyWeeklySkillHelper(MockPRO.getMockPRO(),accessPolicy,false,"weekly reports mail skillset has been disabled");
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.WEEKLY_DIGEST.toString()),times(1));
        }
    }

    @Test
    void modifyWeeklySkill_nullPolicy_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            Mockito.when(userSkillHelper.validateEnterpriseAccountCheck(anyString())).thenReturn(mockResp);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(null);

            Map<String,Object> response = userSkillService.modifyWeeklySkill("accountId","contactId","payload");

            Map<String,Object> expectedResponse = new HashMap<>();
            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).validateEnterpriseAccountCheck("accountId");
            Mockito.verify(userSkillHelper, times(0)).modifyWeeklySkillHelper(any(),any(),anyBoolean(),anyString());
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.WEEKLY_DIGEST.toString()),times(1));
        }
    }

    @Test
    void modifyWeeklySkill_nonEnterpriseAccount_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);

            Map<String,Object> mockAccountResp = new HashMap<>();
            mockAccountResp.put(Commons.SUCCESS,false);

            Mockito.when(userSkillHelper.validateEnterpriseAccountCheck(anyString())).thenReturn(mockAccountResp);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyWeeklySkill("accountId","contactId","payload");

            Assertions.assertEquals(mockAccountResp,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).validateEnterpriseAccountCheck("accountId");
            Mockito.verify(userSkillHelper, times(0)).modifyWeeklySkillHelper(any(),any(),anyBoolean(),anyString());
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.WEEKLY_DIGEST.toString()),times(0));
        }
    }

    @Test
    void modifyClockSkill_addPermission_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,true);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.addPermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            UserDTO userDTO = new UserDTO();
            Mockito.when(userSkillHelper.modifyClockSkillHelper(any(PeopleRelationJDO.class),any(AccessPolicy.class),anyBoolean(),anyString())).thenReturn(userDTO);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyClockSkill("accountId","contactId","payload");

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("clock",true);

            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put("permissions", permissionType);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyClockSkillHelper(MockPRO.getMockPRO(),accessPolicy,true,"Clock skillset has been enabled.");
            accessManagerMockedStatic.verify(() -> AccessManager.addPermission("accountId","contactId", IAMPermission.CLOCK.toString()),times(1));
        }
    }

    @Test
    void modifyClockSkill_removePermission_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            UserDTO userDTO = new UserDTO();
            Mockito.when(userSkillHelper.modifyClockSkillHelper(any(PeopleRelationJDO.class),any(AccessPolicy.class),anyBoolean(),anyString())).thenReturn(userDTO);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyClockSkill("accountId","contactId","payload");

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("clock",false);
            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put("permissions", permissionType);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyClockSkillHelper(MockPRO.getMockPRO(),accessPolicy,false,"Clock skillset has been disabled.");
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.CLOCK.toString()),times(1));
        }
    }

    @Test
    void modifyClockSkill_nullPolicy_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(null);

            Map<String,Object> response = userSkillService.modifyClockSkill("accountId","contactId","payload");

            Map<String,Object> expectedResponse = new HashMap<>();
            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(0)).modifyClockSkillHelper(any(),any(),anyBoolean(),anyString());
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.CLOCK.toString()),times(1));
        }
    }

    @Test
    void modifyForceClockOutSkill_addPermission_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,true);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.addPermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            UserDTO userDTO = new UserDTO();
            Mockito.when(userSkillHelper.modifyForceClockOutSkillHelper(any(PeopleRelationJDO.class),any(AccessPolicy.class),anyBoolean(),anyString())).thenReturn(userDTO);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyForceClockOutSkill("accountId","contactId","payload");

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("forceClockOut",true);

            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put("permissions", permissionType);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyForceClockOutSkillHelper(MockPRO.getMockPRO(),accessPolicy,true,"Force clockout skillset has been enabled");
            accessManagerMockedStatic.verify(() -> AccessManager.addPermission("accountId","contactId", IAMPermission.FORCE_CLOCK_OUT.toString()),times(1));
        }
    }

    @Test
    void modifyForceClockOutSkill_removePermission_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            UserDTO userDTO = new UserDTO();
            Mockito.when(userSkillHelper.modifyForceClockOutSkillHelper(any(PeopleRelationJDO.class),any(AccessPolicy.class),anyBoolean(),anyString())).thenReturn(userDTO);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyForceClockOutSkill("accountId","contactId","payload");

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("forceClockOut",false);

            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put("permissions", permissionType);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyForceClockOutSkillHelper(MockPRO.getMockPRO(),accessPolicy,false,"Force clockout skillset has been disabled");
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.FORCE_CLOCK_OUT.toString()),times(1));
        }
    }

    @Test
    void modifyForceClockOutSkill_nullPolicy_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(null);

            Map<String,Object> response = userSkillService.modifyForceClockOutSkill("accountId","contactId","payload");

            Map<String,Object> expectedResponse = new HashMap<>();
            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(0)).modifyForceClockOutSkillHelper(any(),any(),anyBoolean(),anyString());
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.FORCE_CLOCK_OUT.toString()),times(1));
        }
    }

    @Test
    void modifyReportSkill_valid_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,true);
            mockResp.put(Commons.SUCCESS,true);

            UserDTO userDTO = new UserDTO();
            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            Mockito.when(userSkillHelper.modifyReportSkillHelper(any(PeopleRelationJDO.class),anyBoolean(),any(),any())).thenReturn(expectedResponse);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyReportSkill("accountId","contactId","payload");

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyReportSkillHelper(MockPRO.getMockPRO(),true,null, null);
        }
    }

    @Test
    void modifyAdjustmentSkill_valid_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,true);
            mockResp.put(Commons.SUCCESS,true);

            UserDTO userDTO = new UserDTO();
            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            Mockito.when(userSkillHelper.modifyAdjustmentSkillHelper(any(PeopleRelationJDO.class),anyBoolean(),any())).thenReturn(expectedResponse);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyAdjustmentSkill("accountId","contactId","payload");

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyAdjustmentSkillHelper(MockPRO.getMockPRO(),true,null);
        }
    }

    @Test
    void modifyActivitySkill_addPermission_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,true);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.addPermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            UserDTO userDTO = new UserDTO();
            Mockito.when(userSkillHelper.modifyActivitySkillHelper(any(PeopleRelationJDO.class),any(AccessPolicy.class),anyBoolean(),anyString())).thenReturn(userDTO);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyActivitySkill("accountId","contactId","payload");

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("activity",true);

            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put("permissions", permissionType);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyActivitySkillHelper(MockPRO.getMockPRO(),accessPolicy,true,"Activities access has been enabled.");
            accessManagerMockedStatic.verify(() -> AccessManager.addPermission("accountId","contactId", IAMPermission.ACTIVITY.toString()),times(1));
        }
    }

    @Test
    void modifyActivitySkill_removePermission_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            UserDTO userDTO = new UserDTO();
            Mockito.when(userSkillHelper.modifyActivitySkillHelper(any(PeopleRelationJDO.class),any(AccessPolicy.class),anyBoolean(),anyString())).thenReturn(userDTO);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyActivitySkill("accountId","contactId","payload");

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("activity",false);
            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put("permissions", permissionType);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyActivitySkillHelper(MockPRO.getMockPRO(),accessPolicy,false,"Activities access has been disabled.");
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.ACTIVITY.toString()),times(1));
        }
    }

    @Test
    void modifyActivitySkill_nullPolicy_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(null);

            Map<String,Object> response = userSkillService.modifyActivitySkill("accountId","contactId","payload");

            Map<String,Object> expectedResponse = new HashMap<>();
            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(0)).modifyActivitySkillHelper(any(),any(),anyBoolean(),anyString());
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.ACTIVITY.toString()),times(1));
        }
    }

    @Test
    void modifyConfirmHoursSkill_addPermission_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,true);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.addPermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            UserDTO userDTO = new UserDTO();
            Mockito.when(userSkillHelper.modifyConfirmHoursSkillHelper(any(PeopleRelationJDO.class),any(AccessPolicy.class),anyBoolean(),anyString())).thenReturn(userDTO);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyConfirmHoursSkill("accountId","contactId","payload");

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("confirmHours",true);

            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put("permissions", permissionType);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyConfirmHoursSkillHelper(MockPRO.getMockPRO(),accessPolicy,true,"Confirm hours skillset has been enabled.");
            accessManagerMockedStatic.verify(() -> AccessManager.addPermission("accountId","contactId", IAMPermission.CONFIRM_HOURS.toString()),times(1));
        }
    }

    @Test
    void modifyConfirmHoursSkill_removePermission_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            UserDTO userDTO = new UserDTO();
            Mockito.when(userSkillHelper.modifyConfirmHoursSkillHelper(any(PeopleRelationJDO.class),any(AccessPolicy.class),anyBoolean(),anyString())).thenReturn(userDTO);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyConfirmHoursSkill("accountId","contactId","payload");

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put("confirmHours",false);
            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put("permissions", permissionType);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyConfirmHoursSkillHelper(MockPRO.getMockPRO(),accessPolicy,false,"Confirm hours skillset has been disabled.");
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.CONFIRM_HOURS.toString()),times(1));
        }
    }

    @Test
    void modifyConfirmHoursSkill_nullPolicy_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(null);

            Map<String,Object> response = userSkillService.modifyConfirmHoursSkill("accountId","contactId","payload");

            Map<String,Object> expectedResponse = new HashMap<>();
            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(0)).modifyConfirmHoursSkillHelper(any(),any(),anyBoolean(),anyString());
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.CONFIRM_HOURS.toString()),times(1));
        }
    }

    @Test
    void modifyTeamReportSkill_valid_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,true);
            mockResp.put(Commons.SUCCESS,true);

            UserDTO userDTO = new UserDTO();
            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            Mockito.when(userSkillHelper.modifyTeamReportSkillHelper(any(PeopleRelationJDO.class),anyBoolean(),any(),any())).thenReturn(expectedResponse);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyTeamReportSkill("accountId","contactId","payload");

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyTeamReportSkillHelper(MockPRO.getMockPRO(),true,null, null);
        }
    }

    @Test
    void modifyReminderSkill_addPermission_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,true);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.addPermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            UserDTO userDTO = new UserDTO();
            Mockito.when(userSkillHelper.modifyReminderSkillHelper(any(PeopleRelationJDO.class),any(AccessPolicy.class),anyBoolean(),anyString())).thenReturn(userDTO);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyReminderSkill("accountId","contactId","payload");

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put(UserSkillService.CLOCK_REMINDER,true);

            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put("permissions", permissionType);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyReminderSkillHelper(MockPRO.getMockPRO(),accessPolicy,true,"reminder skillset has been enabled");
            accessManagerMockedStatic.verify(() -> AccessManager.addPermission("accountId","contactId", IAMPermission.REMINDER.toString()),times(1));
        }
    }

    @Test
    void modifyReminderSkill_removePermission_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(accessPolicy);

            UserDTO userDTO = new UserDTO();
            Mockito.when(userSkillHelper.modifyReminderSkillHelper(any(PeopleRelationJDO.class),any(AccessPolicy.class),anyBoolean(),anyString())).thenReturn(userDTO);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String,Object> response = userSkillService.modifyReminderSkill("accountId","contactId","payload");

            Map<String,Object> permissionType = new HashMap<>();
            permissionType.put(UserSkillService.CLOCK_REMINDER,false);
            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put("permissions", permissionType);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(1)).modifyReminderSkillHelper(MockPRO.getMockPRO(),accessPolicy,false,"reminder skillset has been disabled");
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.REMINDER.toString()),times(1));
        }
    }

    @Test
    void modifyReminderSkill_nullPolicy_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(UserSkillService.ACCESS_KEY,false);
            mockResp.put(Commons.SUCCESS,true);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            accessManagerMockedStatic.when(() -> AccessManager.removePermission(anyString(),anyString(),anyString())).thenReturn(null);

            Map<String,Object> response = userSkillService.modifyReminderSkill("accountId","contactId","payload");

            Map<String,Object> expectedResponse = new HashMap<>();
            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId","payload",true);
            Mockito.verify(userSkillHelper, times(0)).modifyReminderSkillHelper(any(),any(),anyBoolean(),anyString());
            accessManagerMockedStatic.verify(() -> AccessManager.removePermission("accountId","contactId", IAMPermission.REMINDER.toString()),times(1));
        }
    }

    @Test
    void updateReminderTime_withOutReminderTime_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(Commons.SUCCESS,true);

            UserDTO userDTO = new UserDTO();
            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            Mockito.when(userSkillHelper.modifyReminderTimeHelper(any(PeopleRelationJDO.class),anyInt())).thenReturn(expectedResponse);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String, Object>payloadMap = new HashMap<>();

            String payloadString = JsonUtil.getJson(payloadMap);

            Map<String,Object> response = userSkillService.updateReminderTime("accountId","contactId", payloadString);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId",payloadString,false);
            Mockito.verify(userSkillHelper, times(1)).modifyReminderTimeHelper(MockPRO.getMockPRO(),0);
        }
    }

    @Test
    void updateReminderTime_valid_test() throws IOException {

        try(MockedStatic<UserSkillHelper> userSkillHelperMockedStatic = Mockito.mockStatic(UserSkillHelper.class)){

            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put(CommonConstants.USER_KEY, MockPRO.getMockPRO());
            mockResp.put(Commons.SUCCESS,true);

            UserDTO userDTO = new UserDTO();
            Map<String,Object> expectedResponse = new HashMap<>();
            expectedResponse.put(Commons.SUCCESS,true);
            expectedResponse.put(UserSkillService.UPDATED_PRO,userDTO);

            UserSkillHelper userSkillHelper = Mockito.mock(UserSkillHelper.class);
            Mockito.when(userSkillHelper.validatePayloadAndExtractSkillDetails(anyString(),anyString(),anyString(),anyBoolean())).thenReturn(mockResp);
            Mockito.when(userSkillHelper.modifyReminderTimeHelper(any(PeopleRelationJDO.class),anyInt())).thenReturn(expectedResponse);
            userSkillHelperMockedStatic.when(UserSkillHelper::getInstance).thenReturn(userSkillHelper);

            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put(UserSkillService.REMINDER_TIME, 2);

            String payloadString = JsonUtil.getJson(payloadMap);

            Map<String,Object> response = userSkillService.updateReminderTime("accountId","contactId", payloadString);

            Assertions.assertEquals(expectedResponse,response);

            Mockito.verify(userSkillHelper, times(1)).validatePayloadAndExtractSkillDetails("accountId","contactId",payloadString,false);
            Mockito.verify(userSkillHelper, times(1)).modifyReminderTimeHelper(MockPRO.getMockPRO(),2);
        }
    }

}