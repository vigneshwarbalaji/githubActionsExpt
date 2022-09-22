package com.yoco.user.helper.skill;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.MockPRO;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.AccountUtil;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.constants.CommonConstants;
import com.yoco.user.enums.USER_ERROR_MESSAGE;
import com.yoco.user.helper.UserTaskInitiator;
import com.yoco.user.service.UserSkillService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class UserSkillHelperTest {

    UserSkillHelper userSkillHelper = UserSkillHelper.getInstance();

    @ParameterizedTest
    @NullAndEmptySource
    void validatePayloadAndExtractSkillDetails_invalid_accountID_test(String testValue){
        try{
            userSkillHelper.validatePayloadAndExtractSkillDetails(testValue,"contactId","payload",true);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value() ,e.getMessage());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void validatePayloadAndExtractSkillDetails_invalid_contactID_test(String testValue){
        try{
            userSkillHelper.validatePayloadAndExtractSkillDetails("accountId",testValue,"payload",true);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value() ,e.getMessage());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"payload"})
    void validatePayloadAndExtractSkillDetails_invalid_payload_test(String testValue){
        try{
            userSkillHelper.validatePayloadAndExtractSkillDetails("accountId","contactId",testValue,true);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value() ,e.getMessage());
        }
    }

   @Test
    void validatePayloadAndExtractSkillDetails_invalid_payloadJson_test(){
        try{
            String payload = JsonUtil.getJson(new HashMap<>());
            userSkillHelper.validatePayloadAndExtractSkillDetails("accountId","contactId",payload,true);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value() ,e.getMessage());
        }
    }

    @Test
    void validatePayloadAndExtractSkillDetails_nullPRO_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserProWithContact(anyString(), anyString())).thenReturn(null);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("type","skill");
            String payload = JsonUtil.getJson(payloadMap);
            userSkillHelper.validatePayloadAndExtractSkillDetails("accountId","contactId",payload,true);
        }catch (Exception e){
            Assertions.assertEquals(USER_ERROR_MESSAGE.USER_NOT_FOUND.value() ,e.getMessage());
        }
    }

    @Test
    void validatePayloadAndExtractSkillDetails_valid_test(){
        PeopleRelationJDO userPRO = new PeopleRelationJDO();
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserProWithContact(anyString(), anyString())).thenReturn(userPRO);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("access",false);
            String payload = JsonUtil.getJson(payloadMap);
            Map<String,Object> response = userSkillHelper.validatePayloadAndExtractSkillDetails("accountId","contactId",payload,true);

            Map<String, Object> expectedMap = new HashMap<>();
            expectedMap.put(CommonConstants.USER_KEY,userPRO);
            expectedMap.put(UserSkillService.ACCESS_KEY,false);
            Assertions.assertEquals(expectedMap,response);
        }
    }

    @Test
    void validatePayloadAndExtractSkillDetails_noAccess_test(){
        PeopleRelationJDO userPRO = new PeopleRelationJDO();
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserProWithContact(anyString(), anyString())).thenReturn(userPRO);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("key","dummy");
            String payload = JsonUtil.getJson(payloadMap);
            Map<String,Object> response = userSkillHelper.validatePayloadAndExtractSkillDetails("accountId","contactId",payload,false);

            Map<String, Object> expectedMap = new HashMap<>();
            expectedMap.put(CommonConstants.USER_KEY,userPRO);
            Assertions.assertEquals(expectedMap,response);
            Assertions.assertFalse(response.containsKey("access"));
        }
    }

    @Test
    void validateAndExtractPayload_error_test(){
        try{
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("access","dummy");
            userSkillHelper.validateAndExtractPayload(payloadMap);
        }catch (Exception e){
            Assertions.assertEquals(USER_ERROR_MESSAGE.ACCESS_VALUE_BOOLEAN_ERROR_MESSAGE.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractPayload_AccessKey_test(){
        Map<String,Object> payloadMap = new HashMap<>();
        payloadMap.put("access",true);
        Assertions.assertEquals(payloadMap,userSkillHelper.validateAndExtractPayload(payloadMap));
    }

    @Test
    void validateAndExtractPayload_containsAccessKey_typeTeamIDAndScope_test(){
        Map<String,Object> payloadMap = new HashMap<>();
        payloadMap.put("access",true);
        payloadMap.put("scope", "all");
        payloadMap.put("type","edit");
        payloadMap.put("teamID","teamID");
        Assertions.assertEquals(payloadMap,userSkillHelper.validateAndExtractPayload(payloadMap));
    }

    @Test
    void validateEnterpriseAccountCheck_successFalse_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accountUtilMockedStatic.when(() -> AccountUtil.isActiveEnterpriseAccount(anyString())).thenReturn(false);
            Map<String,Object> response =  userSkillHelper.validateEnterpriseAccountCheck("accountId");
            Map<String, Object> responseMap = new HashMap<>();
            responseMap.put(Commons.SUCCESS,false);
            responseMap.put(Commons.MESSAGE,COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
            Assertions.assertEquals(responseMap,response);
        }
    }

    @Test
    void validateEnterpriseAccountCheck_successTrue_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accountUtilMockedStatic.when(() -> AccountUtil.isActiveEnterpriseAccount(anyString())).thenReturn(true);
            Map<String,Object> response =  userSkillHelper.validateEnterpriseAccountCheck("accountId");
            Map<String, Object> responseMap = new HashMap<>();
            responseMap.put(Commons.SUCCESS,true);
            Assertions.assertEquals(responseMap,response);
        }
    }

    @Test
    void generateWeeklySkillTaskQueueMap_test(){
        UserDTO userDTO = new UserDTO();
        AccessPolicy accessPolicy = new AccessPolicy();
        Map<String,Object> weeklyMap = userSkillHelper.generateWeeklySkillTaskQueueMap(userDTO,accessPolicy,true);

        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put("user",userDTO);
        queueMap.put("action","weeklySkill");
        queueMap.put("policy",accessPolicy);
        queueMap.put("weeklyMail",true);
        Assertions.assertEquals(queueMap,weeklyMap);
    }

    @Test
    void modifyWeeklySkillHelper_valid_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class);
            MockedStatic<UserSkillTaskHelper> userTaskHelperMockedStatic = Mockito.mockStatic(UserSkillTaskHelper.class)){

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REMINDER, false);

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyWeeklySkillHelper(userPRO,accessPolicy,false,"activity");
            Assertions.assertNotNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            skillMap.put(Skillset.SKILL_SET_KEY_WEEKLY_MAIL, false);
            mockedStatic.verify( () -> UserPROUtil.updatePROSkill(userPRO,skillMap),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",new UserDTO(userPRO,null));
            queueMap.put("action","weeklySkill");
            queueMap.put("policy",accessPolicy);
            queueMap.put("weeklyMail",false);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyWeeklySkillHelper_null_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            Map<String,Object> skillMap = new HashMap<>();
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyWeeklySkillHelper(userPRO,accessPolicy,false,"activity");
            Assertions.assertNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));
        }
    }

    @Test
    void updateSkillHelper_valid_test(){
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){

            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.addPermission(IAMPermission.ACTIVITY.toString());

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_WEEKLY_MAIL, true);

            UserDTO rep = new UserSkillHelper().updateSkillHelper(userPRO,skillMap,"activity",accessPolicy);
            Assertions.assertNotNull(rep);
            Assertions.assertEquals(new UserDTO(userPRO,accessPolicy.getPermissions()),rep);
            mockedStatic.verify( () -> UserPROUtil.updatePROSkill(userPRO,skillMap),times(1));
            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class)),times(1));
        }
    }

    @Test
    void updateSkillHelper_nullPolicy_test(){
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){

            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_WEEKLY_MAIL, true);

            UserDTO rep = new UserSkillHelper().updateSkillHelper(userPRO,skillMap,"activity",null);
            Assertions.assertNotNull(rep);
            Assertions.assertEquals(new UserDTO(userPRO,null),rep);
            mockedStatic.verify( () -> UserPROUtil.updatePROSkill(userPRO,skillMap),times(1));
            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class)),times(1));
        }
    }

    @Test
    void generateClockSkillTaskQueueMap_test(){
        UserDTO userDTO = new UserDTO();
        AccessPolicy accessPolicy = new AccessPolicy();
        Map<String,Object> clockMap = userSkillHelper.generateClockSkillTaskQueueMap(userDTO,accessPolicy,true);

        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put("user",userDTO);
        queueMap.put("action","clockSkill");
        queueMap.put("policy",accessPolicy);
        queueMap.put("clockSkill",true);
        Assertions.assertEquals(queueMap,clockMap);
    }

    @Test
    void modifyClockSkillHelper_disableClockSkillSet_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_WEB_CLOCK_IN, false);

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyClockSkillHelper(userPRO,accessPolicy,false,"activity");
            Assertions.assertNotNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            skillMap.put(Skillset.SKILL_SET_KEY_WEB_CLOCK_IN, false);
            mockedStatic.verify( () -> UserPROUtil.updatePROSkill(userPRO,skillMap),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",new UserDTO(userPRO,null));
            queueMap.put("action","clockSkill");
            queueMap.put("policy",accessPolicy);
            queueMap.put("clockSkill",false);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyClockSkillHelper_valid_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_WEB_CLOCK_IN, true);

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyClockSkillHelper(userPRO,accessPolicy,true,"activity");
            Assertions.assertNotNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            skillMap.put(Skillset.SKILL_SET_KEY_WEB_CLOCK_IN, true);
            mockedStatic.verify( () -> UserPROUtil.updatePROSkill(userPRO,skillMap),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",new UserDTO(userPRO,null));
            queueMap.put("action","clockSkill");
            queueMap.put("policy",accessPolicy);
            queueMap.put("clockSkill",true);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyClockSkillHelper_null_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            Map<String,Object> skillMap = new HashMap<>();
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyClockSkillHelper(userPRO,accessPolicy,false,"activity");
            Assertions.assertNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));
        }
    }

    @Test
    void generateForceClockOutSkillTaskQueueMap_test(){
        UserDTO userDTO = new UserDTO();
        AccessPolicy accessPolicy = new AccessPolicy();
        Map<String,Object> forceClockOutMap = userSkillHelper.generateForceClockOutSkillTaskQueueMap(userDTO,accessPolicy,true);

        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put("user",userDTO);
        queueMap.put("action","forceClockOutSkill");
        queueMap.put("policy",accessPolicy);
        queueMap.put("forceClockOutSkill",true);
        Assertions.assertEquals(queueMap,forceClockOutMap);
    }

    @Test
    void modifyForceClockOutSkillHelper_disableClockSkillSet_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_FORCE_CLOCK_OUT, false);

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyForceClockOutSkillHelper(userPRO,accessPolicy,false,"activity");
            Assertions.assertNotNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            skillMap.put(Skillset.SKILL_SET_KEY_FORCE_CLOCK_OUT, false);
            mockedStatic.verify( () -> UserPROUtil.updatePROSkill(userPRO,skillMap),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",new UserDTO(userPRO,null));
            queueMap.put("action","forceClockOutSkill");
            queueMap.put("policy",accessPolicy);
            queueMap.put("forceClockOutSkill",false);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyForceClockOutSkillHelper_valid_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_FORCE_CLOCK_OUT, true);

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyForceClockOutSkillHelper(userPRO,accessPolicy,true,"activity");
            Assertions.assertNotNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            skillMap.put(Skillset.SKILL_SET_KEY_FORCE_CLOCK_OUT, true);
            mockedStatic.verify( () -> UserPROUtil.updatePROSkill(userPRO,skillMap),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",new UserDTO(userPRO,null));
            queueMap.put("action","forceClockOutSkill");
            queueMap.put("policy",accessPolicy);
            queueMap.put("forceClockOutSkill",true);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyForceClockOutSkillHelper_null_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            Map<String,Object> skillMap = new HashMap<>();
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyForceClockOutSkillHelper(userPRO,accessPolicy,false,"activity");
            Assertions.assertNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));
        }
    }

    @Test
    void generateAdjustmentSkillTaskQueueMap_test(){
        UserDTO userDTO = new UserDTO();
        AccessPolicy accessPolicy = new AccessPolicy();
        Map<String, Object>adjustmentSkillMap = new HashMap<>();
        Map<String,Object> adjustmentMap = userSkillHelper.generateAdjustmentSkillTaskQueueMap(userDTO,accessPolicy,adjustmentSkillMap);

        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put("user",userDTO);
        queueMap.put("action","adjustmentSkill");
        queueMap.put("policy",accessPolicy);
        queueMap.put("adjustmentSkill",adjustmentSkillMap);
        Assertions.assertEquals(queueMap,adjustmentMap);
    }

    @Test
    void generateReportSkillTaskQueueMap_test(){
        UserDTO userDTO = new UserDTO();
        AccessPolicy accessPolicy = new AccessPolicy();
        Map<String, Object>reportSkillMap = new HashMap<>();
        Map<String,Object> reportMap = userSkillHelper.generateReportSkillTaskQueueMap(userDTO,accessPolicy,reportSkillMap);

        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put("user",userDTO);
        queueMap.put("action","reportSkill");
        queueMap.put("policy",accessPolicy);
        queueMap.put("reportSkill",reportSkillMap);
        Assertions.assertEquals(queueMap,reportMap);
    }

    @Test
    void modifyReportSkillHelper_nullResponse_test() throws IOException {
        try (MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class)) {
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(null);
            Map<String, Object>expectedMap = new HashMap<>();

            Map<String, Object>responseMap = userSkillHelper.modifyReportSkillHelper(userPRO,false,null,null);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void modifyReportSkillHelper_enableReportSkillValid_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setId("232");
            accessManagerMockedStatic.when(() -> AccessManager.addViewPermissionToReports(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_VIEW);
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.REPORT_KEY, IAMPermission.REPORTS_VIEW.toString());
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("success", true);
            expectedMap.put("permissions",permissionType);
            expectedMap.put("updatedPRO", userDTO);

            Map<String, Object>responseMap = userSkillHelper.modifyReportSkillHelper(userPRO,true,null,null);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void modifyReportSkillHelper_disableReportSkillValid_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setId("232");
            accessManagerMockedStatic.when(() -> AccessManager.removeReportsPermission(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, "");
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.REPORT_KEY, "");
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("success", true);
            expectedMap.put("permissions",permissionType);
            expectedMap.put("updatedPRO", userDTO);

            Map<String, Object>responseMap = userSkillHelper.modifyReportSkillHelper(userPRO,false,null,null);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void enableReportAccessHelper_editType_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setId("232");
            accessManagerMockedStatic.when(() -> AccessManager.addEditPermissionToReports(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_EDIT);
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.REPORT_KEY, IAMPermission.REPORTS_EDIT.toString());
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("success", true);
            expectedMap.put("permissions",permissionType);
            expectedMap.put("updatedPRO", userDTO);

            Map<String, Object>responseMap = userSkillHelper.enableReportAccessHelper(userPRO,"edit",null,skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void enableReportAccessHelper_viewType_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setId("232");
            accessManagerMockedStatic.when(() -> AccessManager.addViewPermissionToReports(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_VIEW);
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.REPORT_KEY, IAMPermission.REPORTS_VIEW.toString());
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("success", true);
            expectedMap.put("permissions",permissionType);
            expectedMap.put("updatedPRO", userDTO);

            Map<String, Object>responseMap = userSkillHelper.enableReportAccessHelper(userPRO,null,null,skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void enableReportEditAccessHelper_nullAccessPolicy_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            accessManagerMockedStatic.when(() -> AccessManager.addEditPermissionToReports(anyString(),anyString())).thenReturn(null);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_EDIT);
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>expectedMap = new HashMap<>();

            Map<String, Object>responseMap = userSkillHelper.enableReportEditAccessHelper(userPRO,null,skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void enableReportEditAccessHelper_reportEditAccess_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setId("232");
            accessManagerMockedStatic.when(() -> AccessManager.addEditPermissionToReports(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_EDIT);
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.REPORT_KEY, IAMPermission.REPORTS_EDIT.toString());
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("success", true);
            expectedMap.put("permissions",permissionType);
            expectedMap.put("updatedPRO", userDTO);

            Map<String, Object>responseMap = userSkillHelper.enableReportEditAccessHelper(userPRO,null,skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void enableReportEditAccessHelper_reportEditAllAccess_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setId("232");
            accessManagerMockedStatic.when(() -> AccessManager.addEditAllPermissionToReports(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_EDITALL);
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.REPORT_KEY, IAMPermission.REPORTS_EDIT_ALL.toString());
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("success", true);
            expectedMap.put("permissions",permissionType);
            expectedMap.put("updatedPRO", userDTO);

            Map<String, Object>responseMap = userSkillHelper.enableReportEditAccessHelper(userPRO,"all",skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void enableReportViewAccessHelper_nullAccessPolicy_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            accessManagerMockedStatic.when(() -> AccessManager.addViewPermissionToReports(anyString(),anyString())).thenReturn(null);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_VIEW);
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>expectedMap = new HashMap<>();

            Map<String, Object>responseMap = userSkillHelper.enableReportViewAccessHelper(userPRO,null,skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void enableReportViewAccessHelper_reportView_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setId("232");
            accessManagerMockedStatic.when(() -> AccessManager.addViewPermissionToReports(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_VIEW);
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.REPORT_KEY, IAMPermission.REPORTS_VIEW.toString());
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("success", true);
            expectedMap.put("permissions",permissionType);
            expectedMap.put("updatedPRO", userDTO);

            Map<String, Object>responseMap = userSkillHelper.enableReportViewAccessHelper(userPRO,null,skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void enableReportViewAccessHelper_reportViewAll_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setId("232");
            accessManagerMockedStatic.when(() -> AccessManager.addViewAllPermissionToReports(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, Skillset.SKILLSET_VIEWALL);
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.REPORT_KEY, IAMPermission.REPORTS_VIEW_ALL.toString());
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("success", true);
            expectedMap.put("permissions",permissionType);
            expectedMap.put("updatedPRO", userDTO);

            Map<String, Object>responseMap = userSkillHelper.enableReportViewAccessHelper(userPRO,"all",skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void disableReportAccessHelper_nullAccessPolicy_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            accessManagerMockedStatic.when(() -> AccessManager.removeReportsPermission(anyString(),anyString())).thenReturn(null);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, "");
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.REPORT_KEY, "");
            Map<String, Object>expectedMap = new HashMap<>();

            Map<String, Object>responseMap = userSkillHelper.disableReportAccessHelper(userPRO,skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void disableReportAccessHelper_valid_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setId("232");
            accessManagerMockedStatic.when(() -> AccessManager.removeReportsPermission(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_REPORTS, "");
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.REPORT_KEY, "");
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("success", true);
            expectedMap.put("permissions",permissionType);
            expectedMap.put("updatedPRO", userDTO);

            Map<String, Object>responseMap = userSkillHelper.disableReportAccessHelper(userPRO,skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void modifyAdjustmentSkillHelper_view_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setId("232");
            accessManagerMockedStatic.when(() -> AccessManager.addAdjustmentViewPermission(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_ADJUSTMENTS, "view");
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.ADJUSTMENT, IAMPermission.ADJUSTMENTS_VIEW.toString());
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("success", true);
            expectedMap.put("permissions",permissionType);
            expectedMap.put("updatedPRO", userDTO);

            Map<String, Object>responseMap = userSkillHelper.modifyAdjustmentSkillHelper(userPRO,true,null);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void modifyAdjustmentSkillHelper_accessPolicyIsNull_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            accessManagerMockedStatic.when(() -> AccessManager.removeAdjustmentPermission(anyString(),anyString())).thenReturn(null);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_ADJUSTMENTS, "");
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.ADJUSTMENT, "");
            Map<String, Object>expectedMap = new HashMap<>();

            Map<String, Object>responseMap = userSkillHelper.modifyAdjustmentSkillHelper(userPRO,false,null);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void modifyAdjustmentSkillHelper_removeAdjustmentPermission_test() throws IOException {
        try (MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
             MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
             MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
             MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)) {
            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setId("232");
            accessManagerMockedStatic.when(() -> AccessManager.removeAdjustmentPermission(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            var userDTO = new UserDTO();
            userDTO.setContactID("21");
            userDTO.setAccountID("21");
            userDTO.setRole("admin");
            userDTO.setRfID("");
            userDTO.setEmpID("");
            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_ADJUSTMENTS, "");
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.ADJUSTMENT, "");
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("success", true);
            expectedMap.put("permissions",permissionType);
            expectedMap.put("updatedPRO", userDTO);

            Map<String, Object>responseMap = userSkillHelper.modifyAdjustmentSkillHelper(userPRO,false,null);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void addAdjustmentPermissionSkillHelper_addViewPermission_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){
            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.addAdjustmentViewPermission(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            Map<String, Object>skillMap = new HashMap<>();
            skillMap.put("adjustments", "view");
            var activity = "Adjustments - view access has been enabled.";

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.ADJUSTMENT, IAMPermission.ADJUSTMENTS_VIEW.toString());
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put(userSkillHelper.ACCESS_POLICY_KEY, accessPolicy);
            expectedMap.put(CommonConstants.ACTIVITY, activity);
            expectedMap.put(userSkillHelper.SKILL_MAP_KEY, skillMap);
            expectedMap.put(userSkillHelper.PERMISSIONS_KEY, permissionType);

            Map<String, Object>responseMap = userSkillHelper.addAdjustmentPermissionSkillHelper(userPRO,null,skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void addAdjustmentPermissionSkillHelper_addEditPermission_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){
            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.addAdjustmentEditPermission(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            userPRO.setRole("admin");
            Map<String, Object>skillMap = new HashMap<>();
            skillMap.put("adjustments", "edit");
            var activity = "Adjustments - edit access has been enabled.";

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.ADJUSTMENT, IAMPermission.ADJUSTMENTS_EDIT.toString());
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put(userSkillHelper.ACCESS_POLICY_KEY, accessPolicy);
            expectedMap.put(CommonConstants.ACTIVITY, activity);
            expectedMap.put(userSkillHelper.SKILL_MAP_KEY, skillMap);
            expectedMap.put(userSkillHelper.PERMISSIONS_KEY, permissionType);

            Map<String, Object>responseMap = userSkillHelper.addAdjustmentPermissionSkillHelper(userPRO,"edit",skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void removeAdjustmentPermissionSkillHelper_valid_test(){
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){
            AccessPolicy accessPolicy = new AccessPolicy();
            accessManagerMockedStatic.when(() -> AccessManager.removeAdjustmentPermission(anyString(),anyString())).thenReturn(accessPolicy);
            var userPRO = new PeopleRelationJDO();
            userPRO.setContactId("21");
            userPRO.setUniquepin("21");
            Map<String, Object>skillMap = new HashMap<>();
            var activity = "Adjustments access has been disabled.";

            Map<String, Object>permissionType = new HashMap<>();
            permissionType.put(userSkillHelper.ADJUSTMENT, "");
            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put(userSkillHelper.ACCESS_POLICY_KEY, accessPolicy);
            expectedMap.put(CommonConstants.ACTIVITY, activity);
            expectedMap.put(userSkillHelper.SKILL_MAP_KEY, skillMap);
            expectedMap.put(userSkillHelper.PERMISSIONS_KEY, permissionType);

            Map<String, Object>responseMap = userSkillHelper.removeAdjustmentPermissionSkillHelper(userPRO,skillMap);
            Assertions.assertEquals(expectedMap,responseMap);
        }
    }

    @Test
    void generateActivitySkillTaskQueueMap_test(){
        UserDTO userDTO = new UserDTO();
        AccessPolicy accessPolicy = new AccessPolicy();
        Map<String,Object> activityMap = userSkillHelper.generateActivitySkillTaskQueueMap(userDTO,accessPolicy,true);

        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put("user",userDTO);
        queueMap.put("action","activitySkill");
        queueMap.put("policy",accessPolicy);
        queueMap.put("activitySkill",true);
        Assertions.assertEquals(queueMap,activityMap);
    }

    @Test
    void modifyActivitySkillHelper_disableActivitySkillSet_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_SUB_STATUS, false);

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyActivitySkillHelper(userPRO,accessPolicy,false,"activity");
            Assertions.assertNotNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            skillMap.put(Skillset.SKILL_SET_KEY_SUB_STATUS, false);
            mockedStatic.verify( () -> UserPROUtil.updatePROSkill(userPRO,skillMap),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",new UserDTO(userPRO,null));
            queueMap.put("action","activitySkill");
            queueMap.put("policy",accessPolicy);
            queueMap.put("activitySkill",false);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyActivitySkillHelper_valid_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_SUB_STATUS, true);

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyActivitySkillHelper(userPRO,accessPolicy,true,"activity");
            Assertions.assertNotNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            skillMap.put(Skillset.SKILL_SET_KEY_SUB_STATUS, true);
            mockedStatic.verify( () -> UserPROUtil.updatePROSkill(userPRO,skillMap),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",new UserDTO(userPRO,null));
            queueMap.put("action","activitySkill");
            queueMap.put("policy",accessPolicy);
            queueMap.put("activitySkill",true);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyActivitySkillHelper_null_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            Map<String,Object> skillMap = new HashMap<>();
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyActivitySkillHelper(userPRO,accessPolicy,false,"activity");
            Assertions.assertNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));
        }
    }

    @Test
    void generateConfirmHoursSkillTaskQueueMap_test(){
        UserDTO userDTO = new UserDTO();
        AccessPolicy accessPolicy = new AccessPolicy();
        Map<String,Object> confirmHoursMap = userSkillHelper.generateConfirmHoursSkillTaskQueueMap(userDTO,accessPolicy,true);

        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put("user",userDTO);
        queueMap.put("action","confirmHoursSkill");
        queueMap.put("policy",accessPolicy);
        queueMap.put("confirmHoursSkill",true);
        Assertions.assertEquals(queueMap,confirmHoursMap);
    }

    @Test
    void modifyConfirmHoursSkillHelper_disableConfirmHoursSkillSet_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_CAN_CONFIRMHOURS, false);

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyConfirmHoursSkillHelper(userPRO,accessPolicy,false,"activity");
            Assertions.assertNotNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            skillMap.put(Skillset.SKILL_SET_KEY_CAN_CONFIRMHOURS, false);
            mockedStatic.verify( () -> UserPROUtil.updatePROSkill(userPRO,skillMap),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",new UserDTO(userPRO,null));
            queueMap.put("action","confirmHoursSkill");
            queueMap.put("policy",accessPolicy);
            queueMap.put("confirmHoursSkill",false);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyConfirmHoursSkillHelper_valid_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(Skillset.SKILL_SET_KEY_CAN_CONFIRMHOURS, true);

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyConfirmHoursSkillHelper(userPRO,accessPolicy,true,"activity");
            Assertions.assertNotNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            skillMap.put(Skillset.SKILL_SET_KEY_CAN_CONFIRMHOURS, true);
            mockedStatic.verify( () -> UserPROUtil.updatePROSkill(userPRO,skillMap),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",new UserDTO(userPRO,null));
            queueMap.put("action","confirmHoursSkill");
            queueMap.put("policy",accessPolicy);
            queueMap.put("confirmHoursSkill",true);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyConfirmHoursSkillHelper_null_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            Map<String,Object> skillMap = new HashMap<>();
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO rep = new UserSkillHelper().modifyConfirmHoursSkillHelper(userPRO,accessPolicy,false,"activity");
            Assertions.assertNull(rep);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));
        }
    }

    @Test
    void generateTeamReportSkillTaskQueueMap_test(){
        UserDTO userDTO = new UserDTO();
        Map<String,Object> teamReportsSkillMap = userSkillHelper.generateTeamReportSkillTaskQueueMap(userDTO);
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put("user",userDTO);
        queueMap.put("action","teamReportSkill");
        Assertions.assertEquals(queueMap,teamReportsSkillMap);
    }

    @Test
    void addTeamReportPermissionSkillHelper_editPermission_test(){

        String teamID = "23";
        Map<String, Object>expectedTeamSkillSetMap = new HashMap<>();
        expectedTeamSkillSetMap.put(teamID, UserSkillHelper.EDIT);
        Map<String, Object>expectedMap = new HashMap<>();
        expectedMap.put(CommonConstants.ACTIVITY, "Edit access has been enabled for team :"+teamID);
        expectedMap.put("teamSkillSetMap", expectedTeamSkillSetMap);

        Map<String, Object> teamMap = new HashMap<>();
        Map<String,Object> addTeamReportsSkillMap = userSkillHelper.addTeamReportPermissionSkillHelper(teamMap, UserSkillHelper.EDIT, teamID);
        Assertions.assertEquals(expectedMap,addTeamReportsSkillMap);
    }

    @Test
    void addTeamReportPermissionSkillHelper_viewPermission_test(){

        String teamID = "23";
        Map<String, Object>expectedTeamSkillSetMap = new HashMap<>();
        expectedTeamSkillSetMap.put(teamID, UserSkillHelper.VIEW);
        Map<String, Object>expectedMap = new HashMap<>();
        expectedMap.put(CommonConstants.ACTIVITY, "View access has been enabled for team :"+teamID);
        expectedMap.put("teamSkillSetMap", expectedTeamSkillSetMap);

        Map<String, Object> teamMap = new HashMap<>();
        Map<String,Object> addTeamReportsSkillMap = userSkillHelper.addTeamReportPermissionSkillHelper(teamMap, UserSkillHelper.VIEW, teamID);
        Assertions.assertEquals(expectedMap,addTeamReportsSkillMap);
    }

    @Test
    void removeTeamReportPermissionSkillHelper_valid_test(){

        String teamID = "23";
        Map<String, Object>expectedTeamSkillSetMap = new HashMap<>();
        Map<String, Object>expectedMap = new HashMap<>();
        expectedMap.put(CommonConstants.ACTIVITY, "Report access has been revoked for team :"+teamID);
        expectedMap.put("teamSkillSetMap", expectedTeamSkillSetMap);

        Map<String, Object> teamMap = new HashMap<>();
        teamMap.put(teamID, UserSkillHelper.VIEW);
        Map<String,Object> removeTeamReportsSkillMap = userSkillHelper.removeTeamReportPermissionSkillHelper(teamMap,teamID);
        Assertions.assertEquals(expectedMap,removeTeamReportsSkillMap);
    }

    @Test
    void modifyTeamReportSkillHelper_nullSkillMap_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        UserDTO userDTO = new UserDTO(userPRO, null);

        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put(Commons.SUCCESS, true);
            expectedMap.put(UserSkillHelper.UPDATED_PRO, userDTO);

            Map<String,Object> skillMap = new HashMap<>();
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object> responseMap = new UserSkillHelper().modifyTeamReportSkillHelper(userPRO,true, "edit","23");
            Assertions.assertEquals(expectedMap,responseMap);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",userDTO);
            queueMap.put("action","teamReportSkill");
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyTeamReportSkillHelper_ifTeamSkillSetStringIsNull_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        UserDTO userDTO = new UserDTO(userPRO, null);

        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put(Commons.SUCCESS, true);
            expectedMap.put(UserSkillHelper.UPDATED_PRO, userDTO);

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(UserSkillHelper.TEAM_SKILL_SET, "");

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object> responseMap = new UserSkillHelper().modifyTeamReportSkillHelper(userPRO,true, "edit","23");
            Assertions.assertEquals(expectedMap,responseMap);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",userDTO);
            queueMap.put("action","teamReportSkill");
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyTeamReportSkillHelper_valid_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        UserDTO userDTO = new UserDTO(userPRO, null);

        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put(Commons.SUCCESS, true);
            expectedMap.put(UserSkillHelper.UPDATED_PRO, userDTO);

            Map<String, Object>teamSkillMap = new HashMap<>();
            teamSkillMap.put("23", "view");

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(UserSkillHelper.TEAM_SKILL_SET, JsonUtil.getJson(teamSkillMap));

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object> responseMap = new UserSkillHelper().modifyTeamReportSkillHelper(userPRO,true, "edit","23");
            Assertions.assertEquals(expectedMap,responseMap);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",userDTO);
            queueMap.put("action","teamReportSkill");
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyTeamReportSkillHelper_removeTeamReportPermission_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        UserDTO userDTO = new UserDTO(userPRO, null);

        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put(Commons.SUCCESS, true);
            expectedMap.put(UserSkillHelper.UPDATED_PRO, userDTO);

            Map<String, Object>teamSkillMap = new HashMap<>();
            teamSkillMap.put("23", "view");

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(UserSkillHelper.TEAM_SKILL_SET, JsonUtil.getJson(teamSkillMap));

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object> responseMap = new UserSkillHelper().modifyTeamReportSkillHelper(userPRO,false, "edit","23");
            Assertions.assertEquals(expectedMap,responseMap);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put("user",userDTO);
            queueMap.put("action","teamReportSkill");
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void generateReminderSkillTaskQueueMap_test(){
        boolean reminderSkill = true;
        Map<String, Object>teamSkillMap = new HashMap<>();
        teamSkillMap.put("23", "view");
        Map<String,Object> skillMap = new HashMap<>();
        skillMap.put(UserSkillHelper.TEAM_SKILL_SET, JsonUtil.getJson(teamSkillMap));
        UserDTO userDTO = new UserDTO();
        AccessPolicy accessPolicy = new AccessPolicy();
        Map<String,Object> reminderMap = userSkillHelper.generateReminderSkillTaskQueueMap(userDTO,accessPolicy,true, skillMap);

        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"reminderSkill");
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put("reminderSkill", reminderSkill);
        queueMap.put(UserSkillHelper.SKILL_MAP_KEY, skillMap);
        queueMap.put(CommonConstants.POLICY,accessPolicy);
        Assertions.assertEquals(queueMap,reminderMap);
    }

    @Test
    void modifyReminderSkillHelper_skillMapIsNull_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){
            Map<String,Object> skillMap = new HashMap<>();
            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            activityMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);
            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO response = new UserSkillHelper().modifyReminderSkillHelper(userPRO,accessPolicy,true,"activity");
            Assertions.assertNull(response);
            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));
        }
    }

    @Test
    void modifyReminderSkillHelper_valid_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        UserDTO userDTO = new UserDTO(userPRO, null);

        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(UserSkillService.CLOCK_REMINDER, true);

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            AccessPolicy accessPolicy = new AccessPolicy();
            UserDTO response = new UserSkillHelper().modifyReminderSkillHelper(userPRO,accessPolicy,true,"activity");
            Assertions.assertNotNull(response);
            Assertions.assertEquals(userDTO,response);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put(CommonConstants.ACTION,"reminderSkill");
            queueMap.put(CommonConstants.USER_KEY,userDTO);
            queueMap.put("reminderSkill", true);
            queueMap.put(UserSkillHelper.SKILL_MAP_KEY, skillMap);
            queueMap.put(CommonConstants.POLICY,accessPolicy);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void isReminderTimeWithinTheLimit_valid_test(){
        Assertions.assertTrue(userSkillHelper.isReminderTimeWithinTheLimit(2));
    }

    @Test
    void isReminderTimeWithinTheLimit_negativeReminderTime_test(){
        Assertions.assertFalse(userSkillHelper.isReminderTimeWithinTheLimit(-2));
    }

    @Test
    void generateReminderTimeTaskQueueMap_test(){

        Map<String,Object> skillMap = new HashMap<>();
        skillMap.put(UserSkillService.REMINDER_TIME, 3);
        UserDTO userPRO = new UserDTO();
        Map<String,Object> reminderMap = userSkillHelper.generateReminderTimeTaskQueueMap(userPRO, skillMap);

        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION, "modifyReminderTime");
        queueMap.put(UserSkillHelper.SKILL_MAP_KEY, skillMap);
        queueMap.put(CommonConstants.USER_KEY,userPRO);
        Assertions.assertEquals(queueMap,reminderMap);
    }

    @Test
    void modifyReminderTimeHelper_ifSkillMapIsNull_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        UserDTO userDTO = new UserDTO(userPRO, null);

        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put(Commons.SUCCESS, true);
            expectedMap.put(UserSkillHelper.UPDATED_PRO, userDTO);

            Map<String,Object> skillMap = new HashMap<>();

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object> responseMap = new UserSkillHelper().modifyReminderTimeHelper(userPRO, 1);
            Assertions.assertNotNull(responseMap);
            Assertions.assertEquals(expectedMap ,responseMap);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put(CommonConstants.ACTION, "modifyReminderTime");
            queueMap.put(UserSkillHelper.SKILL_MAP_KEY, skillMap);
            queueMap.put(CommonConstants.USER_KEY,userDTO);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @Test
    void modifyReminderTimeHelper_ifReminderTimeIsOutsideTheRangeAndSkillMapIsEmpty_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        UserDTO userDTO = new UserDTO(userPRO, null);

        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put(Commons.SUCCESS, true);
            expectedMap.put(UserSkillHelper.UPDATED_PRO, userDTO);

            Map<String,Object> skillMap = new HashMap<>();

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object> responseMap = new UserSkillHelper().modifyReminderTimeHelper(userPRO, -1);
            Assertions.assertNotNull(responseMap);
            Assertions.assertEquals(expectedMap ,responseMap);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put(CommonConstants.ACTION, "modifyReminderTime");
            queueMap.put(UserSkillHelper.SKILL_MAP_KEY, skillMap);
            queueMap.put(CommonConstants.USER_KEY,userDTO);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

    @ParameterizedTest
    @CsvSource({"-1","100","1","0","2"})
    void modifyReminderTimeHelper_valid_test(int reminderTime) throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        UserDTO userDTO = new UserDTO(userPRO, null);

        try(MockedStatic<UserPROUtil> mockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ActivityUtil> activityMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put(Commons.SUCCESS, true);
            expectedMap.put(UserSkillHelper.UPDATED_PRO, userDTO);

            Map<String,Object> skillMap = new HashMap<>();
            skillMap.put(UserSkillService.REMINDER_TIME, reminderTime);

            mockedStatic.when(() -> UserPROUtil.extractPROSkills(any(PeopleRelationJDO.class))).thenReturn(skillMap);
            mockedStatic.when(() -> UserPROUtil.updatePROSkill(any(PeopleRelationJDO.class),anyMap())).thenReturn(userPRO);
            userTaskInitiatorMockedStatic.when( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            activityMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>) invocation -> null);

            Map<String, Object> responseMap = new UserSkillHelper().modifyReminderTimeHelper(userPRO, reminderTime);
            Assertions.assertNotNull(responseMap);
            Assertions.assertEquals(expectedMap ,responseMap);

            mockedStatic.verify( () -> UserPROUtil.extractPROSkills(userPRO),times(1));

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put(CommonConstants.ACTION, "modifyReminderTime");
            queueMap.put(UserSkillHelper.SKILL_MAP_KEY, skillMap);
            queueMap.put(CommonConstants.USER_KEY,userDTO);
            userTaskInitiatorMockedStatic.verify( () -> UserTaskInitiator.initiateProUpdateOperationsTaskQueue(queueMap),times(1));
        }
    }

}