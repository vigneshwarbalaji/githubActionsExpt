package com.yoco.user.service;

import com.yoco.MockPRO;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.user.helper.UserDownloadHelper;
import com.yoco.user.helper.UserProProfileHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class UserServiceTest {

    UserService userService = new UserService();

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"payload"})
    void updateProfilePRO_invalidJson_test(String testValue){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(null);
            userService.updateProfilePRO("accountId","contactId", testValue);
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void updateProfilePRO_notUser_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(null);
            Map<String,Object> payload = new HashMap<>();
            payload.put("key","data");
            userService.updateProfilePRO("accountId","contactId", JsonUtil.getJson(payload));
        } catch (Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void updateProfilePRO_valid_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserProProfileHelper> userServiceHelperMockedStatic = Mockito.mockStatic(UserProProfileHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(userPRO);

            UserDTO userDTO = new UserDTO();
            UserProProfileHelper userProProfileHelper = Mockito.mock(UserProProfileHelper.class);
            Mockito.when(userProProfileHelper.updateProfilePRO(anyMap(),any(PeopleRelationJDO.class))).thenReturn(userDTO);
            userServiceHelperMockedStatic.when(UserProProfileHelper::getInstance).thenReturn(userProProfileHelper);

            Map<String,Object> payload = new HashMap<>();
            payload.put("key","data");
            Map<String,Object> response = userService.updateProfilePRO("accountId","contactId", JsonUtil.getJson(payload));

            Map<String,Object> expectedMap = new HashMap<>();
            expectedMap.put(UserSkillService.UPDATED_PRO, userDTO);

            Assertions.assertEquals(expectedMap,response);
            userPROUtilMockedStatic.verify(() ->  UserPROUtil.getUserProWithContact("accountId","contactId"),times(1));
            Mockito.verify(userProProfileHelper, times(1)).updateProfilePRO(payload,userPRO);
        }
    }

    @Test
    void updateProfilePRO_null_test() throws IOException {
        PeopleRelationJDO userPRO = MockPRO.getMockPRO();
        try(MockedStatic<UserProProfileHelper> userServiceHelperMockedStatic = Mockito.mockStatic(UserProProfileHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){

            userPROUtilMockedStatic.when(() -> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(userPRO);

            UserProProfileHelper userProProfileHelper = Mockito.mock(UserProProfileHelper.class);
            Mockito.when(userProProfileHelper.updateProfilePRO(anyMap(),any(PeopleRelationJDO.class))).thenReturn(null);
            userServiceHelperMockedStatic.when(UserProProfileHelper::getInstance).thenReturn(userProProfileHelper);

            Map<String,Object> payload = new HashMap<>();
            payload.put("key","data");
            Map<String,Object> response = userService.updateProfilePRO("accountId","contactId", JsonUtil.getJson(payload));

            Map<String,Object> expectedMap = new HashMap<>();

            Assertions.assertEquals(expectedMap,response);
            userPROUtilMockedStatic.verify(() ->  UserPROUtil.getUserProWithContact("accountId","contactId"),times(1));
            Mockito.verify(userProProfileHelper, times(1)).updateProfilePRO(payload,userPRO);
        }
    }

    @Test
    void downloadStaffData_UserNotAdmin_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(() -> UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(false);
            new UserService().downloadStaffData("123","accID","status","outputFields");
        }catch (Exception e){
            Assertions.assertEquals("User not authorized",e.getMessage());
        }
    }

    @Test
    void downloadStaffData_valid_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserDownloadHelper> userDownloadHelperMockedStatic = Mockito.mockStatic(UserDownloadHelper.class)){
            userPROUtilMockedStatic.when(() -> UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(true);
            userDownloadHelperMockedStatic.when(()-> UserDownloadHelper.getIsDeleteQueryValue("status")).thenReturn(Boolean.TRUE);
            List<PeopleRelationJDO> list = new ArrayList();
            userPROUtilMockedStatic.when(()-> UserPROUtil.getAllUsersInCompany("accID",Boolean.TRUE)).thenReturn(list);
            userDownloadHelperMockedStatic.when(()-> UserDownloadHelper.generateStaffDataCsvFileName("accID",Boolean.TRUE)).thenReturn("fileName.csv");
            userDownloadHelperMockedStatic.when(()-> UserDownloadHelper.generateStaffDataCsv(list,"outputFields")).thenReturn("reportcsv");
            Map<String,Object> actual = new UserService().downloadStaffData("123","accID","status","outputFields");
            Assertions.assertEquals("fileName.csv",actual.get("fileName"));
            Assertions.assertEquals("reportcsv",actual.get("report"));
        }
    }

}