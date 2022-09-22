package com.yoco.user.endpoint;

import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.user.helper.skill.UserSkillTaskHelper;
import com.yoco.user.helper.staff.UserStaffTaskHelper;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyMap;

class UserTaskControllerTest {

    @Test
    void proUpdateTaskHandler() throws IOException, ClassNotFoundException {

        try(MockedStatic<UserSkillTaskHelper> userTaskHelperMockedStatic = Mockito.mockStatic(UserSkillTaskHelper.class)) {

            UserSkillTaskHelper userSkillTaskHelper = Mockito.mock(UserSkillTaskHelper.class);
            Mockito.doNothing().when(userSkillTaskHelper).updateProTaskHelper(anyMap());
            userTaskHelperMockedStatic.when(UserSkillTaskHelper::getInstance).thenReturn(userSkillTaskHelper);

            Map<String,Object> payload = new HashMap<>();
            payload.put("key","data");

            new UserTaskController().proUpdateTaskHandler(CloudTaskUtil.convertObjectToByteArray(payload));

            Mockito.verify(userSkillTaskHelper).updateProTaskHelper(payload);
        }
    }

    @Test
    void staffTaskHandler() throws IOException, ClassNotFoundException {

        try(MockedStatic<UserStaffTaskHelper> userTaskHelperMockedStatic = Mockito.mockStatic(UserStaffTaskHelper.class)) {

            UserStaffTaskHelper userStaffTaskHelper = Mockito.mock(UserStaffTaskHelper.class);
            Mockito.doNothing().when(userStaffTaskHelper).staffOperationsTaskHelper(anyMap());
            userTaskHelperMockedStatic.when(UserStaffTaskHelper::getInstance).thenReturn(userStaffTaskHelper);

            Map<String,Object> payload = new HashMap<>();
            payload.put("key","data");

            new UserTaskController().staffTaskHandler(CloudTaskUtil.convertObjectToByteArray(payload));

            Mockito.verify(userStaffTaskHelper).staffOperationsTaskHelper(payload);
        }
    }

    @Test
    void handleUserDeletion_test() throws IOException, ClassNotFoundException {
        try(MockedStatic<UserStaffTaskHelper> userTaskHelperMockedStatic = Mockito.mockStatic(UserStaffTaskHelper.class)) {
            UserStaffTaskHelper userStaffTaskHelper = Mockito.mock(UserStaffTaskHelper.class);
            userTaskHelperMockedStatic.when(UserStaffTaskHelper::getInstance).thenReturn(userStaffTaskHelper);
            Map<String,Object> payload = Map.of("key","value");
            new UserTaskController().handleUserDeletion(CloudTaskUtil.convertObjectToByteArray(payload));
            Mockito.verify(userStaffTaskHelper).handleUserDeletion(payload);
        }
    }
}