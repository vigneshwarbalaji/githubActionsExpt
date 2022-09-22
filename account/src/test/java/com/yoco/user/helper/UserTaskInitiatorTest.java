package com.yoco.user.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.PeopleRelationJDO;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Map;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class UserTaskInitiatorTest {

    @Test
    void initiateProUpdateOperationsTaskQueue() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAppUrl).thenReturn("https://url");
            taskCreatorMockedStatic.when(() -> TaskCreator.createPostTask(anyString(),anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> response = new HashMap<>();
            response.put("key","data");
            UserTaskInitiator.initiateProUpdateOperationsTaskQueue(response);

            var byteOut = new ByteArrayOutputStream();
            var out = new ObjectOutputStream(byteOut);
            out.writeObject(response);

            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("updateUserPro","https://url/task/user/proUpdateTaskHandler",byteOut.toByteArray()));
        }
    }

    @Test
    void initiateStaffOperationsTaskQueue() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAppUrl).thenReturn("https://url");
            taskCreatorMockedStatic.when(() -> TaskCreator.createPostTask(anyString(),anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> response = new HashMap<>();
            response.put("key","data");
            UserTaskInitiator.initiateStaffOperationsTaskQueue(response);

            var byteOut = new ByteArrayOutputStream();
            var out = new ObjectOutputStream(byteOut);
            out.writeObject(response);

            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("staff-operation","https://url/task/user/staffTaskHandler",byteOut.toByteArray()));
        }
    }

    @Test
    void initiateDeleteUserQueue_test() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){
            CommonAppProperties.setAppUrl("url");
            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            UserTaskInitiator.initiateDeleteUserQueue(adminPro,userPro,"web");
            var byteArrayOutputStream = new ByteArrayOutputStream();
            new ObjectOutputStream(byteArrayOutputStream).writeObject(Map.of("adminPro",adminPro,"userPro",userPro,"requestSource","web"));
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("staff-operation","url/task/user/delete",byteArray));
        }
    }

}