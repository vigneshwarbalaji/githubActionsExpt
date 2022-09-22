package com.yoco.adjustment.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.CloudTaskUtil;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class AdjustmentTaskInitiatorTest {

    @Test
    void initiateApproveAdjustmentQueue_test() throws IOException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAppUrl).thenReturn("https://url");
            taskCreatorMockedStatic.when(() -> TaskCreator.createPostTask(anyString(),anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> eventMap = new HashMap<>();
            eventMap.put("key","value");
            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            AdjustmentTaskInitiator.initiateApproveAdjustmentQueue(adminPro,eventMap);

            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("adminPRO",adminPro);
            payloadMap.put("eventMap",eventMap);
            var byteOut = new ByteArrayOutputStream();
            var out = new ObjectOutputStream(byteOut);
            out.writeObject(payloadMap);
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("process-adj-events","https://url/task/adjustment/approve",byteOut.toByteArray()));
        }
    }

    @Test
    void initiateEditApprovedAdjustmentQueue_test() throws IOException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAppUrl).thenReturn("https://url");
            taskCreatorMockedStatic.when(() -> TaskCreator.createPostTask(anyString(),anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> eventMap = new HashMap<>();
            eventMap.put("key","value");
            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            AdjustmentTaskInitiator.initiateEditApprovedAdjustmentQueue(adminPro,eventMap,Map.of());

            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("adminPRO",adminPro);
            payloadMap.put("eventMap",eventMap);
            payloadMap.put("newAdjustmentTimeDetailsInfoMap",Map.of());
            var byteOut = new ByteArrayOutputStream();
            var out = new ObjectOutputStream(byteOut);
            out.writeObject(payloadMap);
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("process-adj-events","https://url/task/adjustment/edit-approve",byteOut.toByteArray()));
        }
    }
    @Test
    void initiateDeleteAdjustmentTaskQueue_validPayload_TaskCreatorShouldHaveBeenCalled() throws IOException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAppUrl).thenReturn("url");
            AdjustmentTaskInitiator.initiateDeleteAdjustmentTaskQueue(List.of(),List.of(),new PeopleRelationJDO(), new PeopleRelationJDO());
            taskCreatorMockedStatic.verify(()->TaskCreator.createPostTask("process-adj-events","url/task/adjustment/delete-handler", CloudTaskUtil.convertObjectToByteArray(Map.of("entries",List.of(),"adjustments",List.of(),"entryContactPro",new PeopleRelationJDO(),"loggedInUserPro",new PeopleRelationJDO()))));
        }
    }
}