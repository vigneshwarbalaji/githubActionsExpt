package com.yoco.commons.cloudservices;

import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.PeopleRelationJDO;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class CommonTaskInitiatorTest {

    @Test
    void initiateClearCacheQueue() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){
            CommonAppProperties.setAppUrl("url");
            Map<String,Object> response = new HashMap<>();
            response.put("key","data");
            CommonTaskInitiator.initiateClearCacheQueue("tokenStr",response);
            var byteArrayOutputStream = new ByteArrayOutputStream();
            new ObjectOutputStream(byteArrayOutputStream).writeObject(Map.of("token","tokenStr","payloadMap",response));
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("api-request","url/task/common/clearCacheTaskHandler",byteArray));
        }
    }


}