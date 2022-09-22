package com.yoco.commons.cloudservices;

import com.google.cloud.tasks.v2.*;
import com.google.protobuf.ByteString;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.utils.GaeUtils;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class TaskCreatorTest {

    @Test
    void createPostTask_exception_test() throws IOException {
        try(MockedStatic<CloudTasksClient> cloudTasksClientMockedStatic = Mockito.mockStatic(CloudTasksClient.class);
            MockedStatic<QueueName> queueNameMockedStatic = Mockito.mockStatic(QueueName.class)){
            cloudTasksClientMockedStatic.when(CloudTasksClient::create).thenThrow(new IllegalArgumentException("test"));
            TaskCreator.createPostTask("queueName","https://url",null);
            queueNameMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void createPostTask_staging_test() throws IOException {

        try(MockedStatic<CloudTasksClient> cloudTasksClientMockedStatic = Mockito.mockStatic(CloudTasksClient.class);
            MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class)){

            CloudTasksClient cloudTasksClient = Mockito.mock(CloudTasksClient.class);
            Mockito.when(cloudTasksClient.create()).thenReturn(cloudTasksClient);

            Task task = Task.newBuilder().setName("queueName").build();
            Mockito.when(cloudTasksClient.createTask(anyString(),any(Task.class))).thenReturn(task);

            String payload = "payload";
            ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
            ObjectOutputStream out = new ObjectOutputStream(byteOut);
            out.writeObject(payload);

            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");

            TaskCreator.createPostTask("queueName","https://url",byteOut.toByteArray());

            String queueName = QueueName.of(GaeUtils.APP_ID_STAGING, "us-central1", "queueName").toString();

            var taskBuilder = Task.newBuilder().setHttpRequest(HttpRequest.newBuilder()
                                    .putHeaders("Authorization","Bearer token")
                                    .setBody(ByteString.copyFrom(byteOut.toByteArray()))
                                    .setUrl("https://url")
                                    .setHttpMethod(HttpMethod.POST)
                                    .build());

            Mockito.verify(cloudTasksClient).createTask(queueName,taskBuilder.build());
        }
    }

}
