package com.yoco.commons.cloudservices;

import com.google.cloud.tasks.v2.*;
import com.google.protobuf.ByteString;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.GaeUtils;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;

@Slf4j
public class TaskCreator {

    private TaskCreator(){}

    private static final String PROJECT_ID = GaeUtils.isAppModeLive() ? GaeUtils.APP_ID_LIVE : GaeUtils.APP_ID_STAGING;
    private static final String LOCATION = "us-central1";

    public static void createPostTask(String queueName,String taskHandlerUrl,byte[] payload) throws IOException {

        try (var client = CloudTasksClient.create()) {

            var queuePath = QueueName.of(PROJECT_ID, LOCATION, queueName).toString();

            var taskBuilder = Task.newBuilder()
                            .setHttpRequest(
                                    HttpRequest.newBuilder()
                                            .putHeaders(UrlFetcher.AUTHORIZATION, UrlFetcher.BEARER + " " + FullAuthService.getServerAccessToken())
                                            .setBody(ByteString.copyFrom(payload))
                                            .setUrl(taskHandlerUrl)
                                            .setHttpMethod(HttpMethod.POST)
                                            .build());

            var task = client.createTask(queuePath, taskBuilder.build());

            log.info("Task created successfully : " + task.getName());
        }catch (Exception e){
            log.warn("Exception in task creator :: " + e.getMessage() );
        }
    }
}
