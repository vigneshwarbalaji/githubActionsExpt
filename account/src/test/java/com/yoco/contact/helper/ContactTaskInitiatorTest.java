package com.yoco.contact.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class ContactTaskInitiatorTest {

    @Test
    void initiateContactUpdateOperationsTaskQueue_valid_test() throws IOException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAppUrl).thenReturn("https://url");
            taskCreatorMockedStatic.when(() -> TaskCreator.createPostTask(anyString(),anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            Map<String,Object> payloadMap = Map.of("anyKey","anyValue");
            ContactTaskInitiator.initiateContactUpdateOperationsTaskQueue(payloadMap);

            var byteOut = new ByteArrayOutputStream();
            var out = new ObjectOutputStream(byteOut);
            out.writeObject(payloadMap);
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("contact-operation","https://url/task/contact/contactTaskHandler",byteOut.toByteArray()));
        }
    }
}