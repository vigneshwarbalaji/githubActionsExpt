package com.yoco.hours.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.constants.EventConstants;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.util.Map;

class HoursTaskInitiatorTest {
    @Test
    void initiateDeleteEntryQueue_valid_test() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAppUrl).thenReturn("url");
            HoursTaskInitiator.initiateDeleteEntryQueue(new PeopleRelationJDO(),new Contact(), Map.of());
            taskCreatorMockedStatic.verify(()->TaskCreator.createPostTask("clockoperation","url/task/entry/delete-handler",
                    CloudTaskUtil.convertObjectToByteArray(Map.of("loggedInUserPro",new PeopleRelationJDO(),"entryContact",new Contact(), EventConstants.ENTRY,Map.of()))));
        }
    }
}
