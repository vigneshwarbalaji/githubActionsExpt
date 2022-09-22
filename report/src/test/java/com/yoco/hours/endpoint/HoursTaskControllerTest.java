package com.yoco.hours.endpoint;

import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.hours.helper.EntryDeleteHelper;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.util.Map;

class HoursTaskControllerTest {
    @Test
    void entryDeletionHandler_ExceptionTest() throws IOException {
        byte[] payload = CloudTaskUtil.convertObjectToByteArray(Map.of());
        try(MockedStatic<CloudTaskUtil> cloudTaskUtilMockedStatic = Mockito.mockStatic(CloudTaskUtil.class);
            MockedStatic<EntryDeleteHelper> entryDeleteHelperMockedStatic = Mockito.mockStatic(EntryDeleteHelper.class)){
            cloudTaskUtilMockedStatic.when(()->CloudTaskUtil.convertByteArrayToObject(payload)).thenThrow(new IllegalArgumentException("test"));
            new HoursTaskController().entryDeletionHandler(payload);
            entryDeleteHelperMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void entryDeletionHandler_valid_Test() throws IOException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        Contact entryContact = new Contact();
        byte[] payload = CloudTaskUtil.convertObjectToByteArray(Map.of("loggedInUserPro",userPro,"entryContact",entryContact,"entry",Map.of("key","value")));
        try(MockedStatic<EntryDeleteHelper> entryDeleteHelperMockedStatic = Mockito.mockStatic(EntryDeleteHelper.class)){
            new HoursTaskController().entryDeletionHandler(payload);
            entryDeleteHelperMockedStatic.verify(()->EntryDeleteHelper.handleEntryDeletion(userPro,entryContact,Map.of("key","value")));
        }
    }
}
