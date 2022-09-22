package com.yoco.account.helper;

import com.yoco.account.modal.AccountUpdatePayloadDTO;
import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.modal.account.AccountDTO;
import com.yoco.constants.CommonConstants;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestTemplate;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.Map;

class AccountTaskInitiatorTest {
    @Test
    void initiateAccountDeletionQueues_test() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){
            CommonAppProperties.setAppUrl("url");
            AccountDTO accountDTO = Mockito.mock(AccountDTO.class);
            AccountTaskInitiator.initiateAccountDeletionQueues(accountDTO,"123");
            var byteArrayOutputStream = new ByteArrayOutputStream();
            new ObjectOutputStream(byteArrayOutputStream).writeObject(Map.of("account", accountDTO,"primaryAdminContactID","123"));
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("staff-operation","url/task/account/delete-handler",byteArray));
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("staff-operation","url/task/account/delete-handler/clock",byteArray));
        }
    }

    @Test
    void initiateAccountDeletionStaffDisablingQueue_test() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){
            CommonAppProperties.setAppUrl("url");
            AccountDTO accountDTO = Mockito.mock(AccountDTO.class);
            AccountTaskInitiator.initiateAccountDeletionStaffDisablingQueue(accountDTO,"123");
            var byteArrayOutputStream = new ByteArrayOutputStream();
            new ObjectOutputStream(byteArrayOutputStream).writeObject(Map.of("account", accountDTO,"primaryAdminContactID","123"));
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("staff-operation","url/task/account/delete-handler/staff",byteArray));
        }
    }

    @Test
    void initiateAccountCreationQueue_valid_test() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){
            CommonAppProperties.setAppUrl("url");
            SettingsJDO account = new SettingsJDO();
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            AccountTaskInitiator.initiateAccountCreationQueue(account,userPro);
            var byteArrayOutputStream = new ByteArrayOutputStream();
            new ObjectOutputStream(byteArrayOutputStream).writeObject(Map.of("account", account,"user",userPro));
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("staff-operation","url/task/account/create-handler",byteArray));
        }
    }

    @Test
    void initiateAccountUpdateQueue_test() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){
            CommonAppProperties.setAppUrl("url");
            Contact contact = new Contact();
            AccountUpdatePayloadDTO payloadDTO = new AccountUpdatePayloadDTO();
            AccountTaskInitiator.initiateAccountUpdateQueue("accID",contact,payloadDTO);
            var byteArrayOutputStream = new ByteArrayOutputStream();
            new ObjectOutputStream(byteArrayOutputStream).writeObject(Map.of("payloadDTO", payloadDTO, CommonConstants.CONTACT_KEY,contact));
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("staff-operation","url/task/account/accID/update-handler",byteArray));
        }
    }
}
