package com.yoco.account.endpoint;

import com.yoco.account.helper.AccountCreateTaskHelper;
import com.yoco.account.helper.AccountDeleteTaskHelper;
import com.yoco.account.helper.AccountUpdateTaskHelper;
import com.yoco.account.modal.AccountUpdatePayloadDTO;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.modal.account.AccountDTO;
import com.yoco.commons.utils.CloudTaskUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.util.Map;

class AccountTaskControllerTest {
    @Test
    void handleAccountDeletion_exception_test(){
        try(MockedStatic<AccountDeleteTaskHelper> accountDeleteTaskHelperMockedStatic = Mockito.mockStatic(AccountDeleteTaskHelper.class)){
            accountDeleteTaskHelperMockedStatic.when(()->AccountDeleteTaskHelper.handleAccountDeletion(new AccountDTO(),"123")).thenThrow(new IllegalArgumentException("Exception"));
            new AccountTaskController().handleAccountDeletion(CloudTaskUtil.convertObjectToByteArray(Map.of("account",new AccountDTO(),"primaryAdminContactID","123")));
        }catch (Exception e){
            Assertions.assertEquals("exception",e.getMessage());
        }
    }

    @Test
    void handleAccountDeletion_valid_test() throws IOException {
        try(MockedStatic<AccountDeleteTaskHelper> accountDeleteTaskHelperMockedStatic = Mockito.mockStatic(AccountDeleteTaskHelper.class)) {
            new AccountTaskController().handleAccountDeletion(CloudTaskUtil.convertObjectToByteArray(Map.of("account", new AccountDTO(), "primaryAdminContactID", "123")));
            accountDeleteTaskHelperMockedStatic.verify(()->AccountDeleteTaskHelper.handleAccountDeletion(new AccountDTO(),"123"));
        }
    }

    @Test
    void handleStaffDisablingOnAccountDeletion_exception_test(){
        try(MockedStatic<AccountDeleteTaskHelper> accountDeleteTaskHelperMockedStatic = Mockito.mockStatic(AccountDeleteTaskHelper.class)){
            accountDeleteTaskHelperMockedStatic.when(()->AccountDeleteTaskHelper.handleStaffDisablingOnAccountDeletion(new AccountDTO(),"123")).thenThrow(new IllegalArgumentException("Exception"));
            new AccountTaskController().handleStaffDisablingOnAccountDeletion(CloudTaskUtil.convertObjectToByteArray(Map.of("account",new AccountDTO(),"primaryAdminContactID","123")));
        }catch (Exception e){
            Assertions.assertEquals("exception",e.getMessage());
        }
    }

    @Test
    void handleStaffDisablingOnAccountDeletion_valid_test() throws IOException {
        try(MockedStatic<AccountDeleteTaskHelper> accountDeleteTaskHelperMockedStatic = Mockito.mockStatic(AccountDeleteTaskHelper.class)) {
            new AccountTaskController().handleStaffDisablingOnAccountDeletion(CloudTaskUtil.convertObjectToByteArray(Map.of("account", new AccountDTO(), "primaryAdminContactID", "123")));
            accountDeleteTaskHelperMockedStatic.verify(()->AccountDeleteTaskHelper.handleStaffDisablingOnAccountDeletion(new AccountDTO(),"123"));
        }
    }

    @Test
    void handleClockEntriesOnAccountDeletion_exception_test(){
        try(MockedStatic<AccountDeleteTaskHelper> accountDeleteTaskHelperMockedStatic = Mockito.mockStatic(AccountDeleteTaskHelper.class)){
            accountDeleteTaskHelperMockedStatic.when(()->AccountDeleteTaskHelper.handleClockEntriesOnAccountDeletion(new AccountDTO())).thenThrow(new IllegalArgumentException("Exception"));
            new AccountTaskController().handleClockEntriesOnAccountDeletion(CloudTaskUtil.convertObjectToByteArray(Map.of("account",new AccountDTO(),"primaryAdminContactID","123")));
        }catch (Exception e){
            Assertions.assertEquals("exception",e.getMessage());
        }
    }

    @Test
    void handleClockEntriesOnAccountDeletion_valid_test() throws IOException {
        try(MockedStatic<AccountDeleteTaskHelper> accountDeleteTaskHelperMockedStatic = Mockito.mockStatic(AccountDeleteTaskHelper.class)) {
            new AccountTaskController().handleClockEntriesOnAccountDeletion(CloudTaskUtil.convertObjectToByteArray(Map.of("account", new AccountDTO())));
            accountDeleteTaskHelperMockedStatic.verify(()->AccountDeleteTaskHelper.handleClockEntriesOnAccountDeletion(new AccountDTO()));
        }
    }

    @Test
    void handleAccountCreation_exception_test(){
        try(MockedStatic<AccountCreateTaskHelper> accountCreateTaskHelperMockedStatic = Mockito.mockStatic(AccountCreateTaskHelper.class)){
            accountCreateTaskHelperMockedStatic.when(()->AccountCreateTaskHelper.handleAccountCreation(new SettingsJDO(),new PeopleRelationJDO())).thenThrow(new IllegalArgumentException("Exception"));
            new AccountTaskController().handleAccountCreation(CloudTaskUtil.convertObjectToByteArray(Map.of("account",new SettingsJDO(),"user",new PeopleRelationJDO())));
        }catch (Exception e){
            Assertions.assertEquals("exception",e.getMessage());
        }
    }

    @Test
    void handleAccountCreation_valid_test() throws IOException {
        try(MockedStatic<AccountCreateTaskHelper> accountCreateTaskHelperMockedStatic = Mockito.mockStatic(AccountCreateTaskHelper.class)) {
            new AccountTaskController().handleAccountCreation(CloudTaskUtil.convertObjectToByteArray(Map.of("account", new SettingsJDO(),"user",new PeopleRelationJDO())));
            accountCreateTaskHelperMockedStatic.verify(()->AccountCreateTaskHelper.handleAccountCreation(new SettingsJDO(),new PeopleRelationJDO()));
        }
    }

    @Test
    void handleAccountUpdate_exception_test(){
        try(MockedStatic<AccountUpdateTaskHelper> accountUpdateTaskHelperMockedStatic = Mockito.mockStatic(AccountUpdateTaskHelper.class)){
            accountUpdateTaskHelperMockedStatic.when(()->AccountUpdateTaskHelper.handleAccountUpdate("accID",new Contact(),new AccountUpdatePayloadDTO())).thenThrow(new IllegalArgumentException("Exception"));
            new AccountTaskController().handleAccountUpdate("accID",CloudTaskUtil.convertObjectToByteArray(Map.of("contact",new Contact(),"payloadDTO",new AccountUpdatePayloadDTO())));
        }catch (Exception e){
            Assertions.assertEquals("exception",e.getMessage());
        }
    }

    @Test
    void handleAccountUpdate_valid_test() throws IOException {
        try(MockedStatic<AccountUpdateTaskHelper> accountUpdateTaskHelperMockedStatic = Mockito.mockStatic(AccountUpdateTaskHelper.class)) {
            new AccountTaskController().handleAccountUpdate("accID",CloudTaskUtil.convertObjectToByteArray(Map.of("contact",new Contact(),"payloadDTO",new AccountUpdatePayloadDTO())));
            accountUpdateTaskHelperMockedStatic.verify(()->AccountUpdateTaskHelper.handleAccountUpdate("accID",new Contact(),new AccountUpdatePayloadDTO()));
        }
    }

}
