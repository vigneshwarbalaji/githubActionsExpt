package com.yoco.account.helper;

import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.utils.ActivityUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

class AccountDeleteHelperTest {
    @Test
    void extractReasonFromPayload_InvalidPayload_test(){
        Assertions.assertEquals("",AccountDeleteHelper.extractReasonFromPayload("invalid"));
    }

    @Test
    void extractReasonFromPayload_nullReason_test(){
        Assertions.assertEquals("",AccountDeleteHelper.extractReasonFromPayload("{}"));
    }

    @Test
    void extractReasonFromPayload_validReason_test(){
        Assertions.assertEquals("yocoboard. com reason",AccountDeleteHelper.extractReasonFromPayload("{\"reason\":\"https://www.yocoboard.com reason\"}"));
    }

    @Test
    void updateSettingsJdoEntry_valid_test(){
        try(MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            SettingsJDO account = new SettingsJDO();
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            accountImplMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            AccountDeleteHelper.updateSettingsJdoEntryForDeletion(account);
            Assertions.assertEquals("INACTIVE",account.getStatus());
            Assertions.assertTrue(account.getDateDeletedLongTime() > 0);
            Mockito.verify(accountImplMock).saveAccount(account);
        }
    }

    @Test
    void saveAccountDeletionActivity_valid_test(){
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accID");
            account.setDateDeletedLongTime(1L);
            Contact contact = new Contact();
            contact.setId("123");
            contact.setEmailID("email");
            AccountDeleteHelper.saveAccountDeletionActivity(contact,account,"reason");
            activityUtilMockedStatic.verify(()->ActivityUtil.saveActivity("accID","123","Account Deletion","email","123 has deleted account: accID. Reason : reason","DUMMY",1L));
        }
    }
}
