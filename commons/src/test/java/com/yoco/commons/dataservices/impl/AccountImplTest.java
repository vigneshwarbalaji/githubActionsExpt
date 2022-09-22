package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.objectify.LocalDatastoreExtension;
import com.yoco.commons.dataservices.objectify.ObjectifyExtension;
import com.yoco.commons.entity.SettingsJDO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.List;

import static com.yoco.commons.dataservices.objectify.OfyService.ofy;

@ExtendWith({
        LocalDatastoreExtension.class,
        ObjectifyExtension.class})
class AccountImplTest {

    AccountImpl account = AccountImpl.getAccountImplInstance();

    @Test
    void getAccountImplInstance_test(){
        Assertions.assertEquals(AccountImpl.class,AccountImpl.getAccountImplInstance().getClass());
    }

    @Test
    void get_nullEntity_test(){
        Assertions.assertNull(account.getById("id"));
    }

    @Test
    void get_validEntity_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("id");
        account.saveAccount(settingsJDO);
        Assertions.assertEquals(settingsJDO,account.getById("id"));
        ofy().delete().entity(settingsJDO).now();
    }

    @Test
    void getAccounts_test(){
        SettingsJDO account1 = new SettingsJDO();
        account1.setPeopleUniquePin("accId");
        SettingsJDO account2 = new SettingsJDO();
        account2.setPeopleUniquePin("accId2");
        account.saveAccounts(List.of(account1,account2));
        Assertions.assertEquals(List.of(account1,account2),account.getAccounts(List.of("accId","accId2")));
        ofy().delete().entity(account1).now();
        ofy().delete().entity(account2).now();
    }
}
