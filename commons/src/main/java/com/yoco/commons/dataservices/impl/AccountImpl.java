package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.dao.AccountDao;
import com.yoco.commons.dataservices.objectify.OfyService;
import com.yoco.commons.entity.SettingsJDO;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

@Slf4j
public class AccountImpl extends OfyService implements AccountDao {

    public static AccountImpl getAccountImplInstance(){
        return new AccountImpl();
    }

    @Override
    public SettingsJDO getById(String id) {
        return get(SettingsJDO.class, id);
    }

    @Override
    public void saveAccount(SettingsJDO account){
        save(account);
    }

    @Override
    public void saveAccounts(List<SettingsJDO> accountsList) {
        saveCollection(accountsList);
    }

    public List<SettingsJDO> getAccounts(List<String> ids){
        return get(SettingsJDO.class,ids);
    }
}
