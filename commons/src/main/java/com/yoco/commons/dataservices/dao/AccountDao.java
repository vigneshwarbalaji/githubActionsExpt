package com.yoco.commons.dataservices.dao;

import com.yoco.commons.entity.SettingsJDO;

import java.util.List;

public interface AccountDao {
    SettingsJDO getById(String id);
    void saveAccount(SettingsJDO account);
    void saveAccounts(List<SettingsJDO> accountsList);
    List<SettingsJDO> getAccounts(List<String> ids);
}
