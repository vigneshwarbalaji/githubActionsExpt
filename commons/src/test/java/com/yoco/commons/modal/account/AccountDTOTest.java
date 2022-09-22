package com.yoco.commons.modal.account;

import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.utils.JsonUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import java.util.Date;
import java.util.Map;

class AccountDTOTest {
    SettingsJDO getMockSettingsJDO(){
        SettingsJDO settingsJDO =  new SettingsJDO();
        settingsJDO.setDisplayDomainName("name");
        settingsJDO.setWeekStartDay("SUN");
        settingsJDO.setPayrollAccessSince(0L);
        settingsJDO.setDateAdded(new Date());
        settingsJDO.setAllowedIPAddresses("");
        settingsJDO.setDateAddedLongTime(new Date().getTime());
        settingsJDO.setDomainName("name");
        settingsJDO.setDisplayTimeFormat("HH MM");
        settingsJDO.setIsNfcRfidEnabled(true);
        settingsJDO.setIsPayrollEnabled(false);
        settingsJDO.setOtThresholdHours("");
        settingsJDO.setTimeZoneDisplayName("(GMT+05:30) Asia/Colombo (India Time)");
        settingsJDO.setPeopleUniquePin("id");
        settingsJDO.setSourceEmail("email");
        settingsJDO.setDateDeletedLongTime(0L);
        settingsJDO.setStatus("ACTIVE");
        Map<String,Object> registeredDetails = Map.of();
        settingsJDO.setRegisteredPersonDetails(JsonUtil.getJson(registeredDetails));
        return settingsJDO;
    }

    @Test
    void AccountDTONoArgTest(){
        AccountDTO accountDTO = new AccountDTO();
        Assertions.assertEquals(AccountDTO.class,accountDTO.getClass());
    }

    @Test
    void AccountDTO_SettingsJDO_null_NFCRFIDTest(){
        SettingsJDO settingsJDO = getMockSettingsJDO();
        settingsJDO.setIsNfcRfidEnabled(null);
        AccountDTO accountDTO = new AccountDTO(settingsJDO);
        Assertions.assertEquals(AccountDTO.class,accountDTO.getClass());
        Assertions.assertFalse(accountDTO.isNfcRfidEnabled());
    }

    @Test
    void AccountDTO_SettingsJDO_valid_NFCRFIDTest(){
        SettingsJDO settingsJDO = getMockSettingsJDO();
        AccountDTO accountDTO = new AccountDTO(settingsJDO);
        Assertions.assertEquals(AccountDTO.class,accountDTO.getClass());
        Assertions.assertTrue(accountDTO.isNfcRfidEnabled());
    }
}
