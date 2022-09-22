package com.yoco.commons.entity;

import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.enums.Status;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.util.HashMap;
import java.util.Map;

class SettingsJDOTest {

    @Test
    void SettingsJDO_test(){
        try(MockedStatic<ClientSource> clientSourceMockedStatic = Mockito.mockStatic(ClientSource.class)){

            Map<String,Object> clientIdMock = new HashMap<>();
            clientIdMock.put("web","web");
            clientSourceMockedStatic.when(() -> ClientSource.getClientIdMap()).thenReturn(clientIdMock);

            Map<String,Object> accountMap = new HashMap<>();
            accountMap.put("accountID","accountId");
            accountMap.put("sourceEmail","email");
            accountMap.put("accountName","name");
            accountMap.put("timeZone","zone");
            accountMap.put("timeZoneDisplayName","zoneDisplayName");
            accountMap.put("srcClientID","web");
            accountMap.put("registrantInfo","registrantInfo");
            accountMap.put("signUpSource","signUpSource");
            SettingsJDO settingsJDO = new SettingsJDO(accountMap);
            Assertions.assertEquals("accountId",settingsJDO.getPeopleUniquePin());
            Assertions.assertEquals("email",settingsJDO.getSourceEmail());
            Assertions.assertEquals("name",settingsJDO.getDomainName());
            Assertions.assertEquals("name",settingsJDO.getDisplayDomainName());
            Assertions.assertEquals("zone",settingsJDO.getTimeZone());
            Assertions.assertEquals("zoneDisplayName",settingsJDO.getTimeZoneDisplayName());
            Assertions.assertEquals("web",settingsJDO.getSource());
            Assertions.assertEquals("\"registrantInfo\"", settingsJDO.getRegisteredPersonDetails());
            Assertions.assertNotNull(settingsJDO.getDateAddedLongTime());
            Assertions.assertNotNull(settingsJDO.getDateAdded());
            Assertions.assertEquals(Status.ACTIVE.toString(),settingsJDO.getStatus());
            Assertions.assertTrue(settingsJDO.getIsForceClockedOut());
            Assertions.assertTrue(settingsJDO.getIsContactManipulated());
            Assertions.assertTrue(settingsJDO.getIsPayrollEnabled());
            Assertions.assertFalse(settingsJDO.getIsExcessClockedInAlert());
            Assertions.assertFalse(settingsJDO.getIsNfcRfidEnabled());
            Assertions.assertEquals("",settingsJDO.getAllowedIPAddresses());
            Assertions.assertEquals("",settingsJDO.getOtThresholdHours());
            Assertions.assertEquals(DateConstants.WEEK_DAY_SUN,settingsJDO.getWeekStartDay());
            Assertions.assertEquals(0l,settingsJDO.getPayrollAccessSince());
            Assertions.assertEquals(0l,settingsJDO.getDateDeletedLongTime());
        }
    }

    @Test
    void settingsJdo_Constructor2_test(){
        SettingsJDO settingsJDO = new SettingsJDO("accountId","email","name","Asia/Kolkata","web",Map.of(),"srcRef");
        Assertions.assertEquals("accountId",settingsJDO.getPeopleUniquePin());
        Assertions.assertEquals("email",settingsJDO.getSourceEmail());
        Assertions.assertEquals("name",settingsJDO.getDomainName());
        Assertions.assertEquals("name",settingsJDO.getDisplayDomainName());
        Assertions.assertEquals("Asia/Kolkata",settingsJDO.getTimeZone());
        Assertions.assertEquals("(GMT+05:30) Asia/Kolkata (India Time)",settingsJDO.getTimeZoneDisplayName());
        Assertions.assertEquals("web",settingsJDO.getSource());
        Assertions.assertEquals("{}", settingsJDO.getRegisteredPersonDetails());
        Assertions.assertNull(settingsJDO.getDateAddedLongTime());
        Assertions.assertNull(settingsJDO.getDateAdded());
        Assertions.assertEquals(Status.ACTIVE.toString(),settingsJDO.getStatus());
        Assertions.assertTrue(settingsJDO.getIsForceClockedOut());
        Assertions.assertTrue(settingsJDO.getIsContactManipulated());
        Assertions.assertTrue(settingsJDO.getIsPayrollEnabled());
        Assertions.assertFalse(settingsJDO.getIsExcessClockedInAlert());
        Assertions.assertFalse(settingsJDO.getIsNfcRfidEnabled());
        Assertions.assertEquals("",settingsJDO.getAllowedIPAddresses());
        Assertions.assertEquals("",settingsJDO.getOtThresholdHours());
        Assertions.assertEquals(DateConstants.WEEK_DAY_SUN,settingsJDO.getWeekStartDay());
        Assertions.assertEquals(0l,settingsJDO.getPayrollAccessSince());
        Assertions.assertEquals(0l,settingsJDO.getDateDeletedLongTime());
    }
}
