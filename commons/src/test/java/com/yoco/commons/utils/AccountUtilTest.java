package com.yoco.commons.utils;

import com.yoco.commons.constants.AccountConstants;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.Status;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.time.DayOfWeek;
import java.util.LinkedHashMap;
import java.util.Map;
import static org.mockito.ArgumentMatchers.anyString;

class AccountUtilTest {

    @Test
    void isActiveEnterpriseAccount_true_test(){
        Map<String, Object> registrantInfo = new LinkedHashMap<>();
        registrantInfo.put(AccountConstants.PRICE, "0.0");
        registrantInfo.put(AccountConstants.PLAN, AccountConstants.ENTERPRISE_PLAN);
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("accountId");
        settingsJDO.setStatus(Status.ACTIVE.toString());
        settingsJDO.setRegisteredPersonDetails(JsonUtil.getJson(registrantInfo));
        Assertions.assertTrue(AccountUtil.isActiveEnterpriseAccount(settingsJDO));
    }

    @Test
    void isActiveEnterpriseAccount_freePlan_test(){
        Map<String, Object> registrantInfo = new LinkedHashMap<>();
        registrantInfo.put(AccountConstants.PRICE, "0.0");
        registrantInfo.put(AccountConstants.PLAN, AccountConstants.FREE_PLAN);
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("accountId");
        settingsJDO.setStatus(Status.ACTIVE.toString());
        settingsJDO.setRegisteredPersonDetails(JsonUtil.getJson(registrantInfo));
        Assertions.assertFalse(AccountUtil.isActiveEnterpriseAccount(settingsJDO));
    }

    @Test
    void isActiveEnterpriseAccount_emptyPlan_test(){
        Map<String, Object> registrantInfo = new LinkedHashMap<>();
        registrantInfo.put(AccountConstants.PRICE, "0.0");
        registrantInfo.put(AccountConstants.PLAN, "");
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("accountId");
        settingsJDO.setStatus(Status.ACTIVE.toString());
        settingsJDO.setRegisteredPersonDetails(JsonUtil.getJson(registrantInfo));
        Assertions.assertFalse(AccountUtil.isActiveEnterpriseAccount(settingsJDO));
    }

    @Test
    void isActiveEnterpriseAccount_emptyRegistrationDetails_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("accountId");
        settingsJDO.setStatus(Status.ACTIVE.toString());
        settingsJDO.setRegisteredPersonDetails("details");
        Assertions.assertFalse(AccountUtil.isActiveEnterpriseAccount(settingsJDO));
    }

    @Test
    void isActiveEnterpriseAccount_InActiveStatus_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("accountId");
        settingsJDO.setStatus("inactive");
        Assertions.assertFalse(AccountUtil.isActiveEnterpriseAccount(settingsJDO));
    }

    @Test
    void isActiveEnterpriseAccount_nullSettingsJDO_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("accountId");
        settingsJDO.setStatus("inactive");
        try(MockedConstruction<AccountImpl> accountMockedConstruction = Mockito.mockConstruction(AccountImpl.class,(accountMock,context) -> {
            Mockito.when(accountMock.getById(anyString())).thenReturn(null);
        })){
            Assertions.assertFalse(AccountUtil.isActiveEnterpriseAccount("accountId"));
        }
    }

    @Test
    void isDecimalDurationActiveForTheGivenAccount_true_test(){
        Map<String, Object> registrantInfo = new LinkedHashMap<>();
        registrantInfo.put(AccountConstants.PRICE, "2.0");
        registrantInfo.put(AccountConstants.PLAN, AccountConstants.FREE_PLAN);
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("accountId");
        settingsJDO.setStatus(Status.ACTIVE.toString());
        settingsJDO.setRegisteredPersonDetails(JsonUtil.getJson(registrantInfo));
        settingsJDO.setDisplayTimeFormat(AccountUtil.DECIMAL);
        Assertions.assertTrue(AccountUtil.isDecimalDurationActiveForTheGivenAccount(settingsJDO));
    }

    @Test
    void isDecimalDurationActiveForTheGivenAccount_false_test(){
        Map<String, Object> registrantInfo = new LinkedHashMap<>();
        registrantInfo.put(AccountConstants.PRICE, "1.0");
        registrantInfo.put(AccountConstants.PLAN, AccountConstants.ENTERPRISE_PLAN);
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("account");
        settingsJDO.setStatus(Status.ACTIVE.toString());
        settingsJDO.setRegisteredPersonDetails(JsonUtil.getJson(registrantInfo));
        settingsJDO.setDisplayTimeFormat("HH:MM");
        Assertions.assertFalse(AccountUtil.isDecimalDurationActiveForTheGivenAccount(settingsJDO));
    }

    @Test
    void isFreeAccount_accountNotOnFreePlan_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("account");
        settingsJDO.setStatus("active");
        Map<String, Object> registrantInfo = new LinkedHashMap<>();
        registrantInfo.put(AccountConstants.PLAN, "Standard");
        String registeredPersonDetails = JsonUtil.getJson(registrantInfo);
        settingsJDO.setRegisteredPersonDetails(registeredPersonDetails);
        boolean isThisAFreePlan = AccountUtil.isFreeAccount(settingsJDO);
        Assertions.assertFalse(isThisAFreePlan);
    }

    @Test
    void isFreeAccount_valid_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("accountId");
        settingsJDO.setStatus("active");
        Map<String, Object> registrantInfo = new LinkedHashMap<>();
        registrantInfo.put(AccountConstants.PLAN, "free");
        String registeredPersonDetails = JsonUtil.getJson(registrantInfo);
        settingsJDO.setRegisteredPersonDetails(registeredPersonDetails);
        boolean isThisAFreePlan = AccountUtil.isFreeAccount(settingsJDO);
        Assertions.assertTrue(isThisAFreePlan);
    }

    @Test
    void getMaxUsersCount_null_test(){
        Assertions.assertEquals(AccountConstants.DEFAULT_USERS_LIMIT,AccountUtil.getMaxUsersCount(null));
    }

    @Test
    void getMaxUsersCount_inActive_test(){
        Map<String, Object> registrantInfo = new LinkedHashMap<>();
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("accountId");
        settingsJDO.setStatus("inactive");
        settingsJDO.setRegisteredPersonDetails(JsonUtil.getJson(registrantInfo));
        Assertions.assertEquals(AccountConstants.DEFAULT_USERS_LIMIT,AccountUtil.getMaxUsersCount(settingsJDO));
    }

    @Test
    void getMaxUsersCount_emptyRegisteredDetails_test(){
        Map<String, Object> registrantInfo = new LinkedHashMap<>();
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("accountId");
        settingsJDO.setStatus(Status.ACTIVE.toString());
        settingsJDO.setRegisteredPersonDetails(JsonUtil.getJson(registrantInfo));
        Assertions.assertEquals(AccountConstants.DEFAULT_USERS_LIMIT,AccountUtil.getMaxUsersCount(settingsJDO));
    }

    @Test
    void getMaxUsersCount_noMaxUsersKey_test(){
        Map<String, Object> registrantInfo = new LinkedHashMap<>();
        registrantInfo.put(AccountConstants.PRICE, "0.0");
        registrantInfo.put(AccountConstants.PLAN, "");
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("accountId");
        settingsJDO.setStatus(Status.ACTIVE.toString());
        settingsJDO.setRegisteredPersonDetails(JsonUtil.getJson(registrantInfo));
        Assertions.assertEquals(AccountConstants.DEFAULT_USERS_LIMIT,AccountUtil.getMaxUsersCount(settingsJDO));
    }

    @Test
    void getMaxUsersCount_valid_test(){
        Map<String, Object> registrantInfo = new LinkedHashMap<>();
        registrantInfo.put(AccountConstants.PRICE, "0.0");
        registrantInfo.put(AccountConstants.PLAN, "");
        registrantInfo.put(AccountConstants.MAX_USERS, "10");
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPeopleUniquePin("accountId");
        settingsJDO.setStatus(Status.ACTIVE.toString());
        settingsJDO.setRegisteredPersonDetails(JsonUtil.getJson(registrantInfo));
        Assertions.assertEquals("10",AccountUtil.getMaxUsersCount(settingsJDO));
    }

    @Test
    void getStartDayOfWeek_SUN_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accountUtilMockedStatic.when(()->AccountUtil.getStartDayOfWeekString("1234")).thenReturn("SUN");
            accountUtilMockedStatic.when(()->AccountUtil.convertWeekStartDayIntoDayOfWeek(anyString())).thenCallRealMethod();
            accountUtilMockedStatic.when(()->AccountUtil.getStartDayOfWeek("1234")).thenCallRealMethod();
            Assertions.assertEquals(DayOfWeek.SUNDAY,AccountUtil.getStartDayOfWeek("1234"));
        }
    }

    @Test
    void getStartDayOfWeek_MON_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accountUtilMockedStatic.when(()->AccountUtil.getStartDayOfWeekString("1234")).thenReturn("MON");
            accountUtilMockedStatic.when(()->AccountUtil.convertWeekStartDayIntoDayOfWeek(anyString())).thenCallRealMethod();
            accountUtilMockedStatic.when(()->AccountUtil.getStartDayOfWeek("1234")).thenCallRealMethod();
            Assertions.assertEquals(DayOfWeek.MONDAY,AccountUtil.getStartDayOfWeek("1234"));
        }
    }

    @Test
    void getStartDayOfWeekString_nullSettingsJDO_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getById("1234")).thenReturn(null);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            Assertions.assertEquals("SUN",AccountUtil.getStartDayOfWeekString("1234"));
        }
    }

    @Test
    void getStartDayOfWeekString_nullWeekStartDay_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getById("1234")).thenReturn(new SettingsJDO());
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            Assertions.assertEquals("SUN",AccountUtil.getStartDayOfWeekString("1234"));
        }
    }

    @Test
    void getStartDayOfWeekString_ValidWeekStartDay_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setWeekStartDay("MON");
            Mockito.when(accountImplMock.getById("1234")).thenReturn(settingsJDO);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            Assertions.assertEquals("MON",AccountUtil.getStartDayOfWeekString("1234"));
        }
    }

    @Test
    void getCompanyDisplayName_nullAccount_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getById("1234")).thenReturn(null);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            Assertions.assertEquals("",AccountUtil.getCompanyDisplayName("1234"));
        }
    }

    @Test
    void getCompanyDisplayName_nullDisplayDomainName_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO settingsJDO = new SettingsJDO();
            Mockito.when(accountImplMock.getById("1234")).thenReturn(settingsJDO);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            Assertions.assertEquals("",AccountUtil.getCompanyDisplayName("1234"));
        }
    }

    @Test
    void getCompanyDisplayName_emptyDisplayDomainName_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setDisplayDomainName("");
            Mockito.when(accountImplMock.getById("1234")).thenReturn(settingsJDO);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            Assertions.assertEquals("",AccountUtil.getCompanyDisplayName("1234"));
        }
    }

    @Test
    void getCompanyDisplayName_ValidDisplayDomainName_test(){
        try(MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO settingsJDO = new SettingsJDO();
            settingsJDO.setDisplayDomainName("company");
            Mockito.when(accountImplMock.getById("1234")).thenReturn(settingsJDO);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            Assertions.assertEquals("company",AccountUtil.getCompanyDisplayName("1234"));
        }
    }

    @Test
    void getCompanyLogo_InvalidRegistrationDetails_test(){
        Assertions.assertEquals("",AccountUtil.getCompanyLogo(new SettingsJDO()));
    }

    @Test
    void getCompanyLogo_InvalidLogo_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setRegisteredPersonDetails("{\"logo\":null}");
        Assertions.assertEquals("",AccountUtil.getCompanyLogo(settingsJDO));
    }

    @Test
    void getCompanyLogo_ValidLogo_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setRegisteredPersonDetails("{\"logo\":\"logoUrl\"}");
        Assertions.assertEquals("logoUrl",AccountUtil.getCompanyLogo(settingsJDO));
    }

    @Test
    void getPayrollAccessSinceDate_0L_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPayrollAccessSince(0L);
        Assertions.assertEquals("",AccountUtil.getPayrollAccessSinceDate(settingsJDO));
    }

    @Test
    void getPayrollAccessSinceDate_valid_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setPayrollAccessSince(1650047400000L);
        settingsJDO.setTimeZone("Asia/Kolkata");
        Assertions.assertEquals("16-Apr-2022",AccountUtil.getPayrollAccessSinceDate(settingsJDO));
    }

    @Test
    void getRegisteredPhoneNumber_InvalidRegistrationDetails_test(){
        Assertions.assertEquals("",AccountUtil.getRegisteredPhoneNumber(new SettingsJDO()));
    }

    @Test
    void getRegisteredPhoneNumber_InvalidPhone_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setRegisteredPersonDetails("{\"phone\":null}");
        Assertions.assertEquals("",AccountUtil.getRegisteredPhoneNumber(settingsJDO));
    }

    @Test
    void getRegisteredPhoneNumber_ValidLogo_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setRegisteredPersonDetails("{\"phone\":\"number\"}");
        Assertions.assertEquals("number",AccountUtil.getRegisteredPhoneNumber(settingsJDO));
    }

    @Test
    void getRegisteredGeoInfoDetails_InvalidRegistrationDetails_test(){
        Assertions.assertEquals(Map.of(),AccountUtil.getRegisteredGeoInfoDetails(new SettingsJDO()));
    }

    @Test
    void getRegisteredGeoInfoDetails_InvalidGeoInfo_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setRegisteredPersonDetails("{\"geoInfo\":null}");
        Assertions.assertEquals(Map.of(),AccountUtil.getRegisteredGeoInfoDetails(settingsJDO));
    }

    @Test
    void getRegisteredGeoInfoDetails_ValidGeoInfo_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setRegisteredPersonDetails("{\"geoInfo\":\"{\\\"location\\\":\\\"IN\\\"}\"}");
        Assertions.assertEquals(Map.of("location","IN"),AccountUtil.getRegisteredGeoInfoDetails(settingsJDO));
    }

    @Test
    void getRegisteredCountryAlphaCode_nullGeoInfo_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setRegisteredPersonDetails("{\"geoInfo\":null}");
        Assertions.assertEquals("US",AccountUtil.getRegisteredCountryAlphaCode(settingsJDO));
    }

    @Test
    void getRegisteredCountryAlphaCode_nullCountry_test(){
        SettingsJDO settingsJdo = new SettingsJDO();
        settingsJdo.setRegisteredPersonDetails("{\"geoInfo\":\"{\\\"country\\\":null}\"}");
        Assertions.assertEquals("US",AccountUtil.getRegisteredCountryAlphaCode(settingsJdo));
    }

    @Test
    void getRegisteredCountryAlphaCode_validCountry_test(){
        SettingsJDO settingsJDO = new SettingsJDO();
        settingsJDO.setRegisteredPersonDetails("{\"geoInfo\":\"{\\\"country\\\":\\\"IN\\\"}\"}");
        Assertions.assertEquals("IN",AccountUtil.getRegisteredCountryAlphaCode(settingsJDO));
    }

    @Test
    void getMaxUsersCountBasedOnPlan_freePlan_test(){
        Assertions.assertEquals("10",AccountUtil.getMaxUsersCountBasedOnPlan("free"));
    }

    @Test
    void getMaxUsersCountBasedOnPlan_StandardPlan_test(){
        Assertions.assertEquals("25",AccountUtil.getMaxUsersCountBasedOnPlan("standard"));
    }

    @Test
    void getMaxUsersCountBasedOnPlan_EnterprisePlan_test(){
        Assertions.assertEquals("unlimited",AccountUtil.getMaxUsersCountBasedOnPlan("enterprise"));
    }

    @Test
    void formatOtHours_nullOtHours_test(){
        Assertions.assertEquals("0 Hours/Day", AccountUtil.formatOtHours(null));
    }

    @Test
    void formatOtHours_ValidOtHours_RangeDay_test(){
        Assertions.assertEquals("1 Hours/Day", AccountUtil.formatOtHours("3600000,HOURS,86400000"));
    }

    @Test
    void formatOtHours_ValidOtHours_RangeWeek_test(){
        Assertions.assertEquals("1 Hours/Week", AccountUtil.formatOtHours("3600000,HOURS,604800000"));
    }

    @Test
    void formatOtHours_ValidOtHours_RangeMonth_test(){
        Assertions.assertEquals("1 Hours/Month", AccountUtil.formatOtHours("3600000,HOURS,2419200000"));
    }

    @Test
    void formatOtHours_ValidOtHours_Rangeinvalid_test(){
        Assertions.assertEquals("1 Hours/", AccountUtil.formatOtHours("3600000,HOURS,0000"));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void validateAndExtractAccount_empty_param_test(String testValue){
        try(MockedStatic<AccountImpl> accountImplMock = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl accountImpl = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImpl.getById(anyString())).thenReturn(null);
            accountImplMock.when(AccountImpl::getAccountImplInstance).thenReturn(accountImpl);
            AccountUtil.validateAndExtractAccount(testValue);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.ACCOUNT_NOT_FOUND.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractAccount_valid_test(){
        try(MockedStatic<AccountImpl> accountImplMock = Mockito.mockStatic(AccountImpl.class)){
            AccountImpl accountImpl = Mockito.mock(AccountImpl.class);
            SettingsJDO account = new SettingsJDO();
            Mockito.when(accountImpl.getById(anyString())).thenReturn(account);
            accountImplMock.when(AccountImpl::getAccountImplInstance).thenReturn(accountImpl);
            Assertions.assertEquals(account,AccountUtil.validateAndExtractAccount("accId"));
        }
    }

}

