package com.yoco.account.helper;

import com.yoco.account.modal.AccountUpdatePayloadDTO;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.entity.SettingsJDO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.util.HashMap;
import java.util.Map;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class AccountUpdateHelperTest {
    @Test
    void validateAndExtractDTO_invalidPayload_test(){
        try{
            AccountUpdateHelper.validateAndExtractDTO("");
        }catch (Exception e){
            Assertions.assertEquals("Payload cannot be empty or null.", e.getMessage());
        }
    }

    @Test
    void validateAndExtractDTO_emptyPayload_test(){
        try{
            AccountUpdateHelper.validateAndExtractDTO("{}");
        }catch (Exception e){
            Assertions.assertEquals("Payload cannot be empty or null.", e.getMessage());
        }
    }

    @Test
    void validateAndExtractDTO_validPayload_test(){
        AccountUpdatePayloadDTO expected= new AccountUpdatePayloadDTO();
        expected.setAccountName(null);
        expected.setDisplayTimeFormat(null);
        expected.setTimeZone(null);
        expected.setWeekStartDay(null);
        expected.setPhoneNumber(null);
        expected.setPayrollAccessSince(null);
        expected.setOtThresholdHours(null);
        expected.setAllowedIPAddresses(null);
        expected.setIsPayrollEnabled(null);
        Assertions.assertEquals(expected,AccountUpdateHelper.validateAndExtractDTO("{\"key\":\"value\"}"));
    }

    @Test
    void updateAccountAndSetActivityMessage_valid_test(){
        SettingsJDO account = new SettingsJDO();
        account.setRegisteredPersonDetails("{}");
        AccountUpdatePayloadDTO accountUpdatePayloadDTO = new AccountUpdatePayloadDTO();
        try(MockedStatic<AccountUpdateHelper> accountUpdateHelperMockedStatic = Mockito.mockStatic(AccountUpdateHelper.class);
            MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            accountUpdateHelperMockedStatic.when(()->AccountUpdateHelper.updateAccountAndSetActivityMessage(account,accountUpdatePayloadDTO)).thenCallRealMethod();
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            accountImplMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            AccountUpdateHelper.updateAccountAndSetActivityMessage(account,accountUpdatePayloadDTO);
            Mockito.verify(accountImplMock).saveAccount(account);
            Assertions.assertEquals("",accountUpdatePayloadDTO.getActivityMessage());
            accountUpdateHelperMockedStatic.verify(()->AccountUpdateHelper.updateGeneralFields(eq(account), eq(Map.of()),eq(accountUpdatePayloadDTO),any(StringBuilder.class)));
            accountUpdateHelperMockedStatic.verify(()->AccountUpdateHelper.updateFieldsBasedOnEnterprisePlan(eq(account),eq("free"), eq(Map.of()),eq(accountUpdatePayloadDTO),any(StringBuilder.class)));
            accountUpdateHelperMockedStatic.verify(()->AccountUpdateHelper.updateDisplayTimeFormatBasedOnPlan(eq(account),eq("free"),eq(accountUpdatePayloadDTO),any(StringBuilder.class)));
        }
    }

    @Test
    void updateGeneralFields_nullValues_test(){
        SettingsJDO account = new SettingsJDO();
        Map<String,Object> registrationDetails = new HashMap<>();
        AccountUpdatePayloadDTO accountUpdatePayloadDTO = new AccountUpdatePayloadDTO();
        StringBuilder activityMessage = new StringBuilder();
        AccountUpdateHelper.updateGeneralFields(account,registrationDetails,accountUpdatePayloadDTO,activityMessage);
        Assertions.assertEquals("",activityMessage.toString());
    }

    @Test
    void updateGeneralFields_validValues_test(){
        SettingsJDO account = new SettingsJDO();
        Map<String,Object> registrationDetails = new HashMap(){{put("phone","oldPhone");}};
        AccountUpdatePayloadDTO accountUpdatePayloadDTO = new AccountUpdatePayloadDTO();
        accountUpdatePayloadDTO.setAccountName("name");
        accountUpdatePayloadDTO.setIsPayrollEnabled(true);
        accountUpdatePayloadDTO.setOtThresholdHours("3600000,HOURS,86400000");
        accountUpdatePayloadDTO.setPayrollAccessSince(1655981793056L);
        accountUpdatePayloadDTO.setPhoneNumber("919000090000");
        accountUpdatePayloadDTO.setTimeZone("Asia/Kolkata");
        accountUpdatePayloadDTO.setWeekStartDay("SUN");
        StringBuilder activityMessage = new StringBuilder();
        AccountUpdateHelper.updateGeneralFields(account,registrationDetails,accountUpdatePayloadDTO,activityMessage);
        Assertions.assertEquals(" AccountName-name PayrollEnabled-true OtHours-1 Hours/Day PayrollAccessSince-1655981793056 Phone-919000090000 TimeZone-Asia/Kolkata WeekStartDay-SUN",activityMessage.toString());
        Assertions.assertEquals("oldPhone",accountUpdatePayloadDTO.getOldPhoneNumber());
        Assertions.assertEquals(Map.of("phone","919000090000"),registrationDetails);
        Assertions.assertEquals("name",account.getDisplayDomainName());
        Assertions.assertTrue(account.getIsPayrollEnabled());
        Assertions.assertEquals("3600000,HOURS,86400000",account.getOtThresholdHours());
        Assertions.assertEquals(1655981793056L,account.getPayrollAccessSince());
        Assertions.assertEquals("Asia/Kolkata",account.getTimeZone());
        Assertions.assertEquals("(GMT+05:30) Asia/Kolkata (India Time)",account.getTimeZoneDisplayName());
        Assertions.assertEquals("SUN",account.getWeekStartDay());
    }

    @Test
    void updateDisplayTimeFormatBasedOnPlan_nullValue_test(){
        SettingsJDO account = new SettingsJDO();
        AccountUpdatePayloadDTO accountUpdatePayloadDTO = new AccountUpdatePayloadDTO();
        StringBuilder activityMessage = new StringBuilder();
        AccountUpdateHelper.updateDisplayTimeFormatBasedOnPlan(account,"free",accountUpdatePayloadDTO,activityMessage);
        Assertions.assertEquals("",activityMessage.toString());
    }

    @Test
    void updateDisplayTimeFormatBasedOnPlan_Decimal_FreePlan_test(){
        SettingsJDO account = new SettingsJDO();
        AccountUpdatePayloadDTO accountUpdatePayloadDTO = new AccountUpdatePayloadDTO();
        accountUpdatePayloadDTO.setDisplayTimeFormat("Decimal");
        StringBuilder activityMessage = new StringBuilder();
        AccountUpdateHelper.updateDisplayTimeFormatBasedOnPlan(account,"free",accountUpdatePayloadDTO,activityMessage);
        Assertions.assertEquals("",activityMessage.toString());
    }

    @Test
    void updateDisplayTimeFormatBasedOnPlan_Decimal_NotFreePlan_test(){
        SettingsJDO account = new SettingsJDO();
        AccountUpdatePayloadDTO accountUpdatePayloadDTO = new AccountUpdatePayloadDTO();
        accountUpdatePayloadDTO.setDisplayTimeFormat("Decimal");
        StringBuilder activityMessage = new StringBuilder();
        AccountUpdateHelper.updateDisplayTimeFormatBasedOnPlan(account,"standard",accountUpdatePayloadDTO,activityMessage);
        Assertions.assertEquals(" DisplayTimeFormat-Decimal",activityMessage.toString());
        Assertions.assertEquals("Decimal",account.getDisplayTimeFormat());
    }

    @Test
    void updateDisplayTimeFormatBasedOnPlan_HH_MM_FreePlan_test(){
        SettingsJDO account = new SettingsJDO();
        AccountUpdatePayloadDTO accountUpdatePayloadDTO = new AccountUpdatePayloadDTO();
        accountUpdatePayloadDTO.setDisplayTimeFormat("HH MM");
        StringBuilder activityMessage = new StringBuilder();
        AccountUpdateHelper.updateDisplayTimeFormatBasedOnPlan(account,"free",accountUpdatePayloadDTO,activityMessage);
        Assertions.assertEquals(" DisplayTimeFormat-HH MM",activityMessage.toString());
        Assertions.assertEquals("HH MM",account.getDisplayTimeFormat());
    }

    @Test
    void updateFieldsBasedOnEnterprisePlan_planNotEnterprise_test(){
        Map<String,Object> regDetails = Map.of();
        StringBuilder activity = new StringBuilder();
        SettingsJDO account = new SettingsJDO();
        AccountUpdateHelper.updateFieldsBasedOnEnterprisePlan(account,"free",regDetails,new AccountUpdatePayloadDTO(), activity);
        Assertions.assertEquals(Map.of(),regDetails);
        Assertions.assertEquals("",activity.toString());
        Assertions.assertEquals(null,account.getAllowedIPAddresses());
    }

    @Test
    void updateFieldsBasedOnEnterprisePlan_planEnterprise_nullValues_test(){
        Map<String,Object> regDetails = Map.of();
        StringBuilder activity = new StringBuilder();
        SettingsJDO account = new SettingsJDO();
        AccountUpdateHelper.updateFieldsBasedOnEnterprisePlan(account,"enterprise",regDetails,new AccountUpdatePayloadDTO(), activity);
        Assertions.assertEquals(Map.of(),regDetails);
        Assertions.assertEquals("",activity.toString());
        Assertions.assertEquals(null,account.getAllowedIPAddresses());
    }

    @Test
    void updateFieldsBasedOnEnterprisePlan_planEnterprise_valid_test(){
        Map<String,Object> regDetails = new HashMap<>();
        StringBuilder activity = new StringBuilder();
        SettingsJDO account = new SettingsJDO();
        AccountUpdatePayloadDTO accountUpdatePayloadDTO = new AccountUpdatePayloadDTO();
        accountUpdatePayloadDTO.setLogo("logoUrl");
        accountUpdatePayloadDTO.setAllowedIPAddresses("ip");
        AccountUpdateHelper.updateFieldsBasedOnEnterprisePlan(account,"enterprise",regDetails,accountUpdatePayloadDTO, activity);
        Assertions.assertEquals(Map.of("logo","logoUrl"),regDetails);
        Assertions.assertEquals(" Logo-logoUrl AllowedIpAddress-ip",activity.toString());
        Assertions.assertEquals("ip",account.getAllowedIPAddresses());
    }
}
