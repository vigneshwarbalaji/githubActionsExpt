package com.yoco.account.modal;

import com.yoco.commons.utils.DateUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import java.util.HashMap;
import java.util.Map;

class AccountUpdatePayloadDTOTest {
    @Test
    void constructor_NullValues_test(){
        AccountUpdatePayloadDTO dto = new AccountUpdatePayloadDTO(Map.of());
        Assertions.assertNull(dto.getAccountName());
        Assertions.assertNull(dto.getLogo());
        Assertions.assertNull(dto.getAllowedIPAddresses());
        Assertions.assertNull(dto.getPhoneNumber());
        Assertions.assertNull(dto.getDisplayTimeFormat());
        Assertions.assertNull(dto.getWeekStartDay());
        Assertions.assertNull(dto.getTimeZone());
        Assertions.assertNull(dto.getIsPayrollEnabled());
        Assertions.assertNull(dto.getPayrollAccessSince());
        Assertions.assertNull(dto.getOtThresholdHours());
    }

    @Test
    void constructor_ValidArgument_payrollDisabled_test(){
        Map<String,Object> payload = new HashMap(){{
           put("accountName","name"); put("logo","logo"); put("country","IN"); put("displayTimeFormat","HH MM");
           put("isPayrollEnabled",false); put("otThresholdHours","1/Day"); put("phoneNumber","919000090000"); put("payrollAccessSince","1655922600000");
           put("allowedIPAddresses","1.1.1.1,2.2.2.2"); put("timeZone","Asia/Kolkata"); put("weekStartDay","SUN");
        }};
        AccountUpdatePayloadDTO dto = new AccountUpdatePayloadDTO(payload);
        Assertions.assertEquals("name",dto.getAccountName());
        Assertions.assertEquals("logo",dto.getLogo());
        Assertions.assertEquals("1.1.1.1,2.2.2.2",dto.getAllowedIPAddresses());
        Assertions.assertEquals("+919000090000",dto.getPhoneNumber());
        Assertions.assertEquals("HH MM",dto.getDisplayTimeFormat());
        Assertions.assertEquals("SUN",dto.getWeekStartDay());
        Assertions.assertEquals("Asia/Kolkata",dto.getTimeZone());
        Assertions.assertEquals(false,dto.getIsPayrollEnabled());
        Assertions.assertNull(dto.getPayrollAccessSince());
        Assertions.assertNull(dto.getOtThresholdHours());
    }

    @Test
    void constructor_ValidArgument_payrollEnabled_test(){
        Map<String,Object> payload = new HashMap(){{
            put("accountName","name"); put("logo","logo"); put("country","IN"); put("displayTimeFormat","HH MM SS");
            put("isPayrollEnabled",true); put("otThresholdHours","1/Day"); put("phoneNumber",""); put("payrollAccessSince","1655922600000");
            put("allowedIPAddresses",","); put("timeZone","Asia/Kolkata"); put("weekStartDay","MON");
        }};
        AccountUpdatePayloadDTO dto = new AccountUpdatePayloadDTO(payload);
        Assertions.assertEquals("name",dto.getAccountName());
        Assertions.assertEquals("logo",dto.getLogo());
        Assertions.assertNull(dto.getAllowedIPAddresses());
        Assertions.assertEquals("",dto.getPhoneNumber());
        Assertions.assertEquals("HH MM SS",dto.getDisplayTimeFormat());
        Assertions.assertEquals("MON",dto.getWeekStartDay());
        Assertions.assertEquals("Asia/Kolkata",dto.getTimeZone());
        Assertions.assertEquals(true,dto.getIsPayrollEnabled());
        Assertions.assertEquals(1655922600000L,dto.getPayrollAccessSince());
        Assertions.assertEquals("3600000,HOURS,86400000",dto.getOtThresholdHours());
    }

    @Test
    void validateAndExtractOtThresholdHours_exception_test(){
        AccountUpdatePayloadDTO dto = new AccountUpdatePayloadDTO();
        Assertions.assertNull(dto.validateAndExtractOtThresholdHours("a/a"));
    }

    @Test
    void validateAndExtractOtThresholdHours_HoursGreaterThanLimit_test(){
        AccountUpdatePayloadDTO dto = new AccountUpdatePayloadDTO();
        Assertions.assertNull(dto.validateAndExtractOtThresholdHours("90000000/Day"));
    }

    @Test
    void getDaysTimeUnitForDuration_valid_test(){
        AccountUpdatePayloadDTO dto = new AccountUpdatePayloadDTO();
        Assertions.assertEquals(1,dto.getDaysTimeUnitForDuration("Day"));
        Assertions.assertEquals(7,dto.getDaysTimeUnitForDuration("Week"));
        Assertions.assertEquals(28,dto.getDaysTimeUnitForDuration("Month"));
    }

    @Test
    void validateAndExtractPayrollAccessSince_greaterThanCurrentTime_test(){
        AccountUpdatePayloadDTO dto = new AccountUpdatePayloadDTO();
        long payrollAccessSince = DateUtil.getCurrentTime() + 3600000L;
        long actual = dto.validateAndExtractPayrollAccessSince("" + payrollAccessSince);
        Assertions.assertTrue(actual < payrollAccessSince);
    }

    @Test
    void validateAndExtractDisplayTimeFormat_decimal_test(){
        AccountUpdatePayloadDTO dto = new AccountUpdatePayloadDTO();
        Assertions.assertEquals("Decimal", dto.validateAndExtractDisplayTimeFormat("Decimal"));
    }

    @Test
    void validateAndExtractPhoneNumber_invalidPhoneNumber_test(){
        AccountUpdatePayloadDTO dto = new AccountUpdatePayloadDTO();
        Assertions.assertNull( dto.validateAndExtractPhoneNumber("PP","123"));
    }

    @Test
    void validateAndExtractAllowedIpAddresses_emptyIpAddress_test(){
        AccountUpdatePayloadDTO dto = new AccountUpdatePayloadDTO();
        Assertions.assertEquals("", dto.validateAndExtractAllowedIpAddresses(""));
    }

}
