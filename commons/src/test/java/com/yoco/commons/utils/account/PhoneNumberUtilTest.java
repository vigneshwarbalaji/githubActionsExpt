package com.yoco.commons.utils.account;

import com.yoco.commons.modal.account.PhoneNumberDetailsDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Map;

class PhoneNumberUtilTest {
    @Test
    void extractUserPhoneNumber_nullPhoneNumber_Test(){
        Assertions.assertNull(PhoneNumberUtil.extractUserPhoneNumber(null,null));
    }

    @Test
    void extractUserPhoneNumber_InvalidPhoneNumber_Test(){
        Assertions.assertNull(PhoneNumberUtil.extractUserPhoneNumber("invalid","IN"));
    }

    @Test
    void extractUserPhoneNumber_validTest(){
        var phoneNumberWithoutCode = PhoneNumberUtil.extractUserPhoneNumber("9999999999","IN");
        var phoneNumberWithCode = PhoneNumberUtil.extractUserPhoneNumber("+919999999999","IN");
        Assertions.assertEquals(phoneNumberWithCode.getNationalNumber(),phoneNumberWithoutCode.getNationalNumber());
        Assertions.assertEquals(phoneNumberWithCode.getCountryCode(),phoneNumberWithoutCode.getCountryCode());
        Assertions.assertEquals(9999999999L,phoneNumberWithoutCode.getNationalNumber());
        Assertions.assertEquals(91,phoneNumberWithoutCode.getCountryCode());
    }

    @Test
    void getCountryCodeFromPhoneNumber_nullPhoneNumber_Test(){
        Assertions.assertNull(PhoneNumberUtil.getCountryCodeFromPhoneNumber(null));
    }

    @Test
    void getCountryCodeFromPhoneNumber_validTest(){
        var phoneNumberWithCode = PhoneNumberUtil.extractUserPhoneNumber("+919999999999","IN");
        Assertions.assertEquals("IN",PhoneNumberUtil.getCountryCodeFromPhoneNumber(phoneNumberWithCode));
    }

    @Test
    void isValidPhoneNumber_null_test(){
        Assertions.assertFalse(PhoneNumberUtil.isValidPhoneNumber(null));
    }

    @Test
    void isValidPhoneNumber_NoCountryCode_test(){
        var phoneNumberWithCode = PhoneNumberUtil.extractUserPhoneNumber("+919999999999","IN");
        phoneNumberWithCode.clearCountryCode();
        Assertions.assertFalse(PhoneNumberUtil.isValidPhoneNumber(phoneNumberWithCode));
    }

    @Test
    void isValidPhoneNumber_NoNationalNumber_test(){
        var phoneNumberWithCode = PhoneNumberUtil.extractUserPhoneNumber("+919999999999","IN");
        phoneNumberWithCode.clearNationalNumber();
        Assertions.assertFalse(PhoneNumberUtil.isValidPhoneNumber(phoneNumberWithCode));
    }

    @Test
    void isValidPhoneNumber_valid_test(){
        var phoneNumberWithCode = PhoneNumberUtil.extractUserPhoneNumber("+919999999999","IN");
        Assertions.assertTrue(PhoneNumberUtil.isValidPhoneNumber(phoneNumberWithCode));
    }

    @Test
    void phoneNumberDetailsDTO_valid_test(){
       var phoneNumberDto = new PhoneNumberDetailsDTO(Map.of("phone","+919999999999","geoInfo","\"country\":\"IN\""));
       Assertions.assertEquals("9999999999",phoneNumberDto.getNationalNumber());
       Assertions.assertEquals("+91",phoneNumberDto.getCountryPhoneCode());
       Assertions.assertEquals("IN",phoneNumberDto.getCountry());
    }
}
