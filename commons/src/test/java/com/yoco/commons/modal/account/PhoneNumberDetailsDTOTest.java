package com.yoco.commons.modal.account;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import java.util.Map;

class PhoneNumberDetailsDTOTest {
    @Test
    void PhoneNumberDetailsDTO_noArg_test(){
        PhoneNumberDetailsDTO phoneNumberDetailsDTO = new PhoneNumberDetailsDTO();
        Assertions.assertEquals(PhoneNumberDetailsDTO.class,phoneNumberDetailsDTO.getClass());
        Assertions.assertFalse(phoneNumberDetailsDTO.isValid());
    }

    @Test
    void PhoneNumberDetailsDTO_nullRegistrationDetails_test(){
        PhoneNumberDetailsDTO phoneNumberDetailsDTO = new PhoneNumberDetailsDTO(null);
        Assertions.assertEquals(PhoneNumberDetailsDTO.class,phoneNumberDetailsDTO.getClass());
        Assertions.assertEquals("US",phoneNumberDetailsDTO.getCountry());
        Assertions.assertEquals("",phoneNumberDetailsDTO.getNationalNumber());
        Assertions.assertEquals("",phoneNumberDetailsDTO.getCountryPhoneCode());
        Assertions.assertFalse(phoneNumberDetailsDTO.isValid());
    }

    @Test
    void PhoneNumberDetailsDTO_validPhoneNumber_test(){
        PhoneNumberDetailsDTO phoneNumberDetailsDTO = new PhoneNumberDetailsDTO(Map.of("phone","9999999999","geoInfo","{\"country\":\"IN\"}"));
        Assertions.assertEquals(PhoneNumberDetailsDTO.class,phoneNumberDetailsDTO.getClass());
        Assertions.assertEquals("IN",phoneNumberDetailsDTO.getCountry());
        Assertions.assertEquals("9999999999",phoneNumberDetailsDTO.getNationalNumber());
        Assertions.assertEquals("+91",phoneNumberDetailsDTO.getCountryPhoneCode());
        Assertions.assertTrue(phoneNumberDetailsDTO.isValid());
    }

    @Test
    void isValid_ValidCountry_code_nullNumber_test(){
        PhoneNumberDetailsDTO phoneNumberDetailsDTO = new PhoneNumberDetailsDTO();
        phoneNumberDetailsDTO.setCountry("IN");
        phoneNumberDetailsDTO.setCountryPhoneCode("91");
        Assertions.assertFalse(phoneNumberDetailsDTO.isValid());
    }
}
