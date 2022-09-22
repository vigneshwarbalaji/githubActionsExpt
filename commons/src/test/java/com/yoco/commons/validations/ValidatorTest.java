package com.yoco.commons.validations;

import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.JsonUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;

import java.util.Map;

class ValidatorTest {
    @Test
    void validateAndReturnLimit_nullLimit_test(){
        Assertions.assertEquals(50,Validator.validateAndReturnLimit(null,50));
    }

    @Test
    void validateAndReturnLimit_0Limit_test(){
        Assertions.assertEquals(50,Validator.validateAndReturnLimit(0,50));
    }

    @Test
    void validateAndReturnLimit_ValidLimit_test(){
        Assertions.assertEquals(100,Validator.validateAndReturnLimit(100,50));
    }

    @Test
    void isValidCoordinates_invalidLatitude_test(){
        Assertions.assertFalse(Validator.isValidCoordinates("ss","1"));
    }

    @Test
    void isValidCoordinates_emptyLatitude_test(){
        Assertions.assertFalse(Validator.isValidCoordinates("","1"));
    }

    @Test
    void isValidCoordinates_zeroFloatLatitude_test(){
        Assertions.assertFalse(Validator.isValidCoordinates("0.00000","1"));
    }

    @Test
    void isValidCoordinates_zeroLatitude_test(){
        Assertions.assertFalse(Validator.isValidCoordinates("0","1"));
    }

    @Test
    void isValidCoordinates_validCoordinates_test(){
        Assertions.assertFalse(Validator.isValidCoordinates("2.34543","-34.34534"));
    }

    @Test
    void isValidTimeZone_nullTimezone_test(){
        Assertions.assertFalse(Validator.isValidTimeZone(null));
    }

    @Test
    void isValidTimeZone_emptyTimezone_test(){
        Assertions.assertFalse(Validator.isValidTimeZone(""));
    }

    @Test
    void isValidTimeZone_invalidTimezone_test(){
        Assertions.assertFalse(Validator.isValidTimeZone("invalid"));
    }

    @Test
    void isValidTimeZone_validTimezone_test(){
        Assertions.assertTrue(Validator.isValidTimeZone("Asia/Kolkata"));
    }

    @Test
    void isValidLongNumber_null_test(){
        Assertions.assertFalse(Validator.isValidLongNumber(null));
    }

    @Test
    void isValidLongNumber_empty_test(){
        Assertions.assertFalse(Validator.isValidLongNumber(""));
    }

    @Test
    void isValidLongNumber_zero_test(){
        Assertions.assertFalse(Validator.isValidLongNumber("0"));
    }

    @Test
    void isValidLongNumber_valid_test(){
        Assertions.assertTrue(Validator.isValidLongNumber("1655981793056"));
    }

    @Test
    void isValidUrl_nullUrl_test(){ Assertions.assertFalse(Validator.isValidUrl(null));}

    @Test
    void isValidUrl_emptyUrl_test(){ Assertions.assertFalse(Validator.isValidUrl(""));}

    @Test
    void isValidUrl_invalidUrl_test(){ Assertions.assertFalse(Validator.isValidUrl("invalid url"));}

    @Test
    void isValidUrl_invalidUrl2_test(){ Assertions.assertFalse(Validator.isValidUrl("url.cc"));}

    @Test
    void isValidUrl_validUrl_http_test(){ Assertions.assertTrue(Validator.isValidUrl("http://www.yocoboard.com"));}

    @Test
    void isValidUrl_validUrl_https_test(){ Assertions.assertTrue(Validator.isValidUrl("https://www.yocoboard.com"));}

    @Test
    void sanitizeUrl_invalidUrl_test(){
        Assertions.assertEquals("",Validator.sanitizeUrl("<img src='asd' onerror='asd'>"));
    }

    @Test
    void sanitizeUrl_validUrl_with_xssContent_test(){
        Assertions.assertEquals("",Validator.sanitizeUrl("http://www.photo.org/photo.jpg?<img src='asd' onerror='asd'>"));
    }

    @Test
    void sanitizeUrl_validUrl_test(){
        Assertions.assertEquals("http://www.photo.org/photo.jpg",Validator.sanitizeUrl("http://www.photo.org/photo.jpg"));
    }

    @Test
    void sanitizeUrl_invalidemail_test(){
        Assertions.assertEquals("",Validator.sanitizeEmail("email invalid"));
    }

    @Test
    void sanitizeUrl_validemail_with_xssContent_test(){
        Assertions.assertEquals("",Validator.sanitizeEmail("test<img onerror='asd'>@test.co"));
    }

    @Test
    void sanitizeUrl_validEmail_test(){
        Assertions.assertEquals("test@test.co",Validator.sanitizeEmail("test@test.co"));
    }

    @Test
    void unescapeHtml_empty_test(){
        Assertions.assertEquals("",Validator.unescapeHtml(""));
    }

    @Test
    void unescapeHtml_htmlTag_test(){
        Assertions.assertEquals("",Validator.unescapeHtml("<div></div>"));
    }

    @Test
    void unescapeHtml_valid_test(){
        Assertions.assertEquals("message",Validator.unescapeHtml("message"));
    }

    @Test
    void unescapeHtml_htmlTag_test2(){
        Assertions.assertEquals("message",Validator.unescapeHtml("<b>message</b>"));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void validateAndExtractPayload_empty_payload_test(String testValue){
        try{
            Validator.validateAndExtractPayload(testValue);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractPayload_empty_payloadMap_test(){
        try{
            Validator.validateAndExtractPayload(JsonUtil.getJson(Map.of()));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractPayload_valid_test(){
        Map<String,Object> payloadMap = Map.of("key","data");
        Assertions.assertEquals(payloadMap,Validator.validateAndExtractPayload(JsonUtil.getJson(payloadMap)));
    }
}
