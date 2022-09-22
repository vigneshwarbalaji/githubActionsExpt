package com.yoco.commons.validations;

import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.*;

class RangeValidatorTest {

    @Test
    void validateRange_empty_range_test(){
        try{
            RangeValidator.validateRange("","","");
        }catch (IllegalArgumentException e){
            assertEquals(COMMON_ERROR_RESPONSE.INVALID_RANGE.value(), e.getMessage());
        }
    }

    @Test
    void validateRange_empty_fromDate_test(){
        try{
            RangeValidator.validateRange(DateConstants.BY_DATE,"","");
        }catch (IllegalArgumentException e){
            assertEquals(COMMON_ERROR_RESPONSE.FROM_AND_TO_DATE_MANDATORY.value(), e.getMessage());
        }
    }

    @Test
    void validateRange_empty_toDate_test(){
        try{
            RangeValidator.validateRange(DateConstants.BY_DATE,"01/31/2022","");
        }catch (IllegalArgumentException e){
            assertEquals(COMMON_ERROR_RESPONSE.FROM_AND_TO_DATE_MANDATORY.value(), e.getMessage());
        }
    }

    @Test
    void validateRange_Invalid_dateformats_test(){
        try{
            RangeValidator.validateRange(DateConstants.BY_DATE,"31/31/2022","31/31/2022");
        }catch (IllegalArgumentException e){
            assertEquals(COMMON_ERROR_RESPONSE.INVALID_DATEFORMAT.value(), e.getMessage());
        }
    }

    @Test
    void validateRange_byDate_valid_test(){
        assertDoesNotThrow(()-> RangeValidator.validateRange(DateConstants.BY_DATE,"01/31/2022","01/31/2022"));
    }

    @Test
    void validateRange_invalid_range_test(){
        try{
            RangeValidator.validateRange("currentDay","","");
        }catch (IllegalArgumentException e){
            assertEquals(COMMON_ERROR_RESPONSE.INVALID_RANGE.value(), e.getMessage());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {DateConstants.TODAY,DateConstants.CURRENT_WEEK,DateConstants.LAST_WEEK})
    void validateRange_valid_range_test(String range){
        assertDoesNotThrow(()-> RangeValidator.validateRange(range,"",""));
    }

}
