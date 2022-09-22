package com.yoco.commons.enums;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class DateFormatsTest {
    @Test
    void parseAndGetDateFormat_invalidDateFormat_ShouldReturnDefaultFormat_test(){
        Assertions.assertEquals(DateFormats.DD_MMM_YYYY_HH_MM_SS_A,DateFormats.parseAndGetDateFormat("invalid"));
    }

    @Test
    void parseAndGetDateFormat_validDateFormat_ShouldReturnTheEquivalentDateFormat_test(){
        Assertions.assertEquals(DateFormats.ZULU,DateFormats.parseAndGetDateFormat("yyyy-MM-dd'T'HH:mm:ss:SSSXXX"));
    }
}
