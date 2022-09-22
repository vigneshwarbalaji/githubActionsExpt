package com.yoco.commons.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ObjUtilsTest {

    @Test
    void isNullOrEmpty_null_test(){
        assertTrue(ObjUtils.isNullOrEmpty((String) null));
    }

    @Test
    void isNullOrEmpty_empty_test(){
        assertTrue(ObjUtils.isNullOrEmpty(""));
    }

    @Test
    void isNullOrEmpty_validString_test(){
        assertFalse(ObjUtils.isNullOrEmpty("test"));
    }

    @Test
    void isNull_null_test(){
        assertTrue(ObjUtils.isNull(null));
    }

    @Test
    void isNull_nonNull_Test(){
        assertFalse(ObjUtils.isNull(""));
    }

    @Test
    void isBlank_null_test(){
        assertTrue(ObjUtils.isBlank(null));
    }

    @Test
    void isBlank_empty_test(){
        assertTrue(ObjUtils.isBlank(""));
    }

    @Test
    void isBlank_empty_test2(){
        assertTrue(ObjUtils.isBlank(new String()));
    }

    @Test
    void isBlank_false_test(){
        assertFalse(ObjUtils.isBlank("test"));
    }

}
