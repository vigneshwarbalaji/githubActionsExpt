package com.yoco.commons.modal;

import com.yoco.commons.enums.error.ApiErrorCode;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GenericResponseTest {

    @Test
    void genericResponse_valid_test(){
        GenericResponse response = new GenericResponse(false, ApiErrorCode.BAD_REQUEST,"test");
        assertEquals("test",response.getErrorMessage());
        assertEquals(false,response.isSuccess());
        assertEquals(ApiErrorCode.BAD_REQUEST,response.getErrorCode());
        response.add("key","data" );
        assertEquals(false,response.getData().isEmpty());
        response.addNonNull("key1","data1");
        assertEquals("data1",response.getData().get("key1"));
        response.addNonNull("key2",null);
        assertEquals(false,response.getData().containsKey("key2"));
    }

}
