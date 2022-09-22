package com.yoco.commons.warmup;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

class WarmupHandlerTest {
    @Test
    void warmupHandler_valid_test(){
        ResponseEntity<Boolean> actual = new WarmupHandler().handleWarmUpRequest();
        ResponseEntity<Boolean> expected = new ResponseEntity<>(true, HttpStatus.OK);
        Assertions.assertEquals(expected,actual);
    }
}
