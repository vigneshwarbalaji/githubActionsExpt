package com.yoco.commons.warmup;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class WarmupHandler {
    @GetMapping("/warmup")
    public ResponseEntity<Boolean> handleWarmUpRequest(){
        return new ResponseEntity<>(true, HttpStatus.OK);
    }
}
