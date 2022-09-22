package com.yoco;

import com.yoco.commons.utils.GaeUtils;
import com.yoco.constants.AccountAppProperties;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import java.io.IOException;

@Slf4j
@SpringBootApplication
public class AccountApplication {

    public static void main(String[] args) {
        SpringApplication.run(AccountApplication.class, args);
        try {
            AccountAppProperties.setupEnvVariables(GaeUtils.getAppMode());
        } catch (IOException e) {
            log.info("exception " + e.getMessage());
        }
    }
}