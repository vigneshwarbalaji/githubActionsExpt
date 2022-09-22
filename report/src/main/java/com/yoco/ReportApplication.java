package com.yoco;

import com.yoco.commons.utils.GaeUtils;
import com.yoco.constants.ReportAppProperties;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import java.io.IOException;

@Slf4j
@SpringBootApplication
public class ReportApplication {

    public static void main(String[] args){
        SpringApplication.run(ReportApplication.class,args);
        try {
            ReportAppProperties.setupEnvVariables(GaeUtils.getAppMode());
        } catch (IOException e) {
            log.info(" exception on setting report env variables " + e.getMessage());
        }
    }

}
