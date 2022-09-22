package com.yoco;

import com.yoco.commons.utils.GaeUtils;
import com.yoco.constants.ProjectAppProperties;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import java.io.IOException;

@SpringBootApplication
@Slf4j
public class ProjectApplication {

    public static void main(String[] args){
        SpringApplication.run(ProjectApplication.class,args);
        try {
            ProjectAppProperties.setupEnvVariables(GaeUtils.getAppMode());
        } catch (IOException e) {
            log.info(" exception on setting env variables " + e.getMessage());
        }
    }

}
