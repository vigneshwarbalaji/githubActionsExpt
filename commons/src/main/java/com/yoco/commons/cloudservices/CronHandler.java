package com.yoco.commons.cloudservices;

import com.yoco.commons.fullservices.FullMetrics;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;


@Slf4j
@RestController
@RequestMapping("/cron")
public class CronHandler {

    @GetMapping("/processMetrics")
    @ResponseStatus(HttpStatus.OK)
    public void processFullMetrics(){

        log.info("inside metrics");

        FullMetrics.pushToFullMetrics();
    }

}
