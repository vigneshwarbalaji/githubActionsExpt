package com.yoco.contact.endpoint;

import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.contact.helper.ContactTaskHandler;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/task/contact")
public class ContactTaskController {

    @PostMapping("/contactTaskHandler")
    @ResponseStatus(HttpStatus.OK)
    public void contactTaskHandler(@RequestBody byte[] payload) throws IOException, ClassNotFoundException {
        log.info(" received in contactTaskHandler");
        Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
        log.info(" payloadMap: " + payloadMap);
        ContactTaskHandler.getInstance().handleContactTaskHandler(payloadMap);
    }

}
