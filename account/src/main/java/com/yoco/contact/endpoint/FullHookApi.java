package com.yoco.contact.endpoint;

import com.yoco.contact.service.ContactHookService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/v2")
public class FullHookApi {

    private final ContactHookService contactHookService = new ContactHookService();

    @PostMapping("/processWebHook")
    @ResponseStatus(HttpStatus.OK)
    public void processFullHook(@RequestBody String payload){
        contactHookService.processFullHook(payload);
    }

}
