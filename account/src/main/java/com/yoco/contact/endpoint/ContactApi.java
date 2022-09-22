package com.yoco.contact.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.contact.enums.CONTACT_ERROR_MESSAGE;
import com.yoco.contact.service.ContactService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class ContactApi {

    private final ContactService contactService = new ContactService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/contacts")
    public ResponseEntity<GenericResponse> getContacts(@RequestParam String ids){
        var response = new GenericResponse();
        try{
            Map<String,Object> getContactsMap = contactService.getContacts(ids);

            if(ObjUtils.isNullOrEmpty(getContactsMap)){
                response.setSuccess(false);
                response.setErrorMessage(CONTACT_ERROR_MESSAGE.CONTACT_DOES_NOT_EXIST.value());
            }else{
                response.setSuccess(true);
                response.setData(getContactsMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch(Exception e){
            log.info("Exception in getContacts :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

}
