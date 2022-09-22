package com.yoco.peers.endpoint;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import com.yoco.peers.service.PeerService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;

@Slf4j
@RestController
@RequestMapping("/v2")
public class Peers {

    PeerService peersService = new PeerService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_CLOCK_READ, ApiScope.YOCOAPIS_FULLACCESS})
    @GetMapping("/account/{accountID}/peers")
    public ResponseEntity<GenericResponse> getAllClockedInPeers (@PathVariable("accountID") String  accountID, HttpServletRequest req){
        var genericResponse = new GenericResponse();
        try {
            var loggedInUserContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);
            var  responseMap = peersService.getClockedInUsers(accountID, loggedInUserContact);

            if(ObjUtils.isNullOrEmpty(responseMap)){
                genericResponse = new GenericResponse(Boolean.FALSE, null, EVENTS_ERROR_RESPONSE.NO_CLOCKEDIN_USERS.value());
            }else {
                genericResponse.setSuccess(true);
                genericResponse.setData(responseMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);
        } catch (Exception e) {
            log.info( "error in fetching clocked in peers: "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

}
