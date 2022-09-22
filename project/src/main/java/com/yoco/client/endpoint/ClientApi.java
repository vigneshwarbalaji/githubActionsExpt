package com.yoco.client.endpoint;

import com.yoco.client.enums.CLIENT_ERROR_RESPONSE;
import com.yoco.client.service.ClientService;
import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.error.ApiErrorCode;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;

@Slf4j
@RestController
@RequestMapping("/v2")
public class ClientApi {

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_CLIENT_READ_WRITE })
    @PostMapping("/account/{accountID}/client")
    public ResponseEntity<GenericResponse> createClient (@PathVariable("accountID") String  accountID, @RequestBody String payload, HttpServletRequest req) throws NoSuchAlgorithmException {

        var genericResponse = new GenericResponse();

        try {

            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);

            HashMap<String, Object> createClientResponse = (HashMap<String, Object>) ClientService.getClientService().createClient(accountID, loggedInContact.getId(), payload);

            if(ObjUtils.isNullOrEmpty(createClientResponse)){
                genericResponse = new GenericResponse(false, null, CLIENT_ERROR_RESPONSE.UNABLE_TO_CREATE_CLIENT.value());
            } else {
                genericResponse.setSuccess(true);
                genericResponse.setData(createClientResponse);
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        } catch (Exception e) {
            log.error( "error on Client creation endpoint : "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_CLIENT_READ_WRITE })
    @GetMapping("/account/{accountID}/client")
    public ResponseEntity<GenericResponse> getAllClients (@PathVariable("accountID") String accountID,
                                         @RequestParam(value = "projectID", required = false) String projectID,
                                         @RequestParam(value = "cursor", required = false) String cursor) {

        var genericResponse = new GenericResponse();
        try {
            HashMap<String, Object> getAllResponse = (HashMap<String, Object>) ClientService.getClientService().getAll(accountID, projectID, cursor);

            if(ObjUtils.isNullOrEmpty(getAllResponse)){
                genericResponse = new GenericResponse(false, null, CLIENT_ERROR_RESPONSE.NO_CLIENT_EXISTS.value());
            } else {
                genericResponse.setSuccess(true);
                genericResponse.setData(getAllResponse);
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        } catch (Exception e) {
            log.error( "error on getAllClients endpoint : "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_CLIENT_READ_WRITE })
    @PutMapping(value = "/account/{accountID}/client/{clientID}")
    public ResponseEntity<GenericResponse> updateClient (@PathVariable("accountID") String accountID,
                                        @PathVariable("clientID") String clientID,
                                        @RequestBody String payload ){

        var genericResponse = new GenericResponse();
        try {

            HashMap<String, Object> updateClientResponse = (HashMap<String, Object>) ClientService.getClientService().updateClient(accountID, clientID, payload);

            if(ObjUtils.isNullOrEmpty(updateClientResponse)){
                genericResponse = new GenericResponse(false, null, CLIENT_ERROR_RESPONSE.INVALID_CLIENT_ID.value());
            } else {
                genericResponse.setSuccess(true);
                genericResponse.setData(updateClientResponse);
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        } catch (Exception e) {
            log.error( "error on Client update : "+e.getMessage());
            genericResponse = new GenericResponse(false, ApiErrorCode.BAD_REQUEST, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_CLIENT_READ_WRITE })
    @GetMapping("/account/{accountID}/client/{clientID}")
    public ResponseEntity<GenericResponse> getClientByID (@PathVariable("clientID") String clientID) {

        var genericResponse = new GenericResponse();

        try {
            HashMap<String, Object> getByIdResponse = (HashMap<String, Object>) ClientService.getClientService().getByID(clientID);

            if(ObjUtils.isNullOrEmpty(getByIdResponse)){
                genericResponse = new GenericResponse(false, null, CLIENT_ERROR_RESPONSE.NO_CLIENT_EXISTS.value());
            } else {
                genericResponse.setSuccess(true);
                genericResponse.setData(getByIdResponse);
            }
            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        } catch (Exception e) {
            log.error( "error on get client by id endpoint : "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_CLIENT_READ_WRITE })
    @DeleteMapping("/account/{accountID}/client/{clientID}")
    public ResponseEntity<GenericResponse> deleteClientByID (@PathVariable("clientID") String clientID,
                                            @PathVariable("accountID") String accountID,
                                            HttpServletRequest request) {

        var genericResponse = new GenericResponse();

        try {
            var loggedInUser = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            HashMap<String, Object> deleteClientResponse = (HashMap<String, Object>) ClientService.getClientService().deleteClient(clientID, accountID, loggedInUser.getId());

            if(ObjUtils.isNullOrEmpty(deleteClientResponse)){
                genericResponse = new GenericResponse(false, null, CLIENT_ERROR_RESPONSE.NO_CLIENT_EXISTS.value());
            } else {
                genericResponse.setSuccess(true);
                genericResponse.setData(deleteClientResponse);
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        } catch (Exception e) {
            log.error( "error on delete client by id endpoint : "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

}
