package com.yoco.team.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.team.service.TeamService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;

@Slf4j
@RestController
@RequestMapping("/v2")
public class TeamApi {
    TeamService teamService = new TeamService();

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS,ApiScope.CONTACTAPIS_FULL_ACCESS})
    @GetMapping("/account/{accountID}/team")
    public ResponseEntity<GenericResponse> getTeamsInAnAccount(@PathVariable("accountID") String accountID,
                                                               @RequestParam(required = false) Integer limit,
                                                               @RequestParam(required = false) String cursor,
                                                               HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            response.setSuccess(true);
            response.setData(teamService.getAllTeamsForAccount(accountID,limit,cursor,loggedInContact.getId()));
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch(Exception e){
            log.info("Error fetching teams for account :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS,ApiScope.CONTACTAPIS_FULL_ACCESS})
    @GetMapping("/account/{accountID}/contact/{contactID}/team")
    public ResponseEntity<GenericResponse> getTeamsForUserInAnAccount(@PathVariable("accountID") String accountID,
                                                               @PathVariable("contactID") String contactID,
                                                               HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            response.setSuccess(true);
            response.setData(teamService.getTeamsForUserInAnAccount(accountID,contactID,loggedInContact.getId()));
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch(Exception e){
            log.info("Error fetching teams for user in account :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS,ApiScope.CONTACTAPIS_FULL_ACCESS})
    @DeleteMapping("/account/{accountID}/team/{teamID}")
    public ResponseEntity<GenericResponse> deleteTeam(@PathVariable("accountID") String accountID,
                                                      @PathVariable("teamID") String teamID,
                                                      HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            response.setSuccess(true);
            response.setData(teamService.deleteTeam(accountID,teamID,loggedInContact.getId()));
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch(Exception e){
            log.info("Error deleting team :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS,ApiScope.CONTACTAPIS_FULL_ACCESS})
    @PostMapping("/account/{accountID}/team")
    public ResponseEntity<GenericResponse> createTeam(@PathVariable("accountID") String accountID,@RequestBody String payload, HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            response.setSuccess(true);
            response.setData(teamService.createTeam(accountID,payload,loggedInContact.getId()));
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch(Exception e){
            log.info("Error creating team :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS,ApiScope.CONTACTAPIS_FULL_ACCESS})
    @PutMapping("/account/{accountID}/team/{teamID}")
    public ResponseEntity<GenericResponse> updateTeamInfo(@PathVariable("accountID") String accountID,@PathVariable("teamID") String teamID,@RequestBody String payload, HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            response.setSuccess(true);
            response.setData(teamService.updateTeamInfo(accountID,teamID,payload,loggedInContact.getId()));
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch(Exception e){
            log.info("Error updating team info:: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS,ApiScope.CONTACTAPIS_FULL_ACCESS})
    @PutMapping("/account/{accountID}/team/{teamID}/contact")
    public ResponseEntity<GenericResponse> updateTeamContacts(@PathVariable("accountID") String accountID,@PathVariable("teamID") String teamID,@RequestBody String payload, HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            response.setSuccess(true);
            response.setData(teamService.updateTeamContacts(accountID,teamID,payload,loggedInContact.getId()));
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch(Exception e){
            log.info("Error updating team info:: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }
}
