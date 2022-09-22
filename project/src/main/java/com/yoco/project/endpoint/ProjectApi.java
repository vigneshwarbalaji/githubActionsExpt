package com.yoco.project.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.project.enums.PROJECT_ERROR_RESPONSE;
import com.yoco.project.service.ProjectService;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2")
public class ProjectApi {

    @NoArgsConstructor
    private enum PROJECT_API_CONSTANTS {

        PROJECTS_KEY("projects"),
        CURSOR("cursor"),
        PROJECT_LIST("projectList"),
        PROJECT_PEOPLE_LIST("projectPeopleList"),
        PROJECT("project");

        private String value;

        PROJECT_API_CONSTANTS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }


    private ProjectService projectService = new ProjectService();


    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_PROJECT_READ_WRITE })
    @PostMapping("/account/{accountID}/project")
    public ResponseEntity<GenericResponse> createProject(@PathVariable("accountID") String accountID,
                                                         @RequestBody String payload,
                                                         HttpServletRequest req){

        var genericResponse  = new GenericResponse();

        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);

            String srcClientId = HeaderUtil.extractClientIdFromRequest(req);

            Map<String,Object> createProjectResponse = projectService.createProject(accountID,loggedInContact,payload,srcClientId,"");

            if(ObjUtils.isNullOrEmpty(createProjectResponse)){
                genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.UNABLE_TO_CREATE.value());
            } else {
                genericResponse.setSuccess(true);
                genericResponse.setData(createProjectResponse);
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch (IOException e){
            log.error( "error on project creation : "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = { ApiScope.YOCOAPIS_FULLACCESS, ApiScope.YOCOAPIS_PROJECT_READ_WRITE })
    @PutMapping("/account/{accountID}/project/{projectID}/enable")
    public ResponseEntity<GenericResponse> enableProject(@PathVariable("accountID") String accountID,
                                                         @PathVariable("projectID") String projectID,
                                                         HttpServletRequest req){

        var genericResponse  = new GenericResponse();

        try{
            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);

            Map<String,Object> enableProjectResponse = projectService.enableProject(accountID,projectID,loggedInContact);

            if(ObjUtils.isNullOrEmpty(enableProjectResponse)){
                genericResponse = new GenericResponse(false, null, PROJECT_ERROR_RESPONSE.PROJECT_NOT_FOUND.value());
            } else {
                genericResponse.setSuccess(true);
                genericResponse.setData(enableProjectResponse);
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch (IOException e){
            log.error( "error on project enable : "+e.getMessage());
            genericResponse = new GenericResponse(false, null, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS,  ApiScope.YOCOAPIS_PROJECT_READ})
    @GetMapping(value = "/account/{accountID}/projects")
    public ResponseEntity<GenericResponse> getAllProjectsOfThisAccount(@PathVariable("accountID") String accountID,
                                                                       @RequestParam(value = "type") String type,
                                                                       @RequestParam(value = "cursor", required = false) String cursor,
                                                                       @RequestParam(defaultValue = "200", required = false) int limit){

       var genericResponse = new GenericResponse();

       try {

           Map<String, Object> responseMap = ProjectService.getProjectService().getAllProjectDetailsOfAnAccount(accountID, type, cursor, limit);

           if(ObjUtils.isNullOrEmpty(responseMap)){

               genericResponse.setSuccess(false);
               genericResponse.add(PROJECT_API_CONSTANTS.PROJECTS_KEY.value(), new ArrayList<HashMap<String, Object>>());

               return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

           }else{

               genericResponse.setSuccess(true);

               genericResponse.add(PROJECT_API_CONSTANTS.PROJECTS_KEY.value(), responseMap.get(PROJECT_API_CONSTANTS.PROJECTS_KEY.value()));

               genericResponse.add(PROJECT_API_CONSTANTS.CURSOR.value(), responseMap.get(PROJECT_API_CONSTANTS.CURSOR.value()));

               return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

           }

       }catch (Exception exception){

           log.info("Exception in getAllProjectsOfThisAccount Api:" + exception.getMessage());

           genericResponse = new GenericResponse(false,null, exception.getMessage());
           return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);

       }

    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS,  ApiScope.YOCOAPIS_PROJECT_READ})
    @GetMapping(value = "/account/{accountID}/contact/{contactID}/project")
    public ResponseEntity<GenericResponse> getAllProjectsAssociatedToTheUser(@PathVariable("accountID") String accountID,
                                                                             @PathVariable(value = "contactID") String contactID,
                                                                             @RequestParam(value = "cursor", required = false) String cursor,
                                                                             @RequestParam(defaultValue = "200", required = false) int limit){

        var genericResponse = new GenericResponse();

        try{

            Map<String, Object> responseMap = ProjectService.getProjectService().getAllProjectsAssociatedToTheUser(accountID, contactID, cursor, limit);

            if(ObjUtils.isNullOrEmpty(responseMap)){

                genericResponse.setSuccess(false);
                genericResponse.add(PROJECT_API_CONSTANTS.PROJECTS_KEY.value(), new ArrayList<>());

            }else{

                genericResponse.setSuccess(true);
                genericResponse.add(PROJECT_API_CONSTANTS.PROJECTS_KEY.value(), responseMap.get(PROJECT_API_CONSTANTS.PROJECTS_KEY.value()));
                genericResponse.add(PROJECT_API_CONSTANTS.PROJECT_PEOPLE_LIST.value(), responseMap.get(PROJECT_API_CONSTANTS.PROJECT_PEOPLE_LIST.value()));
                genericResponse.add(PROJECT_API_CONSTANTS.CURSOR.value(), responseMap.get(PROJECT_API_CONSTANTS.CURSOR.value()));

            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch (Exception exception){

            genericResponse = new GenericResponse(false, null, exception.getMessage());
            log.info("Exception inside GetAllProjectsAssociatedToTheUser api: "+exception.getMessage());

            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);

        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS,  ApiScope.YOCOAPIS_PROJECT_READ_WRITE})
    @PutMapping(value = "/account/{accountID}/project/{projectID}")
    public ResponseEntity<GenericResponse> updateProject(@PathVariable("accountID") String accountID,
                                                         @PathVariable("projectID") String projectID,
                                                         @RequestBody String payload,
                                                         HttpServletRequest req){

        var genericResponse = new GenericResponse();

        try{

            String srcClientId = HeaderUtil.extractClientIdFromRequest(req);

            Map<String, Object> responseMap = ProjectService.getProjectService().updateProject(accountID, projectID, srcClientId, payload);

            if(ObjUtils.isNullOrEmpty(responseMap)){
                genericResponse = new GenericResponse(false, null, PROJECT_ERROR_RESPONSE.PROJECT_NOT_FOUND.value());
            }else{
                genericResponse.setSuccess(true);
                genericResponse.setData(responseMap);
            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch(Exception exception){

            genericResponse = new GenericResponse(false, null, exception.getMessage());
            log.info("Exception in UpdateProject Api :"+ exception.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);

        }

    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS,  ApiScope.YOCOAPIS_PROJECT_READ_WRITE})
    @DeleteMapping(value = "/account/{accountID}/project/{projectID}")
    public ResponseEntity<GenericResponse> deleteProject(@PathVariable("accountID") String accountID,
                                                         @PathVariable("projectID") String projectID,
                                                         HttpServletRequest req){

        var genericResponse = new GenericResponse();

        try {

            var loggedInContact = AnnotationHelper.extractCurrentUserContactFromRequest(req);

            String srcClientId = HeaderUtil.extractClientIdFromRequest(req);

            Map<String, Object> responseMap = ProjectService.getProjectService().deleteProject(accountID, projectID, loggedInContact, srcClientId);

            if(ObjUtils.isNullOrEmpty(responseMap)){

                genericResponse = new GenericResponse(false, null, PROJECT_ERROR_RESPONSE.PROJECT_NOT_FOUND.value());

            }else{

                genericResponse.setSuccess(true);
                genericResponse.setData(responseMap);

            }

            return ResponseEntity.status(HttpStatus.OK).body(genericResponse);

        }catch (Exception exception){
            log.info("Exception in deleteproject Api :"+ exception.getMessage());
            genericResponse = new GenericResponse(false, null, exception.getMessage());

            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(genericResponse);
        }

    }
    
}
