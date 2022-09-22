package com.yoco.project.service;

import com.yoco.commons.dataservices.impl.ProjectPeopleImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.entity.ProjectPeopleJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.dataservices.impl.ProjectImpl;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import com.yoco.project.enums.PROJECT_ERROR_RESPONSE;
import com.yoco.project.helper.ProjectHelper;
import com.yoco.project.helper.ProjectTaskHelper;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import java.io.IOException;
import java.util.*;

@Slf4j
@Service
public class ProjectService {

    public static ProjectService getProjectService() {
        return new ProjectService();
    }

    @NoArgsConstructor
    private enum PROJECT_PAYLOAD_FIELDS {

        PROJECTS_KEY("projects"),
        CURSOR("cursor"),
        PROJECT_LIST("projectList"),
        PROJECT_PEOPLE_LIST("projectPeopleList"),
        PROJECT("project"),
        NAME("name"),
        BILLABLE("billable"),
        CURRENCY("currency"),
        RATE("rate"),
        PLAN("plan"),
        CONTACTS("contacts"),
        DELETED_CONTACTS("deletedContacts"),
        ADDED_CONTACTS("addedContacts"),
        CLIENT("clientID"),
        ACTION("action"),
        PROJECTJDO("projectJDO"),
        CURRENTTIME("currentTime"),
        ACTIVE("active");

        private String value;

        PROJECT_PAYLOAD_FIELDS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    public Map<String,Object> createProject(String accountId, Contact currentUser, String payload,String srClientId,String projectId) throws IOException {

        Validator.checkArgument( ObjUtils.isNullOrEmpty(accountId), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value() );
        Validator.checkArgument( ObjUtils.isNullOrEmpty(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value() );
        Validator.checkArgument( ObjUtils.isNull(currentUser),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());

        Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);

        String  name  = Validator.sanitizeText(payloadMap.get(ProjectService.PROJECT_PAYLOAD_FIELDS.NAME.value()).toString());
        boolean billable  = payloadMap.containsKey(PROJECT_PAYLOAD_FIELDS.BILLABLE.value()) && (boolean)payloadMap.get(PROJECT_PAYLOAD_FIELDS.BILLABLE.value());

        log.info(" billable ? " + billable);

        String  currency     = billable ? payloadMap.get(PROJECT_PAYLOAD_FIELDS.CURRENCY.value()).toString() : "";
        double  rate         = billable ? Double.parseDouble((String) payloadMap.get(PROJECT_PAYLOAD_FIELDS.RATE.value()))  : 0 ;
        String  plan         = billable ? payloadMap.get(PROJECT_PAYLOAD_FIELDS.PLAN.value()).toString() : "";

        String[] staffIDs = payloadMap.containsKey(PROJECT_PAYLOAD_FIELDS.CONTACTS.value())
                             ? payloadMap.get(PROJECT_PAYLOAD_FIELDS.CONTACTS.value()).toString().split(","): new String[0];

        String  clientID = payloadMap.containsKey(PROJECT_PAYLOAD_FIELDS.CLIENT.value())
                            ? payloadMap.get(PROJECT_PAYLOAD_FIELDS.CLIENT.value()).toString() : "";

        var projectHelper = ProjectHelper.getProjectHelper();

        var projectJDO  =  projectHelper.createProjectJDO(accountId,name,billable,currency,rate,plan,projectId);

        projectJDO = projectHelper.createProjectPeopleAssociation(projectJDO,staffIDs);

        projectHelper.associateClientToProject(projectJDO,clientID);

        Map<String,Object> queuePayload = new HashMap<>();
        queuePayload.put(PROJECT_PAYLOAD_FIELDS.ACTION.value(), "create");
        queuePayload.put(PROJECT_PAYLOAD_FIELDS.PROJECTJDO.value(), projectJDO);
        queuePayload.put("sourceClientId",srClientId);
        queuePayload.put("currentUser",currentUser);
        queuePayload.put(PROJECT_PAYLOAD_FIELDS.CURRENTTIME.value(), DateUtil.getCurrentTime());

        ProjectTaskHelper.getProjectTaskHelper().initiateProjectOperationsTaskQueue(queuePayload);

        Map<String,Object> response = new HashMap<>();
        response.put(PROJECT_PAYLOAD_FIELDS.PROJECT.value(),projectJDO);

        return response;
    }

    public Map<String,Object> enableProject(String accountId, String projectId, Contact currentUserPro) throws IOException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountId),COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(projectId),PROJECT_ERROR_RESPONSE.INVALID_PROJECT_ID.value());
        Validator.checkArgument(ObjUtils.isNull(currentUserPro),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());

        Map<String,Object> response = new HashMap<>();

        var projectImplInstance = ProjectImpl.getProjectImplInstance();
        var project = projectImplInstance.getProject(projectId);

        if(!ObjUtils.isNull(project)){
            project.setIsDeleted(false);
            projectImplInstance.saveProject(project);
            project.setContacts(new ArrayList<>());

            Map<String,Object> enableQueuePayload = new HashMap<>();
            enableQueuePayload.put(PROJECT_PAYLOAD_FIELDS.ACTION.value(),"enable");
            enableQueuePayload.put(PROJECT_PAYLOAD_FIELDS.PROJECTJDO.value(), project);
            enableQueuePayload.put("currentUser",currentUserPro);
            enableQueuePayload.put(PROJECT_PAYLOAD_FIELDS.CURRENTTIME.value(), DateUtil.getCurrentTime());

            ProjectTaskHelper.getProjectTaskHelper().initiateProjectOperationsTaskQueue(enableQueuePayload);

            response.put(PROJECT_PAYLOAD_FIELDS.PROJECT.value(),project);
        }

        return response;
    }

    public Map<String,Object> getAllProjectsAssociatedToTheUser(String accountID, String contactID, String cursor, int limit){

        Validator.checkArgument( ObjUtils.isNullOrEmpty( accountID ), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument( ObjUtils.isNullOrEmpty( contactID ), COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value());

        var projectHelper = ProjectHelper.getProjectHelper();

        return projectHelper.getAllProjectsAndItsDetailsAssociatedToTheUser(accountID, contactID, cursor, limit);
    }

    public Map<String, Object> getAllProjectDetailsOfAnAccount(String accountID, String type, String cursor, int limit){

        Validator.checkArgument( ObjUtils.isNullOrEmpty( accountID ), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());

        Map<String, Object> responseMap = new HashMap<>();
        var projectHelperObject = ProjectHelper.getProjectHelper();

        Map<String,Object> listOfProjects = ProjectImpl.getProjectImplInstance().getAllProjectsInAccount(accountID, type, cursor, limit);

        if(type.equals(PROJECT_PAYLOAD_FIELDS.ACTIVE.value())){

            List<ProjectJDO>responseList = projectHelperObject.setProjectAssociatedContactToTheList(accountID, (List<ProjectJDO>) listOfProjects.get(PROJECT_PAYLOAD_FIELDS.PROJECTS_KEY.value()));
            listOfProjects.put(PROJECT_PAYLOAD_FIELDS.PROJECTS_KEY.value(), responseList);

        }

        responseMap.put(PROJECT_PAYLOAD_FIELDS.PROJECTS_KEY.value(), listOfProjects.get(PROJECT_PAYLOAD_FIELDS.PROJECTS_KEY.value()));

        responseMap.put(PROJECT_PAYLOAD_FIELDS.CURSOR.value(), listOfProjects.get(PROJECT_PAYLOAD_FIELDS.CURSOR.value()));

        return responseMap;

    }

    public Map<String, Object> deleteProject(String accountID, String projectID, Contact loggedInContact, String srcClientId) throws IOException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(projectID), PROJECT_ERROR_RESPONSE.INVALID_PROJECT_ID.value());

        Map<String, Object>responseMap = new HashMap<>();

        var projectObject = ProjectImpl.getProjectImplInstance().getProject(projectID);
        if(!ObjUtils.isNull(projectObject)){

            List<ProjectPeopleJDO> projectPeopleList = ProjectPeopleImpl.getProjectPeopleImplInstance().getProjectAssociationByProjectID(projectObject.getUniquepin(), projectObject.getProjectId());
            projectObject = ProjectHelper.getProjectHelper().setContactDetailsInsideProjectObject(projectObject, projectPeopleList);

            Map<String, Object>payloadMap = new HashMap<>();

            List<String> contactsList;
            StringBuilder contactBuilder = new StringBuilder();

            if(!ObjUtils.isNullOrEmpty(projectObject.getContacts())){

                contactsList = projectObject.getContacts();
                for(var contactID : contactsList){
                    contactBuilder.append(contactID);
                    contactBuilder.append(",");
                }

            }

            payloadMap.put(PROJECT_PAYLOAD_FIELDS.DELETED_CONTACTS.value(), contactBuilder);
            projectObject.setIsDeleted(true);
            ProjectImpl.getProjectImplInstance().saveProject(projectObject);
            var updatedProjectObject = ProjectHelper.getProjectHelper().updateProjectDeletedContacts(payloadMap, projectObject,srcClientId);

            Map<String,Object> deleteQueuePayload = new HashMap<>();
            deleteQueuePayload.put(PROJECT_PAYLOAD_FIELDS.ACTION.value(),"delete");
            deleteQueuePayload.put(PROJECT_PAYLOAD_FIELDS.PROJECTJDO.value(), updatedProjectObject);
            deleteQueuePayload.put("loggedInUser", loggedInContact);
            deleteQueuePayload.put("dissociationList",ObjUtils.isNullOrEmpty(payloadMap.get(PROJECT_PAYLOAD_FIELDS.DELETED_CONTACTS.value()).toString()) ?
                                                      new ArrayList<>() :
                                                      List.of(payloadMap.get(PROJECT_PAYLOAD_FIELDS.DELETED_CONTACTS.value()).toString()
                                                      .split(",")));
            deleteQueuePayload.put("srcClientId", srcClientId);
            deleteQueuePayload.put(PROJECT_PAYLOAD_FIELDS.CURRENTTIME.value(), DateUtil.getCurrentTime());

            ProjectTaskHelper.getProjectTaskHelper().initiateProjectOperationsTaskQueue(deleteQueuePayload);

            responseMap.put(PROJECT_PAYLOAD_FIELDS.PROJECT.value(), projectObject);
        }

        return responseMap;
    }

    public Map<String, Object> updateProject(String accountID, String projectID, String srcClientId, String requestPayload) throws IOException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(projectID), PROJECT_ERROR_RESPONSE.INVALID_PROJECT_ID.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(requestPayload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());

        Map<String,Object>responseMap = new HashMap<>();
        Map<String, Object> payloadMap  = JsonUtil.convertJsonToMap(requestPayload);

        ProjectJDO projectObject = ProjectImpl.getProjectImplInstance().getProject(projectID);
        List<ProjectPeopleJDO> projectPeopleList = ProjectPeopleImpl.getProjectPeopleImplInstance().getProjectAssociationByProjectID(projectObject.getUniquepin(), projectObject.getProjectId());
        projectObject = ProjectHelper.getProjectHelper().setContactDetailsInsideProjectObject(projectObject, projectPeopleList);

        var responseObject = ProjectHelper.getProjectHelper().updateProjectAndItsAssociatedDetails(accountID, projectObject, payloadMap, srcClientId);

        Map<String,Object> updateQueuePayload = new HashMap<>();
        updateQueuePayload.put(PROJECT_PAYLOAD_FIELDS.ACTION.value(),"update");
        updateQueuePayload.put(PROJECT_PAYLOAD_FIELDS.PROJECTJDO.value(), responseObject);

        if(payloadMap.containsKey(PROJECT_PAYLOAD_FIELDS.ADDED_CONTACTS.value())){
            List<String>associationList = List.of(payloadMap.get(PROJECT_PAYLOAD_FIELDS.ADDED_CONTACTS.value()).toString().split(","));
            updateQueuePayload.put("associationList", associationList);
        }

        if(payloadMap.containsKey(PROJECT_PAYLOAD_FIELDS.DELETED_CONTACTS.value())){
            List<String>dissociationList = List.of(payloadMap.get(PROJECT_PAYLOAD_FIELDS.DELETED_CONTACTS.value()).toString().split(","));
            updateQueuePayload.put("dissociationList", dissociationList);
        }

        updateQueuePayload.put("srcClientId", srcClientId);
        updateQueuePayload.put(PROJECT_PAYLOAD_FIELDS.CURRENTTIME.value(), DateUtil.getCurrentTime());

        List<String> intersectionlist;
        if(!projectObject.getProjectName().equalsIgnoreCase(responseObject.getProjectName())){
            intersectionlist = new ArrayList<>(projectObject.getContacts());
            intersectionlist.retainAll(responseObject.getContacts());
            updateQueuePayload.put("intersectionList", intersectionlist);
        }

        ProjectTaskHelper.getProjectTaskHelper().initiateProjectOperationsTaskQueue(updateQueuePayload);
        responseMap.put(PROJECT_PAYLOAD_FIELDS.PROJECT.value(), responseObject);

        return responseMap;
    }

}
