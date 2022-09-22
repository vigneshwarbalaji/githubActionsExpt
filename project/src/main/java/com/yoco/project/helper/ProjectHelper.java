package com.yoco.project.helper;

import com.yoco.client.helper.ClientHelper;
import com.yoco.client.service.ClientService;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.*;
import com.yoco.commons.entity.Client;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.entity.ProjectPeopleJDO;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.utils.events.ClockUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.stream.Collectors;

@Slf4j
public class ProjectHelper {

    private ProjectHelper(){}

    public static ProjectHelper getProjectHelper() {
        return new ProjectHelper();
    }

    @NoArgsConstructor
    private enum PROJECT_HELPER_CONSTANTS {

        PROJECTS_KEY("projects"),
        CLIENTS("clients"),
        CURSOR("cursor"),
        CLIENT("clientID"),
        NAME("name"),
        BILLABLE("billable"),
        CURRENCY("currency"),
        PLAN("plan"),
        RATE("rate"),
        DELETED_CONTACTS("deletedContacts"),
        ADDED_CONTACTS("addedContacts"),
        CONTACTS("contacts"),
        SRC_CLIENT_ID("srcClientID"),
        FROM_WHERE("fromWhere"),
        PROJECT_PEOPLE_LIST("projectPeopleList");

        private String value;

        PROJECT_HELPER_CONSTANTS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    public ProjectJDO createProjectPeopleAssociation(ProjectJDO projectJDO,String[] staffIDs){

        List<String> contacts =  new ArrayList<>();

        if( staffIDs.length > 0 ) {

            for( var index = 0 ; index < staffIDs.length ; index++ ) {

                if( !ObjUtils.isNullOrEmpty( staffIDs[index] ) ) {

                    String contactID = staffIDs[index];

                    ProjectPeopleJDO projectPeopleList = ProjectPeopleImpl.getProjectPeopleImplInstance().getProjectsAssociated(projectJDO.getUniquepin(),contactID,projectJDO.getProjectId());

                    if(ObjUtils.isNull(projectPeopleList)) {

                        log.info(" Creating new project association, as there is no association for this user: " + contactID + " with this project: " + projectJDO.getProjectId());

                        this.createProjectPeopleJDO(projectJDO,contactID);

                        contacts.add(contactID);
                    }
                }
            }
        }

        projectJDO.setContacts(contacts);

        return projectJDO;
    }

    public void associateClientToProject(ProjectJDO projectJDO,String clientID){

        if( !ObjUtils.isNullOrEmpty( clientID ) ) {

            var clientObj = ClientImpl.getClientImplInstance().getByID(clientID);

            if(!ObjUtils.isNull(clientObj)){

                log.info(" client exists ");

                Map<String,Object> payloadMap = new HashMap<>();
                payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.PROJECTS.value(),projectJDO.getProjectId());

                boolean isUpdated = ClientHelper.getClientHelper().updateClientProjects(payloadMap,clientObj);

                if(isUpdated){
                    clientObj.setDateModified(DateUtil.getCurrentTime());
                    ClientImpl.getClientImplInstance().saveClient(clientObj);
                }
            }
        }
    }

    public ProjectJDO createProjectJDO(String accountId, String name,boolean billable,String currency,double rate,
                                          String plan,String projectId){

        log.info(" in create project jdo helper ");
        var projectJDO = new ProjectJDO(accountId,name,billable,currency,rate,plan,projectId);

        projectJDO = ProjectImpl.getProjectImplInstance().saveProject(projectJDO);

        return projectJDO;
    }

    public List<ProjectPeopleJDO> createProjectPeopleJDO(ProjectJDO projectJDO,String contactID){

        var projectPeopleJDO = new ProjectPeopleJDO(projectJDO.getUniquepin(),projectJDO.getProjectId(),contactID,projectJDO.getProjectName());

        List<ProjectPeopleJDO>projectPeopleList = new ArrayList<>();
        projectPeopleList.add(projectPeopleJDO);
        projectPeopleList = ProjectPeopleImpl.getProjectPeopleImplInstance().saveProjectAssociations(projectPeopleList);

        return projectPeopleList;
    }

    public List<ProjectJDO> setProjectAssociatedContactToTheList(String accountID, List<ProjectJDO> listOfProjects){

        List<ProjectJDO> projectResponseList = new ArrayList<>();

        for(ProjectJDO project : listOfProjects){

            List<ProjectPeopleJDO>projectPeopleList = ProjectPeopleImpl.getProjectPeopleImplInstance().getProjectAssociationByProjectID(accountID, project.getProjectId());
            ProjectJDO projectObject = setContactDetailsInsideProjectObject(project , projectPeopleList);
            projectResponseList.add(projectObject);

        }

        return projectResponseList;

    }

    public ProjectJDO setContactDetailsInsideProjectObject(ProjectJDO project, List<ProjectPeopleJDO>projectPeopleList){

        List<String> associatedContactsOfTheProject = new ArrayList<>();

        if(!projectPeopleList.isEmpty()){
            associatedContactsOfTheProject = projectPeopleList.stream().map(ProjectPeopleJDO::getPeopleId).collect(Collectors.toList());
        }

        project.setContacts(associatedContactsOfTheProject);

        return project;
    }

    public Map<String, Object> getAllProjectsAndItsDetailsAssociatedToTheUser(String accountID, String contactID, String cursor, int limit){

        CollectionResponse<ProjectPeopleJDO>projectPeopleQueryResponse = ProjectPeopleImpl.getProjectPeopleImplInstance().getProjectsAssociatedToUser(accountID, contactID, cursor, limit);
        List<ProjectPeopleJDO>projectPeopleList = (List<ProjectPeopleJDO>) projectPeopleQueryResponse.getItems();
        Map<String, Object>responseMap = new HashMap<>();

        Set<String> associatedProjectIds = projectPeopleList.stream().map(ProjectPeopleJDO::getProjectId).collect(Collectors.toSet());
        List<ProjectJDO>projectDetailsList = ProjectImpl.getProjectImplInstance().get(ProjectJDO.class,associatedProjectIds);

        if(!ObjUtils.isNullOrEmpty(projectPeopleQueryResponse.getCursor()))
            responseMap.put(PROJECT_HELPER_CONSTANTS.CURSOR.value(), projectPeopleQueryResponse.getCursor());

        responseMap.put(PROJECT_HELPER_CONSTANTS.PROJECTS_KEY.value(), projectDetailsList);
        responseMap.put(PROJECT_HELPER_CONSTANTS.PROJECT_PEOPLE_LIST.value(), projectPeopleList);

        return responseMap;

    }

    public ProjectJDO updateProjectAndItsAssociatedDetails(String accountID, ProjectJDO projectObject, Map<String ,Object> payloadMap, String srcClientID){

        boolean isProjectNameUpdated = this.updateProjectName(payloadMap,projectObject);
        boolean isProjectBillableDetailsUpdated = this.updateProjectBillableDetails(payloadMap, projectObject);
        boolean isProjectCurrencyUpdated = this.updateProjectCurrency(payloadMap, projectObject);
        boolean isProjectRateUpdated = this.updateProjectRate(payloadMap,projectObject);
        boolean isProjectPlanUpdated = this.updateProjectPlan(payloadMap,projectObject);

        if(isProjectNameUpdated || isProjectBillableDetailsUpdated || isProjectCurrencyUpdated || isProjectRateUpdated || isProjectPlanUpdated){
            projectObject = ProjectImpl.getProjectImplInstance().saveProject(projectObject);
        }

        if(isProjectBillableDetailsUpdated){
            ProjectFullMetricHelper.projectBillableUpdateMetric((Boolean) payloadMap.get(PROJECT_HELPER_CONSTANTS.BILLABLE.value()), accountID);
        }

        var updatedObjectOfDeletedContacts = this.updateProjectDeletedContacts(payloadMap, projectObject, srcClientID);

        if(!ObjUtils.isNull(updatedObjectOfDeletedContacts)){
            projectObject.setContacts(updatedObjectOfDeletedContacts.getContacts());
        }

        var updatedObjectOfAddedContacts = this.updateProjectAddedContacts(payloadMap, projectObject);

        if(!ObjUtils.isNull(updatedObjectOfAddedContacts)){
            projectObject.setContacts(updatedObjectOfAddedContacts.getContacts());
        }

        this.updateProjectClient(payloadMap,projectObject);

        return projectObject;

    }

    public Boolean updateProjectName(Map<String ,Object> payloadMap,  ProjectJDO projectObject){
        String projectName = payloadMap.containsKey(PROJECT_HELPER_CONSTANTS.NAME.value()) ? Validator.sanitizeText(payloadMap.get(PROJECT_HELPER_CONSTANTS.NAME.value()).toString()) : "";

        if(!ObjUtils.isNullOrEmpty(projectName)){
            projectObject.setProjectName(projectName);
            return true;
        }
        return false;
    }

    public Boolean updateProjectBillableDetails(Map<String ,Object> payloadMap,  ProjectJDO projectObject){
        if(payloadMap.containsKey(PROJECT_HELPER_CONSTANTS.BILLABLE.value())){
            projectObject.setBillable((Boolean) payloadMap.get(PROJECT_HELPER_CONSTANTS.BILLABLE.value()));
            return true;
        }
        return false;
    }

    public Boolean updateProjectCurrency(Map<String ,Object> payloadMap,  ProjectJDO projectObject){
        if(payloadMap.containsKey(PROJECT_HELPER_CONSTANTS.CURRENCY.value())){
            String currency = Validator.sanitizeText(payloadMap.get(PROJECT_HELPER_CONSTANTS.CURRENCY.value()).toString());
            projectObject.setCurrency(currency);

            return true;
        }
        return false;
    }

    public Boolean updateProjectRate(Map<String ,Object> payloadMap,  ProjectJDO projectObject){
        if(payloadMap.containsKey(PROJECT_HELPER_CONSTANTS.RATE.value())){
            projectObject.setRate(Double.parseDouble((String)payloadMap.get(PROJECT_HELPER_CONSTANTS.RATE.value())));

            return true;
        }
        return false;
    }

    public Boolean updateProjectPlan(Map<String ,Object> payloadMap,  ProjectJDO projectObject){
        if(payloadMap.containsKey(PROJECT_HELPER_CONSTANTS.PLAN.value())){
            String plan = Validator.sanitizeText(payloadMap.get(PROJECT_HELPER_CONSTANTS.PLAN.value()).toString());
            projectObject.setPlan(plan);

            return true;
        }
        return false;
    }

    public ProjectJDO updateProjectDeletedContacts(Map<String ,Object> payloadMap,  ProjectJDO projectObject, String srcClientID){

        ProjectJDO updatedProjectObject = null;

        if(payloadMap.containsKey(PROJECT_HELPER_CONSTANTS.DELETED_CONTACTS.value())){
            var deletedContactPayload = payloadMap.get(PROJECT_HELPER_CONSTANTS.DELETED_CONTACTS.value()).toString();
            String [] deletedContacts = deletedContactPayload.split(",");
            updatedProjectObject = projectObject;
            List<String>projectContactList = projectObject.getContacts();
            List<ProjectPeopleJDO>projectPeopleList = new ArrayList<>();

            for(String deletedContactID : deletedContacts){
                var projectPeopleObject = ProjectPeopleImpl.getProjectPeopleImplInstance().getProjectsAssociated(projectObject.getUniquepin(),deletedContactID,projectObject.getProjectId());
                if(!ObjUtils.isNull(projectPeopleObject)){
                    projectContactList.remove(deletedContactID);
                    projectPeopleList.add(projectPeopleObject);
                    this.clockOutHandlerForProjectDeletedContacts(projectObject.getUniquepin(),deletedContactID, projectObject.getProjectId(),srcClientID);
                }
            }

            if(!projectPeopleList.isEmpty()){
                ProjectPeopleImpl.getProjectPeopleImplInstance().deleteProjectAssociations(projectPeopleList);
            }

            updatedProjectObject.setContacts(projectContactList);
        }

        return updatedProjectObject;
    }

    public ProjectJDO updateProjectAddedContacts(Map<String ,Object> payloadMap,  ProjectJDO projectObject){

        ProjectJDO updatedProjectObject = null;

        if(payloadMap.containsKey(PROJECT_HELPER_CONSTANTS.ADDED_CONTACTS.value())){
            var addedContactPayload = payloadMap.get(PROJECT_HELPER_CONSTANTS.ADDED_CONTACTS.value()).toString();
            String [] addedContacts = addedContactPayload.split(",");
            updatedProjectObject = projectObject;
            List<String>projectContactList = projectObject.getContacts();
            List<ProjectPeopleJDO>projectPeopleList = new ArrayList<>();

            for(String addedContactID : addedContacts){
                var projectPeopleObject = new ProjectPeopleJDO(projectObject.getUniquepin(),projectObject.getProjectId(),addedContactID,projectObject.getProjectName());
                projectContactList.add(addedContactID);
                projectPeopleList.add(projectPeopleObject);
            }

            ProjectPeopleImpl.getProjectPeopleImplInstance().saveProjectAssociations(projectPeopleList);

            updatedProjectObject.setContacts(projectContactList);
        }

        return updatedProjectObject;
    }

    public Boolean updateProjectClient(Map<String ,Object> payloadMap,  ProjectJDO projectObject){
        if(payloadMap.containsKey(PROJECT_HELPER_CONSTANTS.CLIENT.value())){
            String clientID = ObjUtils.isNullOrEmpty(payloadMap.get(PROJECT_HELPER_CONSTANTS.CLIENT.value()).toString())? "":
                    payloadMap.get(PROJECT_HELPER_CONSTANTS.CLIENT.value()).toString();

            Map<String, Object>currentClientResponseMap = ClientImpl.getClientImplInstance().getAllClients(projectObject.getUniquepin(), projectObject.getProjectId(), "");

            if(!ObjUtils.isNullOrEmpty(currentClientResponseMap)){
                List<Client>currentClient = (List<Client>) currentClientResponseMap.get(PROJECT_HELPER_CONSTANTS.CLIENTS.value());

                if(!ObjUtils.isNullOrEmpty(currentClient))
                    associateClientToProject(projectObject, currentClient.get(0).getId());
            }


            associateClientToProject(projectObject, clientID);

            return true;
        }
        return false;
    }

    public void clockOutHandlerForProjectDeletedContacts(String accountID, String contactID, String projectID,String srcClientID){
        try {
            List<Map<String, Object>> activeEntriesList = ReportImpl.getReportImplInstance().getActiveEntry(accountID, contactID);
            if (!ObjUtils.isNullOrEmpty(activeEntriesList)) {
                var payloadMap = ObjUtils.isNullOrEmpty(activeEntriesList.get(0)) ? new HashMap<String, Object>() :
                        activeEntriesList.get(0);
                if(!ObjUtils.isNullOrEmpty(payloadMap) && projectID.equals(payloadMap.get(SchedulingKeys.CALENDAR))) {
                    var userObject = UserImpl.getUserImplInstance().getUserWithoutContact(accountID, contactID);
                    if (!ObjUtils.isNull(userObject)) {
                        var contact = ContactImpl.getContactImplInstance().getByID(contactID);
                        userObject.setContact(contact);
                    }
                    var accountObject = AccountImpl.getAccountImplInstance().getById(accountID);
                    Map<String, Object> headersMap = new HashMap<>();
                    headersMap.put(PROJECT_HELPER_CONSTANTS.SRC_CLIENT_ID.value(), srcClientID);
                    headersMap.put(PROJECT_HELPER_CONSTANTS.FROM_WHERE.value(), "");
                    ClockUtil.clockOutHandler(accountObject, userObject, payloadMap, headersMap);
                }
            }
        }catch (Exception exception){
            log.info("exception in clockOutHandlerForProjectDeletedContacts:"+ exception.getMessage());
        }

    }

}
