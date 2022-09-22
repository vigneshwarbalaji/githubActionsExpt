package com.yoco.project.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.services.FCMService;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.project.enums.PROJECT_ACTIVITY;
import com.yoco.project.enums.PROJECT_CHANNEL_KEYS;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Slf4j
public class ProjectTaskHelper {

    private ProjectTaskHelper(){}

    public static ProjectTaskHelper getProjectTaskHelper() {
        return new ProjectTaskHelper();
    }

    @NoArgsConstructor
    private enum PROJECT_TASK_KEYS {

        PROJECT_OPERATIONS_QUEUE("project-operation");

        private String value;

        PROJECT_TASK_KEYS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    public static String getProjectTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/project/projectOperationsTaskHandler";
    }

    public void initiateProjectOperationsTaskQueue(Map<String,Object> projectObj) throws IOException {

        log.info(" project task queue ");

        var byteOut = new ByteArrayOutputStream();
        var out = new ObjectOutputStream(byteOut);
        out.writeObject(projectObj);

        TaskCreator.createPostTask(PROJECT_TASK_KEYS.PROJECT_OPERATIONS_QUEUE.value(),ProjectTaskHelper.getProjectTaskCallBackUrl(),byteOut.toByteArray());

    }

    public void projectOperationsTaskHandler(Map<String,Object> payloadMap){

        log.info(" project operations task handler ");

        var projectJDO = (ProjectJDO) payloadMap.get("projectJDO");

        String action = (String) payloadMap.get("action");
        log.info(" action " + action);

        Contact currentUser = (Contact) payloadMap.get("currentUser");
        var requestedLongTime = (long) payloadMap.get("currentTime");

        if("create".equalsIgnoreCase(action)){
            String clientKey = (String) payloadMap.get("sourceClientId");
            this.createProjectTaskHandler(projectJDO,currentUser,clientKey,requestedLongTime);
        }else if("enable".equalsIgnoreCase(action)){
            this.enableProjectTaskHandler(projectJDO,currentUser,requestedLongTime);
        }else if("update".equalsIgnoreCase(action)){
            String clientKey = (String) payloadMap.get("srcClientId");
            this.updateProjectTaskHandler((List<String>)payloadMap.get("associationList"), (List<String>)payloadMap.get("dissociationList"), (List<String>) payloadMap.get("intersectionList"), projectJDO, clientKey);
        }else if("delete".equalsIgnoreCase(action)){
            String clientKey = (String) payloadMap.get("srcClientId");
            Contact loggedInUser = (Contact) payloadMap.get("loggedInUser");
            this.deleteProjectTaskHandler((List<String>)payloadMap.get("dissociationList"), projectJDO, clientKey, loggedInUser, requestedLongTime);
        }
    }

    public void createProjectTaskHandler(ProjectJDO projectJDO,Contact currentUser,String clientKey,long requestedLongTime){

        try {
            log.info(" in create project task handler");
            String accountID = projectJDO.getUniquepin();
            String key = PROJECT_CHANNEL_KEYS.PROJECT_KEY.value();
            String currentUserEmailID = currentUser.getEmailID();

            RTMService.publishToChannel(accountID,PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),key,projectJDO);

            var activity = new StringBuilder();
            activity.append(currentUserEmailID + " has created a Project with name : " + projectJDO.getProjectName());

            List<String> staffIDs = projectJDO.getContacts();
            boolean isStaffEmpty = staffIDs.isEmpty();

            if(!isStaffEmpty){

                for (String contactID : staffIDs) {

                    activity.append(" and added ");

                    var contact = ContactImpl.getContactImplInstance().getByID(contactID);

                    if (!ObjUtils.isNull(contact)){
                        activity.append(contact.getEmailID() + " ");
                    }

                    RTMService.publishToAW(accountID,contactID,PROJECT_CHANNEL_KEYS.PROJECT_ASSOCIATION.value(),key,projectJDO);
                }

                Set<String> contactIDSet = new HashSet<>(staffIDs);
                List<String> projectIDList = new ArrayList<>();
                projectIDList.add(projectJDO.getProjectId());

                FCMService.getFCMService().notifyFCM(accountID,contactIDSet,FCMService.FCM_SERVICE_CONSTANTS.FCM_PROJECT.value(),true,
                        projectIDList,clientKey);
            }

            ActivityUtil.saveActivity(accountID,currentUser.getId(),PROJECT_ACTIVITY.OPERATIONS.value(),currentUserEmailID,activity.toString(),
                    ActivityUtil.ACTIVITIES.DUMMY.value(),requestedLongTime);

            ProjectFullMetricHelper.projectCreationMetric(accountID,projectJDO.getBillable());

        } catch (IOException | NoSuchAlgorithmException e) {
           log.info(" exception on create project task handler " + e.getMessage());
        }
    }

    public void enableProjectTaskHandler(ProjectJDO projectJDO,Contact currentUser,long requestedLongTime){

        log.info(" in enable project task handler");
        String accountId = projectJDO.getUniquepin();
        String currentUserContactId = currentUser.getId();
        String currentUserEmailID = currentUser.getEmailID();
        String activity = projectJDO.getProjectId() + " enabled by :: " +  currentUser.getId();

        ActivityUtil.saveActivity(accountId,currentUserContactId,PROJECT_ACTIVITY.ENABLED.value(),currentUserEmailID,activity,
                ActivityUtil.ACTIVITIES.DUMMY.value(),requestedLongTime);

        RTMService.publishToChannel(accountId,PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectJDO);

        ProjectFullMetricHelper.projectActivationMetric(accountId);
    }

    public void updateProjectTaskHandler(List<String>associationList, List<String>disassociationList, List<String>intersectionList, ProjectJDO projectJDO, String srcClientID){

        try {
            log.info("inside updateProjectTaskHandler");

            List<String>associatedList = ObjUtils.isNullOrEmpty(associationList) ? new ArrayList<>() : associationList;
            List<String>disassociatedList = ObjUtils.isNullOrEmpty(disassociationList) ? new ArrayList<>() : disassociationList;
            List<String>intersectedList = ObjUtils.isNullOrEmpty(intersectionList) ? new ArrayList<>() : intersectionList;

            String accountID = projectJDO.getUniquepin();
            String projectID = projectJDO.getProjectId();
            String key = PROJECT_CHANNEL_KEYS.PROJECT_KEY.value();

            RTMService.publishToChannel(accountID, PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(), key, projectJDO);

            log.info("published to Channel");

            publishingAssociatedContactsToAW(accountID, associatedList, key, projectJDO);
            publishingDisAssociatedContactsToAW(accountID, disassociatedList, key, projectJDO);
            publishingIntersectionListToAW(accountID, intersectedList, key, projectJDO);

            log.info("published to AW");

            List<String> unionContactIdList = Stream.of(associatedList, disassociatedList, intersectedList)
                    .flatMap(Collection::stream)
                    .collect(Collectors.toList());

            Set<String> contactIDSet = new HashSet<>(unionContactIdList);
            Set<String> projectIDSet = new HashSet<>();
            projectIDSet.add(projectID);

            FCMService.getFCMService().notifyFCM(accountID, contactIDSet, FCMService.FCM_SERVICE_CONSTANTS.FCM_PROJECT.value(), true, projectIDSet, srcClientID);

            log.info("notified to FCM / There is no device token for user");

        } catch (IOException | NoSuchAlgorithmException e) {
            log.info(" exception on update project task handler " + e.getMessage());
        }

    }

    public void publishingAssociatedContactsToAW(String accountID, List<String>associationList, String key, ProjectJDO projectObject) throws IOException, NoSuchAlgorithmException {

        String action = PROJECT_CHANNEL_KEYS.PROJECT_ASSOCIATION.value();

        for(String associatedContactID:associationList){
            RTMService.publishToAW(accountID, associatedContactID, action, key, projectObject);
        }

    }

    public void publishingDisAssociatedContactsToAW(String accountID, List<String>disAssociationList, String key, ProjectJDO projectObject) throws IOException, NoSuchAlgorithmException {

        String action = PROJECT_CHANNEL_KEYS.PROJECT_DISASSOCIATION.value();

        for(String disAssociatedContactID:disAssociationList){
            RTMService.publishToAW(accountID, disAssociatedContactID, action, key, projectObject);
        }

    }

    public void publishingIntersectionListToAW(String accountID, List<String>intersectionList, String key, ProjectJDO projectObject) throws IOException, NoSuchAlgorithmException {

        String action = PROJECT_CHANNEL_KEYS.PROJECT_NAME_UPDATE.value();

        for(String intersectedContactID:intersectionList){
            RTMService.publishToAW(accountID, intersectedContactID, action, key, projectObject);
        }

    }

    public void deleteProjectTaskHandler(List<String>disassociationList, ProjectJDO projectObject, String srcClientID, Contact adminDetails,long requestedLongTime){

        try{

            String accountID = projectObject.getUniquepin();
            String currentUserEmailID = adminDetails.getEmailID();
            String projectID = projectObject.getProjectId();
            String key = PROJECT_CHANNEL_KEYS.PROJECT_KEY.value();

            var activity = new StringBuilder();
            activity.append(currentUserEmailID + " has deleted a Project with name : " + projectObject.getProjectName());

            ActivityUtil.saveActivity(accountID,adminDetails.getId(),PROJECT_ACTIVITY.OPERATIONS.value(),currentUserEmailID,activity.toString(),
                    ActivityUtil.ACTIVITIES.DUMMY.value(),requestedLongTime);

            publishingDisAssociatedContactsToAW(accountID, disassociationList, key, projectObject);

            RTMService.publishToChannel(accountID, PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(), key, projectObject);

            Set<String> contactIDSet = new HashSet<>(disassociationList);
            Set<String> projectIDSet = new HashSet<>();
            projectIDSet.add(projectID);

            FCMService.getFCMService().notifyFCM(accountID, contactIDSet, FCMService.FCM_SERVICE_CONSTANTS.FCM_PROJECT.value(), true, projectIDSet, srcClientID);

            ProjectFullMetricHelper.projectDeletionMetric(accountID);

        } catch (IOException | NoSuchAlgorithmException e) {
            log.info(" exception on delete project task handler " + e.getMessage());
        }

    }
}
