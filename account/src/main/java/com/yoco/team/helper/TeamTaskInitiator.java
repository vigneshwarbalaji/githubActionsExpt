package com.yoco.team.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.modal.dcm.TeamDTO;
import com.yoco.commons.utils.CloudTaskUtil;
import java.io.IOException;
import java.util.List;
import java.util.Map;

public class TeamTaskInitiator {
    private TeamTaskInitiator(){}
    private static final String PRO_UPDATE_OPERATIONS_QUEUE = "updateUserPro";// need to change later to a new queue

    public static void initiateTeamDeletionTask(TeamDTO teamDTO) throws IOException {
        TaskCreator.createPostTask(PRO_UPDATE_OPERATIONS_QUEUE,getTeamDeletionHandlerUrl(), CloudTaskUtil.convertObjectToByteArray(teamDTO));
    }

    public static String getTeamDeletionHandlerUrl(){
        return CommonAppProperties.getAppUrl() + "/task/team/delete";
    }

    public static void initiateTeamCreationTask(TeamDTO team, List<String> contactsToAddLater) throws IOException {
        Map<String,Object> payloadMap = Map.of("team",team,"contactsToAdd",contactsToAddLater);
        TaskCreator.createPostTask(PRO_UPDATE_OPERATIONS_QUEUE,getTeamCreationHandlerUrl(), CloudTaskUtil.convertObjectToByteArray(payloadMap));
    }

    public static String getTeamCreationHandlerUrl(){
        return CommonAppProperties.getAppUrl() + "/task/team/create";
    }

    public static void initiateTeamInfoUpdateTask(TeamDTO team) throws IOException {
        TaskCreator.createPostTask(PRO_UPDATE_OPERATIONS_QUEUE,getTeamInfoUpdateHandlerUrl(), CloudTaskUtil.convertObjectToByteArray(team));
    }

    public static String getTeamInfoUpdateHandlerUrl(){
        return CommonAppProperties.getAppUrl() + "/task/team/update/info";
    }

    public static void initiateTeamContactsUpdateTask(TeamDTO team, List<String> contactsToUpdateLater, List<String> updatedContacts, String operation) throws IOException {
        Map<String,Object> payloadMap = Map.of("team",team,"contactsToUpdate",contactsToUpdateLater,"updatedContacts",updatedContacts);
        TaskCreator.createPostTask(PRO_UPDATE_OPERATIONS_QUEUE,getTeamContactUpdateHandlerUrl(operation), CloudTaskUtil.convertObjectToByteArray(payloadMap));
    }

    public static String getTeamContactUpdateHandlerUrl(String operation){
        return CommonAppProperties.getAppUrl() + "/task/team/contact/" + operation;
    }
}
