package com.yoco.team.helper;

import com.yoco.commons.modal.dcm.TeamDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.ObjUtils;
import java.util.List;
import java.util.Map;

public class TeamTaskHandler {

    public static final String TEAM_INFO_UPDATE_KEY = "teamInfoUpdate";

    private TeamTaskHandler(){}
    public static final String RTM_TEAM_PAYLOAD_KEY = "teamMap";
    public static void handleTeamDeletion(TeamDTO team) {
        TeamHelper.removeTeamSkilletFromMembersPro(team);
        RTMService.publishToChannel(team.getAccountID(),"teamDeleteOperation", RTM_TEAM_PAYLOAD_KEY,team);
    }

    public static void handleTeamCreation(Map<String,Object> payload) {
        TeamDTO team = (TeamDTO) payload.get("team");
        RTMService.publishToChannel(team.getAccountID(), TEAM_INFO_UPDATE_KEY, RTM_TEAM_PAYLOAD_KEY, team);
        List<String> contactsToAdd = (List<String>) payload.get("contactsToAdd");
        if(!ObjUtils.isNullOrEmpty(contactsToAdd)){
            TeamHelper.batchAddContactsToTeam(team,contactsToAdd);
        }
    }

    public static void handleTeamInfoUpdate(TeamDTO team) {
        RTMService.publishToChannel(team.getAccountID(), TEAM_INFO_UPDATE_KEY, RTM_TEAM_PAYLOAD_KEY, team);
    }

    public static void handleTeamContactsUpdate(Map<String,Object> payload, String operation) {
        TeamDTO team = (TeamDTO) payload.get("team");
        RTMService.publishToChannel(team.getAccountID(), TEAM_INFO_UPDATE_KEY, RTM_TEAM_PAYLOAD_KEY, team);
        List<String> contactsToUpdate = (List<String>) payload.get("contactsToUpdate");
        boolean isOperationDelete = TeamHelper.OPERATION_DELETE.equalsIgnoreCase(operation);
        if(isOperationDelete){
            TeamHelper.removeTeamSkilletFromMembersPro((List<String>) payload.get("updatedContacts"), team.getAccountID(), team.getId());
        }
        if(!ObjUtils.isNullOrEmpty(contactsToUpdate)){
            if(isOperationDelete){
                TeamHelper.batchDeleteContactsFromTeam(team,contactsToUpdate);
            }else{
                TeamHelper.batchAddContactsToTeam(team,contactsToUpdate);
            }
        }
    }
}
