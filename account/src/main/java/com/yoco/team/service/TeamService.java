package com.yoco.team.service;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.dcm.TeamDTO;
import com.yoco.commons.utils.*;
import com.yoco.commons.validations.Validator;
import com.yoco.team.helper.TeamHelper;
import com.yoco.team.helper.TeamTaskInitiator;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class TeamService {
    public static final String CONTACT_KEY = "contact";
    public static final String NAME_KEY = "name";
    public static final String OWNERID_KEY = "ownerID";
    public static final String GROUP_KEY = "group";
    public static final String UNKNOWN_ERROR_OCCURRED = "Unknown error occurred";
    public static final String ERROR_MESSAGE = "error_message";

    public Map<String,Object> getAllTeamsForAccount(String accountID,Integer limit,String cursor, String requesterContactID) throws NoSuchAlgorithmException, IOException {
        Validator.checkArgument(!UserPROUtil.isUserAnAdminInAccount(accountID,requesterContactID),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        return DcmTeamUtil.getTeamsForAnAccount(accountID,Validator.validateAndReturnLimit(limit,200),cursor);
    }

    public Map<String,Object> getTeamsForUserInAnAccount(String accountID,String userContactID,String requesterContactID) throws NoSuchAlgorithmException, IOException {
        Validator.checkArgument(!userContactID.equalsIgnoreCase(requesterContactID) && !UserPROUtil.isUserAnAdminInAccount(accountID,requesterContactID),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        return DcmTeamUtil.getTeamsForUserInAnAccount(accountID,userContactID);
    }

    public Map<String, Object> deleteTeam(String accountID, String teamID, String requesterContactID) throws NoSuchAlgorithmException, IOException {
        Validator.checkArgument(!UserPROUtil.isUserAnAdminInAccount(accountID,requesterContactID),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        var teamDTO = DcmTeamUtil.getTeamInfo(accountID,teamID);
        Validator.checkArgument(teamDTO == null, "Delete failed, Team not found.");
        DcmTeamUtil.deleteTeam(accountID,teamID);
        TeamTaskInitiator.initiateTeamDeletionTask(teamDTO);
        return Map.of("team",teamDTO);
    }

    public Map<String, Object> createTeam(String accountID, String payload, String requesterContactID) throws NoSuchAlgorithmException, IOException {
        Validator.checkArgument(!UserPROUtil.isUserAnAdminInAccount(accountID,requesterContactID),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        TeamHelper.validateCreateTeamPayload(payloadMap,accountID);
        List<String> contactsToAddLater = TeamHelper.extractAndUpdateContactsToAdd(payloadMap);
        Map<String,Object> createTeamResponse = DcmTeamUtil.createTeam(accountID,payloadMap);
        if(Boolean.FALSE.equals(createTeamResponse.get(Commons.SUCCESS))){
            String errorMessage = (String)createTeamResponse.get(ERROR_MESSAGE);
            throw new IllegalStateException(ObjUtils.isNullOrEmpty(errorMessage) ? UNKNOWN_ERROR_OCCURRED : errorMessage);
        }
        var team = TeamDTO.convertToTeamDTO((Map<String, Object>) createTeamResponse.get(GROUP_KEY));
        TeamTaskInitiator.initiateTeamCreationTask(team,contactsToAddLater);
        return Map.of("team",team);
    }

    public Map<String, Object> updateTeamInfo(String accountID, String teamID, String payload, String requesterContactID) throws NoSuchAlgorithmException, IOException {
        Validator.checkArgument(!UserPROUtil.isUserAnAdminInAccount(accountID,requesterContactID),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        TeamHelper.validateUpdateTeamInfoPayload(payloadMap);
        Map<String,Object> updateTeamInfoResponse = DcmTeamUtil.updateTeamInfo(accountID,teamID,payloadMap);
        if(Boolean.FALSE.equals(updateTeamInfoResponse.get(Commons.SUCCESS))){
            String errorMessage = (String)updateTeamInfoResponse.get(ERROR_MESSAGE);
            throw new IllegalStateException(ObjUtils.isNullOrEmpty(errorMessage) ? UNKNOWN_ERROR_OCCURRED : errorMessage);
        }
        var team = TeamDTO.convertToTeamDTO((Map<String, Object>) updateTeamInfoResponse.get(GROUP_KEY));
        TeamTaskInitiator.initiateTeamInfoUpdateTask(team);
        return Map.of("team",team);
    }

    public Map<String, Object> updateTeamContacts(String accountID, String teamID, String payload, String requesterContactID) throws NoSuchAlgorithmException, IOException {
        Validator.checkArgument(!UserPROUtil.isUserAnAdminInAccount(accountID,requesterContactID),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        TeamHelper.validateUpdateTeamContactsPayload(payloadMap);
        List<String> contactsToUpdateLater = TeamHelper.extractAndUpdateContactsToAdd(payloadMap);
        Map<String,Object> updateTeamContactsResponse = DcmTeamUtil.updateTeamContacts(accountID,teamID,payloadMap);
        if(Boolean.FALSE.equals(updateTeamContactsResponse.get(Commons.SUCCESS))){
            String errorMessage = (String)updateTeamContactsResponse.get(ERROR_MESSAGE);
            throw new IllegalStateException(ObjUtils.isNullOrEmpty(errorMessage) ? UNKNOWN_ERROR_OCCURRED : errorMessage);
        }
        var team = TeamDTO.convertToTeamDTO((Map<String, Object>) updateTeamContactsResponse.get(GROUP_KEY));
        TeamTaskInitiator.initiateTeamContactsUpdateTask(team,contactsToUpdateLater,(List<String>)payloadMap.get(CONTACT_KEY),(String)payloadMap.get("type"));
        return Map.of("team",team);
    }

    public void removeUserFromAllTeamsInAnAccount(String accountID, String contactID) throws NoSuchAlgorithmException, IOException {
        Map<String,Object> userTeams = DcmTeamUtil.getTeamsForUserInAnAccount(accountID,contactID);
        Map<String,Object> payload = new HashMap<>();
        payload.put("type","delete");
        ((List<TeamDTO>) userTeams.get(DcmUtil.TEAMS_KEY)).forEach(team -> {
            payload.put(CONTACT_KEY,List.of(contactID));
            try {
                DcmTeamUtil.updateTeamContacts(accountID,team.getId(),payload);
            } catch (Exception e) {
                log.error("Failed to remove user ::" + contactID +" from team ::" + team + " :: " + e.getMessage());
            }
        });
    }
}
