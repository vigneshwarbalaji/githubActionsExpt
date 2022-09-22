package com.yoco.team.helper;

import com.yoco.commons.modal.dcm.TeamDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.DcmTeamUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.team.service.TeamService;
import lombok.extern.slf4j.Slf4j;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

@Slf4j
public class TeamHelper {
    private static final int DCM_API_CONTACT_LIMIT = 5;
    public static final String OPERATION_ADD = "add";
    public static final String OPERATION_DELETE = "delete";
    private TeamHelper(){}
    public static void validateCreateTeamPayload(Map<String,Object> payloadMap, String accountID){
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payloadMap),"Field name is mandatory.");
        Validator.checkArgument(ObjUtils.isNullOrEmpty((String)payloadMap.get(TeamService.NAME_KEY)),"Invalid name.");
        if(ObjUtils.isNullOrEmpty((String)payloadMap.get(TeamService.OWNERID_KEY))){
            payloadMap.put("ownerID", UserPROUtil.getPrimaryAdmin(accountID).getContactId());
        }
        payloadMap.put("type","team");
    }

    public static List<String> extractAndUpdateContactsToAdd(Map<String, Object> payloadMap) {
        List<String> contacts = (List<String>) payloadMap.get(TeamService.CONTACT_KEY);
        List<String> contactsToAddLater = List.of();
        if(!ObjUtils.isNullOrEmpty(contacts) && contacts.size() > DCM_API_CONTACT_LIMIT){
            contactsToAddLater = new ArrayList<>(contacts.subList(DCM_API_CONTACT_LIMIT,contacts.size()));
            contacts = new ArrayList<>(contacts.subList(0,DCM_API_CONTACT_LIMIT));
            payloadMap.put(TeamService.CONTACT_KEY,contacts);
        }
        return contactsToAddLater;
    }

    public static void validateUpdateTeamInfoPayload(Map<String, Object> payloadMap) {
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payloadMap),"Payload cannot be empty");
        String newName = (String)payloadMap.get(TeamService.NAME_KEY);
        String ownerID = (String)payloadMap.get(TeamService.OWNERID_KEY);
        payloadMap.clear();
        if(!ObjUtils.isNullOrEmpty(newName)){
            payloadMap.put(TeamService.NAME_KEY,newName.trim());
        }
        if(!ObjUtils.isNullOrEmpty(ownerID)){
            payloadMap.put(TeamService.OWNERID_KEY,ownerID.trim());
        }
        Validator.checkArgument(payloadMap.isEmpty(),"name or ownerID mandatory");
    }

    public static void validateUpdateTeamContactsPayload(Map<String, Object> payloadMap) {
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payloadMap),"Payload cannot be empty");
        String operationType = (String)payloadMap.get("type");
        List<String> contacts = (List<String>) payloadMap.get("contact");
        Validator.checkArgument(!isOperationTypeValid(operationType),"Invalid type. Valid types include add, delete.");
        Validator.checkArgument(ObjUtils.isNullOrEmpty(contacts),"Empty contacts.");
    }

    public static boolean isOperationTypeValid(String operation){
        return OPERATION_ADD.equalsIgnoreCase(operation) || OPERATION_DELETE.equalsIgnoreCase(operation);
    }

    public static void batchDeleteContactsFromTeam(TeamDTO team, List<String> contactsToDelete) {
        List<List<String>> contactsPayloadList = splitContactListByDcmApiLimit(contactsToDelete);
        Map<String,Object> payload = new HashMap<>();
        payload.put("type",TeamHelper.OPERATION_DELETE);
        contactsPayloadList.forEach(contactList -> batchUpdateContactsToTeam(contactList,payload,team));
        removeTeamSkilletFromMembersPro(team.getMembers(),team.getAccountID(),team.getId());
        RTMService.publishToChannel(team.getAccountID(),"teamInfoUpdate","teamMap",team);
    }

    public static void batchUpdateContactsToTeam(List<String> contactList, Map<String,Object> payload, TeamDTO team){
        payload.put("contact",contactList);
        try {
            team.setMembers(getTeamMembersFromDcmResponse(DcmTeamUtil.updateTeamContacts(team.getAccountID(),team.getId(),payload)));
        } catch (Exception e) {
            log.info("Contacts delete failed :: " + team + "::" + contactList);
        }
    }

    public static void batchAddContactsToTeam(TeamDTO team, List<String> contactsToAdd){
        List<List<String>> contactsPayloadList = splitContactListByDcmApiLimit(contactsToAdd);
        Map<String,Object> payload = new HashMap<>();
        payload.put("type",TeamHelper.OPERATION_ADD);
        contactsPayloadList.forEach(contactList -> batchUpdateContactsToTeam(contactList,payload,team));
        RTMService.publishToChannel(team.getAccountID(),"teamInfoUpdate","teamMap",team);
    }

    public static void removeTeamSkilletFromMembersPro(TeamDTO team){
        removeTeamSkilletFromMembersPro(team.getMembers(), team.getAccountID(), team.getId());
    }

    public static void removeTeamSkilletFromMembersPro(List<String> contacts,String accountID, String teamID){
        contacts.forEach(contactID -> UserPROUtil.removeTeamIDFromProSkillset(accountID,contactID,teamID));
    }

    public static List<List<String>> splitContactListByDcmApiLimit(List<String> contactsList){
        return new ArrayList<>( IntStream.range(0, contactsList.size()).boxed()
                .collect(Collectors.groupingBy(partition -> (partition / DCM_API_CONTACT_LIMIT), Collectors.mapping(contactsList::get, Collectors.toList())))
                .values());
    }

    public static List<String> getTeamMembersFromDcmResponse(Map<String,Object> dcmResponse){
        Map<String,Object> groupMap = (Map<String, Object>) dcmResponse.get("group");
        return (List<String>) groupMap.get("linkedContacts");
    }
}
