package com.yoco.commons.utils;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.modal.dcm.TeamDTO;
import com.yoco.commons.services.UrlFetcher;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
public class DcmTeamUtil {
    private DcmTeamUtil(){}
    public static final String GROUP_KEY = "group";

    public static Map<String,Object> getTeamsForAnAccount(String accountID, int limit, String cursor) throws NoSuchAlgorithmException, IOException {
        Map<String,Object> teamsForAccountResponse = new HashMap<>();
        Map<String,Object> dcmTeamsResponse = getTeamsForAccountResponseFromDCM(accountID,limit, cursor);
        if(ObjUtils.isNullOrEmpty(dcmTeamsResponse) || Boolean.FALSE.equals(dcmTeamsResponse.get(Commons.SUCCESS))){
            teamsForAccountResponse.put(DcmUtil.TEAMS_KEY,new ArrayList<>());
            teamsForAccountResponse.put(Commons.CURSOR,null);
        }else{
            teamsForAccountResponse.put(DcmUtil.TEAMS_KEY, TeamDTO.convertToTeamDTO((List<Map<String,Object>>)dcmTeamsResponse.get(GROUP_KEY)));
            teamsForAccountResponse.put(Commons.CURSOR,dcmTeamsResponse.get(Commons.CURSOR));
        }
        return teamsForAccountResponse;
    }

    public static Map<String, Object> getTeamsForAccountResponseFromDCM (String accountID, int limit, String cursor) throws NoSuchAlgorithmException, IOException {
        var url = createFetchTeamsForAccountUrl(accountID,limit);
        if(!ObjUtils.isNullOrEmpty(cursor)){
            url += "&cursor=" + cursor;
        }
        return UrlFetcher.sendGetRequest(url,HeaderUtil.getServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static String createFetchTeamsForAccountUrl(String accountID, int limit){
        return DcmConstants.getFetchTeamsForAccountUrl().replace(DcmUtil.ACCOUNT_ID_QPARAM,accountID).replace("{$limit}",String.valueOf(limit));
    }

    public static Map<String,Object> getTeamsForUserInAnAccount(String accountID, String contactID) throws NoSuchAlgorithmException, IOException {
        Map<String,Object> teamsForUserInAnAccountResponse = new HashMap<>();
        Map<String,Object> dcmTeamsResponse = getTeamsForUserInAnAccountResponseFromDCM(accountID,contactID);
        if(ObjUtils.isNullOrEmpty(dcmTeamsResponse) || Boolean.FALSE.equals(dcmTeamsResponse.get(Commons.SUCCESS))){
            teamsForUserInAnAccountResponse.put(DcmUtil.TEAMS_KEY,new ArrayList<>());
        }else{
            Map<String,Object> dcmGroupsResponse = (Map<String,Object>)dcmTeamsResponse.get(GROUP_KEY);
            teamsForUserInAnAccountResponse.put(DcmUtil.TEAMS_KEY, TeamDTO.convertToTeamDTO((List<Map<String, Object>>) dcmGroupsResponse.get(contactID)));
        }
        return teamsForUserInAnAccountResponse;
    }

    public static Map<String, Object> getTeamsForUserInAnAccountResponseFromDCM (String accountID, String contactID) throws NoSuchAlgorithmException, IOException {
        Map<String,Object> payload = new HashMap<>();
        List<String> contactList = new ArrayList<>();
        contactList.add(contactID);
        payload.put("contact",contactList);
        return UrlFetcher.sendPostRequest(createFetchTeamsForUserInAnAccountUrl(accountID),payload,HeaderUtil.getServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static String createFetchTeamsForUserInAnAccountUrl(String accountID){
        return DcmConstants.getFetchTeamsForUserUrl().replace(DcmUtil.ACCOUNT_ID_QPARAM,accountID);
    }

    public static String createFetchTeamInfoUrl(String accountID, String teamID){
        return DcmConstants.getFetchTeamInfoUrl().replace(DcmUtil.ACCOUNT_ID_QPARAM,accountID).replace("{$teamID}",teamID);
    }

    public static TeamDTO getTeamInfo(String accountID, String teamID) throws NoSuchAlgorithmException, IOException {
        Map<String,Object> teamInfoDcmResponse = getTeamInfoResponseFromDcm(accountID,teamID);
        if(ObjUtils.isNullOrEmpty(teamInfoDcmResponse) || Boolean.FALSE.equals(teamInfoDcmResponse.get(Commons.SUCCESS))){
            return null;
        }
        return TeamDTO.convertToTeamDTO((Map<String, Object>) teamInfoDcmResponse.get(GROUP_KEY));
    }

    public static Map<String,Object> getTeamInfoResponseFromDcm(String accountID, String teamID) throws NoSuchAlgorithmException, IOException {
        return UrlFetcher.sendGetRequest(createFetchTeamInfoUrl(accountID, teamID),HeaderUtil.getServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static void deleteTeam(String accountID, String teamID) throws NoSuchAlgorithmException, IOException {
        Map<String,Object> deleteResponse = UrlFetcher.sendDeleteRequest(createFetchTeamInfoUrl(accountID, teamID),HeaderUtil.getServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
        if(ObjUtils.isNullOrEmpty(deleteResponse) || Boolean.FALSE.equals(deleteResponse.get(Commons.SUCCESS))){
            throw new IllegalStateException("Delete team request failed");
        }
    }

    public static Map<String, Object> createTeam(String accountID,Map<String,Object> payload) throws NoSuchAlgorithmException, IOException {
        return UrlFetcher.sendPostRequest(createTeamCreationUrl(accountID),payload,HeaderUtil.getJsonUtf8ContentTypedServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static String createTeamCreationUrl(String accountID){
        return DcmConstants.getCreateTeamUrl().replace(DcmUtil.ACCOUNT_ID_QPARAM,accountID);
    }

    public static Map<String, Object> updateTeamInfo(String accountID, String teamID, Map<String, Object> payloadMap) throws NoSuchAlgorithmException, IOException {
        return UrlFetcher.sendPutRequest(createFetchTeamInfoUrl(accountID,teamID),payloadMap,HeaderUtil.getJsonUtf8ContentTypedServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static Map<String, Object> updateTeamContacts(String accountID, String teamID, Map<String, Object> payloadMap) throws NoSuchAlgorithmException, IOException {
        return UrlFetcher.sendPostRequest(createUpdateTeamContactsUrl(accountID,teamID,(String)payloadMap.get("type")),
                payloadMap,HeaderUtil.getServerTokenAuthHeader(),UrlFetcher.getHttpClientInstance());
    }

    public static String createUpdateTeamContactsUrl(String accountID, String teamID, String operation) {
        return DcmConstants.getUpdateTeamContactsUrl().replace(DcmUtil.ACCOUNT_ID_QPARAM,accountID).replace("{$teamID}",teamID).replace("{$operation}",operation);
    }

    public static void deleteAllTeamsUnderAccount(String accountID) throws NoSuchAlgorithmException, IOException {
        List<TeamDTO> teamsUnderAccount = getAllTeamsUnderAccount(accountID);
        teamsUnderAccount.stream().map(TeamDTO::getId).collect(Collectors.toList()).forEach(teamID ->{
            try {
                deleteTeam(accountID,teamID);
            } catch (Exception e) {
                log.error("error deleting team :: " + teamID +" :: " + e.getMessage());
            }
        });
    }

    public static List<TeamDTO> getAllTeamsUnderAccount(String accountID) throws NoSuchAlgorithmException, IOException {
        if(ObjUtils.isNullOrEmpty(accountID)){
            return new ArrayList<>();
        }
        List<TeamDTO> teams = new ArrayList<>();
        var isQueryIncomplete = true;
        Map<String,Object> teamsResponse;
        var cursor = "";
        while(isQueryIncomplete){
            teamsResponse  = getTeamsForAnAccount(accountID,100,cursor);
            teams.addAll((List<TeamDTO>)teamsResponse.get(DcmUtil.TEAMS_KEY));
            if(!ObjUtils.isNullOrEmpty((String) teamsResponse.get(Commons.CURSOR))){
                cursor = (String) teamsResponse.get(Commons.CURSOR);
            }else{
                isQueryIncomplete = false;
            }
        }
        return teams;
    }

    public static Set<String> getAllTeamMembersOfAnUserInAccount(String accountID, String contactID) throws NoSuchAlgorithmException, IOException {
        Set<String> membersList = new HashSet<>();
        Map<String,Object> dcmTeamsForUser = getTeamsForUserInAnAccount(accountID,contactID);
        List<TeamDTO> teamDTOList = (List<TeamDTO>)dcmTeamsForUser.get(DcmUtil.TEAMS_KEY);
        for(TeamDTO teamDTO : teamDTOList){
            membersList.addAll(new HashSet<>(teamDTO.getMembers()));
        }
        return membersList;
    }
}
