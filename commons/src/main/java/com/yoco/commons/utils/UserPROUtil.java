package com.yoco.commons.utils;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.Skillset;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

@Slf4j
public class UserPROUtil {
    private UserPROUtil(){}
    public static final String DEFAULT_ACCOUNT_KEY = "default";
    public static PeopleRelationJDO validateAndExtractUserPRO(String accountID, String contactID){
        PeopleRelationJDO userPro = getUserPro(accountID, contactID);
        return (isUserProActive(userPro)) ? userPro : null;
    }

    public static PeopleRelationJDO validateAndExtractUserPROWithContact(String accountID, String contactID){
        PeopleRelationJDO userPro = getUserProWithContact(accountID, contactID);
        return (isUserProActive(userPro)) ? userPro : null;
    }

    public static PeopleRelationJDO getUserPro(String accountID, String contactID){
        return UserImpl.getUserImplInstance().getUserWithoutContact(accountID,contactID);
    }

    public static PeopleRelationJDO getUserProWithContact(String accountID, String contactID){
        PeopleRelationJDO userPro =  UserImpl.getUserImplInstance().getUserWithoutContact(accountID,contactID);
        if(!ObjUtils.isNull(userPro)){
            var contact = ContactImpl.getContactImplInstance().getByID(contactID);
            if(!ObjUtils.isNull(contact)){
                userPro.setContact(contact);
            }
        }
        return userPro;
    }

    public static PeopleRelationJDO validateAndExtractDefaultUserPRO (String contactID) throws NoSuchAlgorithmException, IOException {
        PeopleRelationJDO userPro = getDefaultUserPro(contactID);
        return (isUserProActive(userPro)) ? userPro : null;
    }

    public static PeopleRelationJDO getDefaultUserPro(String contactID) throws NoSuchAlgorithmException, IOException {
        PeopleRelationJDO userPro = UserImpl.getUserImplInstance().getDefaultUserPro(contactID);
        if(ObjUtils.isNull(userPro)){
            String defaultAccountIDForUser = DcmUtil.getDefaultAccountIDForUser(contactID);
            userPro = validateAndExtractUserPRO(defaultAccountIDForUser, contactID);
            if(!ObjUtils.isNull(userPro)){
                setUserProAsDefault(userPro);
            }
        }
        return userPro;
    }

    public static boolean isUserProActive(PeopleRelationJDO userPro){
        if(userPro == null){
            return false;
        }
        return !userPro.isDelete();
    }

    public static void setUserProAsDefault(PeopleRelationJDO userPro){
        if(isUserProActive(userPro)){
            userPro.setDefault(true);
            userPro.setDateModified(new Date().getTime());
            UserImpl.getUserImplInstance().save(userPro);
        }
    }

    public static String getEmailIdForUser(String contactID){
        var userContact = ContactImpl.getContactImplInstance().getByID(contactID);
        if(ObjUtils.isNull(userContact) || ObjUtils.isNullOrEmpty(userContact.getEmailID())){
            return "";
        }
        return userContact.getEmailID().trim();
    }

    public static Map<String,Object> extractPROSkills(PeopleRelationJDO userPro){
        return Boolean.TRUE.equals(JsonUtil.isValidJson(userPro.getSkillsets())) ? JsonUtil.convertJsonToMap(userPro.getSkillsets()) : null;
    }

    public static PeopleRelationJDO updatePROSkill(PeopleRelationJDO userPRO,Map<String,Object> skill){
        userPRO.setSkillsets(JsonUtil.getJson(skill));
        return updatePRO(userPRO);
    }

    public static PeopleRelationJDO updatePRO(PeopleRelationJDO userPRO){
        userPRO.setDateModified(DateUtil.getCurrentTime());
        UserImpl.getUserImplInstance().savePro(userPRO);
        return userPRO;
    }

    public static Map<String,Object> extractDataProtectionSkill(PeopleRelationJDO userPro){
        return Boolean.TRUE.equals(JsonUtil.isValidJson(userPro.getDataProtectionSkillset()))
                ? JsonUtil.convertJsonToMap(userPro.getDataProtectionSkillset()) : null;
    }

    public static Set<String> getUserPermissions(String accountID, String contactID){
        var userAccessPolicy = new AccessManager().getPolicy(accountID,contactID);
        Set<String> permissions = new HashSet<>();
        if(!ObjUtils.isNull(userAccessPolicy) && !ObjUtils.isNull(userAccessPolicy.getPermissions())){
            permissions = userAccessPolicy.getPermissions();
        }
        return permissions;
    }

    public static boolean isUserAnAdminInAccount(String accountID, String userContactID){
        PeopleRelationJDO userPro = validateAndExtractUserPRO(accountID, userContactID);
        return isUserAnAdminInAccount(userPro);
    }

    public static boolean isUserAnAdminInAccount(PeopleRelationJDO userPro){
        return (userPro != null) && Skillset.ROLE_ADMIN.equalsIgnoreCase(userPro.getRole());
    }

    public static List<PeopleRelationJDO> getAllUsersInCompany(String accountID, Boolean includeDeletedUsers){
        List<PeopleRelationJDO> staffList = new ArrayList<>();
        var userImpl = UserImpl.getUserImplInstance();
        var isQueryIncomplete = true;
        var cursor = "";
        Map<String,Object> userResponse;
        while(isQueryIncomplete){
            userResponse = userImpl.getAllUsers(accountID,includeDeletedUsers,5000,cursor);
            if(ObjUtils.isNull(userResponse.get(UserImpl.USERS))){
                return staffList;
            }
            staffList.addAll((List<PeopleRelationJDO>)userResponse.get(UserImpl.USERS));
            cursor = (String)userResponse.get(UserImpl.CURSOR);
            if(ObjUtils.isNullOrEmpty(cursor)){
                isQueryIncomplete = false;
            }
        }
        return staffList;
    }

    public static void sortProListBasedOnEmail(List<PeopleRelationJDO> staffList){
        staffList.sort(Comparator.comparing(PeopleRelationJDO::getEmailID));
    }

    private static PeopleRelationJDO getPrimaryAdminFromPRO(String accountId){
        List<PeopleRelationJDO> users = UserImpl.getUserImplInstance().getUsersByRole(accountId,Skillset.ROLE_ADMIN);
        if(!ObjUtils.isNull(users)){
            for (PeopleRelationJDO pro : users) {
                if (pro.getContactId().equalsIgnoreCase(pro.getParentContactId())) {
                    var userContact = ContactImpl.getContactImplInstance().getByID(pro.getContactId());
                    if(!ObjUtils.isNull(userContact)){
                        pro.setContact(userContact);
                    }
                    return pro;
                }
            }
        }
        return null;
    }

    public static PeopleRelationJDO getPrimaryAdmin(String accountId){
        AccessPolicy accessPolicy = null;
        try{
            accessPolicy = AccessManager.getSuperAdmin(accountId);
        }catch (Exception e){
            log.info(" exception in fetching IAM super admin info " + e.getMessage());
        }

        if(accessPolicy != null && !ObjUtils.isNullOrEmpty(accessPolicy.getMemberID())){
            String contactId = accessPolicy.getMemberID();
            return getUserProWithContact(accountId,contactId);
        }else{
            return getPrimaryAdminFromPRO(accountId);
        }
    }

    public static PeopleRelationJDO getUserProByEmailID(String accountID, String emailID){
        PeopleRelationJDO userPro =  UserImpl.getUserImplInstance().getUserByEmailID(accountID,emailID);
        if(!ObjUtils.isNull(userPro)){
            var contact = ContactImpl.getContactImplInstance().getByID(userPro.getContactId());
            if(!ObjUtils.isNull(contact)){
                userPro.setContact(contact);
            }
        }
        return userPro;
    }

    public static void removeTeamIDFromProSkillset(String accountID, String contactID, String teamID){
        PeopleRelationJDO userPro = validateAndExtractUserPRO(accountID,contactID);
        if(userPro != null){
            removeTeamIDFromProSkillset(userPro,teamID);
        }
    }

    public static void removeTeamIDFromProSkillset(PeopleRelationJDO userPro, String teamID){
        Map<String,Object> userSkillsetMap = extractPROSkills(userPro);
        Map<String,Object> teamSkillsetMap = extractTeamSkillSetMap(userSkillsetMap);
        if(userSkillsetMap != null && teamSkillsetMap.containsKey(teamID)){
            teamSkillsetMap.remove(teamID);
            userSkillsetMap.put(Skillset.SKILL_SET_KEY_TEAM,JsonUtil.getJson(teamSkillsetMap));
            updatePROSkill(userPro,userSkillsetMap);
        }
    }

    public static Map<String, Object> extractTeamSkillSetMap(PeopleRelationJDO userPro){
        Map<String,Object> userSkillset = extractPROSkills(userPro);
        return extractTeamSkillSetMap(userSkillset);
    }

    public static Map<String, Object> extractTeamSkillSetMap(Map<String,Object> userSkillset){
        if(userSkillset != null){
            String teamSkillsetJson = (String)userSkillset.get(Skillset.SKILL_SET_KEY_TEAM);
            if(Boolean.TRUE.equals(JsonUtil.isValidJson(teamSkillsetJson))){
                return JsonUtil.convertJsonToMap(teamSkillsetJson);
            }
        }
        return new HashMap<>();
    }

    public static boolean isPrimaryAdmin(PeopleRelationJDO userPro){
        try{
            return userPro.getParentContactId().equals(userPro.getContactId());
        }catch (Exception e){
            return false;
        }

    }

    public static boolean isUserAssociatedToYoco(String emailID) {
        List<PeopleRelationJDO> userProList = UserImpl.getUserImplInstance().getAllUserProsForUserByEmail(emailID,false);
        return !ObjUtils.isNullOrEmpty(userProList);
    }
}