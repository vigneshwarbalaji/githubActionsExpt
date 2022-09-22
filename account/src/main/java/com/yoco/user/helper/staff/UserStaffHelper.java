package com.yoco.user.helper.staff;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.account.enums.ACCOUNT_ERROR_MESSAGE;
import com.yoco.commons.constants.AccountConstants;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.modal.user.ProUserInfo;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.AccountUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.CommonConstants;
import com.yoco.contact.helper.ContactHelper;
import com.yoco.user.enums.USER_ERROR_MESSAGE;
import com.yoco.user.helper.UserTaskInitiator;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Slf4j
public class UserStaffHelper {

    public static UserStaffHelper getInstance(){
        return new UserStaffHelper();
    }

    public void validateUserPayload(Map<String,Object> payloadMap){
        Validator.checkArgument( payloadMap.containsKey(ContactConstants.ID), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD_CONTAINS_ID.value());
        Validator.checkArgument( !payloadMap.containsKey(ContactConstants.EMAIL_ID),COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value());
        Validator.checkArgument( !Validator.isValidEmail(payloadMap.get(ContactConstants.EMAIL_ID).toString()),
                COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.value());
        Validator.checkArgument( !payloadMap.containsKey(ContactConstants.FIRST_NAME) ||
                        ObjUtils.isNullOrEmpty(Validator.sanitizeText(payloadMap.get(ContactConstants.FIRST_NAME).toString())),
                COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value());
    }


    public Map<String,Object> validateAndExtractAccountInfo(String accountID){
        Map<String,Object> resp = new HashMap<>();
        SettingsJDO account = AccountImpl.getAccountImplInstance().getById(accountID);
        if(!ObjUtils.isNull(account)){
            if(this.isAccountEligibleToAddStaff(account)){
                log.info(" account is eligible to add new staff....");
                resp.put(Commons.SUCCESS, Boolean.TRUE);
                resp.put(CommonConstants.ACCOUNT_KEY, account);
            }else{
                resp.put(Commons.SUCCESS, Boolean.FALSE);
                resp.put(Commons.ERROR_RESPONSE, ACCOUNT_ERROR_MESSAGE.MAX_USERS_COUNT_LIMIT.value());
            }
        }else{
            resp.put(Commons.SUCCESS, Boolean.FALSE);
            resp.put(Commons.ERROR_RESPONSE, COMMON_ERROR_RESPONSE.ACCOUNT_NOT_FOUND.value());
        }
        return resp;
    }

    public Map<String,Object> validateAndExtractUserInfo(String accountID,Map<String,Object> payloadMap,Map<String,Object> resp){
        String emailID = payloadMap.get(ContactConstants.EMAIL_ID).toString().toLowerCase();
        PeopleRelationJDO user = UserPROUtil.getUserProByEmailID(accountID,emailID);
        if(ObjUtils.isNull(user)){
            resp.put(Commons.SUCCESS, Boolean.TRUE);
            resp.put(ContactConstants.EMAIL_ID,emailID);
        }else{
            log.info("User is already existing in yoco and might be deleted");
            resp.put(Commons.SUCCESS, Boolean.FALSE);
            resp.put(Commons.ERROR_RESPONSE, USER_ERROR_MESSAGE.EMAIL_ID_EXISTS.value());
            resp.put(CommonConstants.USER_KEY, new UserDTO(user,null));
        }
        return resp;
    }

    public Map<String,Object> validateUserAccountCheck(String accountID,Map<String,Object> payloadMap){
        Map<String,Object> resp = this.validateAndExtractAccountInfo(accountID);
        if(Boolean.TRUE.equals(resp.get(Commons.SUCCESS))){
            resp = this.validateAndExtractUserInfo(accountID,payloadMap,resp);
        }
        return resp;
    }


    public boolean isAccountEligibleToAddStaff(SettingsJDO account){
        String maxUsers = AccountUtil.getMaxUsersCount(account);
        if(AccountConstants.UNLIMITED.equalsIgnoreCase(maxUsers)){
            return true;
        }
        Map<String,Object> activeUsersMap = UserImpl.getUserImplInstance().getAllUsers(account.getPeopleUniquePin(),false,30,"");
        if(!ObjUtils.isNullOrEmpty(activeUsersMap) && activeUsersMap.containsKey(CommonConstants.USERS_KEY)){
            List<PeopleRelationJDO> proList = (List<PeopleRelationJDO>) activeUsersMap.get(CommonConstants.USERS_KEY);
            log.info(" proList size::  " + proList.size());
            return proList.isEmpty() || (proList.size() < Integer.parseInt(maxUsers));
        }
        return true;
    }


    public Map<String,Object> processUserCreation(Map<String,Object> dcmResp,Contact currentUser,SettingsJDO account) {

        if(Boolean.TRUE.equals(dcmResp.get(Commons.SUCCESS))){

            boolean markAsDefault = (boolean) dcmResp.get("markAsDefault");
            var dcmContactDTO = (DcmContactDTO) dcmResp.get("dcmContact");
            boolean isNewUser = (boolean) dcmResp.get("isNewUser");

            PeopleRelationJDO userPro = this.createUser(dcmContactDTO,account,markAsDefault);

            if(ObjUtils.isNull(userPro)) {
                return Map.of();
            }

            return this.createUserEventsHandler(currentUser, account, isNewUser,
                    dcmContactDTO.getId() + " has  been added by " + currentUser.getId(), userPro);
        }else{
            return dcmResp;
        }
    }


    public PeopleRelationJDO createUser(DcmContactDTO dcmContact, SettingsJDO account, boolean markAsDefault){
        try{
            var contactCreated = ContactHelper.getInstance().validateAndCreateContact(dcmContact);

            if(!ObjUtils.isNull(contactCreated)){

                PeopleRelationJDO userObj = createUserPRO(account, markAsDefault, contactCreated,"");

                log.info(" created pro " + JsonUtil.getJson(userObj));

                return userObj;
            }

        }catch (Exception e){
            log.info(" exception in creating user ... " + e.getMessage());
        }
        return null;
    }

    public PeopleRelationJDO createUserPRO(SettingsJDO account, boolean markAsDefault, Contact contactCreated,String parentContactId) {
        var proUserInfo = new ProUserInfo(account.getPeopleUniquePin(), contactCreated.getId(), contactCreated.getEmailID(),
                Skillset.ROLE_STAFF,"","");

        var userObj  =  new PeopleRelationJDO(proUserInfo,parentContactId, account.getTimeZone(),
                account.getTimeZoneDisplayName(), markAsDefault, account.getIsPayrollEnabled());

        userObj =  UserImpl.getUserImplInstance().savePro(userObj);
        userObj.setContact(contactCreated);
        return userObj;
    }

    public Map<String,Object> createUserEventsHandler(Contact currentUser, SettingsJDO account, Boolean isNewUser,
                                               String activityMessage, PeopleRelationJDO userPro){
        try {
            AccessPolicy policy = AccessManager.getInstance().createNewPolicyForMember(account.getPeopleUniquePin(),
                    userPro.getContactId(), account.getIsPayrollEnabled());

            Set<String> permissions = null;
            if(!ObjUtils.isNull(policy) && !ObjUtils.isNullOrEmpty(policy.getPermissions())){
                permissions = policy.getPermissions();
            }

            var userDTO = new UserDTO(userPro,permissions);

            UserTaskInitiator.initiateStaffOperationsTaskQueue(this.generateAddStaffQueuePayload(userDTO,
                    account.getDisplayDomainName(), currentUser, policy, isNewUser, activityMessage));

            Map<String,Object> resp = new HashMap<>();
            resp.put( Commons.SUCCESS, Boolean.TRUE);
            resp.put("accessPolicy", policy);
            resp.put( CommonConstants.USER_KEY, userDTO);

            return resp;

        } catch (IOException e) {
           log.info(" Exception in createUserEventsHandler :: " + e.getMessage());
           return Map.of();
        }

    }

    public Map<String,Object> generateAddStaffQueuePayload(UserDTO userDTO, String accountName, Contact adminContact,
                                                           AccessPolicy accessPolicy, Boolean isNewContact,String activityMessage){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"staffAdded");
        queueMap.put("accountName",accountName);
        queueMap.put("adminContact",adminContact);
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put(CommonConstants.POLICY,accessPolicy);
        queueMap.put("isNewContact",isNewContact);
        queueMap.put(CommonConstants.ACTIVITY,activityMessage);
        return queueMap;
    }

    public void updateAndSaveProForUserDisabling(PeopleRelationJDO userPRO){
        if(!ObjUtils.isNull(userPRO)) {
            updateProForUserDisabling(userPRO);
            UserPROUtil.updatePRO(userPRO);
        }
    }

    public void updateProForUserDisabling(PeopleRelationJDO userPRO){
        userPRO.setDelete(true);
        userPRO.setDefault(false);
        userPRO.setRfId("");
        userPRO.setRole(Skillset.ROLE_STAFF);
        userPRO.setSkillsets(Skillset.generateSkillsetForDeletedStaff());
    }

    public void updateProForAccountDeletion(PeopleRelationJDO userPRO,Long dateDeletedLongTime){
        if(!ObjUtils.isNull(userPRO)){
            updateProForUserDisabling(userPRO);
            userPRO.setDateModified(dateDeletedLongTime);
        }
    }

    public PeopleRelationJDO activateUserSkillHelper(PeopleRelationJDO userPro){

        this.updateUserDataProtectionBetaFlag(userPro);

        Map<String,Object> skillMap = UserPROUtil.extractPROSkills(userPro);
        skillMap.put( Skillset.SKILL_SET_KEY_SUB_STATUS,
                InternalUsage.FULL.equalsIgnoreCase(userPro.getUniquepin()) ? "" : Skillset.SKILLSET_VIEW);

        userPro = UserPROUtil.updatePROSkill(userPro,skillMap);

        return userPro;
    }

    public PeopleRelationJDO updateUserDataProtectionBetaFlag(PeopleRelationJDO userPro) {
        Map<String,Object> dataProtectionSkillMap = UserPROUtil.extractDataProtectionSkill(userPro);
        dataProtectionSkillMap.put("beta",true);
        userPro.setDataProtectionSkillset(JsonUtil.getJson(dataProtectionSkillMap));
        return userPro;
    }

    public PeopleRelationJDO validateAndSetIsDefault(String contactID, PeopleRelationJDO userPro){
        PeopleRelationJDO defaultPro = UserImpl.getUserImplInstance().getDefaultUserPro(contactID);
        var isDefault = true;

        if(!ObjUtils.isNull(defaultPro) && !defaultPro.isDelete())
            isDefault = false;

        userPro.setDefault(isDefault);
        return userPro;
    }

    public UserDTO activateUserEventsHandler(SettingsJDO account, PeopleRelationJDO userPro,String activity,String source){
        try{
            AccessPolicy policy = AccessManager.getInstance().createNewPolicyForMember(userPro.getUniquepin(),
                    userPro.getContactId(), account.getIsPayrollEnabled());

            Set<String> permissions = null;
            if(!ObjUtils.isNull(policy) && !ObjUtils.isNullOrEmpty(policy.getPermissions())){
                permissions = policy.getPermissions();
            }

            var userDTO = new UserDTO(userPro,permissions);

            UserTaskInitiator.initiateStaffOperationsTaskQueue(this.generateActivateStaffQueuePayload(userDTO,policy,activity,source));
            return userDTO;
        }catch (Exception e){
            log.error(" exception in activateUserEventsHandler : " + e.getMessage());
            return null;
        }

    }

    public Map<String,Object> generateActivateStaffQueuePayload(UserDTO userDTO,AccessPolicy accessPolicy,String activity,String source){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.ACTION,"staffEnabled");
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put(CommonConstants.POLICY,accessPolicy);
        queueMap.put(CommonConstants.ACTIVITY,activity);
        queueMap.put(CommonConstants.SOURCE,source);
        return queueMap;
    }
}
