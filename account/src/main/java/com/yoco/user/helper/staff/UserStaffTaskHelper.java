package com.yoco.user.helper.staff;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.*;
import com.yoco.commons.utils.events.ClockUtil;
import com.yoco.constants.CommonConstants;
import com.yoco.contact.helper.hook.DeleteContactHookHelper;
import com.yoco.team.service.TeamService;
import com.yoco.user.service.UserStaffService;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class UserStaffTaskHelper {

    public static final String STAFF_DISABLED = "STAFF_DISABLED";
    public static final String ACCOUNT_DELETED = "DELETE_ACCOUNT";

    public static UserStaffTaskHelper getInstance(){
        return new UserStaffTaskHelper();
    }

    public void staffOperationsTaskHelper(Map<String,Object> taskMap){
        var actionType = taskMap.get(CommonConstants.ACTION).toString();
        log.info(" actionType " + actionType);
        if("staffAdded".equalsIgnoreCase(actionType))
            this.addStaffTaskHandler(taskMap);
        else if("staffImported".equalsIgnoreCase(actionType))
            this.importStaffTaskHandler(taskMap);
        else if("staffEnabled".equalsIgnoreCase(actionType))
            this.enableStaffTaskHelper(taskMap);
    }

    private void addStaffTaskHandler(Map<String,Object> userMap){
        try{
            UserDTO userPRO = (UserDTO) userMap.get(CommonConstants.USER_KEY);
            var adminContact = (Contact) userMap.get("adminContact");
            var userContact = userPRO.getContact();
            Boolean isRequestFromHook = ObjUtils.isNull(adminContact);
            String activityMessage = (String) userMap.get(CommonConstants.ACTIVITY);
            String source = Boolean.TRUE.equals(isRequestFromHook) ?  ClientSource.CLIENT_SOURCE_CONSTANTS.HOOK.value()
                                                                    : ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value();

            String accountID = userPRO.getAccountID();
            String contactID = userPRO.getContactID();

            PeopleRelationJDO primaryAdmin = UserPROUtil.getPrimaryAdmin(accountID);
            if(!ObjUtils.isNull(primaryAdmin) && !ObjUtils.isNullOrEmpty(primaryAdmin.getContactId())){
                log.info(" fetched primary admin pro, updating the parent id of new contact in pro ");
                PeopleRelationJDO userPro = UserPROUtil.getUserPro(accountID,contactID);
                userPro.setParentContactId(primaryAdmin.getContactId());
                UserPROUtil.updatePRO(userPro);
                userPRO.setParentContactID(primaryAdmin.getContactId());
            }

            if(Boolean.FALSE.equals(isRequestFromHook)){
                UserStaffEmailHelper.getInstance().addStaffEmailHandler(userMap,accountID,adminContact,userContact);
            }

            ActivityUtil.saveActivity(accountID,contactID,"STAFF_ADDED", userContact.getEmailID(),activityMessage,ActivityUtil.ACTIVITIES.DUMMY.value());

            UserStaffChannelPublishHelper.getInstance().publishToAdminChannelOnStaffOperations((AccessPolicy) userMap.get(CommonConstants.POLICY),
                    userPRO,"staff_added");

            UserStaffFullMetricHelper.getInstance().userCreationMetric(accountID,source);

        }catch (Exception e) {
            log.info(" exception on add user helper " + e.getMessage());
        }
    }

    private void importStaffTaskHandler(Map<String,Object> taskMap) {
        String accountID = (String) taskMap.get(CommonConstants.ACCOUNT_ID_KEY);
        Contact adminPRO = (Contact) taskMap.get(ContactConstants.CONTACT);

        List<Map<String, Object>> contactList = (List<Map<String, Object>>) taskMap.get(UserStaffService.CONTACT_LIST);
        log.info("contactList size :: " + contactList.size());

        LinkedHashMap<String,Object> failedUsersMap = new LinkedHashMap<>();

        contactList.stream().forEach( user -> {

            Map<String,Object> resp = this.importUserHelper(accountID, adminPRO,user);

            if (!ObjUtils.isNullOrEmpty(resp)){
                if(Boolean.FALSE.equals(resp.get(Commons.SUCCESS))) {
                    failedUsersMap.put((String) user.get(ContactConstants.EMAIL_ID),
                            ObjUtils.isNullOrEmpty((String) resp.get(Commons.ERROR_RESPONSE)) ? "" : resp.get(Commons.ERROR_RESPONSE));
                }
            }else{
                failedUsersMap.put((String)user.get(ContactConstants.EMAIL_ID),"");
            }
        });

        UserStaffEmailHelper.getInstance().importStaffEmailHandler(failedUsersMap,adminPRO,contactList.size());

    }

    private Map<String,Object> importUserHelper(String accountID, Contact adminPRO, Map<String, Object> user){
        try {
            return UserStaffService.getInstance().createUser(accountID, adminPRO, JsonUtil.getJson(user),false);
        } catch (NoSuchAlgorithmException|IOException e) {
           log.info(" failed to create the user... " + user.get(ContactConstants.EMAIL_ID));
           return new HashMap<>();
        }
    }

    private void enableStaffTaskHelper(Map<String,Object> taskMap){
        try{
            UserDTO userPRO = (UserDTO) taskMap.get(CommonConstants.USER_KEY);
            var accessPolicy = (AccessPolicy) taskMap.get(CommonConstants.POLICY);
            String activityMessage = (String) taskMap.get(CommonConstants.ACTIVITY);
            String source = (String) taskMap.get(CommonConstants.SOURCE);

            String accountID = userPRO.getAccountID();
            String contactID = userPRO.getContactID();
            ActivityUtil.saveActivity(accountID,contactID,"STAFF_ENABLED", userPRO.getEmailID(),
                    activityMessage,ActivityUtil.ACTIVITIES.DUMMY.value());

            UserStaffChannelPublishHelper.getInstance().publishToAdminAndUserChannelOnStaffOperations(accessPolicy,userPRO,"staff_enabled");

            UserStaffFullMetricHelper.getInstance().userActivationMetric(accountID,source);

        }catch (Exception e){
            log.info(" exception on enable user task helper: " + e.getMessage());
        }
    }

    public void handleUserDeletion(Map<String, Object> payloadMap) {
        try{
            PeopleRelationJDO adminPro = (PeopleRelationJDO) payloadMap.get("adminPro");
            PeopleRelationJDO userPro = (PeopleRelationJDO) payloadMap.get("userPro");
            String accountID = userPro.getUniquepin();
            String userContactID = userPro.getContactId();
            String source = payloadMap.get("requestSource").toString();
            if("web".equalsIgnoreCase(source)){
                AccessManager.deleteYoCoLevelPolicyForUser(accountID, userContactID);
            }
            SettingsJDO account = AccountImpl.getAccountImplInstance().getById(accountID);
            ClockUtil.stopRunningEntryForDeletedUser(userPro,account,source);
            ProjectUtil.disassociateUserFromAllProjectsInAccount(accountID, userContactID);
            new TeamService().removeUserFromAllTeamsInAnAccount(accountID, userContactID);
            DcmUtil.removeUserFromAccount(accountID, userContactID);
            UserStaffFullMetricHelper.getInstance().updateUserDeletionMetric(accountID, (DeleteContactHookHelper.FULL_SYNC_DELETE.equalsIgnoreCase(source) ? "hook" : source));
            RTMService.publishToAW(accountID,userContactID, STAFF_DISABLED,null,null);
            var activity = userPro.getContactId() + " was deleted by " + adminPro.getContactId();
            ActivityUtil.saveActivity(accountID,userContactID,STAFF_DISABLED,userPro.getEmailID(),activity,ActivityUtil.ACTIVITIES.DUMMY.value());
            UserStaffChannelPublishHelper.getInstance().publishToAdminAndUserChannelOnStaffOperations(null,new UserDTO(userPro,null),STAFF_DISABLED);
        }catch (Exception e){
            log.error(" exception on handleUserDeletion: " + e.getMessage() + " :: " + payloadMap);
        }
    }
}
