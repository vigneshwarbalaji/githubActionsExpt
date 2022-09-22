package com.yoco.user.helper;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.CommonConstants;
import com.yoco.user.enums.USER_ERROR_MESSAGE;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

@Slf4j
public class UserProProfileHelper {

    public static UserProProfileHelper getInstance(){
        return new UserProProfileHelper();
    }

    public static final String ROLE = "role";

    public void validateRole(String role){
        boolean isAdminRole = role.equalsIgnoreCase(Skillset.ROLE_ADMIN);
        boolean isStaffRole = role.equalsIgnoreCase(Skillset.ROLE_STAFF);
        Validator.checkArgument( !(isAdminRole || isStaffRole), USER_ERROR_MESSAGE.INVALID_ROLE.value());
    }

    public Map<String,Object> generateUpdateProProfileTaskQueueMap(UserDTO userDTO,Map<String,Object> updatedProMap){
        Map<String,Object> queueMap = new HashMap<>();
        queueMap.put(CommonConstants.USER_KEY,userDTO);
        queueMap.put(CommonConstants.ACTION,"updateProProfile");
        queueMap.put("updatedProMap",updatedProMap);
        return queueMap;
    }

    public UserDTO updateProfilePRO(Map<String,Object> payloadMap, PeopleRelationJDO userPRO) throws IOException {
        Map<String,Object> updatedProMap = new HashMap<>();
        this.updateRole(payloadMap,userPRO,updatedProMap);
        this.updateEmployeeId(payloadMap,userPRO,updatedProMap);
        this.updateRfId(payloadMap,userPRO,updatedProMap);

        userPRO = (PeopleRelationJDO) updatedProMap.get(CommonConstants.USER_KEY);

        AccessPolicy accessPolicy = null;
        UserDTO userDTO = null;
        boolean isRoleUpdated = Boolean.TRUE.equals(updatedProMap.get("isRoleUpdated"));

        if(isRoleUpdated || !ObjUtils.isNullOrEmpty((String) updatedProMap.get(CommonConstants.ACTIVITY))){

            if(isRoleUpdated){
                log.info(" role is updated ");
                accessPolicy = (AccessPolicy) updatedProMap.get(CommonConstants.POLICY);
            }

            userPRO = UserPROUtil.updatePRO(userPRO);
            log.info(" updatePRO " + userPRO);

            Set<String> permissions = accessPolicy == null ? null : accessPolicy.getPermissions();
            userDTO = new UserDTO(userPRO,permissions);

            UserTaskInitiator.initiateProUpdateOperationsTaskQueue(this.generateUpdateProProfileTaskQueueMap(userDTO,updatedProMap));
        }

        return userDTO;
    }

    private Map<String,Object> updateRole(Map<String,Object> payloadMap,PeopleRelationJDO userPRO,Map<String,Object> updatedProMap){

        String role = (String) payloadMap.get(ROLE);
        var isRoleUpdated = false;

        if (!ObjUtils.isNullOrEmpty(role)) {
            UserProProfileHelper.getInstance().validateRole(role);
            AccessPolicy accessPolicy = null;

            if(role.equalsIgnoreCase(Skillset.ROLE_ADMIN)){
                accessPolicy = AccessManager.addPermission(userPRO.getUniquepin(),userPRO.getContactId(), IAMPermission.ADMIN.toString());
            }else{
                accessPolicy = AccessManager.removePermission(userPRO.getUniquepin(),userPRO.getContactId(),IAMPermission.ADMIN.toString());
            }

            if( !(ObjUtils.isNull(accessPolicy) || ObjUtils.isNullOrEmpty(accessPolicy.getPermissions()))){
                updatedProMap.put(CommonConstants.POLICY,accessPolicy);
                isRoleUpdated = true;
                userPRO.setRole(role);
            }

            updatedProMap.put(ROLE,role);
        }

        updatedProMap.put(CommonConstants.USER_KEY,userPRO);
        updatedProMap.put("isRoleUpdated",isRoleUpdated);
        return updatedProMap;
    }

    private Map<String,Object> updateEmployeeId(Map<String,Object> payloadMap, PeopleRelationJDO userPRO,Map<String,Object> updatedProMap){
        String employeeID = (String) payloadMap.get("employeeID");
        var activity = "";
        if (!ObjUtils.isNullOrEmpty(employeeID)) {
            log.info(" proceeding to update emp ID ");
            activity = userPRO.getContactId() + ": EmployeeID has been changed from " + userPRO.getLogin() + " to " + employeeID;
            userPRO.setLogin(employeeID);
        }
        updatedProMap.put(CommonConstants.USER_KEY,userPRO);
        updatedProMap.put(CommonConstants.ACTIVITY,activity);
        return updatedProMap;
    }

    private Map<String,Object> updateRfId(Map<String,Object> payloadMap, PeopleRelationJDO userPRO,Map<String,Object> updatedProMap){
        String nfcID = (String) payloadMap.get("nfcID");
        String activity = (String) updatedProMap.get(CommonConstants.ACTIVITY);
        if (!ObjUtils.isNullOrEmpty(nfcID)) {
            activity += ObjUtils.isNullOrEmpty(activity) ? userPRO.getContactId() + ": NfcRFID has been changed from " + userPRO.getRfId() + " to " + nfcID
                    : "; NfcRFID has been changed from " + userPRO.getRfId() + " to " + nfcID;
            userPRO.setRfId(nfcID);
            log.info(" proceeding to update nfcID  & activity " + activity);
        }

        updatedProMap.put(CommonConstants.USER_KEY,userPRO);
        updatedProMap.put(CommonConstants.ACTIVITY,activity);
        return updatedProMap;
    }

}