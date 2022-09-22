package com.yoco.commons.fullservices.iammanagement;

import com.fullauth.api.manage.FullAuth;
import com.fullauth.api.manage.enums.StatusType;
import com.fullauth.api.manage.exception.FullAuthApiException;
import com.fullauth.api.manage.iam.*;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.utils.GaeUtils;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.util.*;

@Slf4j
@NoArgsConstructor
public class AccessManager {

    public static AccessManager getInstance(){ return new AccessManager();}

    public static final String ACCOUNT_TYPE = AccessPolicyStaticType.ACCOUNT.toString();
    public static final String YOCO_TYPE = "yoco";
    public static final String ACCESS_POLICY = "accessPolicy";
    private static final String MEMBER_PREFIX = "user:";

    private static String getResource(String accountID){
        return "account/" + accountID;
    }

    private static AccessPolicyApi getAccessPolicyApiInstance(){
        return new FullAuth(GaeUtils.isAppModeLive()).manage(CommonAppProperties.getFullAuthApiKey()).accessPolicy();
    }

    private static AccessPolicy newPolicyInstance(String accountID, String contactID, String type){
        String resource = getResource(accountID);
        return new AccessPolicy(type, resource, AccessPolicyMemberType.USER, contactID);
    }

    private static PolicyID getPolicyIdInstance(String accountID, String contactID, String type){
        String resource = getResource(accountID);
        return new PolicyID(type, resource, AccessPolicyMemberType.USER, contactID);
    }

    public AccessPolicy getAccessPolicyByMemberInfo(String accountID, String contactID, String type){
        try {
            var policyID = getPolicyIdInstance(accountID, contactID, type);
            var policyApi = getAccessPolicyApiInstance();
            log.info(" policyApi: " + policyApi);
            var accessPolicy =  policyApi.getAccessPolicy(policyID);
            log.info(" getAccessPolicyByMemberInfo : accessPolicy : " + accessPolicy);
            return accessPolicy;
        } catch (FullAuthApiException|IOException e) {
           log.info(" exception in fetching access policy " + e.getMessage());
            return null;
        }
    }

    public Map<String,Object> getPolicy(PeopleRelationJDO userPro){

        Map<String,Object> accessPolicyMap = new HashMap<>();

        var policy= this.getPolicy(userPro.getUniquepin(),userPro.getContactId());

        if(!ObjUtils.isNull(policy)){

            if(ObjUtils.isNull(policy.getPermissions())){
                policy.setPermissions(new HashSet<>());
            }
            accessPolicyMap.put(ACCESS_POLICY,policy);
        }

        return accessPolicyMap;
    }

    public AccessPolicy getPolicy(String accountID, String contactId){

        return this.getAccessPolicyByMemberInfo(accountID,contactId,YOCO_TYPE);
    }


    public static AccessPolicy addPermission(String accountID, String contactID, String permission){
        try {
            var policyID = getPolicyIdInstance(accountID, contactID, YOCO_TYPE);
            var policyApi = getAccessPolicyApiInstance();

            PolicyUpdateParams updateParams = PolicyUpdateParams.builder().addPermission(permission).build();
            var policyInfo = policyApi.updateAccessPolicy(policyID, updateParams);

            log.info("ADD-PERMISSION: policyInfo : " + policyInfo);
            return policyInfo;

        } catch (FullAuthApiException|IOException ex) {
            log.warn(" ADD-PERMISSION : failed to update the access policy : " + ex.getMessage());
            return null;
        }
    }

    public static AccessPolicy removePermission(String accountID, String contactID, String permission){
        try {
            var policyID = getPolicyIdInstance(accountID, contactID,YOCO_TYPE);
            var policyApi = getAccessPolicyApiInstance();

            PolicyUpdateParams updateParams = PolicyUpdateParams.builder().removePermission(permission).build();
            var policyInfo = policyApi.updateAccessPolicy(policyID, updateParams);

            log.info(" REMOVE-PERMISSION : policyInfo : " + policyInfo);
            return policyInfo;

        } catch (FullAuthApiException|IOException ex) {
            log.warn(" REMOVE-PERMISSION: failed to update the access policy : " + ex.getMessage());
            return null;
        }
    }

    public boolean hasReminderPermission(PeopleRelationJDO userPRO){
        Set<String> userPermissions = new PermissionManager().getUserPermissions(userPRO);
        return  !userPermissions.isEmpty() && userPermissions.contains(IAMPermission.REMINDER.toString());
    }

    public static AccessPolicy getSuperAdmin(String accountID){
        try {
            AccessPolicyFetchParams fetchParams = AccessPolicyFetchParams.builder().type(YOCO_TYPE)
                    .resource(getResource(accountID))
                    .permission(IAMPermission.SUPER_ADMIN.toString())
                    .status(StatusType.ACTIVE)
                    .build();

            AccessPolicyListApiResponse response = getAccessPolicyApiInstance().getAccessPolicies(fetchParams);
            List<AccessPolicy> policies = response.getAccessPolicies();

            log.info(" policies : " + JsonUtil.getJson(policies));
            return policies.get(0);
        }catch (FullAuthApiException|IOException e) {
            log.warn("  failed to fetch the access policy info of owner : " + e.getMessage());
        }catch (Exception e){
            log.warn(" getSuperAdmin Exception : " + e.getMessage());
        }
        return null;
    }

    public AccessPolicy createAccountLevelPolicyForUser(String accountID, String contactID,IAMDefinedRoleType role ){
        var globalPolicy = getAccessPolicyByMemberInfo(accountID,contactID,ACCOUNT_TYPE);
        if(ObjUtils.isNull(globalPolicy)){
            log.info(" no access policy exists for user , so creating one at global level ");
            AccessPolicy newMemberPolicy = newPolicyInstance(accountID, contactID, ACCOUNT_TYPE);
            newMemberPolicy.setRole(role);
            try {
                return getAccessPolicyApiInstance().createAccessPolicy(newMemberPolicy);
            } catch (FullAuthApiException|IOException e) {
                log.info("exception in creating account level access policy : " + e.getMessage());
            } catch (Exception e){
                log.warn(" createAccountLevelPolicyForUser Exception : " + e.getMessage());
            }
        }
        return globalPolicy;
    }

    private AccessPolicy createNewYoCoPolicy(String accountID,String contactID,boolean isCompanyPayrollEnabled){
        log.info(" There is no policy exists at yoco level, so creating a new one ");
        AccessPolicy policy = newPolicyInstance(accountID,contactID,YOCO_TYPE);
        try {
            policy.setRole(IAMDefinedRoleType.MEMBER);
            policy.setStatus(StatusType.ACTIVE);
            policy.setPermissions(PermissionManager.getMemberPermissions(isCompanyPayrollEnabled,accountID));
            var policyCreated = getAccessPolicyApiInstance().createAccessPolicy(policy);
            log.info(" STAFF : newPolicy is created : " + policyCreated);
            return policyCreated;
        } catch (FullAuthApiException|IOException e) {
            log.info("exception in creating yoco level access policy : " + e.getMessage());
        }catch (Exception e){
            log.warn(" createYoCoLevelPolicyForMember Exception : " + e.getMessage());
        }
        return null;
    }

    private AccessPolicy updateYoCoPolicy(String accountID,String contactID, boolean isPayrollEnabled){
        try {
            var policyID = getPolicyIdInstance(accountID, contactID,YOCO_TYPE);
            PolicyUpdateParams updateParams = PolicyUpdateParams.builder()
                    .status(StatusType.ACTIVE).role(IAMDefinedRoleType.MEMBER)
                    .addPermissions(PermissionManager.getMemberPermissions(isPayrollEnabled,accountID)).build();

            var policyUpdated = getAccessPolicyApiInstance().updateAccessPolicy(policyID,updateParams);
            log.info(" STAFF : policy updated : " + policyUpdated);
            return policyUpdated;
        } catch (FullAuthApiException|IOException e) {
            log.info("exception in updating yoco level access policy : " + e.getMessage());
        }catch (Exception e){
            log.warn(" updateYoCoPolicy Exception : " + e.getMessage());
        }
        return null;
    }

    public AccessPolicy createYoCoLevelPolicyForMember(String accountID,String contactID,boolean isPayrollEnabled){
        var policy = getAccessPolicyByMemberInfo(accountID, contactID, YOCO_TYPE);
        if (ObjUtils.isNull(policy)) {
            return createNewYoCoPolicy(accountID,contactID,isPayrollEnabled);
        }else if(StatusType.INACTIVE.equals(policy.getStatus())){
            log.info(" Policy exists, but inactive , so changing the status...");
            return updateYoCoPolicy(accountID,contactID,isPayrollEnabled);
        }
        return policy;
    }

    public AccessPolicy createNewPolicyForMember(String accountId, String contactId, boolean isPayrollEnabled){
        if(!ObjUtils.isNull(createAccountLevelPolicyForUser(accountId,contactId,IAMDefinedRoleType.MEMBER))){
            return createYoCoLevelPolicyForMember(accountId,contactId,isPayrollEnabled);
        }
        return null;
    }

    public AccessPolicy createNewPolicyForOwner(String accountID, String contactID, String plan){
        try{
            if(!ObjUtils.isNull(createAccountLevelPolicyForUser(accountID,contactID,IAMDefinedRoleType.OWNER))){
                AccessPolicy policy = newPolicyInstance(accountID,contactID,YOCO_TYPE);
                policy.setRole(IAMDefinedRoleType.OWNER);
                policy.setStatus(StatusType.ACTIVE);
                policy.setPermissions(new PermissionManager().getPermissionsBasedOnAccountPlan(plan));
                return getAccessPolicyApiInstance().createAccessPolicy(policy);
            }
        }catch (Exception e) {
            log.warn("Exception occurred while creating access policy for owner: " + e.getMessage());
        }
        return null;
    }

    public static AccessPolicy addEditPermissionToReports(String accountID, String contactID){
        try {
            var policyID = getPolicyIdInstance(accountID, contactID, YOCO_TYPE);
            var policyApi = getAccessPolicyApiInstance();

            PolicyUpdateParams updateParams = PolicyUpdateParams.builder()
                    .addPermission(IAMPermission.REPORTS_EDIT.toString())
                    .removePermission(IAMPermission.REPORTS_VIEW_ALL.toString())
                    .removePermission(IAMPermission.REPORTS_VIEW.toString())
                    .removePermission(IAMPermission.REPORTS_EDIT_ALL.toString())
                    .build();

            var policyInfo = policyApi.updateAccessPolicy(policyID, updateParams);

            log.info("ADD-EDIT-PERMISSION: policyInfo : " + policyInfo);
            return policyInfo;

        } catch (FullAuthApiException|IOException ex) {
            log.warn(" ADD-EDIT-PERMISSION : failed to update the access policy : " + ex.getMessage());
            return null;
        }
    }

    public static AccessPolicy removeReportsPermission(String accountID, String contactID){
        try {
            var policyID = getPolicyIdInstance(accountID, contactID, YOCO_TYPE);
            var policyApi = getAccessPolicyApiInstance();
            List<String> removePermissionsList = new ArrayList<>();
            removePermissionsList.add(IAMPermission.REPORTS_EDIT.toString());
            removePermissionsList.add(IAMPermission.REPORTS_EDIT_ALL.toString());
            removePermissionsList.add(IAMPermission.REPORTS_VIEW.toString());
            removePermissionsList.add(IAMPermission.REPORTS_VIEW_ALL.toString());

            PolicyUpdateParams updateParams = PolicyUpdateParams.builder().removePermissions(removePermissionsList).build();

            var policyInfo = policyApi.updateAccessPolicy(policyID, updateParams);

            log.info("REMOVE-REPORTS-PERMISSION: policyInfo : " + policyInfo);
            return policyInfo;

        } catch (FullAuthApiException|IOException ex) {
            log.warn(" REMOVE-REPORTS-PERMISSION : failed to update the access policy : " + ex.getMessage());
            return null;
        }
    }

    public static AccessPolicy addEditAllPermissionToReports(String accountID, String contactID){
        try {
            var policyID = getPolicyIdInstance(accountID, contactID, YOCO_TYPE);
            var policyApi = getAccessPolicyApiInstance();

            PolicyUpdateParams updateParams = PolicyUpdateParams.builder()
                    .removePermission(IAMPermission.REPORTS_EDIT.toString())
                    .removePermission(IAMPermission.REPORTS_VIEW_ALL.toString())
                    .removePermission(IAMPermission.REPORTS_VIEW.toString())
                    .addPermission(IAMPermission.REPORTS_EDIT_ALL.toString())
                    .build();

            var policyInfo = policyApi.updateAccessPolicy(policyID, updateParams);

            log.info("ADD-EDITALL-PERMISSION: policyInfo : " + policyInfo);
            return policyInfo;

        } catch (FullAuthApiException|IOException ex) {
            log.warn(" ADD-EDITALL-PERMISSION : failed to update the access policy : " + ex.getMessage());
            return null;
        }
    }

    public static AccessPolicy addViewPermissionToReports(String accountID, String contactID){
        try {
            var policyID = getPolicyIdInstance(accountID, contactID, YOCO_TYPE);
            var policyApi = getAccessPolicyApiInstance();

            PolicyUpdateParams updateParams = PolicyUpdateParams.builder()
                    .removePermission(IAMPermission.REPORTS_EDIT.toString())
                    .removePermission(IAMPermission.REPORTS_VIEW_ALL.toString())
                    .addPermission(IAMPermission.REPORTS_VIEW.toString())
                    .removePermission(IAMPermission.REPORTS_EDIT_ALL.toString())
                    .build();

            var policyInfo = policyApi.updateAccessPolicy(policyID, updateParams);

            log.info("ADD-VIEW-PERMISSION: policyInfo : " + policyInfo);
            return policyInfo;

        } catch (FullAuthApiException|IOException ex) {
            log.warn(" ADD-VIEW-PERMISSION : failed to update the access policy : " + ex.getMessage());
            return null;
        }
    }

    public static AccessPolicy addViewAllPermissionToReports(String accountID, String contactID){
        try {
            var policyID = getPolicyIdInstance(accountID, contactID, YOCO_TYPE);
            var policyApi = getAccessPolicyApiInstance();

            PolicyUpdateParams updateParams = PolicyUpdateParams.builder()
                    .removePermission(IAMPermission.REPORTS_EDIT.toString())
                    .addPermission(IAMPermission.REPORTS_VIEW_ALL.toString())
                    .removePermission(IAMPermission.REPORTS_VIEW.toString())
                    .removePermission(IAMPermission.REPORTS_EDIT_ALL.toString())
                    .build();

            var policyInfo = policyApi.updateAccessPolicy(policyID, updateParams);

            log.info("ADD-VIEW-ALL-PERMISSION: policyInfo : " + policyInfo);
            return policyInfo;

        } catch (FullAuthApiException|IOException ex) {
            log.warn(" ADD-VIEW-ALL-PERMISSION : failed to update the access policy : " + ex.getMessage());
            return null;
        }
    }

    public static AccessPolicy addAdjustmentEditPermission(String accountID, String contactID){
        try {
            var policyID = getPolicyIdInstance(accountID, contactID, YOCO_TYPE);
            var policyApi = getAccessPolicyApiInstance();

            PolicyUpdateParams updateParams = PolicyUpdateParams.builder()
                    .removePermission(IAMPermission.ADJUSTMENTS_VIEW.toString())
                    .addPermission(IAMPermission.ADJUSTMENTS_EDIT.toString())
                    .build();

            var policyInfo = policyApi.updateAccessPolicy(policyID, updateParams);

            log.info("ADD-EDIT-PERMISSION: policyInfo : " + policyInfo);
            return policyInfo;

        } catch (FullAuthApiException|IOException ex) {
            log.warn(" ADD-EDIT-PERMISSION : failed to update the access policy : " + ex.getMessage());
            return null;
        }
    }

    public static AccessPolicy addAdjustmentViewPermission(String accountID, String contactID){
        try {
            var policyID = getPolicyIdInstance(accountID, contactID, YOCO_TYPE);
            var policyApi = getAccessPolicyApiInstance();

            PolicyUpdateParams updateParams = PolicyUpdateParams.builder()
                    .addPermission(IAMPermission.ADJUSTMENTS_VIEW.toString())
                    .removePermission(IAMPermission.ADJUSTMENTS_EDIT.toString())
                    .build();

            var policyInfo = policyApi.updateAccessPolicy(policyID, updateParams);

            log.info("ADD-VIEW-PERMISSION: policyInfo : " + policyInfo);
            return policyInfo;

        } catch (FullAuthApiException|IOException ex) {
            log.warn(" ADD-VIEW-PERMISSION : failed to update the access policy : " + ex.getMessage());
            return null;
        }
    }

    public static AccessPolicy removeAdjustmentPermission(String accountID, String contactID){
        try {
            var policyID = getPolicyIdInstance(accountID, contactID, YOCO_TYPE);
            var policyApi = getAccessPolicyApiInstance();

            PolicyUpdateParams updateParams = PolicyUpdateParams.builder()
                    .removePermission(IAMPermission.ADJUSTMENTS_VIEW.toString())
                    .removePermission(IAMPermission.ADJUSTMENTS_EDIT.toString())
                    .build();

            var policyInfo = policyApi.updateAccessPolicy(policyID, updateParams);

            log.info("REMOVE-ADJUSTMENT-PERMISSION: policyInfo : " + policyInfo);
            return policyInfo;

        } catch (FullAuthApiException|IOException ex) {
            log.warn(" REMOVE-ADJUSTMENT-PERMISSION : failed to update the access policy : " + ex.getMessage());
            return null;
        }
    }

    public static boolean deleteYoCoLevelPolicyForUser(String accountID, String contactID){
        try{
            var policyID = getPolicyIdInstance(accountID, contactID, YOCO_TYPE);
            return getAccessPolicyApiInstance().deleteAccessPolicy(policyID);
        }catch (FullAuthApiException|IOException e){
            log.warn("Failed to delete access policy" + e.getMessage());
            return false;
        }
    }

    public static String getPolicyID(String accountID, String contactID){
        return getPolicyIdInstance(accountID, contactID, YOCO_TYPE).getID();
    }

    public static void deleteAccessPolicies(Set<String> accessPolicies) throws FullAuthApiException, IOException {
        getAccessPolicyApiInstance().deleteAccessPolicies(accessPolicies);
    }

    public static List<AccessPolicy> getAllActiveAccessPoliciesOfUser(String contactID) throws FullAuthApiException, IOException {
       return getAccessPolicyApiInstance().getAccessPolicies(
               AccessPolicyFetchParams.builder()
                       .type(YOCO_TYPE)
                       .member(MEMBER_PREFIX + contactID)
                       .status(StatusType.ACTIVE)
                       .build()
       ).getAccessPolicies();
    }
}
