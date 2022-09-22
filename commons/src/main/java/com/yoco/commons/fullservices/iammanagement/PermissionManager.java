package com.yoco.commons.fullservices.iammanagement;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.constants.AccountConstants;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

@Slf4j
@NoArgsConstructor
public class PermissionManager {

    public static final String PERMISSIONS_KEY = "permissions";

    private AccessManager accessManager = new AccessManager();

    public Set<String> getDefaultPermissions(){
        Set<String> freePlanPermissions = new HashSet<>();
        freePlanPermissions.add(IAMPermission.SUPER_ADMIN.toString());
        freePlanPermissions.add(IAMPermission.CLOCK.toString());
        freePlanPermissions.add(IAMPermission.ACTIVITY.toString());
        freePlanPermissions.add(IAMPermission.FORCE_CLOCK_OUT.toString());
        freePlanPermissions.add(IAMPermission.REPORTS_EDIT_ALL.toString());
        freePlanPermissions.add(IAMPermission.ADJUSTMENTS_EDIT.toString());
        freePlanPermissions.add(IAMPermission.CONFIRM_HOURS.toString());
        return freePlanPermissions;
    }

    public Set<String> getPaidAccountPermissions(){
        Set<String> paidAccountPermissions = this.getDefaultPermissions();
        paidAccountPermissions.add(IAMPermission.REMINDER.toString());
        paidAccountPermissions.add(IAMPermission.WEEKLY_DIGEST.toString());
        return paidAccountPermissions;
    }


    public Set<String> getPermissionsBasedOnAccountPlan(String pricingPlan){
        Set<String> permissions;
        if(AccountConstants.ENTERPRISE_PLAN.equalsIgnoreCase(pricingPlan)){
            permissions = this.getPaidAccountPermissions();
        }else{
            permissions = this.getDefaultPermissions();
        }
        return permissions;
    }

    public Set<String> getUserPermissions(PeopleRelationJDO userPro){

        Set<String> permissions = this.getPolicyPermissions(userPro);

        if(ObjUtils.isNullOrEmpty(permissions)){
            log.info(" failed to fetch policy permissions, so fetching from user PRO");
            permissions = this.getPROPermissions(userPro);
        }

        return permissions;
    }

    public Set<String> getPolicyPermissions(PeopleRelationJDO userPro){

        Set<String> permissions = new HashSet<>();

        var policy= accessManager.getAccessPolicyByMemberInfo(userPro.getUniquepin(),userPro.getContactId(),AccessManager.YOCO_TYPE);

        if(!ObjUtils.isNull(policy) && !ObjUtils.isNull(policy.getPermissions())) {
            permissions = policy.getPermissions();
        }

        log.info(" policyPermissions : " + permissions);

        return permissions;
    }

    public Set<String> getPROPermissions(PeopleRelationJDO userPro){

        Set<String> permissions = this.getPermissionsBasedOnPROSkills(JsonUtil.convertJsonToMap(userPro.getSkillsets()));

        permissions = this.getRole(userPro,permissions);

        log.info(" permissions : " + permissions);

        return permissions;
    }

    public Set<String> getPermissionsBasedOnPROSkills(Map<String,Object> skillsInfo){

        Set<String> permissions = new HashSet<>();

        log.info(" skillsInfo : " + skillsInfo);

        if(!ObjUtils.isNullOrEmpty(skillsInfo)){
            permissions = this.getClockPermission(skillsInfo,permissions);
            permissions = this.getReportPermission(skillsInfo,permissions);
            permissions = this.getAdjustmentPermission(skillsInfo,permissions);
            permissions = this.getAdditionalPermissions(skillsInfo,permissions);
            permissions = this.getPaidPermissions(skillsInfo,permissions);
        }

        log.info(" permissions : " + permissions);

        return permissions;
    }

    public Set<String> getClockPermission(Map<String,Object> skillsInfo,Set<String> permissions){
        if((skillsInfo.containsKey(Skillset.SKILL_SET_KEY_WEB_CLOCK_IN) &&
                (Skillset.SKILLSET_EDIT.equalsIgnoreCase((String) skillsInfo.get(Skillset.SKILL_SET_KEY_WEB_CLOCK_IN))))){
            permissions.add(IAMPermission.CLOCK.toString());
        }
        return permissions;
    }

    public boolean doesUserHaveReportsEditAllPermission(PeopleRelationJDO userPro){
        return doesUserHaveReportsPermission(userPro,IAMPermission.REPORTS_EDIT_ALL);
    }

    public boolean doesUserHaveReportsTeamEditPermission(PeopleRelationJDO userPro){
        return doesUserHaveReportsPermission(userPro,IAMPermission.REPORTS_EDIT);
    }

    public boolean doesUserHaveReportsPermission(PeopleRelationJDO userPro,IAMPermission permission){
        try{
            if(ObjUtils.isNull(userPro) || ObjUtils.isNullOrEmpty(userPro.getSkillsets())){
                return false;
            }
            return getReportPermission(JsonUtil.convertJsonToMap(userPro.getSkillsets()),new HashSet<>())
                    .contains(permission.toString());
        }catch (Exception e){
            return false;
        }
    }

    public Set<String> getReportPermission(Map<String,Object> skillsInfo, Set<String> permissions){

        if (skillsInfo.containsKey(Skillset.SKILL_SET_KEY_REPORTS)
                && !ObjUtils.isNullOrEmpty((String) skillsInfo.get(Skillset.SKILL_SET_KEY_REPORTS))) {

            String reportsAccessType = (String) skillsInfo.get(Skillset.SKILL_SET_KEY_REPORTS);

            if (Skillset.SKILLSET_EDIT.equalsIgnoreCase(reportsAccessType)) {
                permissions.add(IAMPermission.REPORTS_EDIT.toString());
            } else if (Skillset.SKILLSET_EDITALL.equalsIgnoreCase(reportsAccessType)) {
                permissions.add(IAMPermission.REPORTS_EDIT_ALL.toString());
            } else if (Skillset.SKILLSET_VIEWALL.equalsIgnoreCase(reportsAccessType)) {
                permissions.add(IAMPermission.REPORTS_VIEW_ALL.toString());
            } else {
                permissions.add(IAMPermission.REPORTS_VIEW.toString());
            }
        }
        return permissions;
    }

    public Set<String> getAdjustmentPermission(Map<String,Object> skillsInfo,Set<String> permissions){

        if(skillsInfo.containsKey(Skillset.SKILL_SET_KEY_ADJUSTMENTS)
                && !ObjUtils.isNullOrEmpty((String) skillsInfo.get(Skillset.SKILL_SET_KEY_ADJUSTMENTS))){

            if(Skillset.SKILLSET_EDIT.equalsIgnoreCase((String) skillsInfo.get(Skillset.SKILL_SET_KEY_ADJUSTMENTS))){
                permissions.add(IAMPermission.ADJUSTMENTS_EDIT.toString());
            }else{
                permissions.add(IAMPermission.ADJUSTMENTS_VIEW.toString());
            }
        }

        return permissions;
    }

    public Set<String> getAdditionalPermissions(Map<String,Object> skillsInfo,Set<String> permissions){

        if(skillsInfo.containsKey(Skillset.SKILL_SET_KEY_SUB_STATUS) &&
                !ObjUtils.isNullOrEmpty((String) skillsInfo.get(Skillset.SKILL_SET_KEY_SUB_STATUS))){
            permissions.add(IAMPermission.ACTIVITY.toString());
        }

        if(skillsInfo.containsKey(Skillset.SKILL_SET_KEY_PAYROLL) && Boolean.TRUE.equals(JsonUtil.isValidJson((String) skillsInfo.get(Skillset.SKILL_SET_KEY_PAYROLL)))){

            Map<String, Object> payrollSkillMap = JsonUtil.convertJsonToMap((String) skillsInfo.get(Skillset.SKILL_SET_KEY_PAYROLL));

            if(!ObjUtils.isNull(payrollSkillMap.get(Skillset.SKILL_SET_KEY_CAN_CONFIRMHOURS))
                    && Boolean.TRUE.equals(payrollSkillMap.get(Skillset.SKILL_SET_KEY_CAN_CONFIRMHOURS))){
                permissions.add(IAMPermission.CONFIRM_HOURS.toString());
            }
        }

        if(skillsInfo.containsKey(Skillset.SKILL_SET_KEY_FORCE_CLOCK_OUT) &&
                !ObjUtils.isNullOrEmpty((String) skillsInfo.get(Skillset.SKILL_SET_KEY_FORCE_CLOCK_OUT)) ){
            permissions.add(IAMPermission.FORCE_CLOCK_OUT.toString());
        }

        return permissions;
    }

    public Set<String> getPaidPermissions(Map<String,Object> skillsInfo, Set<String> permissions){

        if(skillsInfo.containsKey(Skillset.SKILL_SET_KEY_WEEKLY_MAIL) && (boolean)skillsInfo.get(Skillset.SKILL_SET_KEY_WEEKLY_MAIL) ){
            permissions.add(IAMPermission.WEEKLY_DIGEST.toString());
        }

        if(skillsInfo.containsKey(Skillset.SKILL_SET_KEY_REMINDER) && (boolean)skillsInfo.get(Skillset.SKILL_SET_KEY_REMINDER) ){
            permissions.add(IAMPermission.REMINDER.toString());
        }

        return permissions;
    }

    public Set<String> getRole(PeopleRelationJDO userPro,Set<String> permissions){

        if(Skillset.ROLE_ADMIN.equalsIgnoreCase(userPro.getRole())){
            if(userPro.getContactId().equalsIgnoreCase(userPro.getParentContactId())){
                permissions.add(IAMPermission.SUPER_ADMIN.toString());
            }else{
                permissions.add(IAMPermission.ADMIN.toString());
            }
        }

        return permissions;
    }

    public static Set<String> getMemberPermissions(boolean isPayrollEnabled, String accountID){
        Set<String> memberPermissions = new HashSet<>();
        if(isPayrollEnabled){
            memberPermissions.add(IAMPermission.CONFIRM_HOURS.toString());
        }
        if (!InternalUsage.FULL.equalsIgnoreCase(accountID)) {
            memberPermissions.add(IAMPermission.CLOCK.toString());
            memberPermissions.add(IAMPermission.ACTIVITY.toString());
        }
        return memberPermissions;
    }

    public static boolean checkUserHasAdminAccess(PeopleRelationJDO userPro){
        AccessPolicy policy = AccessManager.getInstance().getPolicy(userPro.getUniquepin(),userPro.getContactId());
        if(!ObjUtils.isNull(policy)){
            return isAdmin(policy);
        }
        return false;
    }
    public static boolean isAdmin(AccessPolicy accessPolicy){
        Set<String> permissions = accessPolicy.getPermissions();
        return permissions.contains(IAMPermission.ADMIN.toString()) || permissions.contains(IAMPermission.SUPER_ADMIN.toString());
    }

    public static boolean checkUserClockAccess(String accountID, String contactID){
        AccessPolicy policy = AccessManager.getInstance().getPolicy(accountID, contactID);
        if(!ObjUtils.isNull(policy)){
            Set<String> permissions = policy.getPermissions();
            return permissions.contains(IAMPermission.CLOCK.toString());
        }
        return false;
    }

    //need to bring dependency on IAM after cache changes are made.
    public static boolean checkUserHasAdjustmentEditAccess(PeopleRelationJDO userPro){
        if(ObjUtils.isNull(userPro) || ObjUtils.isNull(userPro.getSkillsets())){
            return false;
        }
        Map<String,Object> skillsInfo = JsonUtil.convertJsonToMap(userPro.getSkillsets());
        if(ObjUtils.isNullOrEmpty(skillsInfo)){
            return false;
        }
        if(skillsInfo.containsKey(Skillset.SKILL_SET_KEY_ADJUSTMENTS)
                && !ObjUtils.isNullOrEmpty((String) skillsInfo.get(Skillset.SKILL_SET_KEY_ADJUSTMENTS))) {
            return Skillset.SKILLSET_EDIT.equalsIgnoreCase((String) skillsInfo.get(Skillset.SKILL_SET_KEY_ADJUSTMENTS));
        }

        return false;
    }

}
