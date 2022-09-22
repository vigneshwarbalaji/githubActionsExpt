package com.yoco.commons.fullservices.iammanagement;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.constants.AccountConstants;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;

class PermissionManagerTest {

    PermissionManager permissionManager = new PermissionManager();

    @Test
    void getDefaultPermissions_valid_test(){
        Set<String> response = permissionManager.getDefaultPermissions();
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.CLOCK.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ADJUSTMENTS_EDIT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.FORCE_CLOCK_OUT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.SUPER_ADMIN.toString()));
    }

    @Test
    void getPaidAccountPermissions_valid_test(){
        Set<String> response = permissionManager.getPaidAccountPermissions();
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.CLOCK.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ADJUSTMENTS_EDIT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.FORCE_CLOCK_OUT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.SUPER_ADMIN.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REMINDER.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.WEEKLY_DIGEST.toString()));
    }

    @Test
    void getPermissionsBasedOnAccountPlan_enterprise_test(){
        Set<String> response = permissionManager.getPermissionsBasedOnAccountPlan(AccountConstants.ENTERPRISE_PLAN);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.CLOCK.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ADJUSTMENTS_EDIT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.FORCE_CLOCK_OUT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.SUPER_ADMIN.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REMINDER.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.WEEKLY_DIGEST.toString()));
    }

    @Test
    void getPermissionsBasedOnAccountPlan_non_enterprise_test(){
        Set<String> response = permissionManager.getPermissionsBasedOnAccountPlan(AccountConstants.FREE_PLAN);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.CLOCK.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ADJUSTMENTS_EDIT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.FORCE_CLOCK_OUT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.SUPER_ADMIN.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REMINDER.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.WEEKLY_DIGEST.toString()));
    }

    @Test
    void getUserPermissions_null_policy_test(){
        try(MockedConstruction<AccessManager> mdu = Mockito.mockConstruction(AccessManager.class,
                (mock, context) -> {
                    Mockito.when(mock.getAccessPolicyByMemberInfo(any(),any(),any())).thenReturn(null);
                })){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForStaff("accountId",false)));
            userPro.setRole(Skillset.ROLE_STAFF);
            userPro.setUniquepin("accountId");
            userPro.setContactId("contactId");
            Set<String> response = new PermissionManager().getUserPermissions(userPro);
            Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
            Assertions.assertTrue(response.contains(IAMPermission.CLOCK.toString()));
            Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
        }
    }

    @Test
    void getUserPermissions_null_policyPermissions_test(){
        try(MockedConstruction<AccessManager> mdu = Mockito.mockConstruction(AccessManager.class,
                (mock, context) -> {
                    AccessPolicy mockPolicy = new AccessPolicy();
                    mockPolicy.setPermissions(null);
                    Mockito.when(mock.getAccessPolicyByMemberInfo(any(),any(),any())).thenReturn(mockPolicy);
                })){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForStaff("accountId",false)));
            userPro.setRole(Skillset.ROLE_STAFF);
            userPro.setUniquepin("accountId");
            userPro.setContactId("contactId");
            Set<String> response = new PermissionManager().getUserPermissions(userPro);
            Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
            Assertions.assertTrue(response.contains(IAMPermission.CLOCK.toString()));
            Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
        }
    }

    @Test
    void getUserPermissions_valid_policyPermissions_test(){
        try(MockedConstruction<AccessManager> mdu = Mockito.mockConstruction(AccessManager.class,
                (mock, context) -> {
                    AccessPolicy mockPolicy = new AccessPolicy();
                    mockPolicy.setPermissions(new PermissionManager().getDefaultPermissions());
                    Mockito.when(mock.getAccessPolicyByMemberInfo(any(),any(),any())).thenReturn(mockPolicy);
                })){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForStaff("accountId",true)));
            userPro.setRole(Skillset.ROLE_ADMIN);
            userPro.setUniquepin("accountId");
            userPro.setContactId("contactId");
            userPro.setParentContactId("contactId");
            Set<String> response = new PermissionManager().getUserPermissions(userPro);
            Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
            Assertions.assertTrue(response.contains(IAMPermission.CLOCK.toString()));
            Assertions.assertTrue(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
            Assertions.assertTrue(response.contains(IAMPermission.ADJUSTMENTS_EDIT.toString()));
            Assertions.assertTrue(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
            Assertions.assertTrue(response.contains(IAMPermission.FORCE_CLOCK_OUT.toString()));
            Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
            Assertions.assertTrue(response.contains(IAMPermission.SUPER_ADMIN.toString()));
        }
    }

    @Test
    void getPROPermissions_externalAccount_staff_payroll_disabled_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForStaff("accountId",false)));
        userPro.setRole(Skillset.ROLE_STAFF);
        Set<String> response = permissionManager.getPROPermissions(userPro);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.CLOCK.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
    }

    @Test
    void getPROPermissions_externalAccount_staff_payroll_enabled_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForStaff("accountId",true)));
        userPro.setRole(Skillset.ROLE_STAFF);
        Set<String> response = permissionManager.getPROPermissions(userPro);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.CLOCK.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
    }

    @Test
    void getPROPermissions_externalAccount_admin_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForAdmin()));
        userPro.setRole(Skillset.ROLE_ADMIN);
        userPro.setContactId("contact1");
        userPro.setParentContactId("contact2");
        Set<String> response = permissionManager.getPROPermissions(userPro);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.CLOCK.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ADJUSTMENTS_EDIT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.FORCE_CLOCK_OUT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ADMIN.toString()));
    }

    @Test
    void getPROPermissions_externalAccount_superAdmin_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForAdmin()));
        userPro.setRole(Skillset.ROLE_ADMIN);
        userPro.setContactId("contact1");
        userPro.setParentContactId("contact1");
        Set<String> response = permissionManager.getPROPermissions(userPro);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.CLOCK.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ADJUSTMENTS_EDIT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.FORCE_CLOCK_OUT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.SUPER_ADMIN.toString()));
    }

    @Test
    void getPermissionsBasedOnPROSkills_empty_test(){
        Set<String> response = permissionManager.getPermissionsBasedOnPROSkills(new HashMap<>());
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
    }

    @Test
    void getClockPermission_false_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getClockPermission(skillsInfo,permissions);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.CLOCK.toString()));
    }

    @Test
    void getClockPermission_empty_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_WEB_CLOCK_IN,"");
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getClockPermission(skillsInfo,permissions);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.CLOCK.toString()));
    }

    @Test
    void getClockPermission_clock_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_WEB_CLOCK_IN,Skillset.SKILLSET_EDIT);
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getClockPermission(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.CLOCK.toString()));
    }

    @Test
    void getReportPermission_false_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getReportPermission(skillsInfo,permissions);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_EDIT.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_VIEW_ALL.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_VIEW.toString()));
    }

    @Test
    void getReportPermission_empty_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_REPORTS,"");
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getReportPermission(skillsInfo,permissions);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_EDIT.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_VIEW_ALL.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_VIEW.toString()));
    }

    @Test
    void getReportPermission_edit_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_REPORTS,Skillset.SKILLSET_EDIT);
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getReportPermission(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.REPORTS_EDIT.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_VIEW_ALL.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_VIEW.toString()));
    }

    @Test
    void getReportPermission_editAll_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_REPORTS,Skillset.SKILLSET_EDITALL);
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getReportPermission(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_EDIT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_VIEW_ALL.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_VIEW.toString()));
    }

    @Test
    void getReportPermission_viewAll_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_REPORTS,Skillset.SKILLSET_VIEWALL);
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getReportPermission(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_EDIT.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REPORTS_VIEW_ALL.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_VIEW.toString()));
    }

    @Test
    void getReportPermission_view_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_REPORTS,Skillset.SKILLSET_VIEW);
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getReportPermission(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_EDIT.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_EDIT_ALL.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REPORTS_VIEW_ALL.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REPORTS_VIEW.toString()));
    }

    @Test
    void getAdjustmentPermission_false_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getAdjustmentPermission(skillsInfo,permissions);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.ADJUSTMENTS_EDIT.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.ADJUSTMENTS_VIEW.toString()));
    }

    @Test
    void getAdjustmentPermission_empty_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_ADJUSTMENTS,"");
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getAdjustmentPermission(skillsInfo,permissions);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.ADJUSTMENTS_EDIT.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.ADJUSTMENTS_VIEW.toString()));
    }

    @Test
    void getAdjustmentPermission_edit_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_ADJUSTMENTS,Skillset.SKILLSET_EDIT);
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getAdjustmentPermission(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.ADJUSTMENTS_EDIT.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.ADJUSTMENTS_VIEW.toString()));
    }

    @Test
    void getAdjustmentPermission_view_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_ADJUSTMENTS,Skillset.SKILLSET_VIEW);
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getAdjustmentPermission(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.ADJUSTMENTS_EDIT.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ADJUSTMENTS_VIEW.toString()));
    }

    @Test
    void getAdditionalPermissions_false_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getAdditionalPermissions(skillsInfo,permissions);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.ACTIVITY.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.FORCE_CLOCK_OUT.toString()));
    }

    @Test
    void getAdditionalPermissions_activity_empty_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_SUB_STATUS,"");
        skillsInfo.put(Skillset.SKILL_SET_KEY_PAYROLL,"");
        skillsInfo.put(Skillset.SKILL_SET_KEY_FORCE_CLOCK_OUT,"");
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getAdditionalPermissions(skillsInfo,permissions);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.ACTIVITY.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.FORCE_CLOCK_OUT.toString()));
    }

    @Test
    void getAdditionalPermissions_activity_valid_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_SUB_STATUS,"view");
        Map<String,Object> payrollMap = new HashMap<>();
        payrollMap.put(Skillset.SKILL_SET_KEY_CAN_CONFIRMHOURS,null);
        skillsInfo.put(Skillset.SKILL_SET_KEY_PAYROLL, JsonUtil.getJson(payrollMap));
        skillsInfo.put(Skillset.SKILL_SET_KEY_FORCE_CLOCK_OUT,"");
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getAdditionalPermissions(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.FORCE_CLOCK_OUT.toString()));
    }

    @Test
    void getAdditionalPermissions_confirmHours_valid_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_SUB_STATUS,"view");
        Map<String,Object> payrollMap = new HashMap<>();
        payrollMap.put(Skillset.SKILL_SET_KEY_CAN_CONFIRMHOURS,true);
        skillsInfo.put(Skillset.SKILL_SET_KEY_PAYROLL, JsonUtil.getJson(payrollMap));
        skillsInfo.put(Skillset.SKILL_SET_KEY_FORCE_CLOCK_OUT,"");
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getAdditionalPermissions(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.FORCE_CLOCK_OUT.toString()));
    }

    @Test
    void getAdditionalPermissions_forceClockOut_valid_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_SUB_STATUS,Skillset.SKILLSET_VIEW);
        Map<String,Object> payrollMap = new HashMap<>();
        payrollMap.put(Skillset.SKILL_SET_KEY_CAN_CONFIRMHOURS,false);
        skillsInfo.put(Skillset.SKILL_SET_KEY_PAYROLL, JsonUtil.getJson(payrollMap));
        skillsInfo.put(Skillset.SKILL_SET_KEY_FORCE_CLOCK_OUT,Skillset.SKILLSET_EDIT);
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getAdditionalPermissions(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.ACTIVITY.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.CONFIRM_HOURS.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.FORCE_CLOCK_OUT.toString()));
    }

    @Test
    void getPaidPermissions_false_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getPaidPermissions(skillsInfo,permissions);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.WEEKLY_DIGEST.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REMINDER.toString()));
    }

    @Test
    void getPaidPermissions_weeklyDigest_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_WEEKLY_MAIL,true);
        skillsInfo.put(Skillset.SKILL_SET_KEY_REMINDER,false);
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getPaidPermissions(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.WEEKLY_DIGEST.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.REMINDER.toString()));
    }

    @Test
    void getPaidPermissions_reminder_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_WEEKLY_MAIL,false);
        skillsInfo.put(Skillset.SKILL_SET_KEY_REMINDER,true);
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getPaidPermissions(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.WEEKLY_DIGEST.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REMINDER.toString()));
    }

    @Test
    void getPaidPermissions_weeklyDigest_And_reminder_test(){
        Map<String,Object> skillsInfo = new HashMap<>();
        skillsInfo.put(Skillset.SKILL_SET_KEY_WEEKLY_MAIL,true);
        skillsInfo.put(Skillset.SKILL_SET_KEY_REMINDER,true);
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getPaidPermissions(skillsInfo,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertTrue(response.contains(IAMPermission.WEEKLY_DIGEST.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.REMINDER.toString()));
    }

    @Test
    void getRole_staff_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setRole(Skillset.ROLE_STAFF);
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getRole(userPro,permissions);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.SUPER_ADMIN.toString()));
        Assertions.assertFalse(response.contains(IAMPermission.ADMIN.toString()));
    }

    @Test
    void getRole_admin_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setRole(Skillset.ROLE_ADMIN);
        userPro.setContactId("contact1");
        userPro.setParentContactId("contact2");
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getRole(userPro,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.SUPER_ADMIN.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.ADMIN.toString()));
    }

    @Test
    void getRole_super_admin_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setRole(Skillset.ROLE_ADMIN);
        userPro.setContactId("contact1");
        userPro.setParentContactId("contact1");
        Set<String> permissions = new HashSet<>();
        Set<String> response = permissionManager.getRole(userPro,permissions);
        Assertions.assertFalse(ObjUtils.isNullOrEmpty(response));
        Assertions.assertFalse(response.contains(IAMPermission.ADMIN.toString()));
        Assertions.assertTrue(response.contains(IAMPermission.SUPER_ADMIN.toString()));
    }

    @Test
    void getMemberPermissions_payrollDisabled_test(){
        Set<String> response = PermissionManager.getMemberPermissions(false,"accountId");
        Assertions.assertNotNull(response);
        Set<String> permissions = new HashSet<>();
        permissions.add(IAMPermission.CLOCK.toString());
        permissions.add(IAMPermission.ACTIVITY.toString());
        Assertions.assertEquals(permissions,response);
    }

    @Test
    void getMemberPermissions_payrollDisabled_Full_test(){
        Set<String> response = PermissionManager.getMemberPermissions(false, InternalUsage.FULL);
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
    }

    @Test
    void getMemberPermissions_payrollEnabled_test(){
        Set<String> response = PermissionManager.getMemberPermissions(true,"accountId");
        Assertions.assertNotNull(response);
        Set<String> permissions = new HashSet<>();
        permissions.add(IAMPermission.CONFIRM_HOURS.toString());
        permissions.add(IAMPermission.CLOCK.toString());
        permissions.add(IAMPermission.ACTIVITY.toString());
        Assertions.assertEquals(permissions,response);
    }

    @Test
    void getMemberPermissions_payrollEnabled_Full_test(){
        Set<String> response = PermissionManager.getMemberPermissions(true,InternalUsage.FULL);
        Assertions.assertNotNull(response);
        Set<String> permissions = new HashSet<>();
        permissions.add(IAMPermission.CONFIRM_HOURS.toString());
        Assertions.assertEquals(permissions,response);
    }

    @Test
    void checkUserHasAdjustmentEditAccess_nullPro_test(){
        Assertions.assertFalse(PermissionManager.checkUserHasAdjustmentEditAccess(null));
    }

    @Test
    void checkUserHasAdjustmentEditAccess_nullProSkillSet_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(null);
        Assertions.assertFalse(PermissionManager.checkUserHasAdjustmentEditAccess(userPro));
    }

    @Test
    void checkUserHasAdjustmentEditAccess_emptyProSkillSet_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(""));
        Assertions.assertFalse(PermissionManager.checkUserHasAdjustmentEditAccess(userPro));
    }

    @Test
    void checkUserHasAdjustmentEditAccess_no_adjustment_key_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(Map.of("clock",true)));
        Assertions.assertFalse(PermissionManager.checkUserHasAdjustmentEditAccess(userPro));
    }

    @Test
    void checkUserHasAdjustmentEditAccess_empty_adjustment_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(Map.of("adjustments","")));
        Assertions.assertFalse(PermissionManager.checkUserHasAdjustmentEditAccess(userPro));
    }

    @Test
    void checkUserHasAdjustmentEditAccess_view_adjustment_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(Map.of("adjustments","view")));
        Assertions.assertFalse(PermissionManager.checkUserHasAdjustmentEditAccess(userPro));
    }

    @Test
    void checkUserHasAdjustmentEditAccess_edit_adjustment_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(Map.of("adjustments","edit")));
        Assertions.assertTrue(PermissionManager.checkUserHasAdjustmentEditAccess(userPro));
    }

    @Test
    void doesUserHaveReportsPermission_nullUserPro_shouldReturnFalse_test(){
        Assertions.assertFalse(permissionManager.doesUserHaveReportsPermission(null,IAMPermission.REPORTS_VIEW));
    }

    @Test
    void doesUserHaveReportsPermission_nullProSkillsets_shouldReturnFalse_test(){
        Assertions.assertFalse(permissionManager.doesUserHaveReportsPermission(new PeopleRelationJDO(),IAMPermission.REPORTS_VIEW));
    }

    @Test
    void doesUserHaveReportsPermission_nullPermission_shouldThorwExceptionAndReturnFalse_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(Map.of("reports","edit")));
        Assertions.assertFalse(permissionManager.doesUserHaveReportsPermission(userPro,null));
    }

    @Test
    void doesUserHaveReportsPermission_PermissionNotAvailable_shouldReturnFalse_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(Map.of("reports","view")));
        Assertions.assertFalse(permissionManager.doesUserHaveReportsTeamEditPermission(userPro));
    }

    @Test
    void doesUserHaveReportsPermission_TeamEdit_PermissionAvailable_shouldReturnTrue_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(Map.of("reports","edit")));
        Assertions.assertTrue(permissionManager.doesUserHaveReportsTeamEditPermission(userPro));
    }

    @Test
    void doesUserHaveReportsPermission_EditAll_PermissionAvailable_shouldReturnTrue_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(JsonUtil.getJson(Map.of("reports","edit-all")));
        Assertions.assertTrue(permissionManager.doesUserHaveReportsEditAllPermission(userPro));
    }

}
