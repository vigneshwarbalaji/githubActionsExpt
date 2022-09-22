package com.yoco.commons.modal.user;

import com.google.appengine.api.datastore.Text;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.utils.JsonUtil;
import lombok.Data;
import java.util.HashMap;

@Data
public class Skillset{

    public static final String ROLE_STAFF = "staff";
    public static final String ROLE_ADMIN = "admin";
    public static final String ROLE_PRIMARY_ADMIN = "primary_admin";

    public static final String SKILLSET_VIEW = "view";
    public static final String SKILLSET_EDIT = "edit";
    public static final String SKILLSET_EDITALL = "edit-all";
    public static final String SKILLSET_VIEWALL = "view-all";

    public static final String SKILL_SET_KEY_CAN_CONFIRMHOURS = "canConfirmHours";
    public static final String SKILL_SET_KEY_WEB_CLOCK_IN = "webClockIn";
    public static final String SKILL_SET_KEY_REPORTS = "reports";
    public static final String SKILL_SET_KEY_ADJUSTMENTS = "adjustments";
    public static final String SKILL_SET_KEY_SUB_STATUS = "substatus";
    public static final String SKILL_SET_KEY_PAYROLL = "payroll";
    public static final String SKILL_SET_KEY_FORCE_CLOCK_OUT = "forceClockout";
    public static final String SKILL_SET_KEY_WEEKLY_MAIL = "weeklyMail";
    public static final String SKILL_SET_KEY_REMINDER = "reminder";
    public static final String SKILL_SET_KEY_TEAM = "teamSkillset";

    private String webClockIn;

    private String mobileClockIn;

    private String adjustments;

    private String reports;

    private String substatus;

    private String forceClockout;

    private String payroll;


    public Skillset(String skillSetKeyWebClockIN, String skillSetKeyMobileClockIN, String skillSetKeyAdjustment, String skillSetKeyReports, String skillSetKeySubStatus, String skillSetKeyForceClockOut, String mapPayrollSkillSet){
        this.webClockIn = skillSetKeyWebClockIN;
        this.mobileClockIn = skillSetKeyMobileClockIN;
        this.adjustments = skillSetKeyAdjustment;
        this.reports = skillSetKeyReports;
        this.substatus = skillSetKeySubStatus;
        this.forceClockout = skillSetKeyForceClockOut;
        this.payroll = mapPayrollSkillSet;
    }

    public static String getPayrollSkillset(boolean canConfirmHours){
        HashMap<String, Object> payrollSkillset = new HashMap<>();
        payrollSkillset.put(SKILL_SET_KEY_CAN_CONFIRMHOURS, canConfirmHours);
        return JsonUtil.getJson(payrollSkillset);
    }

    public static String getWebClockInSkillset(String accountID){
        return (InternalUsage.FULL.equalsIgnoreCase(accountID)) ? "" : SKILLSET_EDIT;
    }

    public static String getActivitySkillset(String accountID){
        return (InternalUsage.FULL.equalsIgnoreCase(accountID)) ? "" : SKILLSET_VIEW;
    }

    public static Skillset generateSkillsetForAdmin(){
       return new Skillset( SKILLSET_EDIT,
                SKILLSET_EDIT,
                SKILLSET_EDIT,
                SKILLSET_EDITALL,
                SKILLSET_VIEW,
                SKILLSET_EDIT,
                getPayrollSkillset(true));
    }

    public static Skillset generateSkillsetForStaff(String accountID, boolean isCompanyPayrollEnabled){
        return new Skillset( getWebClockInSkillset(accountID),
                "",
                "",
                "",
                getActivitySkillset(accountID),
                "",
                getPayrollSkillset(isCompanyPayrollEnabled));
    }

    public static Text createSkillsetText (String role, String accountID, boolean isCompanyPayrollEnabled){
        Skillset userSkillset;
        if (role.equalsIgnoreCase(ROLE_ADMIN)){
            userSkillset = generateSkillsetForAdmin();
        } else {
            userSkillset = generateSkillsetForStaff(accountID, isCompanyPayrollEnabled);
        }
        return new Text( JsonUtil.getJson(userSkillset));
    }

    public static String generateSkillsetForDeletedStaff(){
        return JsonUtil.getJson(new Skillset( "",
                "",
                "",
                "",
                "",
                "",
                getPayrollSkillset(false)));
    }
}