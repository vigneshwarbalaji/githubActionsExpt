package com.yoco.payroll.helper;

import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.UserClockinSubStatusJDO;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.ReportsUtil;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class PayrollTaskHelper {

    private PayrollTaskHelper(){}

    public static final String PAYROLL_ACTIVITY = "payroll_activity";

    public static void handlePayrollApproval(Contact adminContact, List<String> payrollIds, String timeFormat) throws IOException, NoSuchAlgorithmException {

        List<Map<String, Object>> payrollEventsList = PayrollHelper.getInstance().getPayrollEvents(payrollIds);

        if(!ObjUtils.isNullOrEmpty(payrollEventsList)){

            PeopleRelationJDO userPro = ReportsUtil.extractUserProFromEvent(payrollEventsList.get(0));

            String zone = userPro.getTimeZone();
            String adminContactID = adminContact.getId();
            boolean isUserProActive = UserPROUtil.isUserProActive(userPro);

            Long totalDuration = 0l;
            StringBuilder builder = new StringBuilder();
            List<UserClockinSubStatusJDO> activitiesListToPersist = new ArrayList<>();
            PayrollEmailHelper payrollEmailHelper = PayrollEmailHelper.getInstance();

            for (Map<String,Object> event: payrollEventsList){
                activitiesListToPersist.add(buildActivity(adminContactID,userPro,ReportsUtil.getIDFromEvent(event)));
                if(isUserProActive){
                    Map<String,Object> sessionMap = payrollEmailHelper.buildSessionContentForMail(event,timeFormat,zone);
                    builder.append(sessionMap.get("session"));
                    totalDuration += (Long) sessionMap.get("duration");
                }
            }

            ActivityUtil.saveActivities(activitiesListToPersist);

            if(!ObjUtils.isNullOrEmpty(builder.toString())){
                payrollEmailHelper.sendApprovalMail(userPro.getUniquepin(),userPro.getContact(),adminContact,builder.toString(),
                        DateUtil.getTimeFormatAsPerCompany(totalDuration,timeFormat));
            }
        }
    }

    private static UserClockinSubStatusJDO buildActivity(String adminContactID,PeopleRelationJDO userPro,String eventID) {
        StringBuilder activityMessage = new StringBuilder();
        activityMessage.append(
                "Admin : " + adminContactID +
                " user : "  + userPro.getContactId() +
                " payrollStatus : ADMIN-APPROVED " +
                " entryID :" + eventID
        );
        return new UserClockinSubStatusJDO(userPro.getUniquepin(), userPro.getContactId(),
                PAYROLL_ACTIVITY, userPro.getEmailID(), activityMessage.toString(),
                ActivityUtil.ACTIVITIES.DUMMY.value(), DateUtil.getCurrentTime());
    }

}
