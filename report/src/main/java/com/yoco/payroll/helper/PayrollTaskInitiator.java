package com.yoco.payroll.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.constants.EventConstants;
import java.io.IOException;
import java.util.List;
import java.util.Map;

public class PayrollTaskInitiator {

    private PayrollTaskInitiator(){}

    private static final String PAYROLL_OPERATIONS_QUEUE = "payroll-operation";

    private static String getApprovePayrollTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/payroll/approve";
    }

    private static String getAdminUpdateTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/payroll/admin-update/handler";
    }

    public static void initiateApprovePayrollQueue(Contact adminPro, List<String> approvedPayrollIds, String timeFormat) throws IOException {
        Map<String,Object> payloadMap = Map.of("adminContact", adminPro, SchedulingKeys.EVENT_IDS,approvedPayrollIds,"timeFormat",timeFormat);
        TaskCreator.createPostTask(PAYROLL_OPERATIONS_QUEUE,getApprovePayrollTaskCallBackUrl(),
                CloudTaskUtil.convertObjectToByteArray(payloadMap));
    }

    public static void initiateAdminUpdateHandlerQueue(PeopleRelationJDO adminPro, PeopleRelationJDO userPro, AdjustmentDTO adjustment) throws IOException {
        Map<String,Object> payloadMap = Map.of("adminPro", adminPro, "userPro", userPro, EventConstants.ADJUSTMENT,adjustment);
        TaskCreator.createPostTask(PAYROLL_OPERATIONS_QUEUE,getAdminUpdateTaskCallBackUrl(),
                CloudTaskUtil.convertObjectToByteArray(payloadMap));
    }
}
