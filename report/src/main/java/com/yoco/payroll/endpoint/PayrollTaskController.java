package com.yoco.payroll.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.constants.EventConstants;
import com.yoco.payroll.helper.PayrollTaskHelper;
import com.yoco.payroll.helper.adminupdate.PayrollAdminUpdateTaskHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import java.util.List;
import java.util.Map;

@Slf4j
@ValidateAccessToken
@RequestMapping("/task/payroll")
@RestController
public class PayrollTaskController {

    @PostMapping("/approve")
    public void handlePayrollApproval(@RequestBody byte[] payload){
        try{
            Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
            PayrollTaskHelper.handlePayrollApproval(
                    (Contact) payloadMap.get("adminContact"),
                    (List<String>) payloadMap.get(SchedulingKeys.EVENT_IDS),
                    DateConstants.DEFAULT_TIME_FORMAT);
        }catch (Exception e){
            log.error("Error processing payroll approve queue :: " + e.getMessage());
        }
    }

    @PostMapping("/admin-update/handler")
    public void handleAdminUpdateEntry(@RequestBody byte[] payload){
        try{
            Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
            new PayrollAdminUpdateTaskHelper().handleAdminUpdate(
                    (PeopleRelationJDO) payloadMap.get("adminPro"),
                    (PeopleRelationJDO) payloadMap.get("userPro"),
                    (AdjustmentDTO) payloadMap.get(EventConstants.ADJUSTMENT));
        }catch (Exception e){
            log.error("Error processing admin update queue :: " + e.getMessage());
        }
    }
}
