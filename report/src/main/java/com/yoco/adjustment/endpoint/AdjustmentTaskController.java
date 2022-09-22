package com.yoco.adjustment.endpoint;

import com.yoco.adjustment.helper.delete.AdjustmentDeleteTaskHelper;
import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.constants.EventConstants;
import com.yoco.adjustment.helper.AdjustmentTaskHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import java.util.List;
import java.util.Map;

@Slf4j
@ValidateAccessToken
@RestController
@RequestMapping("/task/adjustment")
public class AdjustmentTaskController {
    @PostMapping("/delete-handler")
    public void deleteAdjustmentHandler(@RequestBody byte[] payload){
        try{
            Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
            AdjustmentDeleteTaskHelper.handleAdjustmentDeletion((List<ReportsDTO>) payloadMap.get(EventConstants.ENTRIES),(List<AdjustmentDTO>) payloadMap.get(EventConstants.ADJUSTMENTS),
                    (PeopleRelationJDO)payloadMap.get("loggedInUserPro"),(PeopleRelationJDO)payloadMap.get("entryContactPro"));
        }catch (Exception e){
            log.error("Error processing adjustment deletion queue :: " + e.getMessage());
        }
    }

    @PostMapping("/approve")
    public void handleApproveAdjustment(@RequestBody byte[] payload){
        try{
            Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
            AdjustmentTaskHelper.handleApproveAdjustment(payloadMap);
        }catch (Exception e){
            log.error("Error processing on handleApproveAdjustment :: " + e.getMessage());
        }
    }

    @PostMapping("/edit-approve")
    public void handleEditApproveAdjustment(@RequestBody byte[] payload){
        try{
            Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
            AdjustmentTaskHelper.handleEditApproveAdjustment(payloadMap);
        }catch (Exception e){
            log.error("Error processing on handleEditApproveAdjustment :: " + e.getMessage());
        }
    }

}
