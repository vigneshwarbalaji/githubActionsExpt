package com.yoco.adjustment.helper;

import com.yoco.adjustment.modal.AdjustmentPublishDTO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.DateUtil;
import com.yoco.constants.EventConstants;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;

@Slf4j
public class AdjustmentChannelPublishHelper {

    public static AdjustmentChannelPublishHelper getInstance(){
        return new AdjustmentChannelPublishHelper();
    }

    public void publishApproveAdjustment(ReportsDTO reportsDTO, AdjustmentDTO adjustmentDTO){
        AdjustmentPublishDTO adjustmentPublishDTO = new AdjustmentPublishDTO(reportsDTO,adjustmentDTO);
        adjustmentPublishDTO.setStatus("adj_approve");
        RTMService.publishToChannel(reportsDTO.getAccountID(),adjustmentPublishDTO);
    }

    public void publishEditApproveAdjustment(ReportsDTO reportsDTO, AdjustmentDTO adjustmentDTO,String timezone){
        AdjustmentPublishDTO adjustmentPublishDTO = new AdjustmentPublishDTO(reportsDTO,adjustmentDTO);
        adjustmentPublishDTO.setStatus("adj_approve");
        adjustmentPublishDTO.setCancelledAdjDate(DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY_HH_MM_SS_A, adjustmentDTO.getAdjustedInTime(),timezone));
        RTMService.publishToChannel(reportsDTO.getAccountID(),adjustmentPublishDTO);
    }

    public void publishAdjustmentActionToAW(ReportsDTO reportsDTO, AdjustmentDTO adjustmentDTO) throws NoSuchAlgorithmException, IOException {
        HashMap<String,Object> awPayload = new HashMap<>();
        awPayload.put("accountID",reportsDTO.getAccountID());
        awPayload.put("action", "EDIT_AND_APPROVE_ADJUSTMENT");
        awPayload.put(EventConstants.ENTRY,reportsDTO);
        awPayload.put(EventConstants.ADJUSTMENT,adjustmentDTO);
        RTMService.publishToAW(reportsDTO.getContactID(),awPayload);
    }
}
