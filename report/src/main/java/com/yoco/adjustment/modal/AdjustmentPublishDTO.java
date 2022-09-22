package com.yoco.adjustment.modal;

import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.JsonUtil;
import lombok.Data;

@Data
public class AdjustmentPublishDTO {

    private String uniquepin = "";
    private String type = "Adjustment";
    private String email = "";
    private String contactId = "";
    private String userClockInId = "";
    private String status = "";
    private String userClockInJDO = null;
    private String adjustmentJDO = null;
    private String cancelledAdjDate = "";

    public AdjustmentPublishDTO(ReportsDTO reportsDTO,AdjustmentDTO adjustmentDTO){
        this.setUniquepin(reportsDTO.getAccountID());
        this.setEmail(reportsDTO.getEmailID());
        this.setContactId(reportsDTO.getContactID());
        this.setUserClockInId(reportsDTO.getId());
        this.setUserClockInJDO(JsonUtil.getJson(reportsDTO));
        this.setAdjustmentJDO(JsonUtil.getJson(adjustmentDTO));
    }

}
