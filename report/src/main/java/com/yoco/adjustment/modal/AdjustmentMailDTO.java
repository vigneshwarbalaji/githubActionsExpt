package com.yoco.adjustment.modal;

import com.yoco.adjustment.util.AdjustmentUtil;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.utils.DateUtil;
import lombok.Data;

@Data
public class AdjustmentMailDTO {

    private String reason = "";
    private String redirectUrl = CommonAppProperties.getYoCoDashboardUrl() + "hours";
    private String inDate = "";
    private String inTime = "";
    private String outTime = "";
    private String duration = "";
    private String domainImage = CommonAppProperties.getYoCoDashboardUrl();
    private String currentYear =  DateUtil.getCurrentYear();
    private String companyName = "";
    private String durationDiff = "";


    public AdjustmentMailDTO(AdjustmentDTO adjustmentDTO, String timezone, String displayTimeFormat, String message){
        Long approvedInTimeMillis = adjustmentDTO.getAdjustedInTime();
        Long approvedOutTimeMillis = adjustmentDTO.getAdjustedOutTime();
        this.setInDate(DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY,approvedInTimeMillis,timezone));
        this.setInTime(DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A,approvedInTimeMillis,timezone));
        this.setOutTime(DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A,approvedOutTimeMillis,timezone));
        this.setDuration(DateUtil.getTimeFormatAsPerCompany(approvedOutTimeMillis - approvedInTimeMillis,displayTimeFormat));
        this.setReason(AdjustmentUtil.extractRefinedAdjustmentMessage(message));
        this.setRedirectUrl(redirectUrl);
    }

    public AdjustmentMailDTO(AdjustmentDTO adjustmentDTO, String timezone, SettingsJDO account, String message){
        Long approvedInTimeMillis = adjustmentDTO.getAdjustedInTime();
        Long approvedOutTimeMillis = adjustmentDTO.getAdjustedOutTime();
        Long approvedDiff = approvedOutTimeMillis - approvedInTimeMillis;
        this.setInDate(DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY,approvedInTimeMillis,timezone));
        this.setInTime(DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A,approvedInTimeMillis,timezone));
        this.setOutTime(DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A,approvedOutTimeMillis,timezone));
        this.setDuration(DateUtil.getTimeFormatAsPerCompany(approvedDiff,account.getDisplayTimeFormat()));
        this.setReason(AdjustmentUtil.extractRefinedAdjustmentMessage(message));
        this.setRedirectUrl(redirectUrl);
        this.setDurationDiff(DateUtil.getDurationDisplayText(adjustmentDTO.getOriginalOutTime()-adjustmentDTO.getOriginalInTime(),approvedDiff));
        this.setCompanyName(account.getDisplayDomainName());
    }


}
