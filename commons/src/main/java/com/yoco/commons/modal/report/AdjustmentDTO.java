package com.yoco.commons.modal.report;

import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ProjectImpl;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@NoArgsConstructor
@Data
public class AdjustmentDTO implements Serializable {

    private String id;
    private String entryID;
    private String contactID;
    private String accountID;

    private Long adjustedInTime;
    private String adjustedInTimeText = "";

    private Long adjustedOutTime;
    private String adjustedOutTimeText = "";

    private Long originalInTime;
    private String originalInTimeText = "";

    private Long originalOutTime;
    private String originalOutTimeText = "";

    private Boolean isNew;
    private Boolean isDeleted;

    private String projectID = "";
    private String projectName = InternalUsage.NO_PROJECT;
    private String status = "";
    private String payrollStatus = "";

    private Long requestedTime = 0l;
    private String requestedTimeText = "";
    private String requestedBy = "";
    private String requestMessage = "";

    private Long approvedTime = 0l;
    private String approvedTimeText = "";
    private String approvedBy = "";
    private String approvedMessage = "";

    private String rejectMessage = "";

    public AdjustmentDTO(Map<String, Object> adjEvent, Map<String, Object> parentEvent, String timezone, DateFormats dateFormat){

        var isNewEvent = false;

        if(ObjUtils.isNullOrEmpty(parentEvent)) {
            parentEvent = adjEvent;
            isNewEvent = true;
        }

        this.setId(parentEvent.get(SchedulingKeys.ID).toString());

        this.setEntryID(adjEvent.get(SchedulingKeys.ID).toString());
        this.setAccountID(adjEvent.get(SchedulingKeys.MERCHANT).toString());

        List<String> contactIdList = (ArrayList<String>) adjEvent.get(SchedulingKeys.PROVIDER);
        this.setContactID(contactIdList.get(0));

        this.setIsDeleted((Boolean) adjEvent.get(SchedulingKeys.IS_DELETED));
        this.setIsNew(isNewEvent);
        setProjectDetails(adjEvent);
        setTimeDetails(adjEvent,parentEvent,timezone,dateFormat);
        setMetaDetails(adjEvent,timezone,dateFormat);
        setPayrollDetails(adjEvent);
    }

    private void setProjectDetails(Map<String,Object> event){

        if(event.containsKey(SchedulingKeys.CALENDAR)){

            String projectId = (String)event.get(SchedulingKeys.CALENDAR);

            if(!projectId.equalsIgnoreCase((String)event.get(SchedulingKeys.MERCHANT))){

                ProjectJDO objPrj = ProjectImpl.getProjectImplInstance().getProject(projectId);

                if(!ObjUtils.isNull(objPrj) ){
                    this.setProjectID(projectId);
                    this.setProjectName(objPrj.getProjectName());
                }
            }
        }
    }

    private void setTimeDetails(Map<String,Object> adjEvent,Map<String,Object> parentEvent,
                                String timezone ,DateFormats dateFormat ){
        long adjustedInMillis = (long)adjEvent.get(SchedulingKeys.START_TIME);
        long adjustedOutMillis = (long)adjEvent.get(SchedulingKeys.END_TIME);

        this.setAdjustedInTime(adjustedInMillis);
        this.setAdjustedOutTime(adjustedOutMillis);
        this.setAdjustedInTimeText(DateUtil.convertMillisToDateTimeText(dateFormat, adjustedInMillis, timezone));
        this.setAdjustedOutTimeText(DateUtil.convertMillisToDateTimeText(dateFormat, adjustedOutMillis, timezone));

        long originalInMillis = (long)parentEvent.get(SchedulingKeys.START_TIME);
        long originalOutMillis = (long)parentEvent.get(SchedulingKeys.END_TIME);

        this.setOriginalInTime(originalInMillis);
        this.setOriginalOutTime(originalOutMillis);
        this.setOriginalInTimeText(DateUtil.convertMillisToDateTimeText(dateFormat, originalInMillis, timezone));
        this.setOriginalOutTimeText(DateUtil.convertMillisToDateTimeText(dateFormat, originalOutMillis, timezone));
    }

    private void setMetaDetails(Map<String,Object> adjEvent,String timezone,DateFormats dateFormats){
        if(adjEvent.containsKey(SchedulingKeys.METADATA)){
            Map<String, Object> metaMap = (HashMap<String, Object>) adjEvent.get(SchedulingKeys.METADATA);
            this.setStatus(metaMap.get(SchedulingKeys.STATUS).toString());
            this.setAdditionalInfo(metaMap,timezone,dateFormats);
        }
    }

    private void setPayrollDetails(Map<String,Object> adjEvent){

        var paymentStatus = adjEvent.get(SchedulingKeys.PAYMENT_STATUS).toString();

        if( !InternalUsage.DEFAULT_PAYROLL.equalsIgnoreCase(paymentStatus) && !REPORT_STATUS.PENDING.toString().equalsIgnoreCase(paymentStatus)){
            this.setPayrollStatus(paymentStatus);
        }
    }

    private void setAdditionalInfo(Map<String,Object> metaMap, String timezone, DateFormats dateFormat){

        if(metaMap.containsKey(SchedulingKeys.ADDITIONAL_INFO)){
            Map<String, Object> additionalInfo = (HashMap<String, Object>) metaMap.get(SchedulingKeys.ADDITIONAL_INFO);

            if(additionalInfo.containsKey(SchedulingKeys.ADJUSTMENT_INFO)) {
                Map<String, Object> adjustmentInfo = (HashMap<String, Object>) additionalInfo.get(SchedulingKeys.ADJUSTMENT_INFO);
                this.setRequestedInfo(adjustmentInfo,timezone,dateFormat);
                this.setApprovedInfo(adjustmentInfo,timezone,dateFormat);
                this.setRejectedInfo(adjustmentInfo,timezone,dateFormat);
            }
        }
    }

    private Long extractWhenTime(Map<String, Object> infoMap){
        var whenMillis = 0L;
        try {
            var doubleTime = Double.valueOf((double)infoMap.get(SchedulingKeys.WHEN));
            whenMillis = doubleTime.longValue();
        } catch (Exception e) { // for put requests, we receive long value instead of double value from Se.
            log.info(e.getMessage());
            whenMillis = (long)infoMap.get(SchedulingKeys.WHEN);
        }
        return whenMillis;
    }

    private void setRequestedInfo(Map<String,Object> adjustmentInfo,String timezone,DateFormats dateFormat){
        if(adjustmentInfo.containsKey(SchedulingKeys.REQUEST_INFO)) {
            Map<String, Object> requestInfo = (HashMap<String, Object>) adjustmentInfo.get(SchedulingKeys.REQUEST_INFO);
            long requestTime = this.extractWhenTime(requestInfo);
            this.setRequestedTime(requestTime);
            this.setRequestedTimeText(DateUtil.convertMillisToDateTimeText(dateFormat, requestTime, timezone));
            this.setRequestedBy(requestInfo.get(SchedulingKeys.BY).toString());
            this.setRequestMessage(requestInfo.get(SchedulingKeys.NOTES).toString());
        }
    }

    private void setApprovedInfo(Map<String,Object> adjustmentInfo,String timezone,DateFormats dateFormat){
        if(adjustmentInfo.containsKey(SchedulingKeys.APPROVE_INFO)) {
            Map<String, Object> approveInfo = (HashMap<String, Object>) adjustmentInfo.get(SchedulingKeys.APPROVE_INFO);
            long approveTime = this.extractWhenTime(approveInfo);
            this.setApprovedTime(approveTime);
            this.setApprovedTimeText(DateUtil.convertMillisToDateTimeText(dateFormat, approveTime, timezone));
            this.setApprovedBy(approveInfo.get(SchedulingKeys.BY).toString());
            this.setApprovedMessage(approveInfo.get(SchedulingKeys.NOTES).toString());
            this.setStatus(REPORT_STATUS.APPROVED.toString());
        }
    }

    private void setRejectedInfo(Map<String,Object> adjustmentInfo, String timezone,DateFormats dateFormat){
        if(adjustmentInfo.containsKey(SchedulingKeys.REJECT_INFO)) {
            Map<String, Object> rejectInfo = (HashMap<String, Object>) adjustmentInfo.get(SchedulingKeys.REJECT_INFO);
            long rejectedTime = this.extractWhenTime(rejectInfo);
            this.setApprovedTime(rejectedTime);
            this.setApprovedTimeText(DateUtil.convertMillisToDateTimeText(dateFormat, rejectedTime, timezone));
            this.setApprovedBy(rejectInfo.get(SchedulingKeys.BY).toString());
            this.setRejectMessage(rejectInfo.get(SchedulingKeys.NOTES).toString());
        }
    }

    public boolean hasAdjustmentBeenRejected(){
        return this.getStatus().equals(REPORT_STATUS.REJECTED.toString());
    }
}
