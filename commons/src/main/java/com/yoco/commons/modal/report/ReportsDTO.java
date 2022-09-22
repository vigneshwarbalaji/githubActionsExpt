package com.yoco.commons.modal.report;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ProjectImpl;
import com.yoco.commons.dataservices.impl.TaskImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.entity.TaskJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@NoArgsConstructor
@Data
public class ReportsDTO implements Serializable {

    private String id;
    private String contactID;
    private String emailID;
    private String accountID;
    private String status = "";
    private String payrollStatus;
    private String inIP = "";
    private String inSource;
    private long inTime;
    private String outIP =  "";
    private String outSource = "";
    private long outTime;
    @JsonProperty(value="isDeleted")
    private boolean isDeleted;
    private String clockInMessage = "";
    private String clockOutMessage = "";
    private String subStatus = "";
    private String projectID = "";
    private String projectName = InternalUsage.NO_PROJECT;
    private String start;
    private String stop;
    private String taskSource = "";
    private String taskDescription = "";
    private String taskID = "";
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Contact contact = null;


    public ReportsDTO(Map<String,Object> event, String emailID, String timeZone) {

        this.setId(event.get(SchedulingKeys.ID).toString());
        this.setAccountID(event.get(SchedulingKeys.MERCHANT).toString());

        ArrayList<String> contactIdList = (ArrayList<String>) event.get(SchedulingKeys.PROVIDER);
        this.setContactID(contactIdList.get(0));

        this.setLocationDetails(event);

        this.setInSource(event.get(SchedulingKeys.SOURCE).toString());

        var paymentStatus = event.get(SchedulingKeys.PAYMENT_STATUS).toString();
        this.setPayrollStatus(InternalUsage.DEFAULT_PAYROLL.equalsIgnoreCase(paymentStatus) ? "" : paymentStatus);

        this.setDeleted((boolean) event.get(SchedulingKeys.IS_DELETED));
        this.setEmailID(emailID);

        this.setInTime((Long) event.get(SchedulingKeys.START_TIME));
        this.setStart(DateUtil.convertMillisToDateTimeText(DateFormats.YYYY_MM_DD_T_HH_MM_SS_A_Z, (long)event.get(SchedulingKeys.START_TIME),timeZone));

        this.setMetaDetails(event);
        this.setProjectDetails(event);
        long clockOutTime = this.getStatus().equalsIgnoreCase(REPORT_STATUS.CLOCKED_IN.toString()) ? DateUtil.getCurrentTime() : (Long) event.get(SchedulingKeys.END_TIME);
        this.setOutTime(clockOutTime);
        this.setStop(DateUtil.convertMillisToDateTimeText(DateFormats.YYYY_MM_DD_T_HH_MM_SS_A_Z, clockOutTime, timeZone));
    }

    private void setLocationDetails(Map<String,Object> event){

        if(event.containsKey(SchedulingKeys.LOCATION)) {
            Map<String, Object> locationMap = (Map<String, Object>) event.get(SchedulingKeys.LOCATION);
            this.setInLocationDetails(locationMap);
            this.setOutLocationDetails(locationMap);
        }
    }

    private void setInLocationDetails(Map<String,Object> locationMap){

        if (locationMap.containsKey(SchedulingKeys.CLOCK_IN_MESSAGE)) {
            Map<String, Object> clockInMap = (Map<String, Object>) locationMap.get(SchedulingKeys.CLOCK_IN_MESSAGE);
            this.setInIP(clockInMap.containsKey(SchedulingKeys.IP) ? clockInMap.get(SchedulingKeys.IP).toString() : "");
            this.setClockInMessage(clockInMap.containsKey(SchedulingKeys.INFO) ? JsonUtil.getJson(clockInMap.get(SchedulingKeys.INFO)) : "");
        }
    }

    private void setOutLocationDetails(Map<String,Object> locationMap){

        if (locationMap.containsKey(SchedulingKeys.CLOCK_OUT_MESSAGE)) {
            Map<String, Object> clockOutMap = (Map<String, Object>) locationMap.get(SchedulingKeys.CLOCK_OUT_MESSAGE);
            this.setOutIP(clockOutMap.containsKey(SchedulingKeys.IP) ? clockOutMap.get(SchedulingKeys.IP).toString() : "");
            this.setClockOutMessage(clockOutMap.containsKey(SchedulingKeys.INFO) ? JsonUtil.getJson(clockOutMap.get(SchedulingKeys.INFO)) : "");
        }
    }

    private void setMetaDetails(Map<String,Object> event){

        if(event.containsKey(SchedulingKeys.METADATA)){
            Map<String, Object> metaMap = (HashMap<String, Object>) event.get(SchedulingKeys.METADATA);
            this.setStatus(metaMap.get(SchedulingKeys.STATUS).toString());
            this.setOutSource(metaMap.containsKey(SchedulingKeys.OUTSOURCE) ? metaMap.get(SchedulingKeys.OUTSOURCE).toString() : ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value());
            this.setSubStatus(metaMap.containsKey(SchedulingKeys.SUB_STATUS) ? metaMap.get(SchedulingKeys.SUB_STATUS).toString() : "");
            this.setTaskDetails(event,metaMap);
        }
    }

    private void setTaskDetails(Map<String,Object> event,Map<String,Object> metaMap){

        if(metaMap.containsKey(SchedulingKeys.TASK_ID)) {

            this.setTaskID(metaMap.get(SchedulingKeys.TASK_ID).toString());

            TaskJDO objTask = TaskImpl.getTaskImplInstance().getEntryByID(event.get(SchedulingKeys.ID).toString());

            if(!ObjUtils.isNull(objTask)){
                this.setTaskSource(objTask.getTaskSource());
                this.setTaskDescription(objTask.getTaskDescription());
            }
        }
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
            } else {
                this.setProjectID("");
                this.setProjectName(InternalUsage.NO_PROJECT);
            }
        }
    }

}
