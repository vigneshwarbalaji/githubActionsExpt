package com.yoco.adjustment.helper;

import com.yoco.adjustment.helper.approve.ApproveAdjustmentEmailHelper;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.InAppReminderUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

@Slf4j
public class AdjustmentTaskHelper {

    private AdjustmentTaskHelper(){}

    public static void handleApproveAdjustment(Map<String, Object> payloadMap) throws IOException, NoSuchAlgorithmException {
        Map<String,Object> event = (Map<String, Object>) payloadMap.get("eventMap");
        PeopleRelationJDO adminPro = (PeopleRelationJDO) payloadMap.get("adminPRO");
        String accountID = ReportsUtil.getAccountIDFromEvent(event);
        String contactID = ReportsUtil.getContactIDFromEvent(event);
        PeopleRelationJDO userPro = UserPROUtil.getUserProWithContact(accountID,contactID);
        if(!ObjUtils.isNull(userPro)){
            String emailID = userPro.getEmailID();
            String zone = userPro.getTimeZone();
            ReportsDTO reportsDTO = new ReportsDTO(event,emailID,zone);
            Map<String,Object> parentEvent = ReportsUtil.getParentEventFromEvent(event);
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO(event,parentEvent,zone,DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            AdjustmentChannelPublishHelper.getInstance().publishApproveAdjustment(reportsDTO,adjustmentDTO);
            SettingsJDO account = AccountImpl.getAccountImplInstance().getById(accountID);
            if(UserPROUtil.isUserProActive(userPro) && !ObjUtils.isNull(account)){
                ApproveAdjustmentEmailHelper.getInstance().adjustmentApprovedMailHandler(userPro.getContact(),adminPro.getContact(),
                        zone,adjustmentDTO,account);
            }
        }
    }


    public static void handleEditApproveAdjustment(Map<String, Object> payloadMap) throws IOException, NoSuchAlgorithmException {
        Map<String,Object> event = (Map<String, Object>) payloadMap.get("eventMap");
        PeopleRelationJDO adminPro = (PeopleRelationJDO) payloadMap.get("adminPRO");
        String accountID = ReportsUtil.getAccountIDFromEvent(event);
        String contactID = ReportsUtil.getContactIDFromEvent(event);
        PeopleRelationJDO userPro = UserPROUtil.getUserProWithContact(accountID,contactID);
        if(!ObjUtils.isNull(userPro)){
            String emailID = userPro.getEmailID();
            String zone = userPro.getTimeZone();
            AdjustmentChannelPublishHelper adjustmentChannelPublishHelper = AdjustmentChannelPublishHelper.getInstance();
            ReportsDTO reportsDTO = new ReportsDTO(event,emailID,zone);
            Map<String,Object> parentEvent = ReportsUtil.getParentEventFromEvent(event);
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO(event,parentEvent,zone,DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            adjustmentChannelPublishHelper.publishEditApproveAdjustment(reportsDTO,adjustmentDTO,zone);
            adjustmentChannelPublishHelper.publishAdjustmentActionToAW(reportsDTO,adjustmentDTO);
            SettingsJDO account = AccountImpl.getAccountImplInstance().getById(accountID);
            if(UserPROUtil.isUserProActive(userPro) && !ObjUtils.isNull(account)){
                Map<String,Object> timeDetailsMap = (Map<String, Object>) payloadMap.get("newAdjustmentTimeDetailsInfoMap");
                if(!ObjUtils.isNullOrEmpty(timeDetailsMap)){ // for new adjustment, parent map will not be there, so using current time entry values before modification to send mail.
                    adjustmentDTO.setOriginalInTime((Long) timeDetailsMap.get(SchedulingKeys.START_TIME));
                    adjustmentDTO.setOriginalOutTime((Long) timeDetailsMap.get(SchedulingKeys.END_TIME));
                }
                ApproveAdjustmentEmailHelper.getInstance().adjustmentEditApprovedMailHandler(userPro.getContact(),adminPro.getContact(),zone,adjustmentDTO,account);
                InAppReminderUtil.getInstance().handleInAppNotificationForAdjustment(adjustmentDTO,userPro,zone);
            }
        }
    }

}
