package com.yoco.adjustment.helper.approve;

import com.yoco.adjustment.helper.AdjustmentTaskInitiator;
import com.yoco.adjustment.util.AdjustmentUtil;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.enums.ADJUSTMENTS_ERROR_RESPONSE;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public class ApproveAdjustmentHelper {

    public static ApproveAdjustmentHelper getInstance(){
        return new ApproveAdjustmentHelper();
    }

    public PeopleRelationJDO validateAndExtractAdminPro(String accountID, Contact adminContact){
        PeopleRelationJDO adminPro = UserPROUtil.getUserPro(accountID,adminContact.getId());
        Validator.checkArgument(Boolean.FALSE.equals(PermissionManager.checkUserHasAdjustmentEditAccess(adminPro)),
                COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        adminPro.setContact(adminContact);
        return adminPro;
    }

    public Map<String,Object> validateAndExtractEntry(String entryID,String accountID) throws IOException, NoSuchAlgorithmException {
        Map<String,Object> entryMap = ReportImpl.getReportImplInstance().getEntryByID(entryID);
        Validator.checkArgument(Boolean.FALSE.equals(ReportsUtil.getAccountIDFromEvent(entryMap).equalsIgnoreCase(accountID)),
                COMMON_ERROR_RESPONSE.ACCOUNT_NOT_FOUND.value());
        Validator.checkArgument(Boolean.TRUE.equals(ReportsUtil.isEventDeleted(entryMap))
                || Boolean.FALSE.equals(ReportsUtil.isStatusPending(entryMap)), ADJUSTMENTS_ERROR_RESPONSE.ACTION_ALREADY_TAKEN.value());
        return entryMap;
    }

    public Map<String,Object> extractEntryTimeDetailsForNewAdjustment(Map<String,Object> entryMap){
        Map<String,Object> entryTimeDetailsMap = new HashMap<>();
        if(ReportsUtil.isAdjustmentNew(entryMap)){
            entryTimeDetailsMap.put(SchedulingKeys.START_TIME,entryMap.get(SchedulingKeys.START_TIME));
            entryTimeDetailsMap.put(SchedulingKeys.END_TIME,entryMap.get(SchedulingKeys.END_TIME));
        }
        return entryTimeDetailsMap;
    }

    private Map<String,Object> updateAdditionalInfoMap(String approvedBy, String approvalMessage, Map<String,Object> metaData){
        Map<String,Object> additionalInfoMap = AdjustmentUtil.extractAdditionalInfo(metaData);
        Map<String, Object> adjustmentInfoMap = AdjustmentUtil.extractAdjustmentInfo(additionalInfoMap);
        adjustmentInfoMap.put(SchedulingKeys.APPROVE_INFO, AdjustmentUtil.setAdjustmentInfo(approvedBy, approvalMessage));
        additionalInfoMap.put(SchedulingKeys.ADJUSTMENT_INFO, adjustmentInfoMap);
        return additionalInfoMap;
    }


    public Map<String,Object> updateMetaDetailsWithApproveInfo(Map<String,Object> entryMap, String approvedBy, String approvalMessage){
        Map<String, Object> metaData = (HashMap<String, Object>) entryMap.get(SchedulingKeys.METADATA);
        metaData.put(SchedulingKeys.ADDITIONAL_INFO, this.updateAdditionalInfoMap(approvedBy,approvalMessage,metaData));
        metaData.put(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT);
        return metaData;
    }


    public Map<String,Object> updateQueryableMetaDetailsWithApproveInfo(Map<String,Object> entryMap){
        Map<String, Object> queryMap = (HashMap<String, Object>) entryMap.get(SchedulingKeys.QUERYABLE_META);
        queryMap.put(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT);
        queryMap.put(SchedulingKeys.ADJ_STATUS, REPORT_STATUS.APPROVED);
        return queryMap;
    }

    public Long validateAndExtractStartMillisFromPayload(Map<String, Object> payloadMap,String zone){
        String inTime = payloadMap.containsKey("inTime") ? (String)payloadMap.get("inTime") : "";
        Validator.checkArgument(ObjUtils.isNullOrEmpty(inTime),ADJUSTMENTS_ERROR_RESPONSE.INVALID_IN_TIME.value());
        return DateUtil.convertDateTimeTextToMillis(inTime, DateFormats.DD_MMM_YYYY_HH_MM_SS_A,zone);
    }

    public Long validateAndExtractEndMillisFromPayload(Map<String, Object> payloadMap,String zone){
        String outTime = payloadMap.containsKey("outTime") ? (String)payloadMap.get("outTime") : "";
        Validator.checkArgument(ObjUtils.isNullOrEmpty(outTime),ADJUSTMENTS_ERROR_RESPONSE.INVALID_OUT_TIME.value());
        return DateUtil.convertDateTimeTextToMillis(outTime,DateFormats.DD_MMM_YYYY_HH_MM_SS_A,zone);
    }


    public String validateAndExtractApprovalMessage(Map<String, Object> payloadMap){
        String message = payloadMap.containsKey("message") ? Validator.unescapeHtml((String)payloadMap.get("message")) : "";
        Validator.checkArgument(ObjUtils.isNullOrEmpty(message),ADJUSTMENTS_ERROR_RESPONSE.INVALID_MESSAGE.value());
        return message;
    }

    public Boolean validateAndExtractApprovedResponse(Map<String, Object> eventResponse,PeopleRelationJDO adminPro) throws IOException {
        if(!ObjUtils.isNullOrEmpty(eventResponse) && Boolean.TRUE.equals(eventResponse.get(SchedulingKeys.RESPONSE))){
            Map<String,Object> schData = (Map<String, Object>) eventResponse.get(SchedulingKeys.DATA);
            if(!ObjUtils.isNullOrEmpty(schData)){
                AdjustmentTaskInitiator.initiateApproveAdjustmentQueue(adminPro,schData);
                return Boolean.TRUE;
            }
        }
        log.info(" scheduling event response : " + eventResponse);
        return Boolean.FALSE;
    }

    public Map<String,Object> validateAndExtractEditApprovedResponse(Map<String, Object> eventResponse,PeopleRelationJDO adminPro,Map<String,Object> entryTimeDetailsMap) throws IOException {
        Map<String,Object> responseMap = new HashMap<>();
        if(!ObjUtils.isNullOrEmpty(eventResponse) && Boolean.TRUE.equals(eventResponse.get(SchedulingKeys.RESPONSE))){
            Map<String,Object> schData = (Map<String, Object>) eventResponse.get(SchedulingKeys.DATA);
            if(!ObjUtils.isNullOrEmpty(schData)){
                AdjustmentTaskInitiator.initiateEditApprovedAdjustmentQueue(adminPro,schData,entryTimeDetailsMap);
                responseMap.put(Commons.SUCCESS,true);
                return responseMap;
            }
        }
        log.info(" is event update success ? : " + eventResponse);
        return responseMap;
    }

}
