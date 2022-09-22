package com.yoco.adjustment.service;

import com.yoco.adjustment.helper.AdjustmentOverlapHelper;
import com.yoco.adjustment.helper.approve.ApproveAdjustmentHelper;
import com.yoco.adjustment.util.AdjustmentUtil;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import com.yoco.commons.validations.Validator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

@Slf4j
@Service
public class ApproveAdjustmentService {

    public Boolean approveAdjustment(String accountID,String entryID, Contact adminContact) throws IOException, NoSuchAlgorithmException {
        ApproveAdjustmentHelper approveAdjustmentHelper = ApproveAdjustmentHelper.getInstance();
        PeopleRelationJDO adminPro = approveAdjustmentHelper.validateAndExtractAdminPro(accountID,adminContact);
        Map<String,Object> entryMap = approveAdjustmentHelper.validateAndExtractEntry(entryID,accountID);
        entryMap.put(SchedulingKeys.METADATA, approveAdjustmentHelper.updateMetaDetailsWithApproveInfo(entryMap,adminContact.getId(),""));
        entryMap.put(SchedulingKeys.QUERYABLE_META, approveAdjustmentHelper.updateQueryableMetaDetailsWithApproveInfo(entryMap));
        Map<String,Object> updateEventResponse = SchedulingEngineUtil.updateEventReq(entryMap);
        return approveAdjustmentHelper.validateAndExtractApprovedResponse(updateEventResponse,adminPro);
    }

    public Map<String,Object> editApproveAdjustment(String accountID, String entryID, String payload, Contact adminContact) throws IOException, NoSuchAlgorithmException {
        ApproveAdjustmentHelper approveAdjustmentHelper = ApproveAdjustmentHelper.getInstance();
        PeopleRelationJDO adminPro = approveAdjustmentHelper.validateAndExtractAdminPro(accountID,adminContact);
        String zone = adminPro.getTimeZone();

        Map<String, Object> payloadMap = Validator.validateAndExtractPayload(payload);
        Long startMillis = approveAdjustmentHelper.validateAndExtractStartMillisFromPayload(payloadMap,zone);
        Long endMillis = approveAdjustmentHelper.validateAndExtractEndMillisFromPayload(payloadMap,zone);
        String message = approveAdjustmentHelper.validateAndExtractApprovalMessage(payloadMap);

        Map<String,Object> entryMap = approveAdjustmentHelper.validateAndExtractEntry(entryID,accountID);
        Map<String,Object> entryTimeDetailsMap = approveAdjustmentHelper.extractEntryTimeDetailsForNewAdjustment(entryMap);

        String contactID = ReportsUtil.getContactIDFromEvent(entryMap);

        startMillis = AdjustmentUtil.adjustInTimeMillisForBreakSessionOverlapping(accountID,contactID,startMillis,zone);
        log.info(" startMillis: " + startMillis);

        Map<String,Object> response = AdjustmentOverlapHelper.validateAndExtractOverlapEntry(accountID,contactID,startMillis,endMillis,zone,entryID);

        if(ObjUtils.isNullOrEmpty(response)){
            entryMap.put(SchedulingKeys.METADATA, approveAdjustmentHelper.updateMetaDetailsWithApproveInfo(entryMap,adminContact.getId(),message));
            entryMap.put(SchedulingKeys.QUERYABLE_META, approveAdjustmentHelper.updateQueryableMetaDetailsWithApproveInfo(entryMap));
            entryMap.put(SchedulingKeys.START_DATE_TIME, DateUtil.convertMillisToDateTimeText(DateFormats.ZULU,startMillis, DateConstants.ZONE_ID_UTC));
            entryMap.put(SchedulingKeys.END_DATE_TIME, DateUtil.convertMillisToDateTimeText(DateFormats.ZULU,endMillis,DateConstants.ZONE_ID_UTC));
            Map<String,Object> updateEventResponse = SchedulingEngineUtil.updateEventReq(entryMap);
            return approveAdjustmentHelper.validateAndExtractEditApprovedResponse(updateEventResponse,adminPro,entryTimeDetailsMap);
        }else{
           return AdjustmentOverlapHelper.extractOverLapEntryDetails(response,zone);
        }
    }
}
