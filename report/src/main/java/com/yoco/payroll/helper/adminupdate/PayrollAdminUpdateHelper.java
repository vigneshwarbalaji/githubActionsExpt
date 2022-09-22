package com.yoco.payroll.helper.adminupdate;

import com.yoco.adjustment.helper.AdjustmentOverlapHelper;
import com.yoco.adjustment.util.AdjustmentUtil;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.PayrollStatus;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.EventConstants;
import com.yoco.payroll.helper.PayrollTaskInitiator;
import com.yoco.payroll.modal.AdminUpdatePayloadDTO;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;
import static com.yoco.commons.constants.SchedulingKeys.*;

public class PayrollAdminUpdateHelper {
    public Map<String,Object> validateAuthorizationAndGetEvent(String entryID, String userContactID, PeopleRelationJDO loggedInUserPro, boolean isReUpdateRequest) throws IOException, NoSuchAlgorithmException {
        Map<String,Object> event = new ReportImpl().getEntryByID(entryID);
        Validator.checkArgument(!this.isEntryEligibleForAdminUpdate(event,isReUpdateRequest), COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
        Validator.checkArgument(!this.isUserAuthorizedToAdminUpdateEntry(event,userContactID,loggedInUserPro), COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        return event;
    }

    public boolean isUserAuthorizedToAdminUpdateEntry(Map<String, Object> event, String userContactID, PeopleRelationJDO loggedInUserPro) {
        String entryAccountID = ReportsUtil.getAccountIDFromEvent(event);
        if(!loggedInUserPro.getUniquepin().equals(entryAccountID)){
            return false;
        }
        String entryContactID = ReportsUtil.getContactIDFromEvent(event);
        return entryContactID.equalsIgnoreCase(userContactID) && PermissionManager.checkUserHasAdjustmentEditAccess(loggedInUserPro);
    }

    public boolean isEntryEligibleForAdminUpdate(Map<String,Object> event, boolean isReUpdateRequest){
        if(ReportsUtil.isEventDeleted(event)){
            return false;
        }
        return isReUpdateRequest ? (ReportsUtil.isStatusPending(event) && ReportsUtil.isPayrollStatusAdminUpdated(event)) :
                (ReportsUtil.isStatusManualClockedOut(event) && ReportsUtil.isPayrollStatusUserAcknowledged(event));
    }

    public Map<String, Object> checkAndGenerateOverlapResponseForAdjustedTime(PeopleRelationJDO requesterPro, PeopleRelationJDO userPro, AdminUpdatePayloadDTO payloadDTO) throws IOException, NoSuchAlgorithmException {
        payloadDTO.setAdjustedInTime(AdjustmentUtil.adjustInTimeMillisForBreakSessionOverlapping(requesterPro.getUniquepin(),userPro.getContactId(),payloadDTO.getAdjustedInTime(),requesterPro.getTimeZone()));
        Map<String,Object> overlappingEvent = AdjustmentOverlapHelper.validateAndExtractOverlapEntry(requesterPro.getUniquepin(), userPro.getContactId(), payloadDTO.getAdjustedInTime(), payloadDTO.getAdjustedOutTime(), requesterPro.getTimeZone(), ReportsUtil.getIDFromEvent(payloadDTO.getEvent()));
        if(ObjUtils.isNullOrEmpty(overlappingEvent)){
            return Map.of();
        }
        return AdjustmentOverlapHelper.extractAllOverLapEntryDetails(overlappingEvent,requesterPro.getTimeZone(),userPro.getEmailID());
    }

    public GenericResponse createAdminUpdatedEventAndGenerateResponse(PeopleRelationJDO requesterPro, PeopleRelationJDO userPro, AdminUpdatePayloadDTO payloadDTO) throws IOException, NoSuchAlgorithmException {
        Map<String,Object> originalEvent = payloadDTO.getEvent();
        Map<String,Object> adminUpdatedEvent = this.generateAdminUpdatedEventMap(originalEvent,payloadDTO, requesterPro);
        Map<String,Object> eventResponse = (Map<String, Object>)(SchedulingEngineUtil.createEvent(JsonUtil.getJson(adminUpdatedEvent)).get(SchedulingKeys.DATA));
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO(eventResponse,originalEvent, requesterPro.getTimeZone(), DateFormats.YYYY_MM_DD_T_HH_MM_SS_A_Z);
        ReportsDTO reportsDTO = new ReportsDTO(eventResponse,userPro.getEmailID(),requesterPro.getTimeZone());
        GenericResponse response = new GenericResponse();
        response.setSuccess(true);
        response.setData(Map.of(EventConstants.ENTRY,reportsDTO,EventConstants.ADJUSTMENT,adjustmentDTO));
        PayrollTaskInitiator.initiateAdminUpdateHandlerQueue(requesterPro,userPro,adjustmentDTO);
        return response;
    }

    public Map<String,Object> generateAdminUpdatedEventMap(Map<String,Object> originalEvent, AdminUpdatePayloadDTO payloadDTO, PeopleRelationJDO requesterPro){
        Map<String,Object> adminUpdatedEvent = new HashMap<>(originalEvent);
        adminUpdatedEvent.remove("bookingId");
        adminUpdatedEvent.remove(ID);
        adminUpdatedEvent.put(START_DATE_TIME, DateUtil.convertMillisToDateTimeText(DateFormats.ZULU,payloadDTO.getAdjustedInTime(), DateConstants.ZONE_ID_UTC));
        adminUpdatedEvent.put(END_DATE_TIME, DateUtil.convertMillisToDateTimeText(DateFormats.ZULU,payloadDTO.getAdjustedOutTime(), DateConstants.ZONE_ID_UTC));
        adminUpdatedEvent.put(PARENT_ID, payloadDTO.isReUpdateRequest() ? ReportsUtil.getParentIDFromEvent(originalEvent) : ReportsUtil.getIDFromEvent(originalEvent));
        adminUpdatedEvent.put(PAYMENT_STATUS, PayrollStatus.ADMIN_UPDATED.toString());
        adminUpdatedEvent.put(QUERYABLE_META, Map.of(PAYROLL_STATUS,PayrollStatus.ADMIN_UPDATED.toString(), STATUS, REPORT_STATUS.PENDING.toString()));
        Map<String,Object> existingMetaMap = ReportsUtil.getMetaDataMap(originalEvent);
        existingMetaMap.put(ADDITIONAL_INFO, Map.of(ADJUSTMENT_INFO, Map.of(REQUEST_INFO,AdjustmentUtil.setAdjustmentInfo(requesterPro.getContactId(),payloadDTO.getMessage()))));
        existingMetaMap.put(STATUS, REPORT_STATUS.PENDING.toString());
        adminUpdatedEvent.put(METADATA, existingMetaMap);
        return adminUpdatedEvent;
    }
}
