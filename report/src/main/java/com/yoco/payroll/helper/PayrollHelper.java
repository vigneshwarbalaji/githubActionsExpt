package com.yoco.payroll.helper;

import com.google.common.collect.Lists;
import com.yoco.adjustment.helper.AdjustmentHelper;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.PayrollImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.PayrollStatus;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.modal.date.RangeInfoDTO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.ClockUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.validations.Validator;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Slf4j
public class PayrollHelper {

    public static PayrollHelper getInstance() {
        return new PayrollHelper();
    }

    public Map<String, Object> getUserConfirmedPayrollDetails(String accountID, String timeZone, RangeInfoDTO rangeInfoDTO, String cursor) throws IOException, NoSuchAlgorithmException {
        Map<String, Object>payrollAccessVerificationMap = verifyPayrollAccessibilityForTheUser(accountID, rangeInfoDTO);
        Map<String, Object>responseMap = new HashMap<>();
        if(!ObjUtils.isNullOrEmpty(payrollAccessVerificationMap)){
            long verifiedStartMillis = (long) payrollAccessVerificationMap.get(DateConstants.START_MILLIS);
            String startDateTimeText = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, verifiedStartMillis,timeZone);
            String endDateTimeText   = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getToDateEpochMilliseconds(),timeZone);
            Map<String, Object>eventsMap = PayrollImpl.getInstance().getPayrollEvents(accountID,  PayrollStatus.USER_ACKNOWLEDGED.toString(), startDateTimeText, endDateTimeText, cursor, 1000);
            responseMap = convertSchedulingEventsIntoListOfEntries(eventsMap, timeZone);
        }
        return responseMap;
    }

    public Map<String, Object> verifyPayrollAccessibilityForTheUser(String accountID, RangeInfoDTO rangeInfoDTO){
        SettingsJDO accountObject = AccountImpl.getAccountImplInstance().getById(accountID);
        long payrollAccessSince = accountObject.getPayrollAccessSince();
        return checkIfTheGivenTimeIsAccessibleForTheUserBasedOnPayrollDate(payrollAccessSince, rangeInfoDTO);
    }

    public Map<String, Object> checkIfTheGivenTimeIsAccessibleForTheUserBasedOnPayrollDate(long payrollAccessSince, RangeInfoDTO rangeInfoDTO){
        Map<String, Object> responseMap = new HashMap<>();
        long startMilliseconds = rangeInfoDTO.getFromDateEpochMilliseconds();
        long endMilliseconds = rangeInfoDTO.getToDateEpochMilliseconds();

        if(startMilliseconds >= payrollAccessSince){
            responseMap.put(DateConstants.START_MILLIS,startMilliseconds);
            return responseMap;
        }

        if(endMilliseconds >= payrollAccessSince){
            responseMap.put(DateConstants.START_MILLIS, payrollAccessSince);
            return responseMap;
        }

        return responseMap;
    }

    public Map<String, Object> convertSchedulingEventsIntoListOfEntries(Map<String, Object>eventsMap, String timeZone) {
        Map<String, Object>responseMap = new HashMap<>();
        String cursor = null;
        List<ReportsDTO> entriesList = new ArrayList<>();
        Map<String, Object> schRespData = (HashMap<String, Object>) eventsMap.get(SchedulingKeys.DATA);

        if(!ObjUtils.isNullOrEmpty(schRespData)) {
            List<Map<String, Object>> payrollEventsList = (ArrayList) schRespData.get(SchedulingKeys.EVENTS);
            if (schRespData.containsKey(Commons.CURSOR)) {
                cursor = (String) schRespData.get(Commons.CURSOR);
            }
            if (!ObjUtils.isNullOrEmpty(payrollEventsList)) {
                for(var payrollEvent : payrollEventsList){
                    ReportsDTO reportsDTO = new ReportsDTO(payrollEvent, "", timeZone);
                    entriesList.add(reportsDTO);
                }
            }
        }

        if(!ObjUtils.isNullOrEmpty(entriesList)){
            responseMap.put(SchedulingKeys.ENTRIES, entriesList);
            responseMap.put(Commons.CURSOR, cursor);
        }

        return responseMap;
    }

    public Map<String, Object> getAdminUpdatedPayrollDetails(String accountID, String timeZone, RangeInfoDTO rangeInfoDTO) throws IOException, NoSuchAlgorithmException {
        Map<String, Object>payrollAccessVerificationMap = verifyPayrollAccessibilityForTheUser(accountID, rangeInfoDTO);
        Map<String, Object>responseMap = new HashMap<>();
        if(!ObjUtils.isNullOrEmpty(payrollAccessVerificationMap)){
            long verifiedStartMillis = (long) payrollAccessVerificationMap.get(DateConstants.START_MILLIS);
            String startDateTimeText = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, verifiedStartMillis,timeZone);
            String endDateTimeText   = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getToDateEpochMilliseconds(),timeZone);
            Map<String, Object>eventsMap = PayrollImpl.getInstance().getPayrollEventsBasedOnStatus(accountID, REPORT_STATUS.PENDING.toString(),PayrollStatus.ADMIN_UPDATED.toString(), startDateTimeText, endDateTimeText);
            responseMap = convertSchedulingEventsIntoListOfAdjustments(eventsMap, timeZone);
        }
        return responseMap;
    }

    public Map<String, Object> convertSchedulingEventsIntoListOfAdjustments(Map<String, Object>eventsMap, String timeZone) throws IOException, NoSuchAlgorithmException {
        Map<String, Object>responseMap = new HashMap<>();
        List<AdjustmentDTO> adjustmentList = new ArrayList<>();
        Map<String, Object> schRespData = (HashMap<String, Object>) eventsMap.get(SchedulingKeys.DATA);

        if(!ObjUtils.isNullOrEmpty(schRespData)) {
            List<Map<String, Object>> payrollEventsList = (ArrayList) schRespData.get(SchedulingKeys.EVENTS);
            if (!ObjUtils.isNullOrEmpty(payrollEventsList)) {
                adjustmentList = new AdjustmentHelper().convertEventListIntoAdjustmentList(payrollEventsList, timeZone, "");
            }
        }

        if(!ObjUtils.isNullOrEmpty(adjustmentList)){
            responseMap.put(SchedulingKeys.ADJUSTMENT, adjustmentList);
        }

        return responseMap;
    }

    public void validateAdminPro(String accountID, Contact adminContact){
        PeopleRelationJDO adminPro = UserPROUtil.getUserPro(accountID, adminContact.getId());
        Validator.checkArgument(Boolean.FALSE.equals(PermissionManager.checkUserHasAdjustmentEditAccess(adminPro)),
                COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
    }

    public List<String> validateAndExtractApprovalEntryIDsFromPayload(String payload){
        Map<String, Object> payloadMap = Validator.validateAndExtractPayload(payload);
        String entryIds = (String) payloadMap.get("entryID");
        Validator.checkArgument(ObjUtils.isNullOrEmpty(entryIds), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        return List.of(entryIds.split(","));
    }

    public boolean isValidPayrollEvent(Map<String, Object> event, String accountID, String contactID, Long payrollAccessSince){
        return ReportsUtil.getAccountIDFromEvent(event).equalsIgnoreCase(accountID)
                && ReportsUtil.getContactIDFromEvent(event).equalsIgnoreCase(contactID)
                && ReportsUtil.isEventStatusSameAsGivenStatus(event,REPORT_STATUS.MANUAL_CLOCKED_OUT.toString())
                && ReportsUtil.isPayrollStatusUserAcknowledged(event)
                && ((long)event.get(SchedulingKeys.START_TIME) >= payrollAccessSince);
    }

    public List<Map<String, Object>> getPayrollEvents(List<String> payrollIds) throws IOException, NoSuchAlgorithmException {
        Map<String, Object> eventsResp = ReportImpl.getReportImplInstance().getEventsByIds(payrollIds);
        return ObjUtils.isNullOrEmpty(eventsResp) ? List.of() : (ArrayList) eventsResp.get(SchedulingKeys.EVENTS);
    }

    public List<Map<String,Object>> filterValidPayrollEventsAndMarkThemAsApproved(List<Map<String, Object>> payrollEventsList,String accountID,String contactID,Long payrollAccessSince){
        return payrollEventsList.stream()
                .filter(event -> Boolean.TRUE.equals(isValidPayrollEvent(event,accountID,contactID,payrollAccessSince)))
                .map(this::markThisEventAsApproved)
                .collect(Collectors.toList());
    }

    private Map<String, Object> markThisEventAsApproved(Map<String, Object> event) {
        Map<String, Object> queryableMeta = ReportsUtil.getQueryableMetaMap(event);
        queryableMeta.put(SchedulingKeys.PAYROLL_STATUS, PayrollStatus.ADMIN_APPROVED.toString());
        event.put(SchedulingKeys.QUERYABLE_META, queryableMeta);
        event.put(SchedulingKeys.PAYMENT_STATUS, PayrollStatus.ADMIN_APPROVED.toString());
        return event;
    }

    public List<String> validateAndApprovePayrollEvents(List<Map<String, Object>> payrollEventsList,SettingsJDO account,String contactID) throws IOException, NoSuchAlgorithmException {
        List<Map<String, Object>> approveEventsList = this.filterValidPayrollEventsAndMarkThemAsApproved(payrollEventsList,account.getPeopleUniquePin(),contactID,account.getPayrollAccessSince());
        return ObjUtils.isNullOrEmpty(approveEventsList)
                ? List.of()
                : this.approvePayrollEvents(account.getPeopleUniquePin(),approveEventsList);
    }

    private List<String> approvePayrollEvents(String accountID,List<Map<String, Object>> eventsListForApproval) throws IOException, NoSuchAlgorithmException {
        List<String> approvedPayrollIds = new ArrayList<>();
        List<List<Map<String, Object>>> approvedPartitionedList = Lists.partition(eventsListForApproval, 20);
        for(var approveEvents : approvedPartitionedList){
            List<Map<String,Object>> response = updateEvents(accountID,approveEvents);
            if(!ObjUtils.isNullOrEmpty(response)) {
                approvedPayrollIds.addAll(ReportsUtil.getIdsListFromEvents(response));
            }
        }
        return approvedPayrollIds;
    }

    public List<Map<String,Object>> updateEvents(String accountID,List<Map<String, Object>> approveEventsList) throws IOException, NoSuchAlgorithmException {
        Map<String, Object> response = ReportImpl.getReportImplInstance().updateEntries(accountID, approveEventsList);
        return ObjUtils.isNullOrEmpty(response) ? new ArrayList<>() : (ArrayList) response.get(SchedulingKeys.DATA);
    }

    public Map<String, Object> updateEntriesWithUserVerifiedPaymentStatus(String accountID, String contactID, String entryID, PeopleRelationJDO userPRO) throws IOException, NoSuchAlgorithmException {

        Map<String, Object> eventMap = ReportImpl.getReportImplInstance().getEntryByID(entryID);
        Map<String, Object>responseMap = new HashMap<>();

        if(!ObjUtils.isNullOrEmpty(eventMap)){
            eventMap = validateAndUpdateTheEventsAsUserVerified(accountID, contactID, eventMap);
            Map<String, Object>updatedEntryResponse = ClockUtil.updateEvent(eventMap);
            var convertedEntryObject = new ReportsDTO(updatedEntryResponse, userPRO.getEmailID(), userPRO.getTimeZone());
            responseMap.put(SchedulingKeys.ENTRY, convertedEntryObject);
        }

        saveActivityOfTheUserVerifiedEntry(accountID, contactID, userPRO, entryID, responseMap);

        return responseMap;
    }

    public Map<String, Object> validateAndUpdateTheEventsAsUserVerified(String accountID,String contactID, Map<String, Object> eventMap){

        Validator.checkArgument(Boolean.FALSE.equals(ReportsUtil.getAccountIDFromEvent(eventMap).equalsIgnoreCase(accountID)),
                COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());

        Validator.checkArgument(Boolean.FALSE.equals(ReportsUtil.getContactIDFromEvent(eventMap).equalsIgnoreCase(contactID)),
                COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());

        Validator.checkArgument(Boolean.FALSE.equals(ReportsUtil.getEventStatus(eventMap).equalsIgnoreCase(REPORT_STATUS.PENDING.toString())),
                COMMON_ERROR_RESPONSE.ACTION_ALREADY_TAKEN.value());

        Validator.checkArgument(Boolean.FALSE.equals(ReportsUtil.getPayrollStatus(eventMap).equalsIgnoreCase(PayrollStatus.ADMIN_UPDATED.toString())),
                COMMON_ERROR_RESPONSE.ACTION_ALREADY_TAKEN.value());

        Validator.checkArgument(ReportsUtil.isEventDeleted(eventMap), COMMON_ERROR_RESPONSE.FAILED_TO_UPDATE_THE_EVENT.value());

        Map<String, Object> metaMap = (Map<String, Object>) eventMap.get(SchedulingKeys.METADATA);
        Map<String, Object> queryableMetaMap = (Map<String, Object>) eventMap.get(SchedulingKeys.QUERYABLE_META);

        metaMap.put(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString());
        queryableMetaMap.put(SchedulingKeys.STATUS, REPORT_STATUS.MANUAL_CLOCKED_OUT.toString());
        queryableMetaMap.put(SchedulingKeys.PAYROLL_STATUS, PayrollStatus.USER_CONFIRMED.toString());

        eventMap.put(SchedulingKeys.METADATA, metaMap);
        eventMap.put(SchedulingKeys.QUERYABLE_META, queryableMetaMap);
        eventMap.put(SchedulingKeys.PAYMENT_STATUS, PayrollStatus.USER_CONFIRMED.toString());

        return eventMap;
    }


    public void saveActivityOfTheUserVerifiedEntry(String accountID, String contactID, PeopleRelationJDO userPRO, String entryID, Map<String, Object> responseMap){

        long receivedLongTime = DateUtil.getCurrentTime();

        if(!ObjUtils.isNullOrEmpty(responseMap)){
            String activityMessage = "User: " + contactID + "payrollStatus : " +  PayrollStatus.USER_CONFIRMED + "entryID : " + entryID;
            ActivityUtil.saveActivity(accountID, contactID, "", userPRO.getEmailID(), activityMessage, ActivityUtil.ACTIVITIES.DUMMY.value(), receivedLongTime);
        }

    }

}
