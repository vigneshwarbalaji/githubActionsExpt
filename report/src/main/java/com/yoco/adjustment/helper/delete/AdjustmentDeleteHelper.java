package com.yoco.adjustment.helper.delete;

import com.yoco.adjustment.helper.AdjustmentHelper;
import com.yoco.adjustment.helper.AdjustmentTaskInitiator;
import com.yoco.adjustment.helper.reject.AdjustmentRejectHelper;
import com.yoco.adjustment.modal.AdjustmentDeletePayloadDTO;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.DcmTeamUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.EventConstants;
import com.yoco.enums.ADJUSTMENTS_ERROR_RESPONSE;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
public class AdjustmentDeleteHelper {
    private AdjustmentDeleteHelper(){}
    private static final String OVERLAPPING_ENTRIES_KEY = "overlappingEntries";
    private static final String OVERLAPPING_ADJUSTMENTS_KEY = "overlappingAdjustments";
    private static final String ADJUSTMENTS_DELETED_KEY = "adjustmentsDeleted";
    private static final String CANNOT_DELETE_SUBSETS_MESSAGE = "There are one or more overlapping entries which was already approved, cannot delete the adjustment";
    private static final String OVERLAPPING_ERROR_MESSAGE = "There are one or more overlapping entries, deleting this adjustment will delete the subsets of this adjustment";

    public static Map<String,Object> validateAuthorizationAndGetAdjustmentEvent(String entryID, PeopleRelationJDO loggedInUserPro) throws IOException, NoSuchAlgorithmException {
        Map<String,Object> event = ReportImpl.getReportImplInstance().getEntryByID(entryID);
        Validator.checkArgument(!AdjustmentDeleteHelper.isValidAdjustment(event), ADJUSTMENTS_ERROR_RESPONSE.ACTION_ALREADY_TAKEN.value());
        Validator.checkArgument(!AdjustmentDeleteHelper.isUserAuthorizedToDeleteAdjustment(event,loggedInUserPro), COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        return event;
    }

    public static boolean isValidAdjustment(Map<String, Object> event) {
        return ReportsUtil.isStatusPending(event) && !ReportsUtil.isEventDeleted(event);
    }

    public static boolean isUserAuthorizedToDeleteAdjustment(Map<String, Object> event, PeopleRelationJDO loggedInUserPro) throws NoSuchAlgorithmException, IOException {
        String entryAccountID = ReportsUtil.getAccountIDFromEvent(event);
        if(!loggedInUserPro.getUniquepin().equals(entryAccountID)){
            return false;
        }
        String entryContactID = ReportsUtil.getContactIDFromEvent(event);
        PermissionManager permissionManager = new PermissionManager();
        if(loggedInUserPro.getContactId().equals(entryContactID) || permissionManager.doesUserHaveReportsEditAllPermission(loggedInUserPro)){
            return true;
        }
        return permissionManager.doesUserHaveReportsTeamEditPermission(loggedInUserPro) &&
                DcmTeamUtil.getAllTeamMembersOfAnUserInAccount(entryAccountID, loggedInUserPro.getContactId()).contains(entryContactID);
    }

    public static GenericResponse validateAndDeleteAdjustment(AdjustmentDeletePayloadDTO adjustmentDeletePayloadDTO) throws IOException, NoSuchAlgorithmException {
        ReportImpl reportImpl = new ReportImpl();
        if(ReportsUtil.isAdjustmentNew(adjustmentDeletePayloadDTO.getEventToDelete())){
           return deleteNewAdjustment(reportImpl, adjustmentDeletePayloadDTO);
        }else {
            return deleteEditAdjustment(reportImpl, adjustmentDeletePayloadDTO);
        }
    }

    public static GenericResponse deleteNewAdjustment(ReportImpl reportImpl, AdjustmentDeletePayloadDTO adjustmentDeletePayloadDTO ) throws IOException, NoSuchAlgorithmException {
        List<Map<String, Object>> deletedAdjustments = new ArrayList<>(AdjustmentDeleteHelper.deleteEvents(List.of(adjustmentDeletePayloadDTO.getEventToDelete()),adjustmentDeletePayloadDTO,reportImpl));
        List<Map<String, Object>> revertedEntries = new ArrayList<>(deletedAdjustments);
        return AdjustmentDeleteHelper.generateSuccessResponseAndInitiateQueue(deletedAdjustments,revertedEntries,adjustmentDeletePayloadDTO.getEntryContactPro(),adjustmentDeletePayloadDTO.getLoggedInUserPro(), adjustmentDeletePayloadDTO.getTimezone(), adjustmentDeletePayloadDTO.getDateFormat());
    }

    public static GenericResponse deleteEditAdjustmentWithoutOverlap(ReportImpl reportImpl, AdjustmentDeletePayloadDTO adjustmentDeletePayloadDTO) throws IOException, NoSuchAlgorithmException {
        List<Map<String, Object>> deletedAdjustments = new ArrayList<>(AdjustmentDeleteHelper.deleteEvents(List.of(adjustmentDeletePayloadDTO.getEventToDelete()),adjustmentDeletePayloadDTO,reportImpl));
        List<Map<String,Object>> revertedEntries = new ArrayList<>();
        String parentID = ReportsUtil.getParentIDFromEvent(adjustmentDeletePayloadDTO.getEventToDelete());
        revertedEntries.add(reportImpl.revertEvent(parentID));
        return AdjustmentDeleteHelper.generateSuccessResponseAndInitiateQueue(deletedAdjustments,revertedEntries,adjustmentDeletePayloadDTO.getEntryContactPro(),adjustmentDeletePayloadDTO.getLoggedInUserPro(),adjustmentDeletePayloadDTO.getTimezone(), adjustmentDeletePayloadDTO.getDateFormat());
    }

    public static GenericResponse deleteEditAdjustment(ReportImpl reportImpl, AdjustmentDeletePayloadDTO adjustmentDeletePayloadDTO ) throws IOException, NoSuchAlgorithmException {
        Map<String,Object> parentEvent = reportImpl.getEntryByID(ReportsUtil.getParentIDFromEvent(adjustmentDeletePayloadDTO.getEventToDelete()));
        List<Map<String,Object>> overlappingEvents = ReportsUtil.getOverlappingEntriesForEvent(parentEvent);
        if(overlappingEvents.isEmpty()){
            return deleteEditAdjustmentWithoutOverlap(reportImpl, adjustmentDeletePayloadDTO);
        }else{
            List<Map<String,Object>> nonPendingOverlappingEvents = overlappingEvents.stream().filter(overlappingEvent -> !ReportsUtil.isStatusPending(overlappingEvent)).collect(Collectors.toList());
            if(!nonPendingOverlappingEvents.isEmpty()){
                return AdjustmentDeleteHelper.generateCannotDeleteSubsetsResponse(nonPendingOverlappingEvents,adjustmentDeletePayloadDTO.getEntryContactPro().getEmailID(), adjustmentDeletePayloadDTO.getTimezone());
            }
            if(!adjustmentDeletePayloadDTO.isShouldDeleteSubsets()){
                return AdjustmentDeleteHelper.generateOverlappingErrorResponse(overlappingEvents, adjustmentDeletePayloadDTO.getTimezone(), adjustmentDeletePayloadDTO.getDateFormat());
            }
            overlappingEvents.add(adjustmentDeletePayloadDTO.getEventToDelete());
            List<Map<String,Object>> deletedAdjustments = new ArrayList<>(AdjustmentDeleteHelper.deleteEvents(overlappingEvents, adjustmentDeletePayloadDTO, reportImpl));
            List<Map<String,Object>> revertedEntries = new ArrayList<>();
            for(String id : ReportsUtil.getParentIdsListFromEvents(deletedAdjustments)){
                revertedEntries.add(reportImpl.revertEvent(id));
            }
            return AdjustmentDeleteHelper.generateSuccessResponseAndInitiateQueue(deletedAdjustments,revertedEntries,adjustmentDeletePayloadDTO.getEntryContactPro(),adjustmentDeletePayloadDTO.getLoggedInUserPro(), adjustmentDeletePayloadDTO.getTimezone(), adjustmentDeletePayloadDTO.getDateFormat());
        }
    }

    public static List<Map<String,Object>> deleteEvents(List<Map<String,Object>> events, AdjustmentDeletePayloadDTO payloadDTO, ReportImpl reportImpl) throws IOException, NoSuchAlgorithmException {
        if(payloadDTO.isRejectAdjustmentRequest()){
            for(Map<String,Object> event : events){
                AdjustmentRejectHelper.updateEventForRejection(event,payloadDTO.getLoggedInUserPro().getContactId(),payloadDTO.getMessage());
                SchedulingEngineUtil.updateEventReq(event);
            }
        }
        return reportImpl.deleteEvents(ReportsUtil.getIdsListFromEvents(events));
    }

    public static GenericResponse generateCannotDeleteSubsetsResponse(List<Map<String,Object>> nonPendingEvents,String email, String timezone){
        GenericResponse response = new GenericResponse();
        response.setSuccess(false);
        response.setData(Map.of(OVERLAPPING_ENTRIES_KEY,nonPendingEvents.stream().map(event -> new ReportsDTO(event,email,timezone)).collect(Collectors.toList())));
        response.setErrorMessage(CANNOT_DELETE_SUBSETS_MESSAGE);
        return response;
    }

    public static GenericResponse generateOverlappingErrorResponse(List<Map<String, Object>> overlappingEvents, String timeZone, DateFormats dateFormat) throws IOException, NoSuchAlgorithmException {
        GenericResponse response = new GenericResponse();
        response.setSuccess(false);
        List<String> parentIDs = new ArrayList<>();
        List<AdjustmentDTO> adjustmentDTOS = new ArrayList<>();
        List<Map<String,Object>> editAdjustmentEvents = new ArrayList<>();
        for(Map<String,Object> event: overlappingEvents){
            if(ReportsUtil.isAdjustmentNew(event)){
                adjustmentDTOS.add(new AdjustmentDTO(event,new HashMap<>(),timeZone,dateFormat));
            }else{
                parentIDs.add(ReportsUtil.getParentIDFromEvent(event));
                editAdjustmentEvents.add(event);
            }
        }
        if(!editAdjustmentEvents.isEmpty()){
            AdjustmentHelper adjustmentHelper = AdjustmentHelper.getInstance();
            List<Map<String, Object>>parentEventsMapList = adjustmentHelper.getParentEventsUsingListOfParentIDs(parentIDs);
            Map<String, Object> parentIdsMap = adjustmentHelper.constructMapForAllTheParentEventsOfTheList(parentEventsMapList);
            adjustmentDTOS.addAll(editAdjustmentEvents.stream().map(event -> new AdjustmentDTO(event,(Map<String, Object>) parentIdsMap.get(ReportsUtil.getParentIDFromEvent(event)),timeZone,dateFormat)).collect(Collectors.toList()));
        }
        response.setData(Map.of(OVERLAPPING_ENTRIES_KEY,List.of(),OVERLAPPING_ADJUSTMENTS_KEY,adjustmentDTOS));
        response.setErrorMessage(OVERLAPPING_ERROR_MESSAGE);
        return response;
    }

    public static GenericResponse generateSuccessResponseAndInitiateQueue(List<Map<String, Object>> deletedAdjustments, List<Map<String,Object>> revertedEntries, PeopleRelationJDO entryContactPro, PeopleRelationJDO loggedInUserPro, String timezone, DateFormats dateFormats) throws IOException {
        GenericResponse response = new GenericResponse();
        response.setSuccess(true);
        Map<String,Map<String,Object>> parentIdsMap = new HashMap<>();
        List<ReportsDTO> reportsDTOS = new ArrayList<>();
        for(Map<String,Object> event: revertedEntries){
            reportsDTOS.add(new ReportsDTO(event,entryContactPro.getEmailID(),timezone));
            parentIdsMap.put(ReportsUtil.getIDFromEvent(event),event);
        }
        List<AdjustmentDTO> adjustmentDtos = new ArrayList<>();
        for(Map<String,Object> event : deletedAdjustments){
            Map<String,Object> parentEvent = new HashMap<>();
            if(!ReportsUtil.isAdjustmentNew(event)){
                parentEvent = Optional.ofNullable(parentIdsMap.get(ReportsUtil.getParentIDFromEvent(event))).orElse(new HashMap<>());
            }
            adjustmentDtos.add(new AdjustmentDTO(event,parentEvent,timezone, dateFormats));
        }
        response.setData(Map.of(EventConstants.ENTRY,reportsDTOS,ADJUSTMENTS_DELETED_KEY,adjustmentDtos));
        AdjustmentTaskInitiator.initiateDeleteAdjustmentTaskQueue(reportsDTOS,adjustmentDtos,entryContactPro,loggedInUserPro);
        return response;
    }
}
