package com.yoco.adjustment.helper;

import com.google.common.collect.Lists;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.AdjustmentImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.EventConstants;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

@Slf4j
public class AdjustmentHelper {

    public static AdjustmentHelper getInstance(){
        return new AdjustmentHelper();
    }

    public static PeopleRelationJDO getEntryContactPro(PeopleRelationJDO loggedInUserPro, Map<String, Object> event) {
        String entryContactID = ReportsUtil.getContactIDFromEvent(event);
        if(loggedInUserPro.getContactId().equals(entryContactID)){
            return loggedInUserPro;
        }
        return UserPROUtil.getUserProWithContact(loggedInUserPro.getUniquepin(),entryContactID);
    }

    public static PeopleRelationJDO validateAndExtractLoggedInPro(String accountID, Contact loggedInUserContact) {
        PeopleRelationJDO loggedInUserPro = UserPROUtil.getUserPro(accountID, loggedInUserContact.getId());
        Validator.checkArgument(ObjUtils.isNull(loggedInUserPro), COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        loggedInUserPro.setContact(loggedInUserContact);
        return loggedInUserPro;
    }

    public boolean verifyUserSkillSet(String contactID, PeopleRelationJDO userPRO){
        Map<String, Object> skillsInfoMap = JsonUtil.convertJsonToMap(userPRO.getSkillsets());
        boolean canThisUserViewAdjustment;
        if(ObjUtils.isNullOrEmpty(contactID)){
            canThisUserViewAdjustment = checkIfTheUserHasRequiredAdjustmentPermission(skillsInfoMap);
        }else if(!contactID.equals(userPRO.getContactId())){
            canThisUserViewAdjustment = verifyIfThisUserHasRequiredReportsPermission(skillsInfoMap);
        }else{
            canThisUserViewAdjustment = true;
        }
        return canThisUserViewAdjustment;
    }

    public boolean checkIfTheUserHasRequiredAdjustmentPermission(Map<String, Object>skillsInfoMap){
        boolean canThisUserViewAdjustment = false;
        Set<String> adjustmentPermission = new HashSet<>();
        if(!ObjUtils.isNullOrEmpty(skillsInfoMap)){
            adjustmentPermission = new PermissionManager().getAdjustmentPermission(skillsInfoMap, adjustmentPermission);
            if(!adjustmentPermission.isEmpty()){
                canThisUserViewAdjustment = true;
            }
        }
        return canThisUserViewAdjustment;
    }

    public boolean verifyIfThisUserHasRequiredReportsPermission(Map<String, Object>skillsInfoMap){
        boolean canThisUserViewAdjustment = false;
        Set<String> adjustmentPermission = new HashSet<>();
        if(!ObjUtils.isNullOrEmpty(skillsInfoMap)){
            adjustmentPermission = new PermissionManager().getReportPermission(skillsInfoMap, adjustmentPermission);
            if(!adjustmentPermission.isEmpty()){
                canThisUserViewAdjustment = true;
            }
        }

        return canThisUserViewAdjustment;
    }

    public List<AdjustmentDTO> convertEventListIntoAdjustmentList(List<Map<String, Object>> eventList, String timeZone, String isThisRequestFromHoursPage) throws IOException, NoSuchAlgorithmException {
        List<String>listOfParentIDs = getListOfParentIDs(eventList);
        List<Map<String, Object>>parentEventsMapList = getParentEventsUsingListOfParentIDs(listOfParentIDs);
        Map<String, Object> parentIdsMap = constructMapForAllTheParentEventsOfTheList(parentEventsMapList);
        return convertAllEventsIntoAdjustmentDTO(eventList, parentIdsMap, timeZone, isThisRequestFromHoursPage);
    }

    public Map<String, Object> constructMapForAllTheParentEventsOfTheList(List<Map<String, Object>>parentEventsMapList){
        Map<String, Object>parentIdsMap = new HashMap<>();
        for(Map<String, Object> parentEventsMap : parentEventsMapList){
            List<Map<String, Object>>parentEntryList = (List<Map<String, Object>>) parentEventsMap.get(SchedulingKeys.EVENTS);
            parentIdsMap = constructAnHashMapRelatingEachParentIdsWithItsEvents(parentEntryList, parentIdsMap);
        }
        return parentIdsMap;
    }

    public List<String> getListOfParentIDs(List<Map<String, Object>> eventList){
        List<String>listOfParentIDs =  new ArrayList<>();
        for(var eventMap : eventList){
            if(!ObjUtils.isNullOrEmpty((String) eventMap.get(SchedulingKeys.PARENT_ID))){
                listOfParentIDs.add((String) eventMap.get(SchedulingKeys.PARENT_ID));
            }
        }
        return listOfParentIDs;
    }

    public List<Map<String, Object>> getParentEventsUsingListOfParentIDs(List<String>listOfParentIDs) throws IOException, NoSuchAlgorithmException {
        int limit = 100;
        List<Map<String, Object>>parentEventsMapList = new ArrayList<>();
        List<List<String>> dividedTemporaryListsOfParentIds = Lists.partition(listOfParentIDs, limit);
        for(var temporaryParentIDsList : dividedTemporaryListsOfParentIds){
            parentEventsMapList.add(new ReportImpl().getEventsByIds(temporaryParentIDsList));
        }
        return parentEventsMapList;
    }

    public Map<String, Object> constructAnHashMapRelatingEachParentIdsWithItsEvents(List<Map<String, Object>>parentEntryList, Map<String, Object>parentIdsMap){
        for(var parentEventMap : parentEntryList){
            parentIdsMap.put((String) parentEventMap.get(SchedulingKeys.ID), parentEventMap);
        }
        return parentIdsMap;
    }

    public List<AdjustmentDTO> convertAllEventsIntoAdjustmentDTO(List<Map<String, Object>> eventList, Map<String, Object> parentIdsMap,
                                                                 String timeZone, String isThisRequestFromHoursPage){
        List<AdjustmentDTO> convertedList = new ArrayList<>();
        Map<String, Object> parentEventMap = new HashMap<>();
        for(var eventMap : eventList){
            if(!ObjUtils.isNullOrEmpty((String) eventMap.get(SchedulingKeys.PARENT_ID))){
                String parentId = (String) eventMap.get(SchedulingKeys.PARENT_ID);
                parentEventMap = (Map<String, Object>) parentIdsMap.get(parentId);
            }
            DateFormats dateFormat = getAppropriateDateFormatBasedOnTheRequest(isThisRequestFromHoursPage);
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO(eventMap, parentEventMap, timeZone, dateFormat);
            convertedList.add(adjustmentDTO);
            parentEventMap.clear();
        }
        return convertedList;
    }

    public DateFormats getAppropriateDateFormatBasedOnTheRequest(String isThisRequestFromHoursPage) {
        return !ObjUtils.isNullOrEmpty(isThisRequestFromHoursPage) ? DateFormats.DD_MMM_YYYY_HH_MM_SS_A : DateFormats.YYYY_MM_DD_T_HH_MM_SS_A_Z;
    }

    public Map<String, Object> getAdjustmentsBasedOnStatus(String accountID, String contactID, String startDate, String endDate, String status, String cursor) throws IOException, NoSuchAlgorithmException {
        Map<String, Object>adjustmentMap = new HashMap<>();
        List<Map<String, Object>> temporaryEventList;
        List<Map<String, Object>> eventList = new ArrayList<>();
        if(status.equalsIgnoreCase("PENDING,APPROVED")){
            String [] adjustmentStatuses = status.split(",");
            for(var adjustmentStatus : adjustmentStatuses){
                Map<String, Object>eventListMap =  new AdjustmentImpl().getAllAdjustments(accountID, contactID, startDate, endDate, adjustmentStatus, cursor);
                temporaryEventList = (List<Map<String, Object>>) eventListMap.get(EventConstants.EVENT_LIST);
                eventList.addAll(temporaryEventList);
            }
            adjustmentMap.put(EventConstants.EVENT_LIST, eventList);
        }else{
            adjustmentMap = new AdjustmentImpl().getAllAdjustments(accountID, contactID, startDate, endDate, status, cursor);
        }
        return adjustmentMap;
    }
}

