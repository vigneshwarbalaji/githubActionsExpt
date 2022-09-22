package com.yoco.hours.service;

import com.yoco.commons.constants.*;
import com.yoco.commons.dataservices.dao.ReportDao;
import com.yoco.commons.dataservices.dao.TaskDao;
import com.yoco.commons.dataservices.dao.UserDao;
import com.yoco.commons.dataservices.impl.*;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.entity.TaskJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.services.FCMService;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.HashUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import com.yoco.commons.validations.RangeValidator;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.EventConstants;
import com.yoco.hours.helper.EntryDeleteHelper;
import com.yoco.hours.helper.HoursTaskInitiator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
public class HourService {

    private static final String PROJECTID = "projectID";

    public Map<String, Object> getActiveEntry(String accountID, Contact loggedInUserContact, String contactID) throws IOException, NoSuchAlgorithmException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNull(loggedInUserContact), COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value());

        contactID = ObjUtils.isNullOrEmpty(contactID) ? loggedInUserContact.getId() : contactID;
        List<Map<String, Object>> activeEntriesList = new ReportImpl().getActiveEntry(accountID, contactID);

        HashMap<String, Object> responseMap = new HashMap<>();

        if(!activeEntriesList.isEmpty()) {
            UserDao objUserDao = new UserImpl();
            PeopleRelationJDO loggedInUserPRO = objUserDao.getUserWithoutContact(accountID, loggedInUserContact.getId());
            PeopleRelationJDO userPRO = loggedInUserPRO;

            if(!contactID.equalsIgnoreCase(loggedInUserContact.getId()))
                userPRO =  objUserDao.getUserWithoutContact(accountID, contactID);

            ReportsDTO convertedEvent = new ReportsDTO(activeEntriesList.get(0), userPRO.getEmailID(), loggedInUserPRO.getTimeZone());
            responseMap.put(EventConstants.ENTRY, convertedEvent);

        }
        return responseMap;
    }

    public Map<String, Object> getTimerInfo(String accountID, Contact loggedInUserContact, String userAgent) throws IOException, NoSuchAlgorithmException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNull(loggedInUserContact), COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value());

        PeopleRelationJDO userPro = new UserImpl().getUserWithoutContact(accountID, loggedInUserContact.getId());

        var rangeInfoDTO =  DateUtil.getRangeDetails(userPro.getTimeZone(),DateConstants.TODAY, "", "",accountID);

        var startDate= DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getFromDateEpochMilliseconds(), userPro.getTimeZone());
        var endDate= DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getToDateEpochMilliseconds(), userPro.getTimeZone());

        ReportDao objReportDao = new ReportImpl();

        HashMap<String, Object> response  = new HashMap<>();

        List<Map<String, Object>> entries = objReportDao.getEntries(accountID, userPro.getContactId(), startDate, endDate, SchedulingKeys.DESC, false);

        if(entries.isEmpty()) {

            List<Map<String, Object>> activeEntriesList = new ReportImpl().getActiveEntry(accountID, userPro.getContactId());

            if (!activeEntriesList.isEmpty()) {

                Map<String, Object> runningEntry = activeEntriesList.get(0);

                String entryDate = DateUtil.convertMillisToDateTimeText(DateFormats.MM_DD_YYYY, (long) runningEntry.get("startTime"), userPro.getTimeZone());

                rangeInfoDTO = DateUtil.getRangeDetails(userPro.getTimeZone(), DateConstants.BY_DATE, entryDate, entryDate, accountID);

                startDate = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getFromDateEpochMilliseconds(), userPro.getTimeZone());
                endDate = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getToDateEpochMilliseconds(), userPro.getTimeZone());

                entries = objReportDao.getEntries(accountID, userPro.getContactId(), startDate, endDate, SchedulingKeys.DESC, false);

            }
        }

        if(ObjUtils.isNullOrEmpty(userAgent)){ // for mobile team purpose

            Map<String, Object> lastEntry = objReportDao.getLastClockedOutEntry(accountID, userPro.getContactId());

            response.put("last_ClockedIn_Prj_Info", null);

            if(!ObjUtils.isNullOrEmpty(lastEntry)) {
                ReportsDTO lastEvent = new ReportsDTO(lastEntry, userPro.getEmailID(), userPro.getTimeZone());
                if( !ObjUtils.isNull(lastEvent) && !ObjUtils.isNullOrEmpty(lastEvent.getProjectID())) {
                    HashMap<String, Object> projectInfo = new HashMap<>();
                    projectInfo.put(HourService.PROJECTID, lastEvent.getProjectID());
                    projectInfo.put("projectName", lastEvent.getProjectName());
                    response.put("last_ClockedIn_Prj_Info", projectInfo);
                }
            }
        }

        if(!entries.isEmpty()) {

            List<ReportsDTO> convertedEntries = entries.stream().map(event -> new ReportsDTO(event, userPro.getEmailID(), userPro.getTimeZone())).collect(Collectors.toList());
            response.put(SchedulingKeys.ENTRIES, convertedEntries);
        }

        return response;
    }

    public Map<String, Object> updateEntry(String accountID, String entryID, Contact loggedInUserContact, Map<String, Object> payloadMap, String clientID) throws IOException, NoSuchAlgorithmException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(entryID), COMMON_ERROR_RESPONSE.INVALID_ENTRY_ID.value());
        Validator.checkArgument(ObjUtils.isNull(loggedInUserContact), COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value());

        HashMap<String, Object> responseMap = new HashMap<>();

        Validator.checkArgument( !(payloadMap.containsKey(SchedulingKeys.TASK_DESC) || payloadMap.containsKey(SchedulingKeys.PROJECT_ID)),
                COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value());

        Map<String, Object> entry = new ReportImpl().getEntryByID(entryID);
        if(!ObjUtils.isNullOrEmpty(entry)){
            String updateType = "";

            if(payloadMap.containsKey(SchedulingKeys.PROJECT_ID)){
                var projID = !ObjUtils.isNullOrEmpty(payloadMap.get(SchedulingKeys.PROJECT_ID).toString())? payloadMap.get(SchedulingKeys.PROJECT_ID).toString() : accountID;
                entry.put(SchedulingKeys.CALENDAR, projID);
                entry = SchedulingEngineUtil.updateEventReq(entry);
                updateType = "PROJECT_UPDATE";
            } else if(payloadMap.containsKey(SchedulingKeys.TASK_DESC)) {
                var taskDesc = Validator.sanitizeText((String) payloadMap.get(SchedulingKeys.TASK_DESC)).trim();
                entry = updateTask(entry, taskDesc, clientID);
                entry = SchedulingEngineUtil.updateEventReq(entry);
                updateType = "TASK_UPDATE";
            }
            PeopleRelationJDO objUser = new UserImpl().getUserWithoutContact(accountID, loggedInUserContact.getId());

            ReportsDTO objReportDTO = new ReportsDTO((HashMap<String, Object>)entry.get(SchedulingKeys.DATA), loggedInUserContact.getEmailID(), objUser.getTimeZone());

            Set<String> contactID = new HashSet<>();
            contactID.add(loggedInUserContact.getId());

            new FCMService().notifyFCM( accountID, contactID, FCMService.FCM_SERVICE_CONSTANTS.FCM_CLOCK_IN_OUT.value(), false, null , clientID);
            RTMService.publishToAW(accountID, loggedInUserContact.getId(), updateType, SchedulingKeys.ENTRY, objReportDTO);
            RTMService.publishToChannel(accountID, updateType.toLowerCase(), "userClockInJDOJson", objReportDTO);

            responseMap.put(SchedulingKeys.ENTRY,objReportDTO);
        }

        return responseMap;
    }

    public Map<String, Object> updateTask( Map<String, Object> entry, String taskDesc, String clientID) {

        var taskID = "";
        var taskSource = "";
        taskDesc = ObjUtils.isNullOrEmpty(taskDesc) ? "" : taskDesc.trim();

        TaskDao objTaskDao = new TaskImpl();

        var entryID = entry.get(SchedulingKeys.ID).toString();
        var taskJDO = objTaskDao.getEntryByID(entryID);

        if(ObjUtils.isNullOrEmpty(taskDesc)) {
            if(!ObjUtils.isNull(taskJDO)){
                objTaskDao.deleteTask(taskJDO);
            }
        } else {
            taskID = HashUtil.generateUUID();
            taskSource = ClientSource.getClientIdMap().get(clientID);
            TaskJDO objTask = new TaskJDO(entryID, taskID, taskDesc, taskSource);
            objTaskDao.saveTask(objTask);
        }

        setMetaTask(entry, taskID, taskSource);
        setQueryTask(entry, taskID, taskSource);

        return entry;
    }

    public Map<String, Object> getDatesForHoursPage(String accountID, Contact loggedInUserContact, String range, String from, String to) {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNull(loggedInUserContact), COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value());
        RangeValidator.validateRange(range,from,to);

        Map<String, Object> responseMap = new HashMap<>();

        PeopleRelationJDO userPRO = new UserImpl().getUserWithoutContact(accountID, loggedInUserContact.getId());
        SettingsJDO accountInfo = new AccountImpl().getById(accountID);

        List<String> formattedDateList = new ArrayList<>();

        List<HashMap<Long,HashMap<String,String>>> datesList = DateUtil.getDatesList(userPRO.getTimeZone(),range,from,to,"", accountInfo.getWeekStartDay());

       for( int ind=0; ind< datesList.size(); ind++){
           HashMap dateMap = datesList.get(ind);
           formattedDateList.add(dateMap.get("milliSeconds")+"~"+dateMap.get("year")+"-"+dateMap.get("monthValue")+"-"+dateMap.get("day"));
       }
        responseMap.put("dateList", formattedDateList);
        return responseMap;
    }

    public Map<String, Object> getAll (String accountID, Contact loggedInUserContact, Map<String, String> payload) throws IOException, NoSuchAlgorithmException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNull(loggedInUserContact), COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());

        String contactID = !ObjUtils.isNullOrEmpty(payload.get("contactID")) ? payload.get("contactID") : loggedInUserContact.getId();

        PeopleRelationJDO userPRO = new UserImpl().getUserWithoutContact(accountID, contactID);
        Validator.checkArgument(ObjUtils.isNull(userPRO), COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());

        String range = !ObjUtils.isNullOrEmpty(payload.get("range")) ? payload.get("range") : "";

        String projectID = !ObjUtils.isNullOrEmpty(payload.get(HourService.PROJECTID)) ? payload.get(HourService.PROJECTID) : "";
        String timeZoneID = !ObjUtils.isNullOrEmpty(payload.get("zone")) ? payload.get("zone") : userPRO.getTimeZone();
        String from = !ObjUtils.isNullOrEmpty(payload.get("from")) ? payload.get("from") : "";
        String to = !ObjUtils.isNullOrEmpty(payload.get("to")) ? payload.get("to") : "";

        HashMap<String, Object> responseMap = new HashMap<>();

        RangeValidator.validateRange(range, from, to);

        var rangeInfoDTO =  DateUtil.getRangeDetails(timeZoneID, range, from, to,accountID);
        List<String> projectIDs = !ObjUtils.isNullOrEmpty(projectID) ? Arrays.asList(projectID.split(",")) : new ArrayList<>();

        var startDate= DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getFromDateEpochMilliseconds(), timeZoneID);
        var endDate= DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getToDateEpochMilliseconds(),timeZoneID);
        ReportDao objReportDao = new ReportImpl();
        List<Map<String, Object>> entries = objReportDao.getEntries(accountID, contactID, startDate, endDate, SchedulingKeys.DESC, false);
        if(!entries.isEmpty()) {
            if(!projectIDs.isEmpty()){
                entries = entries.stream().filter(i->projectIDs.contains(i.get(SchedulingKeys.CALENDAR))).collect(Collectors.toList());
            }
            List<ReportsDTO> convertedEntries = entries.stream().map(event -> new ReportsDTO(event, userPRO.getEmailID(), timeZoneID)).collect(Collectors.toList());
            responseMap.put(SchedulingKeys.ENTRIES, convertedEntries);

        }

        return responseMap;

    }

    private Map<String, Object> setMetaTask ( Map<String, Object> entry, String taskID, String taskSource) {

        if(entry.containsKey(SchedulingKeys.METADATA)){
            HashMap<String, Object> metaMap = (HashMap<String, Object>) entry.get(SchedulingKeys.METADATA);
            if(ObjUtils.isNullOrEmpty(taskID) && metaMap.containsKey(SchedulingKeys.TASK_ID)){
                metaMap.remove(SchedulingKeys.TASK_ID);
            }else{
                metaMap.put(SchedulingKeys.TASK_ID, taskID);
            }
            if(ObjUtils.isNullOrEmpty(taskSource) && metaMap.containsKey(SchedulingKeys.TASK_SOURCE)){
                metaMap.remove(SchedulingKeys.TASK_SOURCE);
            }else{
                metaMap.put(SchedulingKeys.TASK_SOURCE, taskSource);
            }
            entry.put(SchedulingKeys.METADATA, metaMap);
        }

        return entry;
    }

    private Map<String, Object> setQueryTask ( Map<String, Object> entry, String taskID, String taskSource ) {

        if(entry.containsKey(SchedulingKeys.QUERYABLE_META)) {
            HashMap<String, Object> queryMap = (HashMap<String, Object>) entry.get(SchedulingKeys.QUERYABLE_META);
            if(ObjUtils.isNullOrEmpty(taskID) && queryMap.containsKey(SchedulingKeys.TASK_ID)){
                queryMap.remove(SchedulingKeys.TASK_ID);
            }else{
                queryMap.put(SchedulingKeys.TASK_ID, taskID);
            }
            if(ObjUtils.isNullOrEmpty(taskSource) && queryMap.containsKey(SchedulingKeys.TASK_SOURCE)){
                queryMap.remove(SchedulingKeys.TASK_SOURCE);
            }else{
                queryMap.put(SchedulingKeys.TASK_SOURCE, taskSource);
            }
            entry.put(SchedulingKeys.QUERYABLE_META, queryMap);
        }
        return entry;
    }

    public Map<String,Object> deleteEntry(String accountID, String entryID, Contact loggedInContact, String outputTimezone) throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO loggedInUserPro = UserPROUtil.getUserPro(accountID,loggedInContact.getId());
        Validator.checkArgument(ObjUtils.isNull(loggedInUserPro),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        Map<String,Object> deletedEventMap = EntryDeleteHelper.validateAndDeleteEntry(entryID,loggedInUserPro);
        outputTimezone = Validator.isValidTimeZone(outputTimezone) ? outputTimezone : loggedInUserPro.getTimeZone();
        String entryContactID = ReportsUtil.getContactIDFromEvent(deletedEventMap);
        Contact entryContact = new ContactImpl().getByID(entryContactID);
        HoursTaskInitiator.initiateDeleteEntryQueue(loggedInUserPro,entryContact,deletedEventMap);
        return Map.of(EventConstants.ENTRY,new ReportsDTO(deletedEventMap, entryContact.getEmailID(),outputTimezone));
    }
}
