package com.yoco.hours.helper;

import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.InAppReminderUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.EventConstants;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;

@Slf4j
public class EntryDeleteHelper {
    private EntryDeleteHelper(){}

    private static final String ENTRY_DELETED = "entry_deleted";
    public static final String ENTRY_DELETE_CHANNEL_KEY = "originalEntryOrNewAdjDel";
    public static final String USER_CLOCK_IN_JDO_KEY = "userClockInJDO";
    private static final String ENTRY_DELETE_AW_KEY = "DELETE_ENTRY";

    public static Map<String,Object> validateAndDeleteEntry(String entryID, PeopleRelationJDO loggedInUserPro) throws IOException, NoSuchAlgorithmException {
        var reportImpl = new ReportImpl();
        Map<String,Object> eventMap = reportImpl.getEntryByID(entryID);
        Validator.checkArgument(ObjUtils.isNullOrEmpty(eventMap), COMMON_ERROR_RESPONSE.INVALID_ENTRY_ID.value());
        Validator.checkArgument(!EntryDeleteHelper.isUserAuthorizedForEntryDeletion(eventMap,loggedInUserPro),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        Validator.checkArgument(!EntryDeleteHelper.isEntryAllowedForDeletion(eventMap,loggedInUserPro), EVENTS_ERROR_RESPONSE.UNABLE_TO_DELETE.value());
        List<Map<String,Object>> response = reportImpl.deleteEvents(List.of(entryID));
        Validator.checkArgument(ObjUtils.isNullOrEmpty(response), EVENTS_ERROR_RESPONSE.OPERATION_FAILED.value());
        return response.get(0);
    }

    public static boolean isUserAuthorizedForEntryDeletion(Map<String,Object> eventMap, PeopleRelationJDO loggedInUserPro){
        String entryContactID = ReportsUtil.getContactIDFromEvent(eventMap);
        String entryAccountID = ReportsUtil.getAccountIDFromEvent(eventMap);
        return entryAccountID.equalsIgnoreCase(loggedInUserPro.getUniquepin()) &&
                (loggedInUserPro.getContactId().equals(entryContactID) || UserPROUtil.isPrimaryAdmin(loggedInUserPro));
    }

    public static boolean isEntryAllowedForDeletion(Map<String,Object> eventMap, PeopleRelationJDO loggedInUserPro){
        if(ReportsUtil.isEventDeleted(eventMap) || !ReportsUtil.isStatusManualClockedOut(eventMap)){
            return false;
        }
        if(ReportsUtil.isPayrollStatusDefaultPayroll(eventMap)){
            return true;
        }
        return ReportsUtil.isPayrollStatusUserAcknowledged(eventMap) && UserPROUtil.isPrimaryAdmin(loggedInUserPro);
    }

    public static void handleEntryDeletion(PeopleRelationJDO loggedInUserPro, Contact entryContact, Map<String, Object> deletedEventMap) throws IOException, NoSuchAlgorithmException {
        EntryDeleteHelper.createEntryDeletionActivity(loggedInUserPro,entryContact,deletedEventMap);
        PeopleRelationJDO entryContactUserPro = UserPROUtil.getUserPro(loggedInUserPro.getUniquepin(),entryContact.getId());
        ReportsDTO reportsDTO = new ReportsDTO(deletedEventMap,entryContact.getEmailID(), entryContactUserPro.getTimeZone());
        RTMService.publishToChannel(loggedInUserPro.getUniquepin(),ENTRY_DELETE_CHANNEL_KEY,USER_CLOCK_IN_JDO_KEY,reportsDTO);
        RTMService.publishToAW(loggedInUserPro.getUniquepin(),entryContact.getId(),ENTRY_DELETE_AW_KEY, EventConstants.ENTRY,reportsDTO);
        if(DateUtil.isCurrentDay(reportsDTO.getInTime(),entryContactUserPro.getTimeZone())){
            new InAppReminderUtil().handleInAppNotificationForReminderTimeChange(new UserDTO(entryContactUserPro,null),UserPROUtil.extractPROSkills(entryContactUserPro));
        }
    }

    public static void createEntryDeletionActivity(PeopleRelationJDO loggedInUserPro, Contact entryContact, Map<String,Object> deletedEventMap) {
        String entryID = ReportsUtil.getIDFromEvent(deletedEventMap);
        String activity = "EntryID : " + entryID + " deleted by " + loggedInUserPro.getContactId();
        long receivedLongTime = ReportsUtil.getUpdatedTimeFromEvent(deletedEventMap);
        ActivityUtil.saveActivity(loggedInUserPro.getUniquepin(),entryContact.getId(),ENTRY_DELETED,entryContact.getEmailID(),activity,ActivityUtil.ACTIVITIES.DUMMY.value(),receivedLongTime);
    }
}
