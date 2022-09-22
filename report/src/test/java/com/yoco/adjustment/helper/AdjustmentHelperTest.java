package com.yoco.adjustment.helper;

import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.AdjustmentImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.constants.EventConstants;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import static org.mockito.ArgumentMatchers.*;

class AdjustmentHelperTest {
    @Test
    void verifyUserSkillSet_ifContactIDisEmptyOrNull_test(){
        Set<String> permissions = new HashSet<>();
        permissions.add("HasAdjustmentPermission");

        Map<String, Object> skillsInfoMap = new HashMap<>();
        skillsInfoMap.put("skills", "adjustmentSkills");
        String jsonMap = JsonUtil.getJson(skillsInfoMap);

        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setSkillsets(jsonMap);

        try(MockedConstruction<PermissionManager> mock = Mockito.mockConstruction(PermissionManager.class, (permissionManagerMock, context) -> {
            Mockito.when(permissionManagerMock.getAdjustmentPermission(anyMap(), anySet())).thenReturn(permissions);
        })){
            boolean canUserViewAdjustment = AdjustmentHelper.getInstance().verifyUserSkillSet("",userPro);
            Assertions.assertTrue(canUserViewAdjustment);
        }
    }

    @Test
    void verifyUserSkillSet_ifContactIDAndUserProContactIdAreNotEqual_test(){
        Set<String> permissions = new HashSet<>();
        permissions.add("HasReportsPermission");

        Map<String, Object> skillsInfoMap = new HashMap<>();
        skillsInfoMap.put("skills", "reportsSkills");
        String jsonMap = JsonUtil.getJson(skillsInfoMap);

        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("contactID");
        userPro.setSkillsets(jsonMap);

        try(MockedConstruction<PermissionManager> mock = Mockito.mockConstruction(PermissionManager.class, (permissionManagerMock, context) -> {
            Mockito.when(permissionManagerMock.getReportPermission(anyMap(), anySet())).thenReturn(permissions);
        })){
            boolean canUserViewAdjustment = AdjustmentHelper.getInstance().verifyUserSkillSet("123",userPro);
            Assertions.assertTrue(canUserViewAdjustment);
        }
    }

    @Test
    void verifyUserSkillSet_ifContactIDAndUserProContactIdAreEqual_test(){
        Set<String> permissions = new HashSet<>();
        permissions.add("HasHoursPermission");

        Map<String, Object> skillsInfoMap = new HashMap<>();
        skillsInfoMap.put("skills", "hoursSkills");
        String jsonMap = JsonUtil.getJson(skillsInfoMap);

        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("123");
        userPro.setSkillsets(jsonMap);

        boolean canUserViewAdjustment = AdjustmentHelper.getInstance().verifyUserSkillSet("123",userPro);
        Assertions.assertTrue(canUserViewAdjustment);
    }

    @Test
    void checkIfTheUserHasRequiredAdjustmentPermission_ifSkillsInfoMapIsEmpty_test(){
        Map<String, Object> skillsInfoMap = new HashMap<>();
        Assertions.assertFalse(AdjustmentHelper.getInstance().checkIfTheUserHasRequiredAdjustmentPermission(skillsInfoMap));
    }

    @Test
    void checkIfTheUserHasRequiredAdjustmentPermission_ifUserHasNoPermission_test(){
        Set<String> permissions = new HashSet<>();
        Map<String, Object> skillsInfoMap = new HashMap<>();
        skillsInfoMap.put("skills", "noAdjustmentSkills");
        try(MockedConstruction<PermissionManager> mock = Mockito.mockConstruction(PermissionManager.class, (permissionManagerMock, context) -> {
            Mockito.when(permissionManagerMock.getAdjustmentPermission(anyMap(), anySet())).thenReturn(permissions);
        })){
            Assertions.assertFalse(AdjustmentHelper.getInstance().checkIfTheUserHasRequiredAdjustmentPermission(skillsInfoMap));
        }
    }

    @Test
    void checkIfTheUserHasRequiredAdjustmentPermission_ifUserHasAdjustmentPermission_test(){
        Set<String> permissions = new HashSet<>();
        permissions.add("HasPermission");
        Map<String, Object> skillsInfoMap = new HashMap<>();
        skillsInfoMap.put("skills", "adjustmentSkills");
        try(MockedConstruction<PermissionManager> mock = Mockito.mockConstruction(PermissionManager.class, (permissionManagerMock, context) -> {
            Mockito.when(permissionManagerMock.getAdjustmentPermission(anyMap(), anySet())).thenReturn(permissions);
        })){
            Assertions.assertTrue(AdjustmentHelper.getInstance().checkIfTheUserHasRequiredAdjustmentPermission(skillsInfoMap));
        }
    }

    @Test
    void verifyIfThisUserHasRequiredReportsPermission_ifSkillsInfoMapIsEmpty_test(){
        Map<String, Object> skillsInfoMap = new HashMap<>();
        Assertions.assertFalse(AdjustmentHelper.getInstance().verifyIfThisUserHasRequiredReportsPermission(skillsInfoMap));
    }

    @Test
    void verifyIfThisUserHasRequiredReportsPermission_ifUserHasNoPermission_test(){
        Set<String> permissions = new HashSet<>();
        Map<String, Object> skillsInfoMap = new HashMap<>();
        skillsInfoMap.put("skills", "noReportsSkills");
        try(MockedConstruction<PermissionManager> mock = Mockito.mockConstruction(PermissionManager.class, (permissionManagerMock, context) -> {
            Mockito.when(permissionManagerMock.getAdjustmentPermission(anyMap(), anySet())).thenReturn(permissions);
        })){
            Assertions.assertFalse(AdjustmentHelper.getInstance().verifyIfThisUserHasRequiredReportsPermission(skillsInfoMap));
        }
    }

    @Test
    void verifyIfThisUserHasRequiredReportsPermission_ifUserHasReportsPermission_test(){
        Set<String> permissions = new HashSet<>();
        permissions.add("HasPermission");
        Map<String, Object> skillsInfoMap = new HashMap<>();
        skillsInfoMap.put("skills", "reportsSkills");
        try(MockedConstruction<PermissionManager> mock = Mockito.mockConstruction(PermissionManager.class, (permissionManagerMock, context) -> {
            Mockito.when(permissionManagerMock.getReportPermission(anyMap(), anySet())).thenReturn(permissions);
        })){
            Assertions.assertTrue(AdjustmentHelper.getInstance().verifyIfThisUserHasRequiredReportsPermission(skillsInfoMap));
        }
    }

    @Test
    void convertEventListIntoAdjustmentList_valid_test() throws IOException, NoSuchAlgorithmException {
        List<String> contactIdList = new ArrayList<>();
        contactIdList.add("234");
        Map<String, Object>parentEventMap = new HashMap<>();
        parentEventMap.put(SchedulingKeys.ID, "id");
        List<Map<String, Object>> parentEventMapList = new ArrayList<>();
        parentEventMapList.add(parentEventMap);
        Map<String, Object>eventMap = new HashMap<>();
        eventMap.put(SchedulingKeys.ID,"id");
        eventMap.put(SchedulingKeys.EVENTS, parentEventMapList);
        eventMap.put(SchedulingKeys.MERCHANT, "merchantID");
        eventMap.put(SchedulingKeys.IS_DELETED, false);
        eventMap.put(SchedulingKeys.PROVIDER, contactIdList);
        eventMap.put(SchedulingKeys.START_TIME, 122L);
        eventMap.put(SchedulingKeys.END_TIME, 233L);
        eventMap.put(SchedulingKeys.PAYMENT_STATUS, "PENDING");
        List<Map<String, Object>> eventList = new ArrayList<>();
        eventList.add(eventMap);
        try(MockedConstruction<ReportImpl> mock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getEventsByIds(anyList())).thenReturn(eventMap);
        })){
            List<AdjustmentDTO>adjustmentConvertedList = AdjustmentHelper.getInstance().convertEventListIntoAdjustmentList(eventList,"Asia/kolkata", "");
            Assertions.assertEquals("id",adjustmentConvertedList.get(0).getId());
        }
    }

    @Test
    void constructMapForAllTheParentEventsOfTheList_valid_test(){
        List<Map<String, Object>>parentEntryList = new ArrayList<>();
        Map<String, Object>parentEventMap = new HashMap<>();
        parentEventMap.put(SchedulingKeys.ID, "id");
        parentEntryList.add(parentEventMap);
        List<Map<String, Object>>parentEventsMapList = new ArrayList<>();
        Map<String, Object>events = new HashMap<>();
        events.put(SchedulingKeys.EVENTS, parentEntryList);
        parentEventsMapList.add(events);
        Map<String, Object>parentIdsMap = AdjustmentHelper.getInstance().constructMapForAllTheParentEventsOfTheList(parentEventsMapList);
        Assertions.assertEquals(parentEventMap, parentIdsMap.get("id"));
    }

    @Test
    void getListOfParentIDs_eventListIsEmpty_test(){
        List<Map<String, Object>> eventList = new ArrayList<>();
        List<String>parentIdsList = AdjustmentHelper.getInstance().getListOfParentIDs(eventList);
        Assertions.assertTrue(parentIdsList.isEmpty());
    }

    @Test
    void getListOfParentIDs_valid_test(){
        Map<String, Object>eventMap = new HashMap<>();
        eventMap.put(SchedulingKeys.PARENT_ID, "parentID");
        Map<String, Object>eventMap2 = new HashMap<>();
        eventMap2.put(SchedulingKeys.PARENT_ID, "");
        List<Map<String, Object>> eventList = new ArrayList<>();
        eventList.add(eventMap);
        eventList.add(eventMap2);
        List<String>parentIdsList = AdjustmentHelper.getInstance().getListOfParentIDs(eventList);
        Assertions.assertEquals("parentID", parentIdsList.get(0));
    }

    @Test
    void getParentEventsUsingListOfParentIDs_valid_test() throws IOException, NoSuchAlgorithmException {
        List<String>parentIdsList = new ArrayList<>();
        parentIdsList.add("parentIds");
        Map<String, Object> eventMap = new HashMap<>();
        eventMap.put("event", "event");
        try(MockedConstruction<ReportImpl> mock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getEventsByIds(anyList())).thenReturn(eventMap);
        })){
            List<Map<String, Object>>parentEventsMapList = AdjustmentHelper.getInstance().getParentEventsUsingListOfParentIDs(parentIdsList);
            Assertions.assertEquals("event", parentEventsMapList.get(0).get("event"));
        }
    }

    @Test
    void constructAnHashMapRelatingEachParentIdsWithItsEvents_valid_test() {
        Map<String, Object>parentEventMap = new HashMap<>();
        parentEventMap.put(SchedulingKeys.ID, "123");
        parentEventMap.put(SchedulingKeys.EVENT, "event");
        List<Map<String, Object>>parentEntryList = new ArrayList<>();
        parentEntryList.add(parentEventMap);
        Map<String, Object>parentIdsMap = AdjustmentHelper.getInstance().
                                          constructAnHashMapRelatingEachParentIdsWithItsEvents(parentEntryList, new HashMap<>());
        Assertions.assertEquals(parentEventMap, parentIdsMap.get("123"));
    }

    @Test
    void convertAllEventsIntoAdjustmentDTO_ifThereIsNoParentID_test() {
        List<String> contactIdList = new ArrayList<>();
        contactIdList.add("234");
        Map<String, Object>eventMap = new HashMap<>();
        eventMap.put(SchedulingKeys.ID, "id");
        eventMap.put(SchedulingKeys.MERCHANT, "merchantID");
        eventMap.put(SchedulingKeys.IS_DELETED, false);
        eventMap.put(SchedulingKeys.PROVIDER, contactIdList);
        eventMap.put(SchedulingKeys.START_TIME, 122L);
        eventMap.put(SchedulingKeys.END_TIME, 233L);
        eventMap.put(SchedulingKeys.PAYMENT_STATUS, "PENDING");
        List<Map<String, Object>> eventList = new ArrayList<>();
        eventList.add(eventMap);
        List<AdjustmentDTO> adjustmentList = AdjustmentHelper.getInstance().convertAllEventsIntoAdjustmentDTO(eventList, new HashMap<>(), "Asia/Kolkata", "");
        Assertions.assertEquals("id", adjustmentList.get(0).getId());
    }

    @Test
    void convertAllEventsIntoAdjustmentDTO_valid_test() {
        List<String> contactIdList = new ArrayList<>();
        contactIdList.add("233");
        Map<String, Object>eventMap = new HashMap<>();
        eventMap.put(SchedulingKeys.PARENT_ID, "parentID");
        eventMap.put(SchedulingKeys.ID, "id");
        eventMap.put(SchedulingKeys.MERCHANT, "merchant");
        eventMap.put(SchedulingKeys.IS_DELETED, false);
        eventMap.put(SchedulingKeys.PROVIDER, contactIdList);
        eventMap.put(SchedulingKeys.START_TIME, 111L);
        eventMap.put(SchedulingKeys.END_TIME, 222L);
        eventMap.put(SchedulingKeys.PAYMENT_STATUS, "PENDING");
        List<Map<String, Object>> eventList = new ArrayList<>();
        eventList.add(eventMap);
        Map<String, Object> parentIdsMap = new HashMap<>();
        parentIdsMap.put("parentID", eventMap);
        List<AdjustmentDTO> adjustmentList = AdjustmentHelper.getInstance().convertAllEventsIntoAdjustmentDTO(eventList, parentIdsMap, "Asia/Kolkata", "");
        Assertions.assertEquals("id", adjustmentList.get(0).getId());
    }

    @Test
    void getAppropriateDateFormatBasedOnTheRequest_ifInputIsNullOrEmpty_test() {
        DateFormats dateformat = AdjustmentHelper.getInstance().getAppropriateDateFormatBasedOnTheRequest("");
        Assertions.assertEquals(DateFormats.YYYY_MM_DD_T_HH_MM_SS_A_Z, dateformat);
    }

    @Test
    void getAppropriateDateFormatBasedOnTheRequest_valid_test() {
        DateFormats dateformat = AdjustmentHelper.getInstance().getAppropriateDateFormatBasedOnTheRequest("true");
        Assertions.assertEquals(DateFormats.DD_MMM_YYYY_HH_MM_SS_A, dateformat);
    }

    @Test
    void getAdjustmentsBasedOnStatus_withPendingStatus_test() throws IOException, NoSuchAlgorithmException {
        Map<String, Object>eventMap = new HashMap<>();
        eventMap.put("event", "event");
        List<Map<String, Object>> eventList = new ArrayList<>();
        eventList.add(eventMap);
        Map<String, Object> adjustmentMap = new HashMap<>();
        adjustmentMap.put(EventConstants.EVENT_LIST, eventList);
        try(MockedConstruction<AdjustmentImpl> adjustmentImplMock = Mockito.mockConstruction(AdjustmentImpl.class, (adjustmentImpl, context)->{
            Mockito.when(adjustmentImpl.getAllAdjustments(anyString(), anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(adjustmentMap);
        })
        ){
            Map<String, Object> responseMap = AdjustmentHelper.getInstance().getAdjustmentsBasedOnStatus("accountID", "contactID", "02/02/2021", "02/02/2021", "PENDING", "");
            Assertions.assertEquals(adjustmentMap, responseMap);
        }
    }

    @Test
    void getAdjustmentsBasedOnStatus_valid_test() throws IOException, NoSuchAlgorithmException {
        Map<String, Object>eventMap = new HashMap<>();
        eventMap.put("event", "event");
        List<Map<String, Object>> eventList = new ArrayList<>();
        eventList.add(eventMap);
        Map<String, Object> adjustmentMap = new HashMap<>();
        adjustmentMap.put(EventConstants.EVENT_LIST, eventList);
        Map<String, Object> expectedMap = new HashMap<>();
        List<Map<String, Object>> responseEventList = new ArrayList<>();
        responseEventList.add(eventMap);
        responseEventList.add(eventMap);
        expectedMap.put(EventConstants.EVENT_LIST, responseEventList);
        try(MockedConstruction<AdjustmentImpl> adjustmentImplMock = Mockito.mockConstruction(AdjustmentImpl.class, (adjustmentImpl, context)->{
            Mockito.when(adjustmentImpl.getAllAdjustments(anyString(), anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(adjustmentMap);
        })
        ){
            Map<String, Object> responseMap = AdjustmentHelper.getInstance().getAdjustmentsBasedOnStatus("accountID", "contactID", "02/02/2021", "02/02/2021", "PENDING,APPROVED", "");
            Assertions.assertEquals(expectedMap, responseMap);
        }
    }

    @Test
    void getEntryContactPro_EntryContactSameAsLoggedInContact_ShouldReturnLoggedInPro_test(){
        PeopleRelationJDO loggedInPro = new PeopleRelationJDO();
        loggedInPro.setContactId("123");
        Assertions.assertEquals(loggedInPro,AdjustmentHelper.getEntryContactPro(loggedInPro,Map.of("provider",List.of("123"))));
    }

    @Test
    void getEntryContactPro_EntryContactNotSameAsLoggedInContact_ShouldReturnQueriedPro_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO entryPro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserProWithContact("234","345")).thenReturn(entryPro);
            PeopleRelationJDO loggedInPro = new PeopleRelationJDO();
            loggedInPro.setContactId("123");
            loggedInPro.setUniquepin("234");
            Assertions.assertEquals(entryPro,AdjustmentHelper.getEntryContactPro(loggedInPro,Map.of("provider",List.of("345"))));
        }
    }

    @Test
    void validateAndExtractLoggedInPro_nullPro_ShouldThrowExceptionWithUserNotAuthorizedMessage_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserPro("123","234")).thenReturn(null);
            Contact loggedInContact = new Contact();
            loggedInContact.setId("234");
            Throwable exception = Assertions.assertThrows(IllegalArgumentException.class,()->AdjustmentHelper.validateAndExtractLoggedInPro("123",loggedInContact));
            Assertions.assertEquals("User not authorized", exception.getMessage());
        }
    }

    @Test
    void validateAndExtractLoggedInPro_validPro_ShouldReturnProWithContact_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO pro = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserPro("123","234")).thenReturn(pro);
            Contact loggedInContact = new Contact();
            loggedInContact.setId("234");
            PeopleRelationJDO actual = AdjustmentHelper.validateAndExtractLoggedInPro("123",loggedInContact);
            Assertions.assertEquals(pro,actual);
            Assertions.assertEquals(loggedInContact,actual.getContact());
        }
    }
}
