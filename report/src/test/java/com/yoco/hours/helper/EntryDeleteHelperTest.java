package com.yoco.hours.helper;

import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.InAppReminderUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.List;
import java.util.Map;

class EntryDeleteHelperTest {
    @Test
    void validateAndDeleteEntry_nullEventMap_test(){
        try(MockedConstruction<ReportImpl> reportMockedConstruction = Mockito.mockConstruction(ReportImpl.class, (reportImplMock,context)->{
            Mockito.when(reportImplMock.getEntryByID("123")).thenReturn(null);
        })){
            EntryDeleteHelper.validateAndDeleteEntry("123",null);
        }catch (Exception e){
            Assertions.assertEquals("Invalid entryID",e.getMessage());
        }
    }

    @Test
    void validateAndDeleteEntry_UserNotAuthorized_test(){
        try(MockedConstruction<ReportImpl> reportMockedConstruction = Mockito.mockConstruction(ReportImpl.class, (reportImplMock,context)->{
            Mockito.when(reportImplMock.getEntryByID("123")).thenReturn(Map.of("1","2"));
        }); MockedStatic<EntryDeleteHelper> entryDeleteHelperMockedStatic = Mockito.mockStatic(EntryDeleteHelper.class)){
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.validateAndDeleteEntry("123",null)).thenCallRealMethod();
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.isUserAuthorizedForEntryDeletion(Map.of("1","2"),null)).thenReturn(false);
            EntryDeleteHelper.validateAndDeleteEntry("123",null);
        }catch (Exception e){
            Assertions.assertEquals("User not authorized",e.getMessage());
        }
    }

    @Test
    void validateAndDeleteEntry_entryNotAllowedForDeletion_test(){
        try(MockedConstruction<ReportImpl> reportMockedConstruction = Mockito.mockConstruction(ReportImpl.class, (reportImplMock,context)->{
            Mockito.when(reportImplMock.getEntryByID("123")).thenReturn(Map.of("1","2"));
        }); MockedStatic<EntryDeleteHelper> entryDeleteHelperMockedStatic = Mockito.mockStatic(EntryDeleteHelper.class)){
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.validateAndDeleteEntry("123",null)).thenCallRealMethod();
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.isUserAuthorizedForEntryDeletion(Map.of("1","2"),null)).thenReturn(true);
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.isEntryAllowedForDeletion(Map.of("1","2"),null)).thenReturn(false);
            EntryDeleteHelper.validateAndDeleteEntry("123",null);
        }catch (Exception e){
            Assertions.assertEquals("Unable to delete entry",e.getMessage());
        }
    }

    @Test
    void validateAndDeleteEntry_response_null_test(){
        try(MockedConstruction<ReportImpl> reportMockedConstruction = Mockito.mockConstruction(ReportImpl.class, (reportImplMock,context)->{
            Mockito.when(reportImplMock.getEntryByID("123")).thenReturn(Map.of("1","2"));
            Mockito.when(reportImplMock.deleteEvents(List.of("123"))).thenReturn(null);
        }); MockedStatic<EntryDeleteHelper> entryDeleteHelperMockedStatic = Mockito.mockStatic(EntryDeleteHelper.class)){
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.validateAndDeleteEntry("123",null)).thenCallRealMethod();
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.isUserAuthorizedForEntryDeletion(Map.of("1","2"),null)).thenReturn(true);
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.isEntryAllowedForDeletion(Map.of("1","2"),null)).thenReturn(true);
            EntryDeleteHelper.validateAndDeleteEntry("123",null);
        }catch (Exception e){
            Assertions.assertEquals("Operation failed",e.getMessage());
        }
    }

    @Test
    void validateAndDeleteEntry_response_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedConstruction<ReportImpl> reportMockedConstruction = Mockito.mockConstruction(ReportImpl.class, (reportImplMock,context)->{
            Mockito.when(reportImplMock.getEntryByID("123")).thenReturn(Map.of("1","2"));
            Mockito.when(reportImplMock.deleteEvents(List.of("123"))).thenReturn(List.of(Map.of("success",true)));
        }); MockedStatic<EntryDeleteHelper> entryDeleteHelperMockedStatic = Mockito.mockStatic(EntryDeleteHelper.class)){
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.validateAndDeleteEntry("123",null)).thenCallRealMethod();
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.isUserAuthorizedForEntryDeletion(Map.of("1","2"),null)).thenReturn(true);
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.isEntryAllowedForDeletion(Map.of("1","2"),null)).thenReturn(true);
            Map<String,Object> actual = EntryDeleteHelper.validateAndDeleteEntry("123",null);
            Assertions.assertEquals(Map.of("success",true),actual);
        }
    }

    @Test
    void isUserAuthorizedForEntryDeletion_differentAccountID_test(){
        Assertions.assertFalse(EntryDeleteHelper.isUserAuthorizedForEntryDeletion(Map.of("provider",List.of("123"),"merchant","accID"),new PeopleRelationJDO()));
    }

    @Test
    void isUserAuthorizedForEntryDeletion_UserUnauthorized_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("234");
        userPro.setUniquepin("accID");
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)) {
            userPROUtilMockedStatic.when(()->UserPROUtil.isPrimaryAdmin(userPro)).thenReturn(false);
            Assertions.assertFalse(EntryDeleteHelper.isUserAuthorizedForEntryDeletion(Map.of("provider", List.of("123"), "merchant", "accID"), userPro));
        }
    }

    @Test
    void isUserAuthorizedForEntryDeletion_sameUser_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("123");
        userPro.setUniquepin("accID");
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)) {
            userPROUtilMockedStatic.when(()->UserPROUtil.isPrimaryAdmin(userPro)).thenReturn(false);
            Assertions.assertTrue(EntryDeleteHelper.isUserAuthorizedForEntryDeletion(Map.of("provider", List.of("123"), "merchant", "accID"), userPro));
        }
    }

    @Test
    void isUserAuthorizedForEntryDeletion_UserPrimaryAdmin_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("234");
        userPro.setUniquepin("accID");
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)) {
            userPROUtilMockedStatic.when(()->UserPROUtil.isPrimaryAdmin(userPro)).thenReturn(true);
            Assertions.assertTrue(EntryDeleteHelper.isUserAuthorizedForEntryDeletion(Map.of("provider", List.of("123"), "merchant", "accID"), userPro));
        }
    }

    @Test
    void isEntryAllowedForDeletion_eventDeleted_test(){
        Map<String,Object> event = Map.of("isDeleted",true);
        Assertions.assertFalse(EntryDeleteHelper.isEntryAllowedForDeletion(event,null));
    }

    @Test
    void isEntryAllowedForDeletion_statusNotManualClockedOut_test(){
        Map<String,Object> event = Map.of("isDeleted",false,"metaData",Map.of("status","CLOCKED_IN"));
        Assertions.assertFalse(EntryDeleteHelper.isEntryAllowedForDeletion(event,null));
    }

    @Test
    void isEntryAllowedForDeletion_payrollStatusUserVerified_test(){
        Map<String,Object> event = Map.of("isDeleted",false,"metaData",Map.of("status","MANUAL_CLOCKED_OUT"),"paymentStatus","USER-VERIFIED");
        Assertions.assertFalse(EntryDeleteHelper.isEntryAllowedForDeletion(event,null));
    }

    @Test
    void isEntryAllowedForDeletion_payrollStatusDefault_test(){
        Map<String,Object> event = Map.of("isDeleted",false,"metaData",Map.of("status","MANUAL_CLOCKED_OUT"),"paymentStatus","DEFAULT_PAYROLL");
        Assertions.assertTrue(EntryDeleteHelper.isEntryAllowedForDeletion(event,null));
    }

    @Test
    void isEntryAllowedForDeletion_payrollStatusUserAcknowledged_NotAdmin_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isPrimaryAdmin(null)).thenReturn(false);
            Map<String,Object> event = Map.of("isDeleted",false,"metaData",Map.of("status","MANUAL_CLOCKED_OUT"),"paymentStatus","USER-ACKNOWLEDGED");
            Assertions.assertFalse(EntryDeleteHelper.isEntryAllowedForDeletion(event,null));
        }
    }

    @Test
    void isEntryAllowedForDeletion_payrollStatusUserAcknowledged_Admin_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isPrimaryAdmin(null)).thenReturn(true);
            Map<String,Object> event = Map.of("isDeleted",false,"metaData",Map.of("status","MANUAL_CLOCKED_OUT"),"paymentStatus","USER-ACKNOWLEDGED");
            Assertions.assertTrue(EntryDeleteHelper.isEntryAllowedForDeletion(event,null));
        }
    }

    @Test
    void handleEntryDeletion_notCurrentDay_test() throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        userPro.setTimeZone("Asia/Kolkata");
        Contact entryContact = new Contact();
        entryContact.setId("123");
        Map<String,Object> deletedEvent = Map.of();
        try(MockedStatic<EntryDeleteHelper> entryDeleteHelperMockedStatic = Mockito.mockStatic(EntryDeleteHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedConstruction<ReportsDTO> reportsDTOMockedConstruction = Mockito.mockConstruction(ReportsDTO.class,(reportsDtoMock,context)->{
                Mockito.when(reportsDtoMock.getInTime()).thenReturn(1657391400000L);
            });
            MockedConstruction<InAppReminderUtil> inAppReminderUtilMockedConstruction = Mockito.mockConstruction(InAppReminderUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserPro("accID","123")).thenReturn(userPro);
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.handleEntryDeletion(userPro,entryContact,deletedEvent)).thenCallRealMethod();
            EntryDeleteHelper.handleEntryDeletion(userPro,entryContact,deletedEvent);
            entryDeleteHelperMockedStatic.verify(()->EntryDeleteHelper.createEntryDeletionActivity(userPro,entryContact,deletedEvent));
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","originalEntryOrNewAdjDel","userClockInJDO",reportsDTOMockedConstruction.constructed().get(0)));
            rtmServiceMockedStatic.verify(()->RTMService.publishToAW("accID","123","DELETE_ENTRY","entry",reportsDTOMockedConstruction.constructed().get(0)));
            Assertions.assertTrue(inAppReminderUtilMockedConstruction.constructed().isEmpty());
        }
    }

    @Test
    void handleEntryDeletion_CurrentDay_test() throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        userPro.setTimeZone("Asia/Kolkata");
        Contact entryContact = new Contact();
        entryContact.setId("123");
        Map<String,Object> deletedEvent = Map.of();
        try(MockedStatic<EntryDeleteHelper> entryDeleteHelperMockedStatic = Mockito.mockStatic(EntryDeleteHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedConstruction<ReportsDTO> reportsDTOMockedConstruction = Mockito.mockConstruction(ReportsDTO.class,(reportsDtoMock,context)->{
                Mockito.when(reportsDtoMock.getInTime()).thenReturn(new Date().getTime());
            });
            MockedConstruction<InAppReminderUtil> inAppReminderUtilMockedConstruction = Mockito.mockConstruction(InAppReminderUtil.class);
            MockedConstruction<UserDTO> userDTOMockedConstruction = Mockito.mockConstruction(UserDTO.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.getUserPro("accID","123")).thenReturn(userPro);
            entryDeleteHelperMockedStatic.when(()->EntryDeleteHelper.handleEntryDeletion(userPro,entryContact,deletedEvent)).thenCallRealMethod();
            EntryDeleteHelper.handleEntryDeletion(userPro,entryContact,deletedEvent);
            entryDeleteHelperMockedStatic.verify(()->EntryDeleteHelper.createEntryDeletionActivity(userPro,entryContact,deletedEvent));
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","originalEntryOrNewAdjDel","userClockInJDO",reportsDTOMockedConstruction.constructed().get(0)));
            rtmServiceMockedStatic.verify(()->RTMService.publishToAW("accID","123","DELETE_ENTRY","entry",reportsDTOMockedConstruction.constructed().get(0)));
            Mockito.verify(inAppReminderUtilMockedConstruction.constructed().get(0)).handleInAppNotificationForReminderTimeChange(userDTOMockedConstruction.constructed().get(0),Map.of());
        }
    }

    @Test
    void createEntryDeletionActivity_valid_test(){
        Map<String,Object> event = Map.of("id","1","updatedTime",123L);
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("123");
        userPro.setUniquepin("accID");
        Contact entryContact = new Contact();
        entryContact.setId("123");
        entryContact.setEmailID("email");
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){
            EntryDeleteHelper.createEntryDeletionActivity(userPro,entryContact,event);
            activityUtilMockedStatic.verify(()->ActivityUtil.saveActivity("accID","123","entry_deleted","email","EntryID : 1 deleted by 123","DUMMY",123L));
        }
    }
}
