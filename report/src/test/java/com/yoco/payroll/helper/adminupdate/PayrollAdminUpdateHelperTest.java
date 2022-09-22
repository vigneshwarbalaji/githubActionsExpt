package com.yoco.payroll.helper.adminupdate;

import com.yoco.adjustment.helper.AdjustmentOverlapHelper;
import com.yoco.adjustment.util.AdjustmentUtil;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.PayrollStatus;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import com.yoco.constants.EventConstants;
import com.yoco.payroll.helper.PayrollTaskInitiator;
import com.yoco.payroll.modal.AdminUpdatePayloadDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static com.yoco.commons.constants.SchedulingKeys.*;
import static org.mockito.ArgumentMatchers.*;

class PayrollAdminUpdateHelperTest {
    @Test
    void validateAuthorizationAndGetEvent_EntryNotEligible_ShouldThrowExceptionWithMessage(){
        try(MockedConstruction<ReportImpl> reportMockedConstruction = Mockito.mockConstruction(ReportImpl.class,(mock,context)->{
            Mockito.when(mock.getEntryByID("123")).thenReturn(Map.of());
        })){
            new PayrollAdminUpdateHelper().validateAuthorizationAndGetEvent("123","cID",new PeopleRelationJDO(),false);
        }catch (Exception e){
            Assertions.assertEquals("Operation failed",e.getMessage());
        }
    }

    @Test
    void validateAuthorizationAndGetEvent_UserNotAuthorized_ShouldThrowExceptionWithMessage(){
        try(MockedConstruction<ReportImpl> reportMockedConstruction = Mockito.mockConstruction(ReportImpl.class,(mock,context)->{
            Mockito.when(mock.getEntryByID("123")).thenReturn(Map.of());
        })){
            PayrollAdminUpdateHelper payrollAdminUpdateHelper = Mockito.mock(PayrollAdminUpdateHelper.class);
            Mockito.when(payrollAdminUpdateHelper.isEntryEligibleForAdminUpdate(Map.of(),false)).thenReturn(true);
            Mockito.when(payrollAdminUpdateHelper.isUserAuthorizedToAdminUpdateEntry(Map.of(),"cID",new PeopleRelationJDO())).thenReturn(false);
            Mockito.when(payrollAdminUpdateHelper.validateAuthorizationAndGetEvent("123","cID",new PeopleRelationJDO(),false)).thenCallRealMethod();
            payrollAdminUpdateHelper.validateAuthorizationAndGetEvent("123","cID",new PeopleRelationJDO(),false);
        }catch (Exception e){
            Assertions.assertEquals("User not authorized",e.getMessage());
        }
    }

    @Test
    void validateAuthorizationAndGetEvent_validRun_ShouldReturnEvent() throws IOException, NoSuchAlgorithmException {
        try(MockedConstruction<ReportImpl> reportMockedConstruction = Mockito.mockConstruction(ReportImpl.class,(mock,context)->{
            Mockito.when(mock.getEntryByID("123")).thenReturn(Map.of("event","value"));
        })){
            PayrollAdminUpdateHelper payrollAdminUpdateHelper = Mockito.mock(PayrollAdminUpdateHelper.class);
            Mockito.when(payrollAdminUpdateHelper.isEntryEligibleForAdminUpdate(Map.of("event","value"),false)).thenReturn(true);
            Mockito.when(payrollAdminUpdateHelper.isUserAuthorizedToAdminUpdateEntry(Map.of("event","value"),"cID",new PeopleRelationJDO())).thenReturn(true);
            Mockito.when(payrollAdminUpdateHelper.validateAuthorizationAndGetEvent("123","cID",new PeopleRelationJDO(),false)).thenCallRealMethod();
            Assertions.assertEquals(Map.of("event","value"),payrollAdminUpdateHelper.validateAuthorizationAndGetEvent("123","cID",new PeopleRelationJDO(),false));
        }
    }

    @Test
    void isUserAuthorizedToAdminUpdateEntry_DifferentEntryAndLoggedInUserAccountID_ShouldReturnFalse(){
        Map<String,Object> event = Map.of(SchedulingKeys.MERCHANT,"accID");
        PeopleRelationJDO loggedInPro = new PeopleRelationJDO();
        loggedInPro.setUniquepin("accID2");
        Assertions.assertFalse(new PayrollAdminUpdateHelper().isUserAuthorizedToAdminUpdateEntry(event,"123",loggedInPro));
    }

    @Test
    void isUserAuthorizedToAdminUpdateEntry_DifferentEntryAndUserContactID_ShouldReturnFalse(){
        Map<String,Object> event = Map.of(SchedulingKeys.MERCHANT,"accID",SchedulingKeys.PROVIDER, List.of("123"));
        PeopleRelationJDO loggedInPro = new PeopleRelationJDO();
        loggedInPro.setUniquepin("accID");
        Assertions.assertFalse(new PayrollAdminUpdateHelper().isUserAuthorizedToAdminUpdateEntry(event,"1234",loggedInPro));
    }

    @Test
    void isUserAuthorizedToAdminUpdateEntry_noAdjustmentSkillset_ShouldReturnFalse(){
        PeopleRelationJDO loggedInPro = new PeopleRelationJDO();
        loggedInPro.setUniquepin("accID");
        try(MockedStatic<PermissionManager> permissionManagerMockedStatic = Mockito.mockStatic(PermissionManager.class)){
            permissionManagerMockedStatic.when(()->PermissionManager.checkUserHasAdjustmentEditAccess(loggedInPro)).thenReturn(false);
            Map<String,Object> event = Map.of(SchedulingKeys.MERCHANT,"accID",SchedulingKeys.PROVIDER, List.of("123"));
            Assertions.assertFalse(new PayrollAdminUpdateHelper().isUserAuthorizedToAdminUpdateEntry(event,"123",loggedInPro));
        }
    }

    @Test
    void isUserAuthorizedToAdminUpdateEntry_valid_ShouldReturnTrue(){
        PeopleRelationJDO loggedInPro = new PeopleRelationJDO();
        loggedInPro.setUniquepin("accID");
        try(MockedStatic<PermissionManager> permissionManagerMockedStatic = Mockito.mockStatic(PermissionManager.class)){
            permissionManagerMockedStatic.when(()->PermissionManager.checkUserHasAdjustmentEditAccess(loggedInPro)).thenReturn(true);
            Map<String,Object> event = Map.of(SchedulingKeys.MERCHANT,"accID",SchedulingKeys.PROVIDER, List.of("123"));
            Assertions.assertTrue(new PayrollAdminUpdateHelper().isUserAuthorizedToAdminUpdateEntry(event,"123",loggedInPro));
        }
    }

    @Test
    void isEntryEligibleForAdminUpdate_deletedEvent_shouldReturnFalse(){
        Assertions.assertFalse(new PayrollAdminUpdateHelper().isEntryEligibleForAdminUpdate(Map.of(),false));
    }

    @Test
    void isEntryEligibleForAdminUpdate_reupdateRequest_StatusNotPending_shouldReturnFalse(){
        Assertions.assertFalse(new PayrollAdminUpdateHelper().isEntryEligibleForAdminUpdate(Map.of(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, "MANUAL_CLOCKED_OUT")),true));
    }

    @Test
    void isEntryEligibleForAdminUpdate_reupdateRequest_payrollStatusNotAdminUpdated_shouldReturnFalse(){
        Assertions.assertFalse(new PayrollAdminUpdateHelper().isEntryEligibleForAdminUpdate(Map.of(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, "PENDING"),SchedulingKeys.PAYMENT_STATUS,PayrollStatus.DEFAULT_PAYROLL.toString()),true));
    }

    @Test
    void isEntryEligibleForAdminUpdate_reupdateRequest_ValidStatuses_shouldReturnTrue(){
        Assertions.assertTrue(new PayrollAdminUpdateHelper().isEntryEligibleForAdminUpdate(Map.of(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, "PENDING"),SchedulingKeys.PAYMENT_STATUS,PayrollStatus.ADMIN_UPDATED.toString()),true));
    }

    @Test
    void isEntryEligibleForAdminUpdate_reupdateRequestFalse_StatusNotManualClockedOut_shouldReturnFalse(){
        Assertions.assertFalse(new PayrollAdminUpdateHelper().isEntryEligibleForAdminUpdate(Map.of(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, "PENDING"),SchedulingKeys.PAYMENT_STATUS,PayrollStatus.ADMIN_UPDATED.toString()),false));
    }

    @Test
    void isEntryEligibleForAdminUpdate_reupdateRequestFalse_payrollStatusNotUserAcknowledged_shouldReturnFalse(){
        Assertions.assertFalse(new PayrollAdminUpdateHelper().isEntryEligibleForAdminUpdate(Map.of(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, "MANUAL_CLOCKED_OUT"),SchedulingKeys.PAYMENT_STATUS,PayrollStatus.ADMIN_UPDATED.toString()),false));
    }

    @Test
    void isEntryEligibleForAdminUpdate_reupdateRequestFalse_validStatuses_shouldReturnTrue(){
        Assertions.assertTrue(new PayrollAdminUpdateHelper().isEntryEligibleForAdminUpdate(Map.of(SchedulingKeys.METADATA,Map.of(SchedulingKeys.STATUS, "MANUAL_CLOCKED_OUT"),SchedulingKeys.PAYMENT_STATUS,PayrollStatus.USER_ACKNOWLEDGED.toString()),false));
    }

    @Test
    void checkAndGenerateOverlapResponseForAdjustedTime_emptyOverlappingEvent_shouldReturnEmptyMap() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<AdjustmentUtil> adjustmentUtilMockedStatic = Mockito.mockStatic(AdjustmentUtil.class);
            MockedStatic<AdjustmentOverlapHelper> adjustmentOverlapHelperMockedStatic = Mockito.mockStatic(AdjustmentOverlapHelper.class)){
            adjustmentUtilMockedStatic.when(()-> AdjustmentUtil.adjustInTimeMillisForBreakSessionOverlapping("accID","123",1L,"Asia/Kolkata")).thenReturn(1L);
            adjustmentOverlapHelperMockedStatic.when(()->AdjustmentOverlapHelper.validateAndExtractOverlapEntry("accID","123",1L,2L,"Asia/Kolkata","eID")).thenReturn(Map.of());
            PeopleRelationJDO requesterPro = new PeopleRelationJDO();
            requesterPro.setUniquepin("accID");
            requesterPro.setTimeZone("Asia/Kolkata");
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setContactId("123");
            AdminUpdatePayloadDTO payloadDTO = new AdminUpdatePayloadDTO();
            payloadDTO.setAdjustedInTime(1L);
            payloadDTO.setAdjustedOutTime(2L);
            payloadDTO.setEvent(Map.of("id","eID"));
            Assertions.assertEquals(Map.of(),new PayrollAdminUpdateHelper().checkAndGenerateOverlapResponseForAdjustedTime(requesterPro,userPro,payloadDTO));
        }
    }

    @Test
    void checkAndGenerateOverlapResponseForAdjustedTime_validOverlappingEvent_shouldCallextractAllOverLapEntryDetailsMethod() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<AdjustmentUtil> adjustmentUtilMockedStatic = Mockito.mockStatic(AdjustmentUtil.class);
            MockedStatic<AdjustmentOverlapHelper> adjustmentOverlapHelperMockedStatic = Mockito.mockStatic(AdjustmentOverlapHelper.class)){
            adjustmentUtilMockedStatic.when(()-> AdjustmentUtil.adjustInTimeMillisForBreakSessionOverlapping("accID","123",1L,"Asia/Kolkata")).thenReturn(1L);
            adjustmentOverlapHelperMockedStatic.when(()->AdjustmentOverlapHelper.validateAndExtractOverlapEntry("accID","123",1L,2L,"Asia/Kolkata","eID")).thenReturn(Map.of("test","value"));
            PeopleRelationJDO requesterPro = new PeopleRelationJDO();
            requesterPro.setUniquepin("accID");
            requesterPro.setTimeZone("Asia/Kolkata");
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setContactId("123");
            userPro.setEmailID("email");
            AdminUpdatePayloadDTO payloadDTO = new AdminUpdatePayloadDTO();
            payloadDTO.setAdjustedInTime(1L);
            payloadDTO.setAdjustedOutTime(2L);
            payloadDTO.setEvent(Map.of("id","eID"));
            adjustmentOverlapHelperMockedStatic.when(()->AdjustmentOverlapHelper.extractAllOverLapEntryDetails(Map.of("test","value"),"Asia/Kolkata","email")).thenReturn(Map.of("mock","value"));
            Assertions.assertEquals(Map.of("mock","value"),new PayrollAdminUpdateHelper().checkAndGenerateOverlapResponseForAdjustedTime(requesterPro,userPro,payloadDTO));
        }
    }

    @Test
    void createAdminUpdatedEventAndGenerateResponse_validFlow_test() throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO requesterPro = new PeopleRelationJDO();
        requesterPro.setTimeZone("Asia/Kolkata");
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setEmailID("email");
        AdminUpdatePayloadDTO payloadDTO = new AdminUpdatePayloadDTO();
        payloadDTO.setEvent(Map.of());
        try(MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class);
            MockedConstruction<AdjustmentDTO> adjustmentDTOMockedConstruction = Mockito.mockConstruction(AdjustmentDTO.class);
            MockedConstruction<ReportsDTO> reportsDTOMockedConstruction = Mockito.mockConstruction(ReportsDTO.class);
            MockedStatic<PayrollTaskInitiator> payrollTaskInitiatorMockedStatic = Mockito.mockStatic(PayrollTaskInitiator.class)){
            PayrollAdminUpdateHelper payrollAdminUpdateHelper = Mockito.mock(PayrollAdminUpdateHelper.class);
            Mockito.when(payrollAdminUpdateHelper.generateAdminUpdatedEventMap(Map.of(),payloadDTO,requesterPro)).thenReturn(Map.of("test","value"));
            Mockito.when(payrollAdminUpdateHelper.createAdminUpdatedEventAndGenerateResponse(requesterPro,userPro,payloadDTO)).thenCallRealMethod();
            schedulingEngineUtilMockedStatic.when(()->SchedulingEngineUtil.createEvent("{\"test\":\"value\"}")).thenReturn(Map.of("data",Map.of("test","value")));
            GenericResponse actual = payrollAdminUpdateHelper.createAdminUpdatedEventAndGenerateResponse(requesterPro,userPro,payloadDTO);
            GenericResponse expected = new GenericResponse();
            expected.setSuccess(true);
            expected.setData(Map.of(EventConstants.ENTRY,reportsDTOMockedConstruction.constructed().get(0),EventConstants.ADJUSTMENT,adjustmentDTOMockedConstruction.constructed().get(0)));
            Assertions.assertEquals(expected,actual);
            payrollTaskInitiatorMockedStatic.verify(()->PayrollTaskInitiator.initiateAdminUpdateHandlerQueue(requesterPro,userPro,adjustmentDTOMockedConstruction.constructed().get(0)));
        }
    }

    @Test
    void generateAdminUpdatedEventMap_reupdateRequest_validFlow_test(){
        HashMap<String,Object> originalEvent = new HashMap<>();
        originalEvent.put("id","ID");
        originalEvent.put("bookingId","123123");
        originalEvent.put("parentId","234234");
        originalEvent.put(SchedulingKeys.METADATA,new HashMap<>());
        AdminUpdatePayloadDTO payloadDTO = new AdminUpdatePayloadDTO();
        payloadDTO.setAdjustedInTime(1663007400000L);
        payloadDTO.setAdjustedOutTime(1663011000000L);
        payloadDTO.setReUpdateRequest(true);
        payloadDTO.setMessage("message");
        PeopleRelationJDO requesterPro = new PeopleRelationJDO();
        requesterPro.setContactId("123");
        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)){
            dateUtilMockedStatic.when(DateUtil::getCurrentTime).thenReturn(1L);
            dateUtilMockedStatic.when(()->DateUtil.convertMillisToDateTimeText(any(DateFormats.class),anyLong(),anyString())).thenCallRealMethod();
            dateUtilMockedStatic.when(()->DateUtil.getZonedDateTime(anyLong(),anyString())).thenCallRealMethod();
            Map<String,Object> actual = new PayrollAdminUpdateHelper().generateAdminUpdatedEventMap(originalEvent,payloadDTO,requesterPro);
            Map<String,Object> expected = Map.of("startDateTime","2022-09-12T18:30:00:000Z","endDateTime","2022-09-12T19:30:00:000Z","parentId","234234",SchedulingKeys.PAYMENT_STATUS,PayrollStatus.ADMIN_UPDATED.toString(),
                    SchedulingKeys.QUERYABLE_META,Map.of(PAYROLL_STATUS,PayrollStatus.ADMIN_UPDATED.toString(), STATUS, REPORT_STATUS.PENDING.toString()),
                    SchedulingKeys.METADATA,Map.of(ADDITIONAL_INFO, Map.of(ADJUSTMENT_INFO, Map.of(REQUEST_INFO,AdjustmentUtil.setAdjustmentInfo("123","message"))),
                            STATUS, REPORT_STATUS.PENDING.toString()));
            Assertions.assertEquals(expected,actual);
        }
    }

    @Test
    void generateAdminUpdatedEventMap_updateRequest_validFlow_test(){
        HashMap<String,Object> originalEvent = new HashMap<>();
        originalEvent.put("id","ID");
        originalEvent.put("bookingId","123123");
        originalEvent.put("parentId","234234");
        originalEvent.put(SchedulingKeys.METADATA,new HashMap<>());
        AdminUpdatePayloadDTO payloadDTO = new AdminUpdatePayloadDTO();
        payloadDTO.setAdjustedInTime(1663007400000L);
        payloadDTO.setAdjustedOutTime(1663011000000L);
        payloadDTO.setReUpdateRequest(false);
        payloadDTO.setMessage("message");
        PeopleRelationJDO requesterPro = new PeopleRelationJDO();
        requesterPro.setContactId("123");
        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)){
            dateUtilMockedStatic.when(DateUtil::getCurrentTime).thenReturn(1L);
            dateUtilMockedStatic.when(()->DateUtil.convertMillisToDateTimeText(any(DateFormats.class),anyLong(),anyString())).thenCallRealMethod();
            dateUtilMockedStatic.when(()->DateUtil.getZonedDateTime(anyLong(),anyString())).thenCallRealMethod();
            Map<String,Object> actual = new PayrollAdminUpdateHelper().generateAdminUpdatedEventMap(originalEvent,payloadDTO,requesterPro);
            Map<String,Object> expected = Map.of("startDateTime","2022-09-12T18:30:00:000Z","endDateTime","2022-09-12T19:30:00:000Z","parentId","ID",SchedulingKeys.PAYMENT_STATUS,PayrollStatus.ADMIN_UPDATED.toString(),
                    SchedulingKeys.QUERYABLE_META,Map.of(PAYROLL_STATUS,PayrollStatus.ADMIN_UPDATED.toString(), STATUS, REPORT_STATUS.PENDING.toString()),
                    SchedulingKeys.METADATA,Map.of(ADDITIONAL_INFO, Map.of(ADJUSTMENT_INFO, Map.of(REQUEST_INFO,AdjustmentUtil.setAdjustmentInfo("123","message"))),
                            STATUS, REPORT_STATUS.PENDING.toString()));
            Assertions.assertEquals(expected,actual);
        }
    }
}
