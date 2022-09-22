package com.yoco.adjustment.helper.delete;

import com.yoco.adjustment.helper.AdjustmentHelper;
import com.yoco.adjustment.helper.AdjustmentTaskInitiator;
import com.yoco.adjustment.helper.reject.AdjustmentRejectHelper;
import com.yoco.adjustment.modal.AdjustmentDeletePayloadDTO;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.DcmTeamUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import static org.mockito.ArgumentMatchers.*;

class AdjustmentDeleteHelperTest {
    @Test
    void validateAuthorizationAndGetAdjustmentEvent_NullEvent_ShouldThrowUnableToDeleteException() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl report = Mockito.mock(ReportImpl.class);
            Mockito.when(report.getEntryByID("eID")).thenReturn(null);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(report);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            Throwable exception = Assertions.assertThrows(IllegalArgumentException.class,()->AdjustmentDeleteHelper.validateAuthorizationAndGetAdjustmentEvent("eID",userPro));
            Assertions.assertEquals("Someone already took an action on this adjustment.",exception.getMessage());
        }
    }

    @Test
    void validateAuthorizationAndGetAdjustmentEvent_EventNotFromSameAccount_ShouldThrowUserNotAuthorizedException() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl report = Mockito.mock(ReportImpl.class);
            Mockito.when(report.getEntryByID("eID")).thenReturn(Map.of("merchant","234","metaData",Map.of("status","PENDING")));
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(report);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accID");
            Throwable exception = Assertions.assertThrows(IllegalArgumentException.class,()->AdjustmentDeleteHelper.validateAuthorizationAndGetAdjustmentEvent("eID",userPro));
            Assertions.assertEquals("User not authorized",exception.getMessage());
        }
    }

    @Test
    void validateAuthorizationAndGetAdjustmentEvent_validAuthorization_ShouldReturnEvent() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl report = Mockito.mock(ReportImpl.class);
            Mockito.when(report.getEntryByID("eID")).thenReturn(Map.of("merchant","accID","provider", List.of("123"),"metaData",Map.of("status","PENDING")));
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(report);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accID");
            userPro.setContactId("123");
            Assertions.assertEquals(Map.of("merchant","accID","provider", List.of("123"),"metaData",Map.of("status","PENDING")),AdjustmentDeleteHelper.validateAuthorizationAndGetAdjustmentEvent("eID",userPro));
        }
    }

    @Test
    void isValidAdjustment_statusNotPending_ShouldReturnFalse(){
        Assertions.assertFalse(AdjustmentDeleteHelper.isValidAdjustment(Map.of("metaData",Map.of("status","Approved"))));
    }

    @Test
    void isValidAdjustment_eventDeleted_ShouldReturnFalse(){
        Assertions.assertFalse(AdjustmentDeleteHelper.isValidAdjustment(Map.of("isDeleted",true,"metaData",Map.of("status","PENDING"))));
    }

    @Test
    void isValidAdjustment_eventNotDeleted_ShouldReturnTrue(){
        Assertions.assertTrue(AdjustmentDeleteHelper.isValidAdjustment(Map.of("isDeleted",false,"metaData",Map.of("status","PENDING"))));
    }

    @Test
    void isUserAuthorizedToDeleteAdjustment_differentAccountIDs_shouldReturnFalse() throws NoSuchAlgorithmException, IOException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        Assertions.assertFalse(AdjustmentDeleteHelper.isUserAuthorizedToDeleteAdjustment(Map.of("merchant","123"),userPro));
    }

    @Test
    void isUserAuthorizedToDeleteAdjustment_sameAccountAndContactID_shouldReturnTrue() throws NoSuchAlgorithmException, IOException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        userPro.setContactId("123");
        Assertions.assertTrue(AdjustmentDeleteHelper.isUserAuthorizedToDeleteAdjustment(Map.of("merchant","accID","provider",List.of("123")),userPro));
    }

    @Test
    void isUserAuthorizedToDeleteAdjustment_userHasReportsEditAllPermission_shouldReturnTrue() throws NoSuchAlgorithmException, IOException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        userPro.setContactId("234");
        userPro.setSkillsets(JsonUtil.getJson(Map.of("reports","edit-all")));
        Assertions.assertTrue(AdjustmentDeleteHelper.isUserAuthorizedToDeleteAdjustment(Map.of("merchant","accID","provider",List.of("123")),userPro));
    }

    @Test
    void isUserAuthorizedToDeleteAdjustment_userHasNoReportsPermission_shouldReturnFalse() throws NoSuchAlgorithmException, IOException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("accID");
        userPro.setContactId("234");
        userPro.setSkillsets(JsonUtil.getJson(Map.of("reports","")));
        Assertions.assertFalse(AdjustmentDeleteHelper.isUserAuthorizedToDeleteAdjustment(Map.of("merchant","accID","provider",List.of("123")),userPro));
    }

    @Test
    void isUserAuthorizedToDeleteAdjustment_userHasReportsTeamEditPermissionButNotATeamMember_shouldReturnFalse() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getAllTeamMembersOfAnUserInAccount("accID","234")).thenReturn(Set.of("567"));
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accID");
            userPro.setContactId("234");
            userPro.setSkillsets(JsonUtil.getJson(Map.of("reports","edit")));
            Assertions.assertFalse(AdjustmentDeleteHelper.isUserAuthorizedToDeleteAdjustment(Map.of("merchant","accID","provider",List.of("123")),userPro));
        }
    }

    @Test
    void isUserAuthorizedToDeleteAdjustment_userHasReportsTeamEditPermissionAndIsATeamMember_shouldReturnTrue() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getAllTeamMembersOfAnUserInAccount("accID","234")).thenReturn(Set.of("123"));
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accID");
            userPro.setContactId("234");
            userPro.setSkillsets(JsonUtil.getJson(Map.of("reports","edit")));
            Assertions.assertTrue(AdjustmentDeleteHelper.isUserAuthorizedToDeleteAdjustment(Map.of("merchant","accID","provider",List.of("123")),userPro));
        }
    }

    @Test
    void validateAndDeleteAdjustment_newAdjustment_ShouldCallDeleteNewAdjustmentMethod() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<AdjustmentDeleteHelper> adjustmentDeleteHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteHelper.class)){
            PeopleRelationJDO loggedInUserPro = new PeopleRelationJDO();
            loggedInUserPro.setTimeZone("Asia/Kolkata");
            AdjustmentDeletePayloadDTO payloadDTO = new AdjustmentDeletePayloadDTO(Map.of(),false,loggedInUserPro,new PeopleRelationJDO(),"","","");
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.validateAndDeleteAdjustment(payloadDTO)).thenCallRealMethod();
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.deleteNewAdjustment(any(ReportImpl.class),eq(payloadDTO))).thenReturn(new GenericResponse(true,null,null));
            Assertions.assertEquals(new GenericResponse(true,null,null),AdjustmentDeleteHelper.validateAndDeleteAdjustment(payloadDTO));
        }
    }

    @Test
    void validateAndDeleteAdjustment_editAdjustment_ShouldCallDeleteEditAdjustmentMethod() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<AdjustmentDeleteHelper> adjustmentDeleteHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteHelper.class)){
            PeopleRelationJDO loggedInUserPro = new PeopleRelationJDO();
            AdjustmentDeletePayloadDTO payloadDTO = new AdjustmentDeletePayloadDTO(Map.of("parentId","123"),false,loggedInUserPro,new PeopleRelationJDO(),"","","");
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.validateAndDeleteAdjustment(payloadDTO)).thenCallRealMethod();
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.deleteEditAdjustment(any(ReportImpl.class),eq(payloadDTO))).thenReturn(new GenericResponse(true,null,null));
            Assertions.assertEquals(new GenericResponse(true,null,null),AdjustmentDeleteHelper.validateAndDeleteAdjustment(payloadDTO));
        }
    }

    @Test
    void deleteNewAdjustment_validRun_ShouldCallGenerateSuccessResponseMethod() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<AdjustmentDeleteHelper> adjustmentDeleteHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteHelper.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            AdjustmentDeletePayloadDTO payloadDTO = new AdjustmentDeletePayloadDTO(Map.of("id","eID"),true,new PeopleRelationJDO(),new PeopleRelationJDO(),"Asia/Kolkata",DateFormats.DD_MMM_YYYY_HH_MM_SS_A.toString(),"test");
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.deleteEvents(List.of(Map.of("id","eID")),payloadDTO,reportImplMock)).thenReturn(List.of(Map.of("1","2")));
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.deleteNewAdjustment(reportImplMock,payloadDTO)).thenCallRealMethod();
            AdjustmentDeleteHelper.deleteNewAdjustment(reportImplMock,payloadDTO);
            adjustmentDeleteHelperMockedStatic.verify(()->AdjustmentDeleteHelper.generateSuccessResponseAndInitiateQueue(List.of(Map.of("1","2")),List.of(Map.of("1","2")),new PeopleRelationJDO(),new PeopleRelationJDO(),"Asia/Kolkata",DateFormats.DD_MMM_YYYY_HH_MM_SS_A));
        }
    }

    @Test
    void deleteEditAdjustmentWithoutOverlap_validRun_shouldCallGenerateSuccessResponseMethod() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<AdjustmentDeleteHelper> adjustmentDeleteHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteHelper.class)){
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            AdjustmentDeletePayloadDTO payloadDTO = new AdjustmentDeletePayloadDTO(Map.of("id","eID","parentId","pID"),true,new PeopleRelationJDO(),new PeopleRelationJDO(),"Asia/Kolkata",DateFormats.DD_MMM_YYYY_HH_MM_SS_A.toString(),"test");
            Mockito.when(reportImplMock.revertEvent("pID")).thenReturn(Map.of("2","3"));
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.deleteEvents(List.of(Map.of("id","eID","parentId","pID")),payloadDTO,reportImplMock)).thenReturn(List.of(Map.of("1","2")));
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.deleteEditAdjustmentWithoutOverlap(reportImplMock,payloadDTO)).thenCallRealMethod();
            AdjustmentDeleteHelper.deleteEditAdjustmentWithoutOverlap(reportImplMock, payloadDTO);
            adjustmentDeleteHelperMockedStatic.verify(()->AdjustmentDeleteHelper.generateSuccessResponseAndInitiateQueue(List.of(Map.of("1","2")),List.of(Map.of("2","3")),new PeopleRelationJDO(),new PeopleRelationJDO(),"Asia/Kolkata",DateFormats.DD_MMM_YYYY_HH_MM_SS_A));
        }
    }

    @Test
    void deleteEditAdjustment_noOverlappingEntries_ShouldCallDeleteEditAdjustmentWithoutOverlapMethod() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<AdjustmentDeleteHelper> adjustmentDeleteHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteHelper.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            AdjustmentDeletePayloadDTO payloadDTO = new AdjustmentDeletePayloadDTO(Map.of("parentId","pID"),false,new PeopleRelationJDO(),new PeopleRelationJDO(),"",DateFormats.DD_MMM_YYYY_HH_MM_SS_A.toString(),"");
            reportsUtilMockedStatic.when(()->ReportsUtil.getParentIDFromEvent(any())).thenCallRealMethod();
            reportsUtilMockedStatic.when(()->ReportsUtil.getOverlappingEntriesForEvent(Map.of("id","pID"))).thenReturn(List.of());
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getEntryByID("pID")).thenReturn(Map.of("id","pID"));
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.deleteEditAdjustment(reportImplMock,payloadDTO)).thenCallRealMethod();
            AdjustmentDeleteHelper.deleteEditAdjustment(reportImplMock,payloadDTO);
            adjustmentDeleteHelperMockedStatic.verify(()->AdjustmentDeleteHelper.deleteEditAdjustmentWithoutOverlap(reportImplMock,payloadDTO));
        }
    }

    @Test
    void deleteEditAdjustment_NonPendingOverlappingEntriesPresent_ShouldCallGenerateCannotDeleteSubsetsResponseMethod() throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO entryContactPro = new PeopleRelationJDO();
        entryContactPro.setEmailID("email");
        try(MockedStatic<AdjustmentDeleteHelper> adjustmentDeleteHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteHelper.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            AdjustmentDeletePayloadDTO payloadDTO = new AdjustmentDeletePayloadDTO(Map.of("parentId","pID"),false,new PeopleRelationJDO(),entryContactPro,"Asia/Kolkata",DateFormats.DD_MMM_YYYY_HH_MM_SS_A.toString(),"");
            reportsUtilMockedStatic.when(()->ReportsUtil.getParentIDFromEvent(any())).thenCallRealMethod();
            reportsUtilMockedStatic.when(()->ReportsUtil.isStatusPending(any())).thenReturn(false);
            reportsUtilMockedStatic.when(()->ReportsUtil.getOverlappingEntriesForEvent(Map.of("id","pID"))).thenReturn(List.of(Map.of("metaData",Map.of("status","Approved"))));
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getEntryByID("pID")).thenReturn(Map.of("id","pID"));
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.deleteEditAdjustment(reportImplMock,payloadDTO)).thenCallRealMethod();
            AdjustmentDeleteHelper.deleteEditAdjustment(reportImplMock,payloadDTO);
            adjustmentDeleteHelperMockedStatic.verify(()->AdjustmentDeleteHelper.generateCannotDeleteSubsetsResponse(List.of(Map.of("metaData",Map.of("status","Approved"))),"email","Asia/Kolkata"));
        }
    }

    @Test
    void deleteEditAdjustment_NoPendingOverlappingEntriesPresent_deleteSubsetsFalse_ShouldCallGenerateOverlappingErrorResponseMethod() throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO entryContactPro = new PeopleRelationJDO();
        entryContactPro.setEmailID("email");
        try(MockedStatic<AdjustmentDeleteHelper> adjustmentDeleteHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteHelper.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            AdjustmentDeletePayloadDTO payloadDTO = new AdjustmentDeletePayloadDTO(Map.of("parentId","pID"),false,new PeopleRelationJDO(),entryContactPro,"Asia/Kolkata",DateFormats.DD_MMM_YYYY_HH_MM_SS_A.toString(),"");
            reportsUtilMockedStatic.when(()->ReportsUtil.getParentIDFromEvent(any())).thenCallRealMethod();
            reportsUtilMockedStatic.when(()->ReportsUtil.isStatusPending(any())).thenReturn(true);
            reportsUtilMockedStatic.when(()->ReportsUtil.getOverlappingEntriesForEvent(Map.of("id","pID"))).thenReturn(List.of(Map.of("metaData",Map.of("status","PENDING"))));
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getEntryByID("pID")).thenReturn(Map.of("id","pID"));
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.deleteEditAdjustment(reportImplMock,payloadDTO)).thenCallRealMethod();
            AdjustmentDeleteHelper.deleteEditAdjustment(reportImplMock,payloadDTO);
            adjustmentDeleteHelperMockedStatic.verify(()->AdjustmentDeleteHelper.generateOverlappingErrorResponse(List.of(Map.of("metaData",Map.of("status","PENDING"))),"Asia/Kolkata",DateFormats.DD_MMM_YYYY_HH_MM_SS_A));
        }
    }

    @Test
    void deleteEditAdjustment_NoPendingOverlappingEntriesPresent_deleteSubsetsTrue_ShouldCallGenerateSuccessResponseAndInitiateQueueMethod() throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO entryContactPro = new PeopleRelationJDO();
        entryContactPro.setEmailID("email");
        try(MockedStatic<AdjustmentDeleteHelper> adjustmentDeleteHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteHelper.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            AdjustmentDeletePayloadDTO payloadDTO = new AdjustmentDeletePayloadDTO(Map.of("parentId","pID"),true,new PeopleRelationJDO(),entryContactPro,"Asia/Kolkata",DateFormats.DD_MMM_YYYY_HH_MM_SS_A.toString(),"");
            reportsUtilMockedStatic.when(()->ReportsUtil.getParentIDFromEvent(any())).thenReturn("pID");
            reportsUtilMockedStatic.when(()->ReportsUtil.isStatusPending(any())).thenReturn(true);
            reportsUtilMockedStatic.when(()->ReportsUtil.getOverlappingEntriesForEvent(Map.of("id","pID"))).thenReturn(new ArrayList<>(){{add(Map.of("metaData",Map.of("status","PENDING")));}});
            reportsUtilMockedStatic.when(()->ReportsUtil.getIdsListFromEvents(anyList())).thenReturn(List.of("1","2"));
            reportsUtilMockedStatic.when(()->ReportsUtil.getParentIdsListFromEvents(List.of(Map.of("id","adj1"),Map.of("id","adj2")))).thenReturn(List.of("3","4"));
            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportImplMock.getEntryByID("pID")).thenReturn(Map.of("id","pID"));
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.deleteEvents(List.of(Map.of("metaData",Map.of("status","PENDING")),Map.of("parentId","pID")),payloadDTO,reportImplMock)).thenReturn(List.of(Map.of("id","adj1"),Map.of("id","adj2")));
            Mockito.when(reportImplMock.revertEvent("3")).thenReturn(Map.of("id","evt1"));
            Mockito.when(reportImplMock.revertEvent("4")).thenReturn(Map.of("id","evt2"));
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.deleteEditAdjustment(reportImplMock,payloadDTO)).thenCallRealMethod();
            AdjustmentDeleteHelper.deleteEditAdjustment(reportImplMock,payloadDTO);
            adjustmentDeleteHelperMockedStatic.verify(()->AdjustmentDeleteHelper.generateSuccessResponseAndInitiateQueue(List.of(Map.of("id","adj1"),Map.of("id","adj2")),List.of(Map.of("id","evt1"),Map.of("id","evt2")),entryContactPro,new PeopleRelationJDO(),"Asia/Kolkata", DateFormats.DD_MMM_YYYY_HH_MM_SS_A));
        }
    }

    @Test
    void deleteEvents_isRejectAdjustmentRequestTrue_ShouldOnlyCallRejectEventsAndDeleteEventsMethod_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<AdjustmentRejectHelper> adjustmentRejectHelperMockedStatic = Mockito.mockStatic(AdjustmentRejectHelper.class);
            MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class)){
            ReportImpl mockImpl =  Mockito.mock(ReportImpl.class);
            AdjustmentDeletePayloadDTO payload = new AdjustmentDeletePayloadDTO();
            PeopleRelationJDO pro = new PeopleRelationJDO();
            pro.setContactId("123");
            payload.setLoggedInUserPro(pro);
            payload.setMessage("test");
            payload.setRejectAdjustmentRequest(true);
            AdjustmentDeleteHelper.deleteEvents(List.of(Map.of("id","123")),payload,mockImpl);
            Mockito.verify(mockImpl).deleteEvents(List.of("123"));
            adjustmentRejectHelperMockedStatic.verify(()->AdjustmentRejectHelper.updateEventForRejection(Map.of("id","123"),"123","test"));
            schedulingEngineUtilMockedStatic.verify(()->SchedulingEngineUtil.updateEventReq(Map.of("id","123")));
        }
    }

    @Test
    void deleteEvents_isRejectAdjustmentRequestFalse_ShouldOnlyCallDeleteEventsMethod_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<AdjustmentRejectHelper> adjustmentRejectHelperMockedStatic = Mockito.mockStatic(AdjustmentRejectHelper.class)){
            ReportImpl mockImpl =  Mockito.mock(ReportImpl.class);
            AdjustmentDeleteHelper.deleteEvents(List.of(Map.of("id","123")),new AdjustmentDeletePayloadDTO(),mockImpl);
            Mockito.verify(mockImpl).deleteEvents(List.of("123"));
            adjustmentRejectHelperMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void generateCannotDeleteSubsetsResponse_validRun_ShouldReturnGenericResponse(){
        try(MockedConstruction<ReportsDTO> reportsDTOMockedConstruction = Mockito.mockConstruction(ReportsDTO.class)){
            GenericResponse response = AdjustmentDeleteHelper.generateCannotDeleteSubsetsResponse(List.of(Map.of("1","2")),"email","");
            GenericResponse expected = new GenericResponse(false,null,"There are one or more overlapping entries which was already approved, cannot delete the adjustment");
            expected.setData(Map.of("overlappingEntries",List.of(reportsDTOMockedConstruction.constructed().get(0))));
            Assertions.assertEquals(expected,response);
        }
    }

    @Test
    void generateOverlappingErrorResponse_oneNewAdjustmentEvent_ShouldReturnResponseWithOneAdjustmentDTO() throws IOException, NoSuchAlgorithmException {
        try(MockedConstruction<AdjustmentDTO> adjustmentDTOMockedConstruction = Mockito.mockConstruction(AdjustmentDTO.class)){
            GenericResponse actual = AdjustmentDeleteHelper.generateOverlappingErrorResponse(List.of(Map.of()),"",DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            GenericResponse expected = new GenericResponse(false,null,"There are one or more overlapping entries, deleting this adjustment will delete the subsets of this adjustment");
            expected.setData(Map.of("overlappingEntries",List.of(),"overlappingAdjustments",List.of(adjustmentDTOMockedConstruction.constructed().get(0))));
            Assertions.assertEquals(actual,expected);
        }
    }

    @Test
    void generateOverlappingErrorResponse_oneNewOneEditAdjustmentEvent_ShouldReturnResponseWithTwoAdjustmentDTO() throws IOException, NoSuchAlgorithmException {
        try(MockedConstruction<AdjustmentDTO> adjustmentDTOMockedConstruction = Mockito.mockConstruction(AdjustmentDTO.class);
            MockedStatic<AdjustmentHelper> adjustmentHelperMockedStatic = Mockito.mockStatic(AdjustmentHelper.class)){
            AdjustmentHelper adjustmentHelperMock = Mockito.mock(AdjustmentHelper.class);
            Mockito.when(adjustmentHelperMock.getParentEventsUsingListOfParentIDs(List.of("pID"))).thenReturn(List.of(Map.of()));
            Mockito.when(adjustmentHelperMock.constructMapForAllTheParentEventsOfTheList(List.of(Map.of()))).thenReturn(Map.of("pID",Map.of()));
            adjustmentHelperMockedStatic.when(AdjustmentHelper::getInstance).thenReturn(adjustmentHelperMock);
            GenericResponse actual = AdjustmentDeleteHelper.generateOverlappingErrorResponse(List.of(Map.of(),Map.of("parentId","pID")),"",DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            GenericResponse expected = new GenericResponse(false,null,"There are one or more overlapping entries, deleting this adjustment will delete the subsets of this adjustment");
            expected.setData(Map.of("overlappingEntries",List.of(),"overlappingAdjustments",List.of(adjustmentDTOMockedConstruction.constructed().get(0),adjustmentDTOMockedConstruction.constructed().get(1))));
            Assertions.assertEquals(actual,expected);
        }
    }

    @Test
    void generateSuccessResponseAndInitiateQueue_oneNewAdjustmentDeleted_ShouldReturnResponseWithOneAdjustmentDto() throws IOException {
        try(MockedConstruction<AdjustmentDTO> adjustmentDTOMockedConstruction = Mockito.mockConstruction(AdjustmentDTO.class);
            MockedStatic<AdjustmentTaskInitiator> adjustmentTaskInitiatorMockedStatic = Mockito.mockStatic(AdjustmentTaskInitiator.class)){
            GenericResponse actual = AdjustmentDeleteHelper.generateSuccessResponseAndInitiateQueue(List.of(Map.of()),List.of(), new PeopleRelationJDO(), new PeopleRelationJDO(),"",DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            adjustmentTaskInitiatorMockedStatic.verify(()->AdjustmentTaskInitiator.initiateDeleteAdjustmentTaskQueue(List.of(),List.of(adjustmentDTOMockedConstruction.constructed().get(0)),new PeopleRelationJDO(), new PeopleRelationJDO()));
            GenericResponse expected = new GenericResponse(true,null,null);
            expected.setData(Map.of("entry",List.of(),"adjustmentsDeleted",List.of(adjustmentDTOMockedConstruction.constructed().get(0))));
            Assertions.assertEquals(actual,expected);
        }
    }

    @Test
    void generateSuccessResponseAndInitiateQueue_oneNewOneEditAdjustmentDeleted_ShouldReturnResponseWithTwoAdjustmentDtoAndOneReportDto() throws IOException {
        PeopleRelationJDO entryContactPro = new PeopleRelationJDO();
        entryContactPro.setEmailID("email");
        try(MockedConstruction<AdjustmentDTO> adjustmentDTOMockedConstruction = Mockito.mockConstruction(AdjustmentDTO.class);
            MockedConstruction<ReportsDTO> reportsDTOMockedConstruction = Mockito.mockConstruction(ReportsDTO.class);
            MockedStatic<AdjustmentTaskInitiator> adjustmentTaskInitiatorMockedStatic = Mockito.mockStatic(AdjustmentTaskInitiator.class)){
            GenericResponse actual = AdjustmentDeleteHelper.generateSuccessResponseAndInitiateQueue(List.of(Map.of(),Map.of("parentId","pID")),List.of(Map.of("id","pID")), entryContactPro, new PeopleRelationJDO(),"",DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            adjustmentTaskInitiatorMockedStatic.verify(()->AdjustmentTaskInitiator.initiateDeleteAdjustmentTaskQueue(List.of(reportsDTOMockedConstruction.constructed().get(0)),List.of(adjustmentDTOMockedConstruction.constructed().get(0),adjustmentDTOMockedConstruction.constructed().get(1)),entryContactPro, new PeopleRelationJDO()));
            GenericResponse expected = new GenericResponse(true,null,null);
            expected.setData(Map.of("entry",List.of(reportsDTOMockedConstruction.constructed().get(0)),"adjustmentsDeleted",List.of(adjustmentDTOMockedConstruction.constructed().get(0),adjustmentDTOMockedConstruction.constructed().get(1))));
            Assertions.assertEquals(actual,expected);
        }
    }
}
