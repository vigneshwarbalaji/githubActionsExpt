package com.yoco.adjustment.helper.approve;

import com.yoco.adjustment.helper.AdjustmentTaskInitiator;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.enums.ADJUSTMENTS_ERROR_RESPONSE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;

class ApproveAdjustmentHelperTest {

    ApproveAdjustmentHelper approveAdjustmentHelper = ApproveAdjustmentHelper.getInstance();

    @Test
    void validateAndExtractAdminPro_null_pro_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(null);
            Contact contact = new Contact();
            contact.setId("123");
            approveAdjustmentHelper.validateAndExtractAdminPro("accountId",contact);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractAdminPro_valid_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<PermissionManager> permissionManagerMockedStatic = Mockito.mockStatic(PermissionManager.class)){
            permissionManagerMockedStatic.when(()-> PermissionManager.checkUserHasAdjustmentEditAccess(any(PeopleRelationJDO.class))).thenReturn(true);
            PeopleRelationJDO user = new PeopleRelationJDO();
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(user);
            Contact contact = new Contact();
            contact.setId("123");
            PeopleRelationJDO pro = approveAdjustmentHelper.validateAndExtractAdminPro("accountId",contact);
            user.setContact(contact);
            Assertions.assertEquals(user,pro);
        }
    }

    @Test
    void validateAndExtractEntry_accountNotFound_test() {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            reportsUtilMockedStatic.when(()-> ReportsUtil.getAccountIDFromEvent(anyMap())).thenReturn("");
            ReportImpl reportMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportMock.getEntryByID(anyString())).thenReturn(Map.of());
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportMock);
            approveAdjustmentHelper.validateAndExtractEntry("entryId","accountId");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.ACCOUNT_NOT_FOUND.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractEntry_eventDeleted_test() {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            reportsUtilMockedStatic.when(()-> ReportsUtil.getAccountIDFromEvent(anyMap())).thenReturn("accountId");
            reportsUtilMockedStatic.when(()-> ReportsUtil.isEventDeleted(anyMap())).thenReturn(true);
            ReportImpl reportMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportMock.getEntryByID(anyString())).thenReturn(Map.of());
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportMock);
            approveAdjustmentHelper.validateAndExtractEntry("entryId","accountId");
        }catch (Exception e){
            Assertions.assertEquals(ADJUSTMENTS_ERROR_RESPONSE.ACTION_ALREADY_TAKEN.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractEntry_event_not_pending_test() {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            reportsUtilMockedStatic.when(()-> ReportsUtil.getAccountIDFromEvent(anyMap())).thenReturn("accountId");
            reportsUtilMockedStatic.when(()-> ReportsUtil.isEventDeleted(anyMap())).thenReturn(false);
            reportsUtilMockedStatic.when(()-> ReportsUtil.isStatusPending(anyMap())).thenReturn(false);
            ReportImpl reportMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportMock.getEntryByID(anyString())).thenReturn(Map.of());
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportMock);
            approveAdjustmentHelper.validateAndExtractEntry("entryId","accountId");
        }catch (Exception e){
            Assertions.assertEquals(ADJUSTMENTS_ERROR_RESPONSE.ACTION_ALREADY_TAKEN.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractEntry_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            reportsUtilMockedStatic.when(()-> ReportsUtil.getAccountIDFromEvent(anyMap())).thenReturn("accountId");
            reportsUtilMockedStatic.when(()-> ReportsUtil.isEventDeleted(anyMap())).thenReturn(false);
            reportsUtilMockedStatic.when(()-> ReportsUtil.isStatusPending(anyMap())).thenReturn(true);
            ReportImpl reportMock = Mockito.mock(ReportImpl.class);
            Mockito.when(reportMock.getEntryByID(anyString())).thenReturn(Map.of());
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportMock);
            Assertions.assertNotNull(approveAdjustmentHelper.validateAndExtractEntry("entryId","accountId"));
        }
    }

    @Test
    void extractEntryTimeDetailsForNewAdjustment_newAdjustment_test(){
        try(MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            reportsUtilMockedStatic.when(()-> ReportsUtil.isAdjustmentNew(anyMap())).thenReturn(true);
            Map<String,Object> entryMap = new HashMap<>();
            entryMap.put(SchedulingKeys.START_TIME,1659609896000l);
            entryMap.put(SchedulingKeys.END_TIME,1659611858000l);
            Map<String,Object> response = approveAdjustmentHelper.extractEntryTimeDetailsForNewAdjustment(entryMap);
            Assertions.assertEquals(entryMap,response);
        }
    }

    @Test
    void extractEntryTimeDetailsForNewAdjustment_editAdjustment_test(){
        try(MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class)){
            reportsUtilMockedStatic.when(()-> ReportsUtil.isAdjustmentNew(anyMap())).thenReturn(false);
            Map<String,Object> entryMap = new HashMap<>();
            entryMap.put(SchedulingKeys.START_TIME,1659609896000l);
            entryMap.put(SchedulingKeys.END_TIME,1659611858000l);
            Map<String,Object> response = approveAdjustmentHelper.extractEntryTimeDetailsForNewAdjustment(entryMap);
            Assertions.assertEquals(Map.of(),response);
        }
    }

    @Test
    void updateMetaDetailsWithApproveInfo_test(){
        Map<String,Object> entryMap = new HashMap<>();
        Map<String,Object> additionalInfo = new HashMap<>();
        Map<String,Object> adjustmentInfo = new HashMap<>();
        adjustmentInfo.put(SchedulingKeys.ADJUSTMENT_INFO,new HashMap<>());
        additionalInfo.put(SchedulingKeys.ADDITIONAL_INFO,adjustmentInfo);
        entryMap.put(SchedulingKeys.METADATA,additionalInfo);
        Map<String,Object> responseMap = approveAdjustmentHelper.updateMetaDetailsWithApproveInfo(entryMap,"contact123","Test");
        Map<String,Object> expectedAdditionalInfo = (Map<String, Object>) responseMap.get(SchedulingKeys.ADDITIONAL_INFO);
        Map<String,Object> expectedAdjustmentInfo = (Map<String, Object>) expectedAdditionalInfo.get(SchedulingKeys.ADJUSTMENT_INFO);
        Assertions.assertTrue(expectedAdjustmentInfo.containsKey(SchedulingKeys.APPROVE_INFO));
        Assertions.assertNotNull(expectedAdjustmentInfo.get(SchedulingKeys.APPROVE_INFO));
        Assertions.assertEquals(REPORT_STATUS.MANUAL_CLOCKED_OUT,responseMap.get(SchedulingKeys.STATUS));
    }

    @Test
    void updateQueryableMetaDetailsWithApproveInfo_test(){
        Map<String,Object> entryMap = new HashMap<>();
        Map<String,Object> queryableMeta = new HashMap<>();
        entryMap.put(SchedulingKeys.QUERYABLE_META,queryableMeta);
        Map<String,Object> expectedQueryableMeta = approveAdjustmentHelper.updateQueryableMetaDetailsWithApproveInfo(entryMap);
        Assertions.assertEquals(REPORT_STATUS.APPROVED, expectedQueryableMeta.get(SchedulingKeys.ADJ_STATUS));
        Assertions.assertEquals(REPORT_STATUS.MANUAL_CLOCKED_OUT, expectedQueryableMeta.get(SchedulingKeys.STATUS));
    }

    @Test
    void validateAndExtractStartMillisFromPayload_invalid_payload_test(){
        try{
            approveAdjustmentHelper.validateAndExtractStartMillisFromPayload(Map.of(), DateConstants.ZONE_ID_IST);
        }catch (Exception e){
            Assertions.assertEquals(ADJUSTMENTS_ERROR_RESPONSE.INVALID_IN_TIME.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractStartMillisFromPayload_invalid_inTime_test(){
        try{
            approveAdjustmentHelper.validateAndExtractStartMillisFromPayload(Map.of("inTime",""), DateConstants.ZONE_ID_IST);
        }catch (Exception e){
            Assertions.assertEquals(ADJUSTMENTS_ERROR_RESPONSE.INVALID_IN_TIME.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractStartMillisFromPayload_valid_inTime_test(){
        Assertions.assertEquals(1657204740000l,approveAdjustmentHelper.validateAndExtractStartMillisFromPayload(Map.of("inTime","07-Jul-2022 08:09:00 PM"), DateConstants.ZONE_ID_IST));
    }

    @Test
    void validateAndExtractEndMillisFromPayload_invalid_payload_test(){
        try{
            approveAdjustmentHelper.validateAndExtractEndMillisFromPayload(Map.of(), DateConstants.ZONE_ID_IST);
        }catch (Exception e){
            Assertions.assertEquals(ADJUSTMENTS_ERROR_RESPONSE.INVALID_OUT_TIME.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractEndMillisFromPayload_invalid_outTime_test(){
        try{
            approveAdjustmentHelper.validateAndExtractEndMillisFromPayload(Map.of("outTime",""), DateConstants.ZONE_ID_IST);
        }catch (Exception e){
            Assertions.assertEquals(ADJUSTMENTS_ERROR_RESPONSE.INVALID_OUT_TIME.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractEndMillisFromPayload_valid_outTime_test(){
        Assertions.assertEquals(1657204740000l,approveAdjustmentHelper.validateAndExtractEndMillisFromPayload(Map.of("outTime","07-Jul-2022 08:09:00 PM"), DateConstants.ZONE_ID_IST));
    }

    @Test
    void validateAndExtractApprovalMessage_noMessageKey_test(){
        try{
            approveAdjustmentHelper.validateAndExtractApprovalMessage(Map.of());
        }catch (Exception e){
            Assertions.assertEquals(ADJUSTMENTS_ERROR_RESPONSE.INVALID_MESSAGE.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractApprovalMessage_emptyMessage_test(){
        try{
            approveAdjustmentHelper.validateAndExtractApprovalMessage(Map.of("message",""));
        }catch (Exception e){
            Assertions.assertEquals(ADJUSTMENTS_ERROR_RESPONSE.INVALID_MESSAGE.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractApprovalMessage_escapeHtmlMessage_test(){
        try{
            approveAdjustmentHelper.validateAndExtractApprovalMessage(Map.of("message","<script></script>"));
        }catch (Exception e){
            Assertions.assertEquals(ADJUSTMENTS_ERROR_RESPONSE.INVALID_MESSAGE.value(),e.getMessage());
        }
    }

    @Test
    void validateAndExtractApprovalMessage_validMessage_test(){
        Assertions.assertEquals("Test",approveAdjustmentHelper.validateAndExtractApprovalMessage(Map.of("message","Test")));
    }

    @Test
    void validateAndExtractApprovedResponse_empty_eventResponse_test() throws IOException {
        Assertions.assertFalse(approveAdjustmentHelper.validateAndExtractApprovedResponse(Map.of(),null));
    }

    @Test
    void validateAndExtractApprovedResponse_false_eventResponse_test() throws IOException {
        Assertions.assertFalse(approveAdjustmentHelper.validateAndExtractApprovedResponse(Map.of(SchedulingKeys.RESPONSE,false),null));
    }

    @Test
    void validateAndExtractApprovedResponse_no_eventData_test() throws IOException {
        Assertions.assertFalse(approveAdjustmentHelper.validateAndExtractApprovedResponse(Map.of(SchedulingKeys.RESPONSE,true),null));
    }

    @Test
    void validateAndExtractApprovedResponse_empty_eventData_test() throws IOException {
        Assertions.assertFalse(approveAdjustmentHelper.validateAndExtractApprovedResponse(Map.of(SchedulingKeys.RESPONSE,true,SchedulingKeys.DATA,Map.of()),null));
    }

    @Test
    void validateAndExtractApprovedResponse_valid_eventData_test() throws IOException {
        try(MockedStatic<AdjustmentTaskInitiator> adjustmentTaskInitiatorMockedStatic = Mockito.mockStatic(AdjustmentTaskInitiator.class)){
            adjustmentTaskInitiatorMockedStatic.when(() -> AdjustmentTaskInitiator.initiateApproveAdjustmentQueue(any(PeopleRelationJDO.class),anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            Assertions.assertTrue(approveAdjustmentHelper.validateAndExtractApprovedResponse(Map.of(SchedulingKeys.RESPONSE,true,SchedulingKeys.DATA,Map.of("key","data")),userPro));
            adjustmentTaskInitiatorMockedStatic.verify(() -> AdjustmentTaskInitiator.initiateApproveAdjustmentQueue(userPro,Map.of("key","data")));
        }
    }

    @Test
    void validateAndExtractEditApprovedResponse_empty_eventResponse_test() throws IOException {
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(approveAdjustmentHelper.validateAndExtractEditApprovedResponse(Map.of(),null,Map.of())));
    }

    @Test
    void validateAndExtractEditApprovedResponse_false_eventResponse_test() throws IOException {
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(approveAdjustmentHelper.validateAndExtractEditApprovedResponse(Map.of(SchedulingKeys.RESPONSE,false),null,Map.of())));
    }

    @Test
    void validateAndExtractEditApprovedResponse_no_eventData_test() throws IOException {
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(approveAdjustmentHelper.validateAndExtractEditApprovedResponse(Map.of(SchedulingKeys.RESPONSE,true),null,Map.of())));
    }

    @Test
    void validateAndExtractEditApprovedResponse_empty_eventData_test() throws IOException {
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(approveAdjustmentHelper.validateAndExtractEditApprovedResponse(Map.of(SchedulingKeys.RESPONSE,true,SchedulingKeys.DATA,Map.of()),null,Map.of())));
    }

    @Test
    void validateAndExtractEditApprovedResponse_valid_eventData_test() throws IOException {
        try(MockedStatic<AdjustmentTaskInitiator> adjustmentTaskInitiatorMockedStatic = Mockito.mockStatic(AdjustmentTaskInitiator.class)){
            adjustmentTaskInitiatorMockedStatic.when(() -> AdjustmentTaskInitiator.initiateEditApprovedAdjustmentQueue(any(PeopleRelationJDO.class),anyMap(),anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            Map<String,Object> response = approveAdjustmentHelper.validateAndExtractEditApprovedResponse(Map.of(SchedulingKeys.RESPONSE,true,SchedulingKeys.DATA,Map.of("key","data")),userPro,Map.of());
            Assertions.assertEquals(Map.of(Commons.SUCCESS,true),response);
            adjustmentTaskInitiatorMockedStatic.verify(() -> AdjustmentTaskInitiator.initiateEditApprovedAdjustmentQueue(userPro,Map.of("key","data"),Map.of()));
        }
    }

}