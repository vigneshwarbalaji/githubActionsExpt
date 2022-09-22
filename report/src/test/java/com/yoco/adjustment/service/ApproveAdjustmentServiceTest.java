package com.yoco.adjustment.service;

import com.yoco.MockEvent;
import com.yoco.adjustment.helper.AdjustmentOverlapHelper;
import com.yoco.adjustment.helper.AdjustmentTaskInitiator;
import com.yoco.adjustment.helper.approve.ApproveAdjustmentHelper;
import com.yoco.adjustment.util.AdjustmentUtil;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.SchedulingEngineUtil;
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

class ApproveAdjustmentServiceTest {

    ApproveAdjustmentService approveAdjustmentService = new ApproveAdjustmentService();

    @Test
    void approveAdjustment_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<ApproveAdjustmentHelper> approveAdjustmentHelperMockedStatic = Mockito.mockStatic(ApproveAdjustmentHelper.class);
            MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class);
            MockedStatic<AdjustmentTaskInitiator> adjustmentTaskInitiatorMockedStatic = Mockito.mockStatic(AdjustmentTaskInitiator.class)){

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForAdmin()));
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(userPro);

            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Map<String,Object> eventMap = MockEvent.getRawEvent();
            Map<String,Object> metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS, REPORT_STATUS.PENDING.toString());
            eventMap.put(SchedulingKeys.METADATA,metaData);
            Mockito.when(reportImplMock.getEntryByID(anyString())).thenReturn(eventMap);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);

            ApproveAdjustmentHelper approveAdjustmentHelperMock = Mockito.mock(ApproveAdjustmentHelper.class);
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractAdminPro(anyString(),any(Contact.class))).thenCallRealMethod();
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractEntry(anyString(),anyString())).thenCallRealMethod();
            Mockito.when(approveAdjustmentHelperMock.updateMetaDetailsWithApproveInfo(anyMap(),anyString(),anyString())).thenReturn(eventMap);
            Mockito.when(approveAdjustmentHelperMock.updateQueryableMetaDetailsWithApproveInfo(anyMap())).thenReturn(eventMap);
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractApprovedResponse(anyMap(),any(PeopleRelationJDO.class))).thenCallRealMethod();
            approveAdjustmentHelperMockedStatic.when(ApproveAdjustmentHelper::getInstance).thenReturn(approveAdjustmentHelperMock);

            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.updateEventReq(anyMap())).thenReturn(Map.of(SchedulingKeys.RESPONSE,true,SchedulingKeys.DATA,eventMap));
            adjustmentTaskInitiatorMockedStatic.when(()-> AdjustmentTaskInitiator.initiateApproveAdjustmentQueue(any(PeopleRelationJDO.class),anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            Contact adminContact = new Contact();
            adminContact.setId("id");
            Assertions.assertTrue(approveAdjustmentService.approveAdjustment("123","entry123",adminContact));
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserPro("123","id"));
            Mockito.verify(reportImplMock).getEntryByID("entry123");
        }
    }

    @Test
    void editApproveAdjustment_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<ApproveAdjustmentHelper> approveAdjustmentHelperMockedStatic = Mockito.mockStatic(ApproveAdjustmentHelper.class);
            MockedStatic<SchedulingEngineUtil> schedulingEngineUtilMockedStatic = Mockito.mockStatic(SchedulingEngineUtil.class);
            MockedStatic<AdjustmentTaskInitiator> adjustmentTaskInitiatorMockedStatic = Mockito.mockStatic(AdjustmentTaskInitiator.class);
            MockedStatic<AdjustmentUtil> adjustmentUtilMockedStatic = Mockito.mockStatic(AdjustmentUtil.class);
            MockedStatic<AdjustmentOverlapHelper> adjustmentOverlapHelperMockedStatic = Mockito.mockStatic(AdjustmentOverlapHelper.class)){

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForAdmin()));
            userPro.setTimeZone(DateConstants.ZONE_ID_IST);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(userPro);

            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Map<String,Object> eventMap = MockEvent.getRawEvent();
            Map<String,Object> metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS, REPORT_STATUS.PENDING.toString());
            eventMap.put(SchedulingKeys.METADATA,metaData);
            Mockito.when(reportImplMock.getEntryByID(anyString())).thenReturn(eventMap);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);

            ApproveAdjustmentHelper approveAdjustmentHelperMock = Mockito.mock(ApproveAdjustmentHelper.class);
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractAdminPro(anyString(),any(Contact.class))).thenCallRealMethod();
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractStartMillisFromPayload(anyMap(),anyString())).thenCallRealMethod();
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractEndMillisFromPayload(anyMap(),anyString())).thenCallRealMethod();
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractApprovalMessage(anyMap())).thenCallRealMethod();
          //  Mockito.when(approveAdjustmentHelperMock.getExclusionEntryIds(anyMap(),anyString())).thenCallRealMethod();
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractEntry(anyString(),anyString())).thenCallRealMethod();

            adjustmentUtilMockedStatic.when(()-> AdjustmentUtil.adjustInTimeMillisForBreakSessionOverlapping(anyString(),anyString(),anyLong(),anyString())).thenReturn(1657204740000l);
            adjustmentOverlapHelperMockedStatic.when(()-> AdjustmentOverlapHelper.validateAndExtractOverlapEntry(anyString(),anyString(),anyLong(),anyLong(),anyString(),anyString())).thenReturn(Map.of());

            Mockito.when(approveAdjustmentHelperMock.updateMetaDetailsWithApproveInfo(anyMap(),anyString(),anyString())).thenReturn(eventMap);
            Mockito.when(approveAdjustmentHelperMock.updateQueryableMetaDetailsWithApproveInfo(anyMap())).thenReturn(eventMap);
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractEditApprovedResponse(anyMap(),any(PeopleRelationJDO.class),anyMap())).thenCallRealMethod();
            approveAdjustmentHelperMockedStatic.when(ApproveAdjustmentHelper::getInstance).thenReturn(approveAdjustmentHelperMock);

            schedulingEngineUtilMockedStatic.when(()-> SchedulingEngineUtil.updateEventReq(anyMap())).thenReturn(Map.of(SchedulingKeys.RESPONSE,true,SchedulingKeys.DATA,eventMap));
            adjustmentTaskInitiatorMockedStatic.when(()-> AdjustmentTaskInitiator.initiateEditApprovedAdjustmentQueue(any(PeopleRelationJDO.class),anyMap(),anyMap())).thenAnswer((Answer<Void>) invocation -> null);

            Contact adminContact = new Contact();
            adminContact.setId("id");
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("inTime","07-Jul-2022 08:09:00 PM");
            payloadMap.put("outTime","07-Jul-2022 08:10:00 PM");
            payloadMap.put("originalID","id1");
            payloadMap.put("message","test");

            approveAdjustmentService.editApproveAdjustment("123","entry123",JsonUtil.getJson(payloadMap),adminContact);
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserPro("123","id"));
            Mockito.verify(reportImplMock).getEntryByID("entry123");
        }
    }

    @Test
    void editApproveAdjustment_overlap_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<ApproveAdjustmentHelper> approveAdjustmentHelperMockedStatic = Mockito.mockStatic(ApproveAdjustmentHelper.class);
            MockedStatic<AdjustmentUtil> adjustmentUtilMockedStatic = Mockito.mockStatic(AdjustmentUtil.class);
            MockedStatic<AdjustmentOverlapHelper> adjustmentOverlapHelperMockedStatic = Mockito.mockStatic(AdjustmentOverlapHelper.class)){

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForAdmin()));
            userPro.setTimeZone(DateConstants.ZONE_ID_IST);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(userPro);

            ReportImpl reportImplMock = Mockito.mock(ReportImpl.class);
            Map<String,Object> eventMap = MockEvent.getRawEvent();
            Map<String,Object> metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS, REPORT_STATUS.PENDING.toString());
            eventMap.put(SchedulingKeys.METADATA,metaData);
            Mockito.when(reportImplMock.getEntryByID(anyString())).thenReturn(eventMap);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportImplMock);

            ApproveAdjustmentHelper approveAdjustmentHelperMock = Mockito.mock(ApproveAdjustmentHelper.class);
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractAdminPro(anyString(),any(Contact.class))).thenCallRealMethod();
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractStartMillisFromPayload(anyMap(),anyString())).thenCallRealMethod();
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractEndMillisFromPayload(anyMap(),anyString())).thenCallRealMethod();
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractApprovalMessage(anyMap())).thenCallRealMethod();
          //  Mockito.when(approveAdjustmentHelperMock.getExclusionEntryIds(anyMap(),anyString())).thenCallRealMethod();
            Mockito.when(approveAdjustmentHelperMock.validateAndExtractEntry(anyString(),anyString())).thenCallRealMethod();
            approveAdjustmentHelperMockedStatic.when(ApproveAdjustmentHelper::getInstance).thenReturn(approveAdjustmentHelperMock);

            adjustmentUtilMockedStatic.when(()-> AdjustmentUtil.adjustInTimeMillisForBreakSessionOverlapping(anyString(),anyString(),anyLong(),anyString())).thenReturn(1657204740000l);
            adjustmentOverlapHelperMockedStatic.when(()-> AdjustmentOverlapHelper.validateAndExtractOverlapEntry(anyString(),anyString(),anyLong(),anyLong(),anyString(),anyString())).thenReturn(Map.of(Commons.SUCCESS,false));

            Contact adminContact = new Contact();
            adminContact.setId("id");
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("inTime","07-Jul-2022 08:09:00 PM");
            payloadMap.put("outTime","07-Jul-2022 08:10:00 PM");
            payloadMap.put("originalID","id1");
            payloadMap.put("message","test");

            approveAdjustmentService.editApproveAdjustment("123","entry123",JsonUtil.getJson(payloadMap),adminContact);
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserPro("123","id"));
            Mockito.verify(reportImplMock).getEntryByID("entry123");
            Mockito.verify(approveAdjustmentHelperMock).validateAndExtractAdminPro("123",adminContact);
            Mockito.verify(approveAdjustmentHelperMock).validateAndExtractStartMillisFromPayload(payloadMap,DateConstants.ZONE_ID_IST);
            Mockito.verify(approveAdjustmentHelperMock).validateAndExtractEndMillisFromPayload(payloadMap,DateConstants.ZONE_ID_IST);
            Mockito.verify(approveAdjustmentHelperMock).validateAndExtractApprovalMessage(payloadMap);
           // Mockito.verify(approveAdjustmentHelperMock).getExclusionEntryIds(payloadMap,"entry123");
            Mockito.verify(approveAdjustmentHelperMock).validateAndExtractEntry("entry123","123");
        }
    }

}