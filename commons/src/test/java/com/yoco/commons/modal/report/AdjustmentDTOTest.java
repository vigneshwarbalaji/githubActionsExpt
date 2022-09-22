package com.yoco.commons.modal.report;

import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.InternalUsage;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ProjectImpl;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.REPORT_STATUS;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyString;

class AdjustmentDTOTest {

    Map<String,Object> adjEvent = new HashMap<>();

    @BeforeEach
    void setup(){
        adjEvent.put(SchedulingKeys.ID,"id");
        adjEvent.put(SchedulingKeys.MERCHANT,"accountId");
        ArrayList<String> contactIdList = new ArrayList<>();
        contactIdList.add("contactId");
        adjEvent.put(SchedulingKeys.PROVIDER,contactIdList);

        adjEvent.put(SchedulingKeys.SOURCE, ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value());
        adjEvent.put(SchedulingKeys.IS_DELETED,false);

        adjEvent.put(SchedulingKeys.START_TIME,1629104514000L);
        adjEvent.put(SchedulingKeys.END_TIME,1629104534000L);
    }

    @Test
    void AdjustmentDTO_valid_keys_test(){

        adjEvent.put(SchedulingKeys.PAYMENT_STATUS,InternalUsage.DEFAULT_PAYROLL);

        AdjustmentDTO adjustmentDTO = new AdjustmentDTO(adjEvent,null, DateConstants.ZONE_ID_IST, DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
        Assertions.assertEquals("id",adjustmentDTO.getId());
        Assertions.assertEquals("id",adjustmentDTO.getEntryID());
        Assertions.assertEquals("accountId",adjustmentDTO.getAccountID());
        Assertions.assertEquals("contactId",adjustmentDTO.getContactID());
        Assertions.assertEquals(false,adjustmentDTO.getIsDeleted());
        Assertions.assertEquals(true,adjustmentDTO.getIsNew());
        Assertions.assertEquals("",adjustmentDTO.getProjectID());
        Assertions.assertEquals(InternalUsage.NO_PROJECT,adjustmentDTO.getProjectName());
        Assertions.assertEquals(1629104514000L,adjustmentDTO.getAdjustedInTime());
        Assertions.assertEquals("16-Aug-2021 02:31:54 PM",adjustmentDTO.getAdjustedInTimeText());
        Assertions.assertEquals(1629104534000L,adjustmentDTO.getAdjustedOutTime());
        Assertions.assertEquals("16-Aug-2021 02:32:14 PM",adjustmentDTO.getAdjustedOutTimeText());
        Assertions.assertEquals(1629104514000L,adjustmentDTO.getOriginalInTime());
        Assertions.assertEquals("16-Aug-2021 02:31:54 PM",adjustmentDTO.getOriginalInTimeText());
        Assertions.assertEquals(1629104534000L,adjustmentDTO.getOriginalOutTime());
        Assertions.assertEquals("16-Aug-2021 02:32:14 PM",adjustmentDTO.getOriginalOutTimeText());
    }

    @Test
    void AdjustmentDTO_valid_project_test(){
        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class)){

            ProjectJDO projectJDO = new ProjectJDO();
            projectJDO.setProjectName("projName");
            ProjectImpl project = Mockito.mock(ProjectImpl.class);
            Mockito.when(project.getProject(anyString())).thenReturn(projectJDO);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(project);

            adjEvent.put(SchedulingKeys.CALENDAR,"projectId");

            Map<String,Object> parentEvent = new HashMap<>();
            parentEvent.put(SchedulingKeys.ID,"entryId");
            parentEvent.put(SchedulingKeys.START_TIME,1629104400000L);
            parentEvent.put(SchedulingKeys.END_TIME,1629104700000L);

            Map<String,Object> metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");

            adjEvent.put(SchedulingKeys.METADATA,metaData);
            adjEvent.put(SchedulingKeys.PAYMENT_STATUS,REPORT_STATUS.PENDING.toString());

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO(adjEvent,parentEvent, DateConstants.ZONE_ID_IST, DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            Assertions.assertFalse(adjustmentDTO.getIsNew());
            Assertions.assertEquals("projectId",adjustmentDTO.getProjectID());
            Assertions.assertEquals("projName",adjustmentDTO.getProjectName());
            Assertions.assertEquals(1629104514000L,adjustmentDTO.getAdjustedInTime());
            Assertions.assertEquals("16-Aug-2021 02:31:54 PM",adjustmentDTO.getAdjustedInTimeText());
            Assertions.assertEquals(1629104534000L,adjustmentDTO.getAdjustedOutTime());
            Assertions.assertEquals("16-Aug-2021 02:32:14 PM",adjustmentDTO.getAdjustedOutTimeText());
            Assertions.assertEquals(1629104400000L,adjustmentDTO.getOriginalInTime());
            Assertions.assertEquals("16-Aug-2021 02:30:00 PM",adjustmentDTO.getOriginalInTimeText());
            Assertions.assertEquals(1629104700000L,adjustmentDTO.getOriginalOutTime());
            Assertions.assertEquals("16-Aug-2021 02:35:00 PM",adjustmentDTO.getOriginalOutTimeText());
            Assertions.assertEquals("MANUAL_CLOCKED_OUT",adjustmentDTO.getStatus());
            Assertions.assertEquals("",adjustmentDTO.getPayrollStatus());
            Assertions.assertEquals(0L,adjustmentDTO.getRequestedTime());
            Assertions.assertEquals("",adjustmentDTO.getRequestedTimeText());
            Assertions.assertEquals("",adjustmentDTO.getRequestedBy());
            Assertions.assertEquals("",adjustmentDTO.getRequestMessage());
            Assertions.assertEquals(0L,adjustmentDTO.getApprovedTime());
            Assertions.assertEquals("",adjustmentDTO.getApprovedTimeText());
            Assertions.assertEquals("",adjustmentDTO.getApprovedBy());
            Assertions.assertEquals("",adjustmentDTO.getApprovedMessage());
            Assertions.assertEquals("",adjustmentDTO.getRejectMessage());
        }
    }

    @Test
    void AdjustmentDTO_valid_paymentStatus_test(){
        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class)){

            ProjectImpl project = Mockito.mock(ProjectImpl.class);
            Mockito.when(project.getProject(anyString())).thenReturn(null);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(project);

            adjEvent.put(SchedulingKeys.CALENDAR,"projectId");

            Map<String,Object> parentEvent = new HashMap<>();
            parentEvent.put(SchedulingKeys.ID,"entryId");
            parentEvent.put(SchedulingKeys.START_TIME,1629104400000L);
            parentEvent.put(SchedulingKeys.END_TIME,1629104700000L);

            Map<String,Object> metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");
            metaData.put(SchedulingKeys.ADDITIONAL_INFO,new HashMap<>());
            adjEvent.put(SchedulingKeys.METADATA,metaData);
            adjEvent.put(SchedulingKeys.PAYMENT_STATUS,REPORT_STATUS.APPROVED.toString());

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO(adjEvent,parentEvent, DateConstants.ZONE_ID_IST, DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            Assertions.assertFalse(adjustmentDTO.getIsDeleted());
            Assertions.assertFalse(adjustmentDTO.getIsNew());
            Assertions.assertEquals("",adjustmentDTO.getProjectID());
            Assertions.assertEquals(InternalUsage.NO_PROJECT,adjustmentDTO.getProjectName());
            Assertions.assertEquals(1629104514000L,adjustmentDTO.getAdjustedInTime());
            Assertions.assertEquals("16-Aug-2021 02:31:54 PM",adjustmentDTO.getAdjustedInTimeText());
            Assertions.assertEquals(1629104534000L,adjustmentDTO.getAdjustedOutTime());
            Assertions.assertEquals("16-Aug-2021 02:32:14 PM",adjustmentDTO.getAdjustedOutTimeText());
            Assertions.assertEquals(1629104400000L,adjustmentDTO.getOriginalInTime());
            Assertions.assertEquals("16-Aug-2021 02:30:00 PM",adjustmentDTO.getOriginalInTimeText());
            Assertions.assertEquals(1629104700000L,adjustmentDTO.getOriginalOutTime());
            Assertions.assertEquals("16-Aug-2021 02:35:00 PM",adjustmentDTO.getOriginalOutTimeText());
            Assertions.assertEquals("MANUAL_CLOCKED_OUT",adjustmentDTO.getStatus());
            Assertions.assertEquals(REPORT_STATUS.APPROVED.toString(),adjustmentDTO.getPayrollStatus());
            Assertions.assertEquals(0L,adjustmentDTO.getRequestedTime());
            Assertions.assertEquals("",adjustmentDTO.getRequestedTimeText());
            Assertions.assertEquals("",adjustmentDTO.getRequestedBy());
            Assertions.assertEquals("",adjustmentDTO.getRequestMessage());
            Assertions.assertEquals(0L,adjustmentDTO.getApprovedTime());
            Assertions.assertEquals("",adjustmentDTO.getApprovedTimeText());
            Assertions.assertEquals("",adjustmentDTO.getApprovedBy());
            Assertions.assertEquals("",adjustmentDTO.getApprovedMessage());
            Assertions.assertEquals("",adjustmentDTO.getRejectMessage());
        }
    }

    @Test
    void AdjustmentDTO_empty_adjustmentInfo_test(){
        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class)){

            ProjectImpl project = Mockito.mock(ProjectImpl.class);
            Mockito.when(project.getProject(anyString())).thenReturn(null);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(project);

            adjEvent.put(SchedulingKeys.CALENDAR,"projectId");

            Map<String,Object> parentEvent = new HashMap<>();
            parentEvent.put(SchedulingKeys.ID,"entryId");
            parentEvent.put(SchedulingKeys.START_TIME,1629104400000L);
            parentEvent.put(SchedulingKeys.END_TIME,1629104700000L);

            Map<String,Object> metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");
            Map<String,Object> additionalInfo = new HashMap<>();
            additionalInfo.put(SchedulingKeys.ADJUSTMENT_INFO,new HashMap<>());
            metaData.put(SchedulingKeys.ADDITIONAL_INFO,additionalInfo);
            adjEvent.put(SchedulingKeys.METADATA,metaData);
            adjEvent.put(SchedulingKeys.PAYMENT_STATUS,REPORT_STATUS.APPROVED.toString());

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO(adjEvent,parentEvent, DateConstants.ZONE_ID_IST, DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            Assertions.assertFalse(adjustmentDTO.getIsDeleted());
            Assertions.assertFalse(adjustmentDTO.getIsNew());
            Assertions.assertEquals("",adjustmentDTO.getProjectID());
            Assertions.assertEquals(InternalUsage.NO_PROJECT,adjustmentDTO.getProjectName());
            Assertions.assertEquals(1629104514000L,adjustmentDTO.getAdjustedInTime());
            Assertions.assertEquals("16-Aug-2021 02:31:54 PM",adjustmentDTO.getAdjustedInTimeText());
            Assertions.assertEquals(1629104534000L,adjustmentDTO.getAdjustedOutTime());
            Assertions.assertEquals("16-Aug-2021 02:32:14 PM",adjustmentDTO.getAdjustedOutTimeText());
            Assertions.assertEquals(1629104400000L,adjustmentDTO.getOriginalInTime());
            Assertions.assertEquals("16-Aug-2021 02:30:00 PM",adjustmentDTO.getOriginalInTimeText());
            Assertions.assertEquals(1629104700000L,adjustmentDTO.getOriginalOutTime());
            Assertions.assertEquals("16-Aug-2021 02:35:00 PM",adjustmentDTO.getOriginalOutTimeText());
            Assertions.assertEquals("MANUAL_CLOCKED_OUT",adjustmentDTO.getStatus());
            Assertions.assertEquals(REPORT_STATUS.APPROVED.toString(),adjustmentDTO.getPayrollStatus());
            Assertions.assertEquals(0L,adjustmentDTO.getRequestedTime());
            Assertions.assertEquals("",adjustmentDTO.getRequestedTimeText());
            Assertions.assertEquals("",adjustmentDTO.getRequestedBy());
            Assertions.assertEquals("",adjustmentDTO.getRequestMessage());
            Assertions.assertEquals(0L,adjustmentDTO.getApprovedTime());
            Assertions.assertEquals("",adjustmentDTO.getApprovedTimeText());
            Assertions.assertEquals("",adjustmentDTO.getApprovedBy());
            Assertions.assertEquals("",adjustmentDTO.getApprovedMessage());
            Assertions.assertEquals("",adjustmentDTO.getRejectMessage());
        }
    }

    @Test
    void AdjustmentDTO_valid_requestInfo_test(){
        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class)){

            ProjectImpl project = Mockito.mock(ProjectImpl.class);
            Mockito.when(project.getProject(anyString())).thenReturn(null);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(project);

            adjEvent.put(SchedulingKeys.CALENDAR,"projectId");

            Map<String,Object> parentEvent = new HashMap<>();
            parentEvent.put(SchedulingKeys.ID,"entryId");
            parentEvent.put(SchedulingKeys.START_TIME,1629104400000L);
            parentEvent.put(SchedulingKeys.END_TIME,1629104700000L);

            Map<String,Object> metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");
            Map<String,Object> requestInfo = new HashMap<>();
            requestInfo.put(SchedulingKeys.WHEN,1629104400000d);
            requestInfo.put(SchedulingKeys.BY,"requestedId");
            requestInfo.put(SchedulingKeys.NOTES,"requestMessage");

            Map<String,Object> adjustmentInfo = new HashMap<>();
            adjustmentInfo.put(SchedulingKeys.REQUEST_INFO,requestInfo);

            Map<String,Object> additionalInfo = new HashMap<>();
            additionalInfo.put(SchedulingKeys.ADJUSTMENT_INFO,adjustmentInfo);
            metaData.put(SchedulingKeys.ADDITIONAL_INFO,additionalInfo);

            adjEvent.put(SchedulingKeys.METADATA,metaData);
            adjEvent.put(SchedulingKeys.PAYMENT_STATUS,REPORT_STATUS.APPROVED.toString());

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO(adjEvent,parentEvent, DateConstants.ZONE_ID_IST, DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            Assertions.assertFalse(adjustmentDTO.getIsDeleted());
            Assertions.assertFalse(adjustmentDTO.getIsNew());
            Assertions.assertEquals("",adjustmentDTO.getProjectID());
            Assertions.assertEquals(InternalUsage.NO_PROJECT,adjustmentDTO.getProjectName());
            Assertions.assertEquals(1629104514000L,adjustmentDTO.getAdjustedInTime());
            Assertions.assertEquals("16-Aug-2021 02:31:54 PM",adjustmentDTO.getAdjustedInTimeText());
            Assertions.assertEquals(1629104534000L,adjustmentDTO.getAdjustedOutTime());
            Assertions.assertEquals("16-Aug-2021 02:32:14 PM",adjustmentDTO.getAdjustedOutTimeText());
            Assertions.assertEquals(1629104400000L,adjustmentDTO.getOriginalInTime());
            Assertions.assertEquals("16-Aug-2021 02:30:00 PM",adjustmentDTO.getOriginalInTimeText());
            Assertions.assertEquals(1629104700000L,adjustmentDTO.getOriginalOutTime());
            Assertions.assertEquals("16-Aug-2021 02:35:00 PM",adjustmentDTO.getOriginalOutTimeText());
            Assertions.assertEquals("MANUAL_CLOCKED_OUT",adjustmentDTO.getStatus());
            Assertions.assertEquals(REPORT_STATUS.APPROVED.toString(),adjustmentDTO.getPayrollStatus());
            Assertions.assertEquals(1629104400000L,adjustmentDTO.getRequestedTime());
            Assertions.assertEquals("16-Aug-2021 02:30:00 PM",adjustmentDTO.getRequestedTimeText());
            Assertions.assertEquals("requestedId",adjustmentDTO.getRequestedBy());
            Assertions.assertEquals("requestMessage",adjustmentDTO.getRequestMessage());
            Assertions.assertEquals(0L,adjustmentDTO.getApprovedTime());
            Assertions.assertEquals("",adjustmentDTO.getApprovedTimeText());
            Assertions.assertEquals("",adjustmentDTO.getApprovedBy());
            Assertions.assertEquals("",adjustmentDTO.getApprovedMessage());
            Assertions.assertEquals("",adjustmentDTO.getRejectMessage());
        }
    }

    @Test
    void AdjustmentDTO_valid_ApproveInfo_test(){
        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class)){

            ProjectImpl project = Mockito.mock(ProjectImpl.class);
            Mockito.when(project.getProject(anyString())).thenReturn(null);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(project);

            adjEvent.put(SchedulingKeys.CALENDAR,"projectId");

            Map<String,Object> parentEvent = new HashMap<>();
            parentEvent.put(SchedulingKeys.ID,"entryId");
            parentEvent.put(SchedulingKeys.START_TIME,1629104400000L);
            parentEvent.put(SchedulingKeys.END_TIME,1629104700000L);

            Map<String,Object> metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");
            Map<String,Object> approveInfo = new HashMap<>();
            approveInfo.put(SchedulingKeys.WHEN,1629104400000d);
            approveInfo.put(SchedulingKeys.BY,"requestedId");
            approveInfo.put(SchedulingKeys.NOTES,"requestMessage");

            Map<String,Object> adjustmentInfo = new HashMap<>();
            adjustmentInfo.put(SchedulingKeys.APPROVE_INFO,approveInfo);

            Map<String,Object> additionalInfo = new HashMap<>();
            additionalInfo.put(SchedulingKeys.ADJUSTMENT_INFO,adjustmentInfo);
            metaData.put(SchedulingKeys.ADDITIONAL_INFO,additionalInfo);

            adjEvent.put(SchedulingKeys.METADATA,metaData);
            adjEvent.put(SchedulingKeys.PAYMENT_STATUS,REPORT_STATUS.APPROVED.toString());

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO(adjEvent,parentEvent, DateConstants.ZONE_ID_IST, DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            Assertions.assertFalse(adjustmentDTO.getIsDeleted());
            Assertions.assertFalse(adjustmentDTO.getIsNew());
            Assertions.assertEquals("",adjustmentDTO.getProjectID());
            Assertions.assertEquals(InternalUsage.NO_PROJECT,adjustmentDTO.getProjectName());
            Assertions.assertEquals(1629104514000L,adjustmentDTO.getAdjustedInTime());
            Assertions.assertEquals("16-Aug-2021 02:31:54 PM",adjustmentDTO.getAdjustedInTimeText());
            Assertions.assertEquals(1629104534000L,adjustmentDTO.getAdjustedOutTime());
            Assertions.assertEquals("16-Aug-2021 02:32:14 PM",adjustmentDTO.getAdjustedOutTimeText());
            Assertions.assertEquals(1629104400000L,adjustmentDTO.getOriginalInTime());
            Assertions.assertEquals("16-Aug-2021 02:30:00 PM",adjustmentDTO.getOriginalInTimeText());
            Assertions.assertEquals(1629104700000L,adjustmentDTO.getOriginalOutTime());
            Assertions.assertEquals("16-Aug-2021 02:35:00 PM",adjustmentDTO.getOriginalOutTimeText());
            Assertions.assertEquals(REPORT_STATUS.APPROVED.toString(),adjustmentDTO.getStatus());
            Assertions.assertEquals(REPORT_STATUS.APPROVED.toString(),adjustmentDTO.getPayrollStatus());
            Assertions.assertEquals(0L,adjustmentDTO.getRequestedTime());
            Assertions.assertEquals("",adjustmentDTO.getRequestedTimeText());
            Assertions.assertEquals("",adjustmentDTO.getRequestedBy());
            Assertions.assertEquals("",adjustmentDTO.getRequestMessage());
            Assertions.assertEquals(1629104400000L,adjustmentDTO.getApprovedTime());
            Assertions.assertEquals("16-Aug-2021 02:30:00 PM",adjustmentDTO.getApprovedTimeText());
            Assertions.assertEquals("requestedId",adjustmentDTO.getApprovedBy());
            Assertions.assertEquals("requestMessage",adjustmentDTO.getApprovedMessage());
            Assertions.assertEquals("",adjustmentDTO.getRejectMessage());
        }
    }

    @Test
    void AdjustmentDTO_valid_ApproveInfo_with_long_when_test(){
        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class)){

            ProjectImpl project = Mockito.mock(ProjectImpl.class);
            Mockito.when(project.getProject(anyString())).thenReturn(null);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(project);

            adjEvent.put(SchedulingKeys.CALENDAR,"projectId");

            Map<String,Object> parentEvent = new HashMap<>();
            parentEvent.put(SchedulingKeys.ID,"entryId");
            parentEvent.put(SchedulingKeys.START_TIME,1629104400000L);
            parentEvent.put(SchedulingKeys.END_TIME,1629104700000L);

            Map<String,Object> metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");
            Map<String,Object> approveInfo = new HashMap<>();
            approveInfo.put(SchedulingKeys.WHEN,1629104400000l);
            approveInfo.put(SchedulingKeys.BY,"requestedId");
            approveInfo.put(SchedulingKeys.NOTES,"requestMessage");

            Map<String,Object> adjustmentInfo = new HashMap<>();
            adjustmentInfo.put(SchedulingKeys.APPROVE_INFO,approveInfo);

            Map<String,Object> additionalInfo = new HashMap<>();
            additionalInfo.put(SchedulingKeys.ADJUSTMENT_INFO,adjustmentInfo);
            metaData.put(SchedulingKeys.ADDITIONAL_INFO,additionalInfo);

            adjEvent.put(SchedulingKeys.METADATA,metaData);
            adjEvent.put(SchedulingKeys.PAYMENT_STATUS,REPORT_STATUS.APPROVED.toString());

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO(adjEvent,parentEvent, DateConstants.ZONE_ID_IST, DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            Assertions.assertFalse(adjustmentDTO.getIsDeleted());
            Assertions.assertFalse(adjustmentDTO.getIsNew());
            Assertions.assertEquals("",adjustmentDTO.getProjectID());
            Assertions.assertEquals(InternalUsage.NO_PROJECT,adjustmentDTO.getProjectName());
            Assertions.assertEquals(1629104514000L,adjustmentDTO.getAdjustedInTime());
            Assertions.assertEquals("16-Aug-2021 02:31:54 PM",adjustmentDTO.getAdjustedInTimeText());
            Assertions.assertEquals(1629104534000L,adjustmentDTO.getAdjustedOutTime());
            Assertions.assertEquals("16-Aug-2021 02:32:14 PM",adjustmentDTO.getAdjustedOutTimeText());
            Assertions.assertEquals(1629104400000L,adjustmentDTO.getOriginalInTime());
            Assertions.assertEquals("16-Aug-2021 02:30:00 PM",adjustmentDTO.getOriginalInTimeText());
            Assertions.assertEquals(1629104700000L,adjustmentDTO.getOriginalOutTime());
            Assertions.assertEquals("16-Aug-2021 02:35:00 PM",adjustmentDTO.getOriginalOutTimeText());
            Assertions.assertEquals(REPORT_STATUS.APPROVED.toString(),adjustmentDTO.getStatus());
            Assertions.assertEquals(REPORT_STATUS.APPROVED.toString(),adjustmentDTO.getPayrollStatus());
            Assertions.assertEquals(0L,adjustmentDTO.getRequestedTime());
            Assertions.assertEquals("",adjustmentDTO.getRequestedTimeText());
            Assertions.assertEquals("",adjustmentDTO.getRequestedBy());
            Assertions.assertEquals("",adjustmentDTO.getRequestMessage());
            Assertions.assertEquals(1629104400000L,adjustmentDTO.getApprovedTime());
            Assertions.assertEquals("16-Aug-2021 02:30:00 PM",adjustmentDTO.getApprovedTimeText());
            Assertions.assertEquals("requestedId",adjustmentDTO.getApprovedBy());
            Assertions.assertEquals("requestMessage",adjustmentDTO.getApprovedMessage());
            Assertions.assertEquals("",adjustmentDTO.getRejectMessage());
        }
    }

    @Test
    void AdjustmentDTO_valid_RejectInfo_test(){
        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class)){

            ProjectImpl project = Mockito.mock(ProjectImpl.class);
            Mockito.when(project.getProject(anyString())).thenReturn(null);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(project);

            adjEvent.put(SchedulingKeys.CALENDAR,"accountId");

            Map<String,Object> parentEvent = new HashMap<>();
            parentEvent.put(SchedulingKeys.ID,"entryId");
            parentEvent.put(SchedulingKeys.START_TIME,1629104400000L);
            parentEvent.put(SchedulingKeys.END_TIME,1629104700000L);

            Map<String,Object> metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");
            Map<String,Object> rejectInfo = new HashMap<>();
            rejectInfo.put(SchedulingKeys.WHEN,1629104400000d);
            rejectInfo.put(SchedulingKeys.BY,"requestedId");
            rejectInfo.put(SchedulingKeys.NOTES,"requestMessage");

            Map<String,Object> adjustmentInfo = new HashMap<>();
            adjustmentInfo.put(SchedulingKeys.REJECT_INFO,rejectInfo);

            Map<String,Object> additionalInfo = new HashMap<>();
            additionalInfo.put(SchedulingKeys.ADJUSTMENT_INFO,adjustmentInfo);
            metaData.put(SchedulingKeys.ADDITIONAL_INFO,additionalInfo);

            adjEvent.put(SchedulingKeys.METADATA,metaData);
            adjEvent.put(SchedulingKeys.PAYMENT_STATUS,REPORT_STATUS.APPROVED.toString());

            AdjustmentDTO adjustmentDTO = new AdjustmentDTO(adjEvent,parentEvent, DateConstants.ZONE_ID_IST, DateFormats.DD_MMM_YYYY_HH_MM_SS_A);
            Assertions.assertFalse(adjustmentDTO.getIsDeleted());
            Assertions.assertFalse(adjustmentDTO.getIsNew());
            Assertions.assertEquals("",adjustmentDTO.getProjectID());
            Assertions.assertEquals(InternalUsage.NO_PROJECT,adjustmentDTO.getProjectName());
            Assertions.assertEquals(1629104514000L,adjustmentDTO.getAdjustedInTime());
            Assertions.assertEquals("16-Aug-2021 02:31:54 PM",adjustmentDTO.getAdjustedInTimeText());
            Assertions.assertEquals(1629104534000L,adjustmentDTO.getAdjustedOutTime());
            Assertions.assertEquals("16-Aug-2021 02:32:14 PM",adjustmentDTO.getAdjustedOutTimeText());
            Assertions.assertEquals(1629104400000L,adjustmentDTO.getOriginalInTime());
            Assertions.assertEquals("16-Aug-2021 02:30:00 PM",adjustmentDTO.getOriginalInTimeText());
            Assertions.assertEquals(1629104700000L,adjustmentDTO.getOriginalOutTime());
            Assertions.assertEquals("16-Aug-2021 02:35:00 PM",adjustmentDTO.getOriginalOutTimeText());
            Assertions.assertEquals("MANUAL_CLOCKED_OUT",adjustmentDTO.getStatus());
            Assertions.assertEquals(REPORT_STATUS.APPROVED.toString(),adjustmentDTO.getPayrollStatus());
            Assertions.assertEquals(0L,adjustmentDTO.getRequestedTime());
            Assertions.assertEquals("",adjustmentDTO.getRequestedTimeText());
            Assertions.assertEquals("",adjustmentDTO.getRequestedBy());
            Assertions.assertEquals("",adjustmentDTO.getRequestMessage());
            Assertions.assertEquals(1629104400000L,adjustmentDTO.getApprovedTime());
            Assertions.assertEquals("16-Aug-2021 02:30:00 PM",adjustmentDTO.getApprovedTimeText());
            Assertions.assertEquals("requestedId",adjustmentDTO.getApprovedBy());
            Assertions.assertEquals("",adjustmentDTO.getApprovedMessage());
            Assertions.assertEquals("requestMessage",adjustmentDTO.getRejectMessage());
        }
    }

    @Test
    void isAdjustmentRejected_statusNotRejected_returnFalse(){
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        adjustmentDTO.setStatus("PENDING");
        Assertions.assertFalse(adjustmentDTO.hasAdjustmentBeenRejected());
    }

    @Test
    void isAdjustmentRejected_statusRejected_returnTrue(){
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        adjustmentDTO.setStatus("REJECTED");
        Assertions.assertTrue(adjustmentDTO.hasAdjustmentBeenRejected());
    }

}
