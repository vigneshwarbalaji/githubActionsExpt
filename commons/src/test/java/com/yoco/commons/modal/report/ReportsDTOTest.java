package com.yoco.commons.modal.report;

import com.google.appengine.api.datastore.Text;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ProjectImpl;
import com.yoco.commons.dataservices.impl.TaskImpl;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.entity.TaskJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyString;

class ReportsDTOTest {

    Map<String,Object> event = new HashMap<>();
    Map<String,Object> clockMsg = new HashMap<>();

    @BeforeEach
    void setup(){
        event.put(SchedulingKeys.ID,"id");
        event.put(SchedulingKeys.MERCHANT,"accountId");
        ArrayList<String> contactIdList = new ArrayList<>();
        contactIdList.add("contactId");
        event.put(SchedulingKeys.PROVIDER,contactIdList);

        event.put(SchedulingKeys.SOURCE, ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value());
        event.put(SchedulingKeys.IS_DELETED,false);

        event.put(SchedulingKeys.START_TIME,1629104514000L);
        event.put(SchedulingKeys.END_TIME,1629104534000L);
    }

    @Test
    void ReportsDTO_valid_test(){

        event.put(SchedulingKeys.PAYMENT_STATUS,"DEFAULT_PAYROLL");

        clockMsg.put("msg","msg");
        Map<String, Object> clockMap = new HashMap<>();
        clockMap.put(SchedulingKeys.IP,"12.12");
        clockMap.put(SchedulingKeys.INFO, clockMsg);

        Map<String, Object> locationMap = new HashMap<>();
        locationMap.put(SchedulingKeys.CLOCK_IN_MESSAGE,clockMap);
        locationMap.put(SchedulingKeys.CLOCK_OUT_MESSAGE,clockMap);

        event.put(SchedulingKeys.LOCATION,locationMap);

        Map<String,Object> metaData = new HashMap<>();
        metaData.put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");
        metaData.put(SchedulingKeys.OUTSOURCE,ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value());
        metaData.put(SchedulingKeys.SUB_STATUS,"activity");

        event.put(SchedulingKeys.METADATA,metaData);

        ReportsDTO reportsDTO = new ReportsDTO(event,"test@gmail.com", DateConstants.ZONE_ID_UTC);

        Assertions.assertEquals("id",reportsDTO.getId());
        Assertions.assertEquals("accountId",reportsDTO.getAccountID());
        Assertions.assertEquals("contactId",reportsDTO.getContactID());
        Assertions.assertEquals("12.12",reportsDTO.getInIP());
        Assertions.assertEquals(JsonUtil.getJson(clockMsg),reportsDTO.getClockInMessage());
        Assertions.assertEquals("12.12",reportsDTO.getOutIP());
        Assertions.assertEquals(JsonUtil.getJson(clockMsg),reportsDTO.getClockOutMessage());
        Assertions.assertEquals(ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value(),reportsDTO.getInSource());
        Assertions.assertEquals("",reportsDTO.getPayrollStatus());
        Assertions.assertFalse(reportsDTO.isDeleted());
        Assertions.assertEquals("test@gmail.com",reportsDTO.getEmailID());
        Assertions.assertEquals(1629104514000L,reportsDTO.getInTime());
        Assertions.assertEquals(1629104534000L,reportsDTO.getOutTime());

        String startDateTime = DateUtil.convertMillisToDateTimeText(DateFormats.YYYY_MM_DD_T_HH_MM_SS_A_Z,1629104514000L,DateConstants.ZONE_ID_UTC);
        String endDateTime = DateUtil.convertMillisToDateTimeText(DateFormats.YYYY_MM_DD_T_HH_MM_SS_A_Z,1629104534000L,DateConstants.ZONE_ID_UTC);
        Assertions.assertEquals(startDateTime,reportsDTO.getStart());
        Assertions.assertEquals(endDateTime,reportsDTO.getStop());
        Assertions.assertEquals("MANUAL_CLOCKED_OUT",reportsDTO.getStatus());
        Assertions.assertEquals(ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value(),reportsDTO.getOutSource());
        Assertions.assertEquals("activity",reportsDTO.getSubStatus());
        Assertions.assertEquals("",reportsDTO.getTaskID());
        Assertions.assertEquals("",reportsDTO.getTaskSource());
        Assertions.assertEquals(new Text("").getValue(),reportsDTO.getTaskDescription());
        Assertions.assertEquals("",reportsDTO.getProjectID());
        Assertions.assertEquals("No Project",reportsDTO.getProjectName());
    }

    @Test
    void ReportsDTO_no_location_test(){

        event.put(SchedulingKeys.CALENDAR,"accountId");
        event.put(SchedulingKeys.PAYMENT_STATUS,"paymentStatus");

        ReportsDTO reportsDTO = new ReportsDTO(event,"test@gmail.com", DateConstants.ZONE_ID_UTC);
        Assertions.assertEquals("paymentStatus",reportsDTO.getPayrollStatus());
        Assertions.assertEquals("",reportsDTO.getInIP());
        Assertions.assertEquals("",reportsDTO.getClockInMessage());
        Assertions.assertEquals("",reportsDTO.getOutIP());
        Assertions.assertEquals("",reportsDTO.getClockOutMessage());
        Assertions.assertEquals("",reportsDTO.getStatus());
        Assertions.assertEquals("",reportsDTO.getOutSource());
        Assertions.assertEquals("",reportsDTO.getSubStatus());
    }

    @Test
    void ReportsDTO_empty_project_test(){

        try(MockedStatic<TaskImpl> taskMockedStatic = Mockito.mockStatic(TaskImpl.class);
            MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class)){

            TaskJDO taskJDO = new TaskJDO("id","taskId","taskDesc","taskSrc");
            TaskImpl task = Mockito.mock(TaskImpl.class);
            Mockito.when(task.getEntryByID(anyString())).thenReturn(taskJDO);
            taskMockedStatic.when(TaskImpl::getTaskImplInstance).thenReturn(task);

            ProjectJDO projectJDO = new ProjectJDO();
            projectJDO.setProjectName("projName");
            ProjectImpl project = Mockito.mock(ProjectImpl.class);
            Mockito.when(project.getProject(anyString())).thenReturn(projectJDO);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(project);

            event.put(SchedulingKeys.CALENDAR,"projectId");
            event.put(SchedulingKeys.PAYMENT_STATUS,"paymentStatus");

            Map<String,Object> metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");
            metaData.put(SchedulingKeys.TASK_ID,"taskId");
            event.put(SchedulingKeys.METADATA,metaData);

            Map<String, Object> locationMap = new HashMap<>();
            event.put(SchedulingKeys.LOCATION,locationMap);

            ReportsDTO reportsDTO = new ReportsDTO(event,"test@gmail.com", DateConstants.ZONE_ID_UTC);
            Assertions.assertEquals("paymentStatus",reportsDTO.getPayrollStatus());
            Assertions.assertEquals("",reportsDTO.getInIP());
            Assertions.assertEquals("",reportsDTO.getClockInMessage());
            Assertions.assertEquals("",reportsDTO.getOutIP());
            Assertions.assertEquals("",reportsDTO.getClockOutMessage());
            Assertions.assertEquals("MANUAL_CLOCKED_OUT",reportsDTO.getStatus());
            Assertions.assertEquals("web",reportsDTO.getOutSource());
            Assertions.assertEquals("",reportsDTO.getSubStatus());

        }
    }

    @Test
    void ReportsDTO_empty_Ip_test(){

        try(MockedStatic<TaskImpl> taskMockedStatic = Mockito.mockStatic(TaskImpl.class);
            MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class)){

            TaskImpl task = Mockito.mock(TaskImpl.class);
            Mockito.when(task.getEntryByID(anyString())).thenReturn(null);
            taskMockedStatic.when(TaskImpl::getTaskImplInstance).thenReturn(task);

            ProjectImpl project = Mockito.mock(ProjectImpl.class);
            Mockito.when(project.getProject(anyString())).thenReturn(null);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(project);

            event.put(SchedulingKeys.CALENDAR,"projectId");
            event.put(SchedulingKeys.PAYMENT_STATUS,"paymentStatus");

            Map<String,Object> metaData = new HashMap<>();
            metaData.put(SchedulingKeys.STATUS,"MANUAL_CLOCKED_OUT");
            metaData.put(SchedulingKeys.TASK_ID,"taskId");
            event.put(SchedulingKeys.METADATA,metaData);

            Map<String, Object> clockMap = new HashMap<>();
            Map<String, Object> locationMap = new HashMap<>();
            locationMap.put(SchedulingKeys.CLOCK_IN_MESSAGE,clockMap);
            locationMap.put(SchedulingKeys.CLOCK_OUT_MESSAGE,clockMap);

            event.put(SchedulingKeys.LOCATION,locationMap);

            ReportsDTO reportsDTO = new ReportsDTO(event,"test@gmail.com", DateConstants.ZONE_ID_UTC);
            Assertions.assertEquals("paymentStatus",reportsDTO.getPayrollStatus());
            Assertions.assertEquals("",reportsDTO.getInIP());
            Assertions.assertEquals("",reportsDTO.getClockInMessage());
            Assertions.assertEquals("",reportsDTO.getOutIP());
            Assertions.assertEquals("",reportsDTO.getClockOutMessage());
            Assertions.assertEquals("MANUAL_CLOCKED_OUT",reportsDTO.getStatus());
            Assertions.assertEquals("web",reportsDTO.getOutSource());
            Assertions.assertEquals("",reportsDTO.getSubStatus());

        }
    }
}
