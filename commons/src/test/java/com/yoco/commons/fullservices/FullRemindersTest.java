package com.yoco.commons.fullservices;

import com.fullreminders.api.client.exception.ApiException;
import com.fullreminders.api.client.model.action.HookAction;
import com.fullreminders.api.client.model.job.JobFetchParams;
import com.fullreminders.api.client.model.job.JobRequest;
import com.fullreminders.api.client.model.response.JobsDeleteApiResponse;
import com.fullreminders.api.client.service.JobsApi;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.utils.GaeUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;

class FullRemindersTest {

    @Test
    void createJob_bufferLimit_Zero_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<JobsApi> mock = Mockito.mockConstruction(JobsApi.class, (jobsApiMock, context) -> {
                JobRequest request = JobRequest.builder().acctId("accountId").userId("contactId").jobTime("2").jobTimeZone(DateConstants.ZONE_ID_IST)
                        .bufferSecs(0).metaData("type", "type").queryableMetaKey("type").hookAction(HookAction.builder("hact1").url("callbackUrl").build()).build();
                Mockito.when(jobsApiMock.createJob(request)).thenReturn(123L);
        })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullReminderApiKey).thenReturn("fullReminderApiKey");
            FullReminders.createJob("accountId","2", DateConstants.ZONE_ID_IST,"contactId","callbackUrl","type",0);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullReminderApiKey(),times(1));
        }
    }

    @Test
    void createJob_bufferLimit_Non_Zero_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<JobsApi> mock = Mockito.mockConstruction(JobsApi.class, (jobsApiMock, context) -> {
                JobRequest request = JobRequest.builder().acctId("accountId").userId("contactId").jobTime("2").jobTimeZone(DateConstants.ZONE_ID_IST)
                        .bufferSecs(10).metaData("type", "type").queryableMetaKey("type").hookAction(HookAction.builder("hact1").url("callbackUrl").build()).build();
                Mockito.when(jobsApiMock.createJob(request)).thenReturn(123L);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullReminderApiKey).thenReturn("fullReminderApiKey");
            FullReminders.createJob("accountId","2", DateConstants.ZONE_ID_IST,"contactId","callbackUrl","type",10);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullReminderApiKey(),times(1));
        }
    }
    @Test
    void createJob_Exception_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<JobsApi> mock = Mockito.mockConstruction(JobsApi.class, (jobsApiMock, context) -> {
                Mockito.when(jobsApiMock.createJob(any(JobRequest.class))).thenThrow(new ApiException());
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullReminderApiKey).thenReturn("fullReminderApiKey");
            FullReminders.createJob("accountId","2", DateConstants.ZONE_ID_IST,"contactId","callbackUrl","type",10);
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullReminderApiKey(),times(1));
        }
    }

    @Test
    void deleteJob_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<JobsApi> mock = Mockito.mockConstruction(JobsApi.class, (jobsApiMock, context) -> {
                JobFetchParams params = JobFetchParams.builder().acctId("accountId").userId("contactId").addMetaQuery("type", "type").build();
                JobsDeleteApiResponse mc = Mockito.mock(JobsDeleteApiResponse.class);
                Mockito.when(mc.getDeletedJobs()).thenReturn(2);
                Mockito.when(jobsApiMock.deleteJobs(params)).thenReturn(mc);
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullReminderApiKey).thenReturn("fullReminderApiKey");
            FullReminders.deleteJobs("accountId","contactId","type");
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullReminderApiKey(),times(1));
        }
    }

    @Test
    void deleteJob_Exception_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<JobsApi> mock = Mockito.mockConstruction(JobsApi.class, (jobsApiMock, context) -> {
                Mockito.when(jobsApiMock.deleteJobs(any(JobFetchParams.class))).thenThrow(new ApiException());
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullReminderApiKey).thenReturn("fullReminderApiKey");
            FullReminders.deleteJobs("accountId","contactId","type");
            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullReminderApiKey(),times(1));
        }
    }

    @Test
    void scheduleWeeklyReportMailJob_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<JobsApi> mock = Mockito.mockConstruction(JobsApi.class, (jobsApiMock, context) -> {
                Mockito.when(jobsApiMock.createJob(any(JobRequest.class))).thenReturn(123L);
            })){

            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullReminderApiKey).thenReturn("fullReminderApiKey");
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getYoCoDefaultApiUrl).thenReturn("https://yoco");
            FullReminders.scheduleWeeklyReportMailJob("accountId","contactId", DateConstants.ZONE_ID_IST);

            gaeUtilsMockedStatic.verify(()->GaeUtils.isAppModeLive(),times(1));
            commonAppPropertiesMockedStatic.verify(()->CommonAppProperties.getFullReminderApiKey(),times(1));
        }
    }

    @Test
    void deleteAllJobsForAccount_exception_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<JobsApi> mock = Mockito.mockConstruction(JobsApi.class, (jobsApiMock, context) -> {
                Mockito.when(jobsApiMock.deleteJobs(any(JobFetchParams.class))).thenThrow(new IllegalArgumentException("exception"));
            })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullReminderApiKey).thenReturn("fullReminderApiKey");
            Assertions.assertEquals(0,FullReminders.deleteAllJobsForAccount("accID"));
        }
    }

    @Test
    void deleteAllJobsForAccount_valid_test(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedConstruction<JobsApi> mock = Mockito.mockConstruction(JobsApi.class, (jobsApiMock, context) -> {
            JobFetchParams paramsWithoutCursorMock = JobFetchParams.builder().acctId("accID").build();
            JobFetchParams paramsWithCursorMock = JobFetchParams.builder().acctId("accID").build();
            paramsWithCursorMock.setCursor("cursor");
            JobsDeleteApiResponse responseWithCursor = Mockito.mock(JobsDeleteApiResponse.class);
            Mockito.when(responseWithCursor.getCursor()).thenReturn("cursor");
            Mockito.when(responseWithCursor.getDeletedJobs()).thenReturn(5);
            JobsDeleteApiResponse responseWithoutCursor = Mockito.mock(JobsDeleteApiResponse.class);
            Mockito.when(responseWithoutCursor.getCursor()).thenReturn(null);
            Mockito.when(responseWithoutCursor.getDeletedJobs()).thenReturn(3);
            Mockito.when(jobsApiMock.deleteJobs(paramsWithoutCursorMock)).thenReturn(responseWithCursor);
            Mockito.when(jobsApiMock.deleteJobs(paramsWithCursorMock)).thenReturn(responseWithoutCursor);
        })){
            gaeUtilsMockedStatic.when(GaeUtils::isAppModeLive).thenReturn(false);
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getFullReminderApiKey).thenReturn("fullReminderApiKey");
            Assertions.assertEquals(8,FullReminders.deleteAllJobsForAccount("accID"));
        }
    }
}
