package com.yoco.downloads.report.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.downloads.report.service.ReportDownloadService;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;

import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class ReportGlobalTest {

    @ParameterizedTest
    @NullAndEmptySource
    void downloadGlobalreports_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<ReportDownloadService> mock = Mockito.mockConstruction(ReportDownloadService.class, (reportDownloadServiceMock, context) -> {
            Mockito.when(reportDownloadServiceMock.downloadGlobal(anyString(), any(Contact.class), any(Map.class), anyString())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new ReportGlobal().downloadGlobalreports("accountId", "current_week", "", "", "", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void downloadGlobalreports_exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new ReportGlobal().downloadGlobalreports("accountID", "current_week", "", "", "", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void downloadGlobalreports_service_method_exception_test(){
        Contact objContact = new Contact();
        objContact.setId("1234");
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<ReportDownloadService> reportDownload = Mockito.mockConstruction(ReportDownloadService.class, (objReportDownloadService, context)->{
                Mockito.when(objReportDownloadService.downloadGlobal(anyString(), any(Contact.class), any(Map.class), anyString())).thenThrow(new IllegalArgumentException("Exception test" ) );
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(objContact);

            ResponseEntity<GenericResponse> resp = new ReportGlobal().downloadGlobalreports("accountID", "current_week", "", "", "", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void downloadGlobalreports_valid_resp_test(){
        Contact objContact = new Contact();
        objContact.setId("1234");
        HashMap<String, Object> resp = new HashMap<>();
        resp.put("entries", "entries");
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<ReportDownloadService> reportDownload = Mockito.mockConstruction(ReportDownloadService.class, (objReportDownloadService, context)->{
                Mockito.when(objReportDownloadService.downloadGlobal(anyString(), any(Contact.class), any(Map.class), anyString())).thenReturn(resp);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(objContact);

            ResponseEntity<GenericResponse> response = new ReportGlobal().downloadGlobalreports("accountID", "current_week", "", "", "", new MockHttpServletRequest());
            var genericResponse = new GenericResponse();
            genericResponse.setData(resp);
            genericResponse.setSuccess(true);
            Assertions.assertEquals(genericResponse,response.getBody());
            Assertions.assertEquals(HttpStatus.OK,response.getStatusCode());
        }
    }


}
