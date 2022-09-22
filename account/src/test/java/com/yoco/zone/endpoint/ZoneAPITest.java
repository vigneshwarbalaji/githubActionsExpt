package com.yoco.zone.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.error.ApiErrorCode;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.TimeZoneUtil;
import com.yoco.zone.enums.ZONE_ERROR_MESSAGE;
import com.yoco.zone.service.ZoneService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;

import javax.servlet.http.HttpServletRequest;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

import java.util.ArrayList;
import java.util.HashMap;


class ZoneAPITest {

    @Test
    void getTimeZoneList_exception_test() {
        try(MockedStatic<TimeZoneUtil> timeZoneUtilStatic = Mockito.mockStatic(TimeZoneUtil.class)){
            timeZoneUtilStatic.when(()-> TimeZoneUtil.getAllZoneIds()).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new ZoneAPI().getTimeZoneList();
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void getTimeZoneList_success_test() {
        try(MockedStatic<TimeZoneUtil> timeZoneUtilStatic = Mockito.mockStatic(TimeZoneUtil.class)){
            timeZoneUtilStatic.when(()-> TimeZoneUtil.getAllZoneIds()).thenReturn(new HashMap<>());
            ResponseEntity<GenericResponse> resp = new ZoneAPI().getTimeZoneList();
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add("zones", new HashMap<>());
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void autoDetect_exception_test() {

        try(MockedConstruction<ZoneService> mockZoneService = Mockito.mockConstruction(ZoneService.class,  (zoneServiceMock, context) -> {
            Mockito.when(zoneServiceMock.getAutoDetectedZoneSevice(anyString())).thenThrow(new IllegalArgumentException("Exception Test"));
        })){
            ResponseEntity<GenericResponse> resp = new ZoneAPI().autoDetect("330");
            var genericResponse = new GenericResponse(false, null, "Exception Test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void autoDetect_success_test() {

        String mockZone = "Asia/kolkata";
        try(MockedConstruction<ZoneService> mockZoneService = Mockito.mockConstruction(ZoneService.class,  (zoneServiceMock, context) -> {
            Mockito.when(zoneServiceMock.getAutoDetectedZoneSevice(anyString())).thenReturn(mockZone);
        })){
            ResponseEntity<GenericResponse> resp = new ZoneAPI().autoDetect("330");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add("zone", mockZone);
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void getDates_exception_test(){

        try(MockedConstruction<ZoneService> mockZoneService = Mockito.mockConstruction(ZoneService.class,  (zoneServiceMock, context) -> {
            Mockito.when(zoneServiceMock.getDates(anyString(),anyString(),anyString(),anyString(),anyString(),anyString())).thenThrow(new IllegalArgumentException("Exception Test"));
        })){
            ResponseEntity<GenericResponse> resp = new ZoneAPI().getDates("Asia/kokata", "current_week", "","", "", "");
            var genericResponse = new GenericResponse(false, null, "Exception Test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void getDates_success_false_test(){

        HashMap<String, Object> mockresp = new HashMap<>();
        mockresp.put("success",false);

        try(MockedConstruction<ZoneService> mockZoneService = Mockito.mockConstruction(ZoneService.class,  (zoneServiceMock, context) -> {
            Mockito.when(zoneServiceMock.getDates(anyString(),anyString(),anyString(),anyString(),anyString(),anyString())).thenReturn(mockresp);
        })){
            ResponseEntity<GenericResponse> resp = new ZoneAPI().getDates("Asia/kokata", "current_week", "","", "", "");
            var genericResponse = new GenericResponse(false, ApiErrorCode.BAD_REQUEST, ZONE_ERROR_MESSAGE.ERROR_ON_GETTING_DATES.value());
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void getDates_success_true_test(){

        HashMap<String, Object> mockresp = new HashMap<>();
        mockresp.put("success",true);
        mockresp.put("datesList",new ArrayList());

        try(MockedConstruction<ZoneService> mockZoneService = Mockito.mockConstruction(ZoneService.class,  (zoneServiceMock, context) -> {
            Mockito.when(zoneServiceMock.getDates(anyString(),anyString(),anyString(),anyString(),anyString(),anyString())).thenReturn(mockresp);
        })){
            ResponseEntity<GenericResponse> resp = new ZoneAPI().getDates("Asia/kokata", "current_week", "","", "", "");
            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.add("dates", mockresp.get("datesList"));

            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void updateTimezone_exception_accessToken_test() {
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedConstruction<ZoneService> mock = Mockito.mockConstruction(ZoneService.class, (mockedZonedService, context)->{
              Mockito.when(mockedZonedService.updateTimezone(anyString(), anyString(), anyString(), anyString())).thenReturn(new HashMap<>());
            })){
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(MockHttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new ZoneAPI().updateTimezone("accountID", "payload", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void updateTimezone_exception_headerUtil_test() {
        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedConstruction<ZoneService> mock = Mockito.mockConstruction(ZoneService.class, (mockedZonedService, context)->{
                Mockito.when(mockedZonedService.updateTimezone(anyString(), anyString(), anyString(), anyString())).thenReturn(new HashMap<>());
            })){
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(MockHttpServletRequest.class))).thenReturn(mockContact);
            headerUtilMockedStatic.when(()-> HeaderUtil.extractClientIdFromRequest(any(MockHttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new ZoneAPI().updateTimezone("accountID", "payload", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void updateTimezone_empty_responseMap_test() {
        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedConstruction<ZoneService> mock = Mockito.mockConstruction(ZoneService.class, (mockedZonedService, context)->{
                Mockito.when(mockedZonedService.updateTimezone(anyString(), anyString(), anyString(), anyString())).thenReturn(new HashMap<>());
            })){
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(MockHttpServletRequest.class))).thenReturn(mockContact);
            headerUtilMockedStatic.when(()-> HeaderUtil.extractClientIdFromRequest(any(MockHttpServletRequest.class))).thenReturn("");
            ResponseEntity<GenericResponse> resp = new ZoneAPI().updateTimezone("accountID", "payload", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, ApiErrorCode.BAD_REQUEST, "Time-zone update failed");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void updateTimezone_valid_responseMap_test() {

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        HashMap<String, Object> response = new HashMap<>();
        response.put("startTimeOfDay", "startTimeOfDay");
        response.put("user", "user");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMockedStatic = Mockito.mockStatic(HeaderUtil.class);
            MockedConstruction<ZoneService> mock = Mockito.mockConstruction(ZoneService.class, (mockedZonedService, context)->{
                Mockito.when(mockedZonedService.updateTimezone(anyString(), anyString(), anyString(), anyString())).thenReturn(response);
            })){
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(MockHttpServletRequest.class))).thenReturn(mockContact);
            headerUtilMockedStatic.when(()-> HeaderUtil.extractClientIdFromRequest(any(MockHttpServletRequest.class))).thenReturn("");
            ResponseEntity<GenericResponse> resp = new ZoneAPI().updateTimezone("accountID", "payload", new MockHttpServletRequest());
            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.add("startTimeOfDay", response.get("startTimeOfDay"));
            genericResponse.add("user", response.get("user"));
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }
}
