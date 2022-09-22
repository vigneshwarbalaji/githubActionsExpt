package com.yoco.hours.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import com.yoco.hours.service.HourService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;

import javax.servlet.http.HttpServletRequest;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class HoursTest {

    @Test
    void getActiveEntry_accesstoken_exception_test(){

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new Hours().getActiveEntry("accID", new MockHttpServletRequest(), "contactID");
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }

    }

    @Test
    void getActiveEntry_service_method_exception_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.getActiveEntry(anyString(),any(Contact.class),anyString())).thenThrow(new IllegalArgumentException("Exception test"));
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new Hours().getActiveEntry("accID", new MockHttpServletRequest(), "contactID");

            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void getActiveEntry_empty_responseMap_test() {

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.getActiveEntry(anyString(),any(Contact.class),anyString())).thenReturn(new HashMap<>());
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new Hours().getActiveEntry("accID", new MockHttpServletRequest(), "contactID");

            var genericResponse = new GenericResponse(false, null, EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value());
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void getActiveEntry_valid_responseMap_test() {

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        HashMap<String, Object> respMap = new HashMap<>();
        respMap.put("entries", "entries");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.getActiveEntry(anyString(),any(Contact.class),anyString())).thenReturn(respMap);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new Hours().getActiveEntry("accID", new MockHttpServletRequest(), "contactID");

            var genericResponse = new GenericResponse();
            genericResponse.setData(respMap);
            genericResponse.setSuccess(true);

            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void timer_accesstoken_exception_test(){

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new Hours().timerInfo("accID", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void timer_userAgent_exception_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMpckedStatis = Mockito.mockStatic(HeaderUtil.class)){

            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            headerUtilMpckedStatis.when(()->HeaderUtil.isMobileUserAgent(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new Hours().timerInfo("accID", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }

    }

    @Test
    void timerInfo_service_method_exception_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMpckedStatis = Mockito.mockStatic(HeaderUtil.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.getTimerInfo(anyString(),any(Contact.class),anyString())).thenThrow(new IllegalArgumentException("Exception test"));
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            headerUtilMpckedStatis.when(()->HeaderUtil.isMobileUserAgent(any(HttpServletRequest.class))).thenReturn("user-agent");
            headerUtilMpckedStatis.when(()->HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class))).thenReturn("clientID");

            ResponseEntity<GenericResponse> resp = new Hours().timerInfo("accountID", new MockHttpServletRequest());

            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void timerInfo_empty_responseMap_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMpckedStatis = Mockito.mockStatic(HeaderUtil.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.getTimerInfo(anyString(),any(Contact.class),anyString())).thenReturn(new HashMap<>());
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            headerUtilMpckedStatis.when(()->HeaderUtil.isMobileUserAgent(any(HttpServletRequest.class))).thenReturn("user-agent");
            headerUtilMpckedStatis.when(()->HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class))).thenReturn("clientID");

            ResponseEntity<GenericResponse> resp = new Hours().timerInfo("accountID", new MockHttpServletRequest());

            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.add(SchedulingKeys.ENTRIES, new ArrayList<>());

            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void timerInfo_empty_time_entries_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMpckedStatis = Mockito.mockStatic(HeaderUtil.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.getTimerInfo(anyString(),any(Contact.class),anyString())).thenReturn(new HashMap<>());
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            headerUtilMpckedStatis.when(()->HeaderUtil.isMobileUserAgent(any(HttpServletRequest.class))).thenReturn("user-agent");
            headerUtilMpckedStatis.when(()->HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class))).thenReturn("clientID");

            ResponseEntity<GenericResponse> resp = new Hours().timerInfo("accountID", new MockHttpServletRequest());

            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.add(SchedulingKeys.ENTRIES, new ArrayList<>());

            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void timerInfo_valid_time_entries_test() {

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        HashMap<String, Object> respMap = new HashMap<>();
        respMap.put("entries", "entries");
        respMap.put("last_ClockedIn_Prj_Info", "recentlyClockedInProjectInfo");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMpckedStatis = Mockito.mockStatic(HeaderUtil.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.getTimerInfo(anyString(),any(Contact.class),anyString())).thenReturn(respMap);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            headerUtilMpckedStatis.when(()->HeaderUtil.isMobileUserAgent(any(HttpServletRequest.class))).thenReturn("user-agent");
            headerUtilMpckedStatis.when(()->HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class))).thenReturn("clientID");

            ResponseEntity<GenericResponse> resp = new Hours().timerInfo("accountID", new MockHttpServletRequest());

            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.add(SchedulingKeys.ENTRIES, respMap.get("entries"));
            genericResponse.add("recentlyClockedInProjectInfo", respMap.get("last_ClockedIn_Prj_Info"));

            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void updateEntry_accesstoken_exception_test(){

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new Hours().updateEntry("accID", "entryID",  new MockHttpServletRequest(), "payload");
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }

    }

    @Test
    void updateEntry_service_exception_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMpckedStatis = Mockito.mockStatic(HeaderUtil.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.updateEntry("accID", "entryID", mockContact, JsonUtil.convertJsonToMap("payload"), "clientID")).thenThrow(new IllegalArgumentException("Exception test"));
            })){

            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            headerUtilMpckedStatis.when(()->HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class))).thenReturn("clientID");
            ResponseEntity<GenericResponse> resp = new Hours().updateEntry("accID", "entryID", new MockHttpServletRequest(), "payload");
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void updateEntry_clientID_exception_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMpckedStatis = Mockito.mockStatic(HeaderUtil.class)){

            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            headerUtilMpckedStatis.when(()->HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new Hours().updateEntry("accID", "entryID", new MockHttpServletRequest(), "payload");
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void updateEntry_empty_response_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMpckedStatis = Mockito.mockStatic(HeaderUtil.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.updateEntry("accID", "entryID", mockContact, JsonUtil.convertJsonToMap("payload"), "clientID")).thenReturn(new HashMap<>());
            })){

            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            headerUtilMpckedStatis.when(()->HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class))).thenReturn("clientID");
            ResponseEntity<GenericResponse> resp = new Hours().updateEntry("accID", "entryID", new MockHttpServletRequest(), "payload");

            var genericResponse = new GenericResponse( Boolean.FALSE, null, EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value());
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void updateEntry_valid_response_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        HashMap<String, Object> respMap = new HashMap<>();
        respMap.put("entry", "entry");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedStatic<HeaderUtil> headerUtilMpckedStatis = Mockito.mockStatic(HeaderUtil.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.updateEntry("accID", "entryID", mockContact, JsonUtil.convertJsonToMap("payload"), "clientID")).thenReturn(respMap);
            })){

            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            headerUtilMpckedStatis.when(()->HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class))).thenReturn("clientID");
            ResponseEntity<GenericResponse> resp = new Hours().updateEntry("accID", "entryID", new MockHttpServletRequest(), "payload");

            var genericResponse = new GenericResponse();
            genericResponse.setSuccess( Boolean.TRUE );
            genericResponse.setData(respMap);

            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void datesInfo_accesstoken_exception_test(){

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new Hours().datesInfo("accID", "range", "", "", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }

    }

    @Test
    void datesInfo_service_method_exception_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.getDatesForHoursPage(anyString(),any(Contact.class),anyString(), anyString(), anyString())).thenThrow(new IllegalArgumentException("Exception test"));
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new Hours().datesInfo("accID", "range", "", "", new MockHttpServletRequest());

            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void datesInfo_valid_resp_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        HashMap<String, Object> respMap = new HashMap<>();
        respMap.put("dates", "dates");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.getDatesForHoursPage(anyString(),any(Contact.class),anyString(), anyString(), anyString())).thenReturn(respMap);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new Hours().datesInfo("accID", "range", "", "", new MockHttpServletRequest());

            var genericResponse = new GenericResponse();
            genericResponse.setSuccess( Boolean.TRUE );
            genericResponse.setData(respMap);

            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void getAll_accesstoken_exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)) {
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new Hours().getAll("accID", new MockHttpServletRequest(), "range", "", "", "", "", "");
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void getAll_service_method_exception_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.getAll(anyString(),any(Contact.class), any(Map.class))).thenThrow(new IllegalArgumentException("Exception test"));
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new Hours().getAll("accID", new MockHttpServletRequest(), "range", "", "", "", "", "");

            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void getAll_empty_resp_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.getAll(anyString(),any(Contact.class), any(Map.class))).thenReturn(new HashMap());
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new Hours().getAll("accID", new MockHttpServletRequest(), "range", "", "", "", "", "");

            var genericResponse = new GenericResponse(false, null, EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value());
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void getAll_valid_resp_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        HashMap<String, Object> responseMap = new HashMap<>();
        responseMap.put("entry", "entry");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<HourService> mock = Mockito.mockConstruction(HourService.class, (hourServiceMock, context) -> {
                Mockito.when(hourServiceMock.getAll(anyString(),any(Contact.class), any(Map.class))).thenReturn(responseMap);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new Hours().getAll("accID", new MockHttpServletRequest(), "range", "", "", "", "", "");

            var genericResponse = new GenericResponse();
            genericResponse.setSuccess( Boolean.TRUE );
            genericResponse.setData(responseMap);

            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void deleteEntry_ExceptionTest(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenThrow(new IllegalArgumentException("test"));
            ResponseEntity<GenericResponse> response = new Hours().deleteEntry("accID","123",null,request);
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
            var genericResponse = new GenericResponse();
            genericResponse.setSuccess( Boolean.FALSE );
            genericResponse.setErrorMessage("test");
            Assertions.assertEquals(genericResponse,response.getBody());
        }
    }

    @Test
    void deleteEntry_200Test(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        Contact contact = new Contact();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
        MockedConstruction<HourService> hourServiceMockedConstruction = Mockito.mockConstruction(HourService.class,(hoursServiceMock, context)->{
            Mockito.when(hoursServiceMock.deleteEntry("accID","123",contact,null)).thenReturn(Map.of("key","value"));
        })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenReturn(contact);
            ResponseEntity<GenericResponse> response = new Hours().deleteEntry("accID","123",null,request);
            Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
            var genericResponse = new GenericResponse();
            genericResponse.setSuccess( Boolean.TRUE );
            genericResponse.setData(Map.of("key","value"));
            Assertions.assertEquals(genericResponse,response.getBody());
        }
    }

}
