package com.yoco.payroll.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.constants.EventConstants;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import com.yoco.payroll.service.PayrollService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import java.util.HashMap;
import java.util.Map;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class PayrollApiTest {

    @Test
    void getUserConfirmedEntries_exception_test(){

        try(MockedConstruction<PayrollService> payrollServiceMock = Mockito.mockConstruction(PayrollService.class, (payrollService, context) -> {
            Mockito.when(payrollService.getUserAcknowledgedEntries(anyString(), any(), anyString(), anyString(), anyString(), anyString())).thenThrow(new IllegalArgumentException("Invalid parameters"));
        })) {

            new PayrollApi().getUserConfirmedEntries("accountId", "by_date", "02/02/1740", "02/02/1740", "",new MockHttpServletRequest());
        }catch (Exception e){
            Assertions.assertEquals("Invalid parameters", e.getMessage());
        }
    }

    @Test
    void getUserConfirmedEntries_ifResponseMapIsNullOrEmpty_test(){

        Map<String, Object> responseMap = new HashMap<>();

        try(MockedConstruction<PayrollService> payrollServiceMock = Mockito.mockConstruction(PayrollService.class, (payrollService, context) -> {
            Mockito.when(payrollService.getUserAcknowledgedEntries(anyString(), any(),anyString(), anyString(), anyString(), anyString())).thenReturn(responseMap);
        })) {
            ResponseEntity<GenericResponse> responseEntity = new PayrollApi().getUserConfirmedEntries("accountID", DateConstants.BY_DATE, "15/09/2022",
                    "15/09/2022","",new MockHttpServletRequest());

            Assertions.assertEquals(200, responseEntity.getStatusCodeValue());
            Assertions.assertEquals(false, responseEntity.getBody().isSuccess());
            Assertions.assertEquals(EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value(),responseEntity.getBody().getErrorMessage());
            Assertions.assertEquals(null, responseEntity.getBody().getData());
        }
    }

    @Test
    void getUserConfirmedEntries_valid_response_test(){

        Map<String, Object>responseMap = new HashMap<>();
        responseMap.put(EventConstants.ENTRIES, "entries");
        responseMap.put(Commons.SUCCESS, true);

        try(MockedConstruction<PayrollService> payrollServiceMock = Mockito.mockConstruction(PayrollService.class, (payrollService, context) -> {
            Mockito.when(payrollService.getUserAcknowledgedEntries(anyString(), any(), anyString(), anyString(),anyString(), anyString())).thenReturn(responseMap);
        })) {
            ResponseEntity<GenericResponse> responseEntity = new PayrollApi().getUserConfirmedEntries("accountID", DateConstants.BY_DATE, "11/12/2022",
                    "13/12/2022","",new MockHttpServletRequest());

            Assertions.assertEquals(200, responseEntity.getStatusCodeValue());
            Assertions.assertEquals(true, responseEntity.getBody().isSuccess());
            Assertions.assertEquals("entries", responseEntity.getBody().getData().get(EventConstants.ENTRIES));
        }
    }

    @Test
    void getAdminUpdatedEntries_exception_test(){

        try(MockedConstruction<PayrollService> payrollServiceMock = Mockito.mockConstruction(PayrollService.class, (payrollService, context) -> {
            Mockito.when(payrollService.getAdminUpdatedEntries(anyString(), any(), anyString(), anyString(), anyString())).thenThrow(new IllegalArgumentException("Invalid parameters"));
        })) {

            new PayrollApi().getAdminUpdatedEntries("accountId", "by_date", "02/02/1740", "02/02/1740", new MockHttpServletRequest());
        }catch (Exception e){
            Assertions.assertEquals("Invalid parameters", e.getMessage());
        }
    }

    @Test
    void getAdminUpdatedEntries_ifResponseMapIsNullOrEmpty_test(){

        Map<String, Object> responseMap = new HashMap<>();

        try(MockedConstruction<PayrollService> payrollServiceMock = Mockito.mockConstruction(PayrollService.class, (payrollService, context) -> {
            Mockito.when(payrollService.getAdminUpdatedEntries(anyString(), any(),anyString(), anyString(), anyString())).thenReturn(responseMap);
        })) {
            ResponseEntity<GenericResponse> responseEntity = new PayrollApi().getAdminUpdatedEntries("accountID", DateConstants.BY_DATE, "15/09/2022",
                    "15/09/2022",new MockHttpServletRequest());

            Assertions.assertEquals(200, responseEntity.getStatusCodeValue());
            Assertions.assertEquals(false, responseEntity.getBody().isSuccess());
            Assertions.assertEquals(EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value(),responseEntity.getBody().getErrorMessage());
            Assertions.assertEquals(null, responseEntity.getBody().getData());
        }
    }

    @Test
    void getAdminUpdatedEntries_valid_response_test(){

        Map<String, Object>responseMap = new HashMap<>();
        responseMap.put(EventConstants.ADJUSTMENTS, "adjustments");
        responseMap.put(Commons.SUCCESS, true);

        try(MockedConstruction<PayrollService> payrollServiceMock = Mockito.mockConstruction(PayrollService.class, (payrollService, context) -> {
            Mockito.when(payrollService.getAdminUpdatedEntries(anyString(), any(), anyString(), anyString(),anyString())).thenReturn(responseMap);
        })) {
            ResponseEntity<GenericResponse> responseEntity = new PayrollApi().getAdminUpdatedEntries("accountID", DateConstants.BY_DATE, "11/12/2022",
                    "13/12/2022",new MockHttpServletRequest());

            Assertions.assertEquals(200, responseEntity.getStatusCodeValue());
            Assertions.assertEquals(true, responseEntity.getBody().isSuccess());
            Assertions.assertEquals("adjustments", responseEntity.getBody().getData().get(EventConstants.ADJUSTMENTS));
        }
    }

    @Test
    void approvePayroll_success_true_test() {
        Map<String, Object> mockResponse = new HashMap<>();
        mockResponse.put("data", "dummy");
        MockHttpServletRequest mockHttpServletRequest = new MockHttpServletRequest();
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
             MockedConstruction<PayrollService> mock = Mockito.mockConstruction(PayrollService.class, (payrollServiceMock, context) -> {
                 Mockito.when(payrollServiceMock.approvePayrollEvents(anyString(),anyString(), any(Contact.class), anyString())).thenReturn(mockResponse);
             })) {
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserContactFromRequest(mockHttpServletRequest)).thenReturn(new Contact());
            ResponseEntity<GenericResponse> responseEntity = new PayrollApi().approvePayroll("accountId", "contactId","payload", mockHttpServletRequest);
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse, responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(200, responseEntity.getStatusCode().value());
        }
    }

    @Test
    void approvePayroll_success_false_test() {
        Map<String, Object> mockResponse = new HashMap<>();
        MockHttpServletRequest mockHttpServletRequest = new MockHttpServletRequest();
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
             MockedConstruction<PayrollService> mock = Mockito.mockConstruction(PayrollService.class, (payrollServiceMock, context) -> {
                 Mockito.when(payrollServiceMock.approvePayrollEvents(anyString(),anyString(),any(Contact.class), anyString())).thenReturn(mockResponse);
             })) {
            annotationHelperMockedStatic.when(() -> AnnotationHelper.extractCurrentUserContactFromRequest(mockHttpServletRequest)).thenReturn(new Contact());
            ResponseEntity<GenericResponse> responseEntity = new PayrollApi().approvePayroll("accountId", "contactId","payload", mockHttpServletRequest);
            var genericResponse = new GenericResponse(false, null, "No entries were modified");
            Assertions.assertEquals(genericResponse, responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(200, responseEntity.getStatusCode().value());
        }
    }

    @Test
    void approvePayroll_exception_test(){
        MockHttpServletRequest mockHttpServletRequest = new MockHttpServletRequest();
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
             MockedConstruction<PayrollService> mock = Mockito.mockConstruction(PayrollService.class, (payrollServiceMock, context) -> {
                 Mockito.when(payrollServiceMock.approvePayrollEvents(anyString(),anyString(),any(Contact.class),anyString())).thenThrow(new IllegalArgumentException("exception"));
             })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(mockHttpServletRequest)).thenReturn(new Contact());
            ResponseEntity<GenericResponse> responseEntity = new PayrollApi().approvePayroll("accountId", "contactId","payload",mockHttpServletRequest);
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updateUserVerifiedEntries_exception_test(){

        try(MockedConstruction<PayrollService> payrollServiceMock = Mockito.mockConstruction(PayrollService.class, (payrollService, context) -> {
            Mockito.when(payrollService.updateUserVerifiedEntries(anyString(), anyString(), anyString(), any())).thenThrow(new IllegalArgumentException("Invalid parameters"));
        })) {

            ResponseEntity<GenericResponse> responseEntity = new PayrollApi().updateUserVerifiedEntries("accountID", "contactID", "", new MockHttpServletRequest());
        }catch (Exception e){
            Assertions.assertEquals("Invalid parameters", e.getMessage());
        }
    }

    @Test
    void updateUserVerifiedEntries_ifResponseMapIsNullOrEmpty_test(){

        Map<String, Object> responseMap = new HashMap<>();

        try(MockedConstruction<PayrollService> payrollServiceMock = Mockito.mockConstruction(PayrollService.class, (payrollService, context) -> {
            Mockito.when(payrollService.updateUserVerifiedEntries(anyString(), anyString(), anyString(), any())).thenReturn(responseMap);
        })) {
            ResponseEntity<GenericResponse> responseEntity = new PayrollApi().updateUserVerifiedEntries("accountID", "contactID","", new MockHttpServletRequest());

            Assertions.assertEquals(200, responseEntity.getStatusCodeValue());
            Assertions.assertEquals(false, responseEntity.getBody().isSuccess());
            Assertions.assertEquals(EVENTS_ERROR_RESPONSE.NO_ENTRIES_FOUND.value(),responseEntity.getBody().getErrorMessage());
            Assertions.assertEquals(null, responseEntity.getBody().getData());
        }
    }

    @Test
    void updateUserVerifiedEntries_valid_response_test(){

        Map<String, Object>responseMap = new HashMap<>();
        responseMap.put(SchedulingKeys.ENTRY, "entry");
        responseMap.put(Commons.SUCCESS, true);

        try(MockedConstruction<PayrollService> payrollServiceMock = Mockito.mockConstruction(PayrollService.class, (payrollService, context) -> {
            Mockito.when(payrollService.updateUserVerifiedEntries(anyString(), anyString(),anyString(), any())).thenReturn(responseMap);
        })) {
            ResponseEntity<GenericResponse> responseEntity = new PayrollApi().updateUserVerifiedEntries("accountID", "contactID", "entryID", new MockHttpServletRequest());

            Assertions.assertEquals(200, responseEntity.getStatusCodeValue());
            Assertions.assertEquals(true, responseEntity.getBody().isSuccess());
            Assertions.assertEquals("entry", responseEntity.getBody().getData().get(SchedulingKeys.ENTRY));
        }
    }

    @Test
    void adminUpdateEntry_exceptionThrown_ShouldReturn400Response(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenThrow(new IllegalArgumentException("test"));
            ResponseEntity<GenericResponse> response = new PayrollApi().adminUpdateEntry("accID","123","eID","",request);
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
            Assertions.assertFalse(response.getBody().isSuccess());
            Assertions.assertEquals("test",response.getBody().getErrorMessage());
        }
    }

    @Test
    void adminUpdateEntry_ValidServiceLayerResponse_ShouldReturnSuccessTrueResponse(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<PayrollService> payrollServiceMockedConstruction = Mockito.mockConstruction(PayrollService.class,(payrollServiceMock, context)->{
                Mockito.when(payrollServiceMock.adminUpdateEntry("accID","123","eID","",new Contact())).thenReturn(new GenericResponse(true,null,null));
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenReturn(new Contact());
            ResponseEntity<GenericResponse> response = new PayrollApi().adminUpdateEntry("accID","123","eID","", request);
            Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
            Assertions.assertTrue(response.getBody().isSuccess());
        }
    }
}

