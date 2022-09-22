package com.yoco.adjustment.endpoint;

import com.yoco.adjustment.service.AdjustmentService;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.constants.EventConstants;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
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

import static org.mockito.ArgumentMatchers.*;

class AdjustmentApiTest {
    @Test
    void getAdjustments_exception_test(){

        try(MockedConstruction<AdjustmentService> adjustmentServiceMock = Mockito.mockConstruction(AdjustmentService.class, (adjustmentService, context) -> {
            Mockito.when(adjustmentService.getAdjustments(anyString(), anyString(), any(), anyString(), anyString())).thenThrow(new IllegalArgumentException("Invalid parameters"));
        })) {
            new AdjustmentApi().getAdjustments("account", "contact", DateConstants.BY_DATE, "10/09/2022",
                    "15/09/2022", "REJECTED", "Asia/Kolkata","", "",new MockHttpServletRequest());
        }catch (Exception e){
            Assertions.assertEquals("Invalid parameters", e.getMessage());
        }
    }

    @Test
    void getAdjustments_ifResponseMapIsNullOrEmpty_test(){

        Map<String, Object> responseMap = new HashMap<>();

        try(MockedConstruction<AdjustmentService> adjustmentServiceMock = Mockito.mockConstruction(AdjustmentService.class, (adjustmentService, context) -> {
            Mockito.when(adjustmentService.getAdjustments(anyString(), anyString(), any(), anyString(), anyString())).thenReturn(responseMap);
        })) {
            ResponseEntity<GenericResponse> responseEntity = new AdjustmentApi().getAdjustments("accountID", "contactID", DateConstants.BY_DATE, "10/09/2022",
                    "15/09/2022", "APPROVED", "Asia/Kolkata", "","",new MockHttpServletRequest());

            Assertions.assertEquals(200, responseEntity.getStatusCodeValue());
            Assertions.assertEquals(false, responseEntity.getBody().isSuccess());
            Assertions.assertEquals(EVENTS_ERROR_RESPONSE.OPERATION_FAILED.value(),responseEntity.getBody().getErrorMessage());
            Assertions.assertEquals(null, responseEntity.getBody().getData());
        }
    }

    @Test
    void getAdjustments_valid_response_test(){

        Map<String, Object>responseMap = new HashMap<>();
        responseMap.put(EventConstants.ADJUSTMENTS, "adjustment");

        try(MockedConstruction<AdjustmentService> adjustmentServiceMock = Mockito.mockConstruction(AdjustmentService.class, (adjustmentService, context) -> {
            Mockito.when(adjustmentService.getAdjustments(anyString(), anyString(), any(), anyString(), anyString())).thenReturn(responseMap);
        })) {
            ResponseEntity<GenericResponse> responseEntity = new AdjustmentApi().getAdjustments("accountID", "contactID", DateConstants.BY_DATE, "11/12/2022",
                    "13/12/2022", "PENDING",  "Asia/Kolkata", "","",new MockHttpServletRequest());

            Assertions.assertEquals(200, responseEntity.getStatusCodeValue());
            Assertions.assertEquals(true, responseEntity.getBody().isSuccess());
            Assertions.assertEquals("adjustment", responseEntity.getBody().getData().get(EventConstants.ADJUSTMENTS));
        }
    }

    @Test
    void deleteAdjustment_ExceptionThrown_ShouldReturnSuccessFalseResponse(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenThrow(new IllegalArgumentException("test"));
            ResponseEntity<GenericResponse> response = new AdjustmentApi().deleteAdjustment("accID","123",false, "Asia/Kolkata", null,request);
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
            Assertions.assertFalse(response.getBody().isSuccess());
            Assertions.assertEquals("test",response.getBody().getErrorMessage());
        }
    }

    @Test
    void deleteAdjustment_ValidServiceLayerResponse_ShouldReturnSuccessTrueResponse(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<AdjustmentService> adjustmentServiceMockedConstruction = Mockito.mockConstruction(AdjustmentService.class,(adjustmentServiceMock, context)->{
                Mockito.when(adjustmentServiceMock.deleteAdjustment("accID","123",false,new Contact(),"Asia/Kolkata",null)).thenReturn(new GenericResponse(true,null,null));
        })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenReturn(new Contact());
            ResponseEntity<GenericResponse> response = new AdjustmentApi().deleteAdjustment("accID","123",false, "Asia/Kolkata", null,request);
            Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
            Assertions.assertTrue(response.getBody().isSuccess());
        }
    }

    @Test
    void rejectAdjustment_exceptionThrown_ShouldReturn400Response(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenThrow(new IllegalArgumentException("test"));
            ResponseEntity<GenericResponse> response = new AdjustmentApi().rejectAdjustment("accID","123",false, "Asia/Kolkata", "",null,request);
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
            Assertions.assertFalse(response.getBody().isSuccess());
            Assertions.assertEquals("test",response.getBody().getErrorMessage());
        }
    }

    @Test
    void rejectAdjustment_ValidServiceLayerResponse_ShouldReturnSuccessTrueResponse(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<AdjustmentService> adjustmentServiceMockedConstruction = Mockito.mockConstruction(AdjustmentService.class,(adjustmentServiceMock, context)->{
                Mockito.when(adjustmentServiceMock.rejectAdjustment("accID","123",false,new Contact(),"Asia/Kolkata",null,"")).thenReturn(new GenericResponse(true,null,null));
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenReturn(new Contact());
            ResponseEntity<GenericResponse> response = new AdjustmentApi().rejectAdjustment("accID","123",false, "Asia/Kolkata", null,"",request);
            Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
            Assertions.assertTrue(response.getBody().isSuccess());
        }
    }
}
