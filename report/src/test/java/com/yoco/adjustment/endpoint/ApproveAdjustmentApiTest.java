package com.yoco.adjustment.endpoint;

import com.yoco.adjustment.service.ApproveAdjustmentService;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
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

class ApproveAdjustmentApiTest {

    @Test
    void approveAdjustment_valid_test(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        Contact contact = new Contact();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<ApproveAdjustmentService> adjustmentServiceMock = Mockito.mockConstruction(ApproveAdjustmentService.class, (adjustmentService, context) -> {
            Mockito.when(adjustmentService.approveAdjustment(anyString(),anyString(),any())).thenReturn(true);
        })) {
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenReturn(contact);
            ResponseEntity<GenericResponse> responseEntity = new ApproveAdjustmentApi().approveAdjustment("accountID", "contactID", request);
            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void approveAdjustment_exception_test(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        Contact contact = new Contact();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<ApproveAdjustmentService> adjustmentServiceMock = Mockito.mockConstruction(ApproveAdjustmentService.class, (adjustmentService, context) -> {
            Mockito.when(adjustmentService.approveAdjustment(anyString(),anyString(),any())).thenThrow(new IllegalArgumentException("exception"));
        })) {
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenReturn(contact);
            ResponseEntity<GenericResponse> responseEntity = new ApproveAdjustmentApi().approveAdjustment("accountID", "contactID", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false,null,"exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void editApproveAdjustment_success_false_test(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        Contact contact = new Contact();
        Map<String,Object> mockResponse = new HashMap<>();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<ApproveAdjustmentService> adjustmentServiceMock = Mockito.mockConstruction(ApproveAdjustmentService.class, (adjustmentService, context) -> {
                Mockito.when(adjustmentService.editApproveAdjustment(anyString(),anyString(),anyString(),any())).thenReturn(mockResponse);
            })) {
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenReturn(contact);
            ResponseEntity<GenericResponse> responseEntity = new ApproveAdjustmentApi().editApproveAdjustment("accountID", "entryId","payload",request);
            var genericResponse = new GenericResponse(false,null, COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void editApproveAdjustment_success_false_test2(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        Contact contact = new Contact();
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put(Commons.MESSAGE,"error message");
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<ApproveAdjustmentService> adjustmentServiceMock = Mockito.mockConstruction(ApproveAdjustmentService.class, (adjustmentService, context) -> {
                Mockito.when(adjustmentService.editApproveAdjustment(anyString(),anyString(),anyString(),any())).thenReturn(mockResponse);
            })) {
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenReturn(contact);
            ResponseEntity<GenericResponse> responseEntity = new ApproveAdjustmentApi().editApproveAdjustment("accountID", "entryId","payload",request);
            var genericResponse = new GenericResponse(false,null, null);
            genericResponse.add(Commons.MESSAGE,"error message");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void editApproveAdjustment_valid_test(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        Contact contact = new Contact();
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put(Commons.SUCCESS,true);
        mockResponse.put("data","entry");
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<ApproveAdjustmentService> adjustmentServiceMock = Mockito.mockConstruction(ApproveAdjustmentService.class, (adjustmentService, context) -> {
                Mockito.when(adjustmentService.editApproveAdjustment(anyString(),anyString(),anyString(),any())).thenReturn(mockResponse);
            })) {
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenReturn(contact);
            ResponseEntity<GenericResponse> responseEntity = new ApproveAdjustmentApi().editApproveAdjustment("accountID", "entryId","payload",request);
            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void editApproveAdjustment_exception_test(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        Contact contact = new Contact();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<ApproveAdjustmentService> adjustmentServiceMock = Mockito.mockConstruction(ApproveAdjustmentService.class, (adjustmentService, context) -> {
                Mockito.when(adjustmentService.editApproveAdjustment(anyString(),anyString(),anyString(),any())).thenThrow(new IllegalArgumentException("exception"));
            })) {
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenReturn(contact);
            ResponseEntity<GenericResponse> responseEntity = new ApproveAdjustmentApi().editApproveAdjustment("accountID", "entryId","payload", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false,null,"exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

}