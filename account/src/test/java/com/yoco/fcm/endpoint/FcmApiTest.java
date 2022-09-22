package com.yoco.fcm.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.error.ApiErrorCode;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.fcm.enums.FCM_ERROR_MESSAGE;
import com.yoco.fcm.helper.FcmHelper;
import com.yoco.fcm.service.FcmService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mockStatic;

class FcmApiTest {

    @Test
    void getSyncInfo_exception_test(){

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<FcmService> mock = Mockito.mockConstruction(FcmService.class, (fcmServiceMock, context) -> {
                 Mockito.when(fcmServiceMock.getFcmSyncInfo(anyString(),anyString(),anyString())).thenReturn(null);
             })){
            Contact contact = new Contact();
            contact.setId("122");
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new FcmApi().getSyncInfo("accountId", "deviceID",httpServletRequest);
            var genericResponse = new GenericResponse(false, ApiErrorCode.BAD_REQUEST, null);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getSyncInfo_deviceInfoNotAvailable_test(){

        Map<String,Object> mockServiceMethodSyncResponse = new HashMap<>();
        mockServiceMethodSyncResponse.put(FcmHelper.ERROR_MESSAGE, FCM_ERROR_MESSAGE.INVALID_DEVICE_ID.value());
        mockServiceMethodSyncResponse.put(FcmHelper.IS_DEVICE_INFO_AVAILABLE, false);

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<FcmService> mock = Mockito.mockConstruction(FcmService.class, (fcmServiceMock, context) -> {
                 Mockito.when(fcmServiceMock.getFcmSyncInfo(anyString(),anyString(),anyString())).thenReturn(mockServiceMethodSyncResponse);
             })){
            Contact contact = new Contact();
            contact.setId("122");
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new FcmApi().getSyncInfo("accountId", "deviceID",httpServletRequest);
            var genericResponse = new GenericResponse(false, ApiErrorCode.NOT_FOUND, FCM_ERROR_MESSAGE.INVALID_DEVICE_ID.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.NOT_FOUND,responseEntity.getStatusCode());
            Assertions.assertEquals(404,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void getSyncInfo_validResponse_test(){

        Map<String,Object> mockServiceMethodSyncResponse = new HashMap<>();
        mockServiceMethodSyncResponse.put("dummy", "mock");
        mockServiceMethodSyncResponse.put(FcmHelper.IS_DEVICE_INFO_AVAILABLE, true);

        Map<String,Object> mockSyncInfo = new HashMap<>();
        mockSyncInfo.put("dummy", "mock");
        Map<String,Object> expectedResponse = new HashMap<>();
        expectedResponse.put(FcmHelper.SYNC_OBJECT_INFO, mockSyncInfo);
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<FcmService> mock = Mockito.mockConstruction(FcmService.class, (fcmServiceMock, context) -> {
             Mockito.when(fcmServiceMock.getFcmSyncInfo(anyString(),anyString(),anyString())).thenReturn(mockServiceMethodSyncResponse);
        })){
            Contact contact = new Contact();
            contact.setId("122");
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new FcmApi().getSyncInfo("accountId", "deviceID",httpServletRequest);
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(expectedResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void unRegisterDeviceInfo_exception_test(){

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<FcmService> mock = Mockito.mockConstruction(FcmService.class, (fcmServiceMock, context) -> {
                 Mockito.when(fcmServiceMock.deleteDeviceInfo(anyString(),anyString(),anyString())).thenReturn(null);
             })){
            Contact contact = new Contact();
            contact.setId("122");
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new FcmApi().unRegisterDeviceInfo("accountId", "deviceID",httpServletRequest);
            var genericResponse = new GenericResponse(false, ApiErrorCode.BAD_REQUEST, null);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void unRegisterDeviceInfo_deviceInfoNotAvailable_test(){

        Map<String,Object> mockServiceMethodSyncResponse = new HashMap<>();
        mockServiceMethodSyncResponse.put(FcmHelper.ERROR_MESSAGE, FCM_ERROR_MESSAGE.INVALID_DEVICE_ID.value());
        mockServiceMethodSyncResponse.put(FcmHelper.IS_DEVICE_UN_REGISTERED, false);

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<FcmService> mock = Mockito.mockConstruction(FcmService.class, (fcmServiceMock, context) -> {
                 Mockito.when(fcmServiceMock.deleteDeviceInfo(anyString(),anyString(),anyString())).thenReturn(mockServiceMethodSyncResponse);
             })){
            Contact contact = new Contact();
            contact.setId("122");
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new FcmApi().unRegisterDeviceInfo("accountId", "deviceID",httpServletRequest);
            var genericResponse = new GenericResponse(false, ApiErrorCode.NOT_FOUND, FCM_ERROR_MESSAGE.INVALID_DEVICE_ID.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.NOT_FOUND,responseEntity.getStatusCode());
            Assertions.assertEquals(404,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void unRegisterDeviceInfo_validResponse_test(){

        Map<String,Object> mockServiceMethodSyncResponse = new HashMap<>();
        mockServiceMethodSyncResponse.put("dummy", "mock");
        mockServiceMethodSyncResponse.put(FcmHelper.IS_DEVICE_UN_REGISTERED, true);

        Map<String,Object> mockSyncInfo = new HashMap<>();
        mockSyncInfo.put("dummy", "mock");
        mockSyncInfo.put(FcmHelper.IS_DEVICE_UN_REGISTERED,true);
        Map<String,Object> expectedResponse = new HashMap<>();
        expectedResponse.put(FcmHelper.UNREGISTER_DEVICE_INFO, mockSyncInfo);
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<FcmService> mock = Mockito.mockConstruction(FcmService.class, (fcmServiceMock, context) -> {
                 Mockito.when(fcmServiceMock.deleteDeviceInfo(anyString(),anyString(),anyString())).thenReturn(mockServiceMethodSyncResponse);
             })){
            Contact contact = new Contact();
            contact.setId("122");
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new FcmApi().unRegisterDeviceInfo("accountId", "deviceID",httpServletRequest);
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(expectedResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void registerDevice_exception_test(){

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<FcmService> mock = Mockito.mockConstruction(FcmService.class, (fcmServiceMock, context) -> {
                 Mockito.when(fcmServiceMock.registerDeviceInfo(anyString(),any(),anyString())).thenThrow(new NullPointerException());
             })){
            Contact contact = new Contact();
            contact.setId("122");
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new FcmApi().registerDevice("accountId", "payload",httpServletRequest);
            var genericResponse = new GenericResponse(false, ApiErrorCode.BAD_REQUEST, null);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void registerDevice_validResponse_test(){

        Map<String,Object> mockServiceMethodSyncResponse = new HashMap<>();
        mockServiceMethodSyncResponse.put("dummy", "mock");
        Map<String,Object> expectedResponse = new HashMap<>();
        expectedResponse.put("dummy", "mock");
        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<FcmService> mock = Mockito.mockConstruction(FcmService.class, (fcmServiceMock, context) -> {
                 Mockito.when(fcmServiceMock.registerDeviceInfo(anyString(),any(),anyString())).thenReturn(mockServiceMethodSyncResponse);
             })){
            Contact contact = new Contact();
            contact.setId("122");
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);
            ResponseEntity<GenericResponse> responseEntity = new FcmApi().registerDevice("accountId", "payload",httpServletRequest);
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(expectedResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

}
