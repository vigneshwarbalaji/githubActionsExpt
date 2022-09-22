package com.yoco.user.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.modal.date.RangeInfoDTO;
import com.yoco.user.service.UserDateTimeService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;

import javax.servlet.http.HttpServletRequest;

import java.util.HashMap;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;


class UserDateTimeApiTest {
    @Test
    void getRangeInfo_exception_test(){
        try(MockedConstruction<UserDateTimeService> userDateTimeService = Mockito.mockConstruction(UserDateTimeService.class, (userDateTimeServiceMock, context) -> {
            Mockito.when(userDateTimeServiceMock.getRangeInfo("123","234","range","","")).thenThrow(new IllegalArgumentException("Exception Test"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserDateTimeApi().getRangeInfo("123","234","range","","");
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertFalse(responseEntity.getBody().isSuccess());
            Assertions.assertEquals("Exception Test",responseEntity.getBody().getErrorMessage());
        }
    }

    @Test
    void getRangeInfo_valid_test(){
        RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
        try(MockedConstruction<UserDateTimeService> userDateTimeService = Mockito.mockConstruction(UserDateTimeService.class, (userDateTimeServiceMock, context) -> {
            Mockito.when(userDateTimeServiceMock.getRangeInfo("123","234","range","","")).thenReturn(rangeInfoDTO);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserDateTimeApi().getRangeInfo("123","234","range","","");
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertTrue(responseEntity.getBody().isSuccess());
            Assertions.assertEquals(rangeInfoDTO,responseEntity.getBody().getData().get("range"));
            Assertions.assertNull(responseEntity.getBody().getErrorMessage());
        }
    }

    @Test
    void isDSTorNot_accesstoken_exception_test(){

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new UserDateTimeApi().isDSTorNot("accID", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }

    }

    @Test
    void isDSTorNot_service_method_exception_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<UserDateTimeService> mock = Mockito.mockConstruction(UserDateTimeService.class, (userDateTimeServiceMock, context) -> {
                Mockito.when(userDateTimeServiceMock.checkDSTorNot(anyString(),anyString())).thenThrow(new IllegalArgumentException("Exception test"));
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new UserDateTimeApi().isDSTorNot("accID", new MockHttpServletRequest());

            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void isDSTorNot_valid_resp_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<UserDateTimeService> mock = Mockito.mockConstruction(UserDateTimeService.class, (userDateTimeServiceMock, context) -> {
                Mockito.when(userDateTimeServiceMock.checkDSTorNot(anyString(),anyString())).thenReturn(new HashMap<>());
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new UserDateTimeApi().isDSTorNot("accID", new MockHttpServletRequest());

            var genericResponse = new GenericResponse();
            genericResponse.setData(new HashMap<>());
            genericResponse.setSuccess(true);

            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }
}
