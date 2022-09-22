package com.yoco.activity.endpoint;

import com.yoco.activity.service.ActivityService;
import com.yoco.commons.annotation.helper.AnnotationHelper;
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

import javax.servlet.http.HttpServletRequest;

import java.util.HashMap;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class ActivityApiTest {

    @Test
    void createActivity_accesstoken_exception_test() {
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new ActivityApi().createActivity( new MockHttpServletRequest() , "accountID", "payload");
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void createActivity_serviceMethod_exception_test() {
        Contact mockContact = new Contact();
        mockContact.setId("1234");
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<ActivityService> mock = Mockito.mockConstruction(ActivityService.class, (activityServiceObj, context)->{
                Mockito.when(activityServiceObj.createActivity(anyString(),any(Contact.class),anyString())).thenThrow(new IllegalArgumentException("Exception test"));
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new ActivityApi().createActivity( new MockHttpServletRequest() , "accountID", "payload");
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void createActivity_empty_response_test(){
        Contact mockContact = new Contact();
        mockContact.setId("1234");
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<ActivityService> mock = Mockito.mockConstruction(ActivityService.class, (activityServiceOBj, context)->{
                Mockito.when(activityServiceOBj.createActivity(anyString(), any(Contact.class),anyString())).thenReturn(new HashMap<>());
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new ActivityApi().createActivity( new MockHttpServletRequest() , "accountID", "payload");
            var genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.UNABLE_TO_CREATE.value());
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void createActivity_valid_response_test(){
        Contact mockContact = new Contact();
        mockContact.setId("1234");

        HashMap<String, Object> response = new HashMap<>();
        response.put("activity", "activity");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<ActivityService> mock = Mockito.mockConstruction(ActivityService.class, (activityServiceOBj, context)->{
                Mockito.when(activityServiceOBj.createActivity(anyString(), any(Contact.class),anyString())).thenReturn(response);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new ActivityApi().createActivity( new MockHttpServletRequest() , "accountID", "payload");

            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.add("activity", resp);

            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }
}

