package com.yoco.peers.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import com.yoco.hours.endpoint.Hours;
import com.yoco.hours.service.HourService;
import com.yoco.peers.service.PeerService;
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

class PeersTest {

    @Test
    void getAllClockedInPeers_accesstoken_exception_test(){

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> resp = new Peers().getAllClockedInPeers("accID", new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }

    }

    @Test
    void getAllClockedInPeers_service_method_exception_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<PeerService> mock = Mockito.mockConstruction(PeerService.class, (peerServiceMock, context) -> {
                Mockito.when(peerServiceMock.getClockedInUsers(anyString(),any(Contact.class))).thenThrow(new IllegalArgumentException("Exception test"));
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new Peers().getAllClockedInPeers("accID", new MockHttpServletRequest());

            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,resp.getStatusCode());
        }
    }

    @Test
    void getAllClockedInPeers_service_empty_resp_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<PeerService> obj = Mockito.mockConstruction(PeerService.class, (objPeerserive, context)->{
                Mockito.when(objPeerserive.getClockedInUsers(anyString(), any(Contact.class))).thenReturn(new HashMap<>());
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new Peers().getAllClockedInPeers("accID", new MockHttpServletRequest());

            var genericResponse = new GenericResponse(false, null, EVENTS_ERROR_RESPONSE.NO_CLOCKEDIN_USERS.value());
            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }

    @Test
    void getAllClockedInPeers_valid_test(){

        Contact mockContact = new Contact();
        mockContact.setId("1234");
        HashMap<String, Object> response =  new HashMap<>();
        response.put("entries", "entry");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<PeerService> obj = Mockito.mockConstruction(PeerService.class, (objPeerserive, context)->{
                Mockito.when(objPeerserive.getClockedInUsers(anyString(), any(Contact.class))).thenReturn(response);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> resp = new Peers().getAllClockedInPeers("accID", new MockHttpServletRequest());

            var genericResponse = new GenericResponse();
            genericResponse.setData(response);
            genericResponse.setSuccess(true);

            Assertions.assertEquals(genericResponse,resp.getBody());
            Assertions.assertEquals(HttpStatus.OK,resp.getStatusCode());
        }
    }
}
