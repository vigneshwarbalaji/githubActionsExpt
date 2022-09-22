package com.yoco.team.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.team.service.TeamService;
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

class TeamApiTest {
    @Test
    void getTeamsInAnAccount_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new TeamApi().getTeamsInAnAccount("accID",100,"",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void getTeamsInAnAccount_valid_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        HashMap<String,Object> mockResponse = new HashMap<>();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<TeamService> mock = Mockito.mockConstruction(TeamService.class, (teamServiceMock, context) -> {
                Mockito.when(teamServiceMock.getAllTeamsForAccount("accID",100,"","123")).thenReturn(mockResponse);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new TeamApi().getTeamsInAnAccount("accID",100,"",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

    @Test
    void getTeamsForUserInAnAccount_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new TeamApi().getTeamsForUserInAnAccount("accID","123",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void getTeamsForUserInAnAccount_valid_test(){
        Contact mockContact = new Contact();
        mockContact.setId("234");
        HashMap<String,Object> mockResponse = new HashMap<>();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<TeamService> mock = Mockito.mockConstruction(TeamService.class, (teamServiceMock, context) -> {
                Mockito.when(teamServiceMock.getTeamsForUserInAnAccount("accID","123","234")).thenReturn(mockResponse);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new TeamApi().getTeamsForUserInAnAccount("accID","123",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

    @Test
    void deleteTeam_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new TeamApi().deleteTeam("accID","teamID",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void deleteTeam_valid_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        HashMap<String,Object> mockResponse = new HashMap<>();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<TeamService> mock = Mockito.mockConstruction(TeamService.class, (teamServiceMock, context) -> {
                Mockito.when(teamServiceMock.deleteTeam("accID","teamID","123")).thenReturn(mockResponse);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new TeamApi().deleteTeam("accID","teamID",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

    @Test
    void createTeam_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new TeamApi().createTeam("accID","payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void createTeam_valid_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        HashMap<String,Object> mockResponse = new HashMap<>();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<TeamService> mock = Mockito.mockConstruction(TeamService.class, (teamServiceMock, context) -> {
                Mockito.when(teamServiceMock.createTeam("accID","payload","123")).thenReturn(mockResponse);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new TeamApi().createTeam("accID","payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

    @Test
    void updateTeamInfo_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new TeamApi().updateTeamInfo("accID","teamID","payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void updateTeamInfo_valid_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        HashMap<String,Object> mockResponse = new HashMap<>();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<TeamService> mock = Mockito.mockConstruction(TeamService.class, (teamServiceMock, context) -> {
                Mockito.when(teamServiceMock.updateTeamInfo("accID","teamID","payload","123")).thenReturn(mockResponse);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new TeamApi().updateTeamInfo("accID","teamID","payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

    @Test
    void updateTeamContacts_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new TeamApi().updateTeamContacts("accID","teamID","payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void updateTeamContacts_valid_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        HashMap<String,Object> mockResponse = new HashMap<>();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<TeamService> mock = Mockito.mockConstruction(TeamService.class, (teamServiceMock, context) -> {
                Mockito.when(teamServiceMock.updateTeamContacts("accID","teamID","payload","123")).thenReturn(mockResponse);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new TeamApi().updateTeamContacts("accID","teamID","payload",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

}
