package com.yoco.project.endpointTest;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.project.endpoint.ProjectApi;
import com.yoco.project.enums.PROJECT_ERROR_RESPONSE;
import com.yoco.project.service.ProjectService;
import org.junit.jupiter.api.*;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mockStatic;

class ProjectApiTest {

    @Test
    void createProject_null_response_test(){

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedStatic<HeaderUtil> headerUtilMockedStatic = mockStatic(HeaderUtil.class);
             MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                 Mockito.when(projectServiceMock.createProject(any(),any(),any(),any(),any())).thenReturn(null);
             })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            headerUtilMockedStatic.when(() -> HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class))).thenReturn("srcClientId");

            Map<String,Object> payload = new HashMap<>();
            payload.put("name","project");

            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().createProject("accountId", JsonUtil.getJson(payload),httpServletRequest);

            var genericResponse = new GenericResponse(false, null, COMMON_ERROR_RESPONSE.UNABLE_TO_CREATE.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
            annotationHelperMockedStatic.verify(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class)));
            headerUtilMockedStatic.verify(()-> HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class)));
        }
    }

    @Test
    void createProject_valid_response_test(){

        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("project","projectJDO");

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedStatic<HeaderUtil> headerUtilMockedStatic = mockStatic(HeaderUtil.class);
             MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                 Mockito.when(projectServiceMock.createProject(any(),any(),any(),any(),any())).thenReturn(mockResponse);
             })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            headerUtilMockedStatic.when(() -> HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class))).thenReturn("srcClientId");

            Map<String,Object> payload = new HashMap<>();
            payload.put("name","project");

            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().createProject("accountId", JsonUtil.getJson(payload),httpServletRequest);

            var genericResponse = new GenericResponse(true, null,null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
            annotationHelperMockedStatic.verify(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class)));
            headerUtilMockedStatic.verify(()-> HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class)));
        }
    }

    @Test
    void createProject_exception_test(){

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedStatic<HeaderUtil> headerUtilMockedStatic = mockStatic(HeaderUtil.class);
             MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                 Mockito.when(projectServiceMock.createProject(any(),any(),any(),any(),any())).thenThrow(new IOException("exception"));
             })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            headerUtilMockedStatic.when(() -> HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class))).thenReturn("srcClientId");

            Map<String,Object> payload = new HashMap<>();
            payload.put("name","project");

            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().createProject("accountId", JsonUtil.getJson(payload),httpServletRequest);

            var genericResponse = new GenericResponse(false, null,"exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
            annotationHelperMockedStatic.verify(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class)));
            headerUtilMockedStatic.verify(()-> HeaderUtil.extractClientIdFromRequest(any(HttpServletRequest.class)));
        }
    }

    @Test
    void enableProject_null_response_test(){

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
               Mockito.when(projectServiceMock.enableProject(any(),any(),any())).thenReturn(null);
             })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);

            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().enableProject("accountId", "projectId",httpServletRequest);

            var genericResponse = new GenericResponse(false, null, PROJECT_ERROR_RESPONSE.PROJECT_NOT_FOUND.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
            annotationHelperMockedStatic.verify(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class)));
        }
    }

    @Test
    void enableProject_valid_response_test(){

        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("project","projectJDO");

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                Mockito.when(projectServiceMock.enableProject(any(),any(),any())).thenReturn(mockResponse);
             })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);

            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().enableProject("accountId", "projectId",httpServletRequest);

            var genericResponse = new GenericResponse(true, null,null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
            annotationHelperMockedStatic.verify(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class)));
        }
    }

    @Test
    void enableProject_exception_test(){

        try (MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
             MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                 Mockito.when(projectServiceMock.enableProject(any(),any(),any())).thenThrow(new IOException("exception"));
             })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);

            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().enableProject("accountId", "projectId",httpServletRequest);

            var genericResponse = new GenericResponse(false, null,"exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
            annotationHelperMockedStatic.verify(() -> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class)));
        }
    }

    @Test
    void getAllProjectsOfThisAccount_nullResponseTest(){

        try (MockedStatic<ProjectService> projectServiceMockedStatic = mockStatic(ProjectService.class)){
            ProjectService projectService = Mockito.mock(ProjectService.class);
            Mockito.when(projectService.getAllProjectDetailsOfAnAccount(anyString(), anyString(), anyString(), anyInt())).thenReturn(null);
            projectServiceMockedStatic.when(ProjectService::getProjectService).thenReturn(projectService);

            Map<String, Object>responseMap = new HashMap<>();
            responseMap.put("projects", new ArrayList<>());

            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(false);
            genericResponse.setData(responseMap);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().getAllProjectsOfThisAccount("123", "all", "cursor", 50);
            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(genericResponse,responseEntity.getBody() );
        }

    }

    @Test
    void getAllProjectsOfThisAccount_emptyResponseTest(){

        try (MockedStatic<ProjectService> projectServiceMockedStatic = mockStatic(ProjectService.class)){
            ProjectService projectService = Mockito.mock(ProjectService.class);
            Mockito.when(projectService.getAllProjectDetailsOfAnAccount(anyString(), anyString(), anyString(), anyInt())).thenReturn(new HashMap<>());
            projectServiceMockedStatic.when(ProjectService::getProjectService).thenReturn(projectService);

            Map<String, Object>responseMap = new HashMap<>();
            responseMap.put("projects", new ArrayList<>());

            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(false);
            genericResponse.setData(responseMap);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().getAllProjectsOfThisAccount("123", "all", "cursor", 50);
            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(genericResponse,responseEntity.getBody() );
        }
    }

    @Test
    void getAllProjectsOfThisAccount_exceptionTest(){

        try (MockedStatic<ProjectService> projectServiceMockedStatic = mockStatic(ProjectService.class)){

            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(false);
            genericResponse.setData(null);
            genericResponse.setErrorMessage("Invalid accountID");

            ProjectService projectService = Mockito.mock(ProjectService.class);
            Mockito.when(projectService.getAllProjectDetailsOfAnAccount(anyString(), anyString(), anyString(), anyInt())).thenThrow(new IllegalArgumentException("Invalid accountID"));
            projectServiceMockedStatic.when(ProjectService::getProjectService).thenReturn(projectService);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().getAllProjectsOfThisAccount("invalidAccountId","active","cursor",20);
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
            Assertions.assertEquals(genericResponse, responseEntity.getBody());

        }

    }

    @Test
    void getAllProjectsOfThisAccount_validTest() {

        try (MockedStatic<ProjectService> projectServiceMockedStatic = mockStatic(ProjectService.class)){

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setUniquepin("134");
            projectObject.setProjectName("newProject");
            projectObject.setProjectId("2323");
            projectObject.setIsDeleted(true);

            List<ProjectJDO>projectList = new ArrayList<>();
            projectList.add(0,projectObject);

            Map<String, Object>responseMap = new HashMap<>();
            responseMap.put("projects", projectList);
            responseMap.put("cursor", null);

            var genericResponse = new GenericResponse();
            genericResponse.setSuccess(true);
            genericResponse.setData(responseMap);

            ProjectService projectService = Mockito.mock(ProjectService.class);
            Mockito.when(projectService.getAllProjectDetailsOfAnAccount(anyString(), anyString(), anyString(), anyInt())).thenReturn(responseMap);
            projectServiceMockedStatic.when(ProjectService::getProjectService).thenReturn(projectService);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().getAllProjectsOfThisAccount("134", "all", "cursor", 200);
            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(genericResponse, responseEntity.getBody());
        }

    }

    @Test
    void getAllProjectsAssociatedToTheUser_nullResponseTest() {

        try (MockedStatic<ProjectService> projectServiceMockedStatic = mockStatic(ProjectService.class)){

            Map<String, Object> responseMap = new HashMap<>();
            responseMap.put("projects", new ArrayList<>());

            ProjectService projectService = Mockito.mock(ProjectService.class);
            Mockito.when(projectService.getAllProjectsAssociatedToTheUser(anyString(), anyString(), any(), anyInt())).thenReturn(null);
            projectServiceMockedStatic.when(ProjectService::getProjectService).thenReturn(projectService);

            var expectedResponse = new GenericResponse();
            expectedResponse.setSuccess(false);
            expectedResponse.setData(responseMap);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().getAllProjectsAssociatedToTheUser("308", "975", null, 20);
            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(expectedResponse, responseEntity.getBody());

        }

    }

    @Test
    void getAllProjectsAssociatedToTheUser_emptyResponseTest() {

        try (MockedStatic<ProjectService> projectServiceMockedStatic = mockStatic(ProjectService.class)){

            Map<String, Object> responseMap = new HashMap<>();
            responseMap.put("projects", new ArrayList<>());

            ProjectService projectService = Mockito.mock(ProjectService.class);
            Mockito.when(projectService.getAllProjectsAssociatedToTheUser(anyString(), anyString(), any(), anyInt())).thenReturn(new HashMap<>());
            projectServiceMockedStatic.when(ProjectService::getProjectService).thenReturn(projectService);

            var expectedResponse = new GenericResponse();
            expectedResponse.setSuccess(false);
            expectedResponse.setData(responseMap);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().getAllProjectsAssociatedToTheUser("658", "565", null, 100);
            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(expectedResponse, responseEntity.getBody());

        }

    }

    @Test
    void getAllProjectsAssociatedToTheUser_exceptionTest() {

        try (MockedStatic<ProjectService> projectServiceMockedStatic = mockStatic(ProjectService.class)){

            ProjectService projectService = Mockito.mock(ProjectService.class);
            Mockito.when(projectService.getAllProjectsAssociatedToTheUser(anyString(), anyString(), any(), anyInt())).thenThrow(new IllegalArgumentException("Invalid accountID."));
            projectServiceMockedStatic.when(ProjectService::getProjectService).thenReturn(projectService);

            var expectedResponse = new GenericResponse();
            expectedResponse.setSuccess(false);
            expectedResponse.setErrorMessage("Invalid accountID.");

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().getAllProjectsAssociatedToTheUser("658", "565", null, 100);
            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
            Assertions.assertEquals(expectedResponse, responseEntity.getBody());

        }

    }

    @Test
    void getAllProjectsAssociatedToTheUser_validTest() {

        try (MockedStatic<ProjectService> projectServiceMockedStatic = mockStatic(ProjectService.class)){

            ProjectJDO projectObject = new ProjectJDO("545", "TrialProject", false, "dollar", 0.0, "no plan", "434");

            List<ProjectJDO> projectList = new ArrayList<>();
            projectList.add(0, projectObject);

            Map<String, Object> responseMap = new HashMap<>();
            responseMap.put("projects", projectList);
            responseMap.put("projectPeopleList", new ArrayList<>());
            responseMap.put("cursor", null);

            ProjectService projectService = Mockito.mock(ProjectService.class);
            Mockito.when(projectService.getAllProjectsAssociatedToTheUser(anyString(), anyString(), any(), anyInt())).thenReturn(responseMap);
            projectServiceMockedStatic.when(ProjectService::getProjectService).thenReturn(projectService);

            var expectedResponse = new GenericResponse();
            expectedResponse.setSuccess(true);
            expectedResponse.setData(responseMap);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().getAllProjectsAssociatedToTheUser("545", "768", null, 50);
            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(expectedResponse, responseEntity.getBody());

        }

    }

    @Test
    void updateProject_emptyResponse_Test(){

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                Mockito.when(projectServiceMock.updateProject(any(),any(),any(),any())).thenReturn(null);
            })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            var expectedResponse = new GenericResponse();
            expectedResponse.setSuccess(false);
            expectedResponse.setErrorMessage("no project found");

            String payload = "{\"name\":\"experimentation\",\"clientID\":\"cd12\",\"contacts\":\"eb33,a7a75\",\"billable\":true,\"currency\":\"ARS $\",\"rate\":\"2\",\"plan\":\"Per Month\"}";

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().updateProject("123", "456", payload, httpServletRequest);

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals("no project found", responseEntity.getBody().getErrorMessage());
        }

    }

    @Test
    void updateProject_nullResponse_Test(){

        Map<String, Object> responseMap = new HashMap<>();

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                Mockito.when(projectServiceMock.updateProject(any(),any(),any(),any())).thenReturn(responseMap);
            })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            var expectedResponse = new GenericResponse();
            expectedResponse.setSuccess(false);
            expectedResponse.setErrorMessage("no project found");

            String payload = "{\"name\":\"test\"}";

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().updateProject("2525", "675", payload, httpServletRequest);

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals("no project found", responseEntity.getBody().getErrorMessage());
        }

    }

    @Test
    void updateProject_exception_Test(){

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                Mockito.when(projectServiceMock.updateProject(any(),any(),any(),any())).thenThrow(new IllegalArgumentException("Invalid accountID."));
            })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            var expectedResponse = new GenericResponse();
            expectedResponse.setSuccess(false);
            expectedResponse.setErrorMessage("Invalid accountID.");

            String payload = "{\"name\":\"test\"}";

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().updateProject("", "879", payload, httpServletRequest);

            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
            Assertions.assertEquals(expectedResponse, responseEntity.getBody());
        }

    }

    @Test
    void updateProject_validTest(){

        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("project", "newProject");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                Mockito.when(projectServiceMock.updateProject(any(),any(),any(),any())).thenReturn(responseMap);
            })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            var expectedResponse = new GenericResponse();
            expectedResponse.setSuccess(true);
            expectedResponse.setData(responseMap);

            String payload = "{\"name\":\"test experimentation\",\"clientID\":\"cb9d\",\"contacts\":\"eb3123,a7a45\",\"billable\":true,\"currency\":\"ARS $\",\"rate\":\"2\",\"plan\":\"Per Month\"}";

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().updateProject("212", "432", payload, httpServletRequest);

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(expectedResponse, responseEntity.getBody());
        }

    }

    @Test
    void deleteProject_emptyResponse_Test(){

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                Mockito.when(projectServiceMock.deleteProject(any(),any(),any(),any())).thenReturn(null);
            })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            var expectedResponse = new GenericResponse();
            expectedResponse.setSuccess(false);
            expectedResponse.setErrorMessage("no project found");

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().deleteProject("123", "456",httpServletRequest);

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals("no project found", responseEntity.getBody().getErrorMessage());
        }

    }

    @Test
    void deleteProject_nullResponse_Test(){

        Map<String, Object> responseMap = new HashMap<>();

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                Mockito.when(projectServiceMock.deleteProject(any(),any(),any(),any())).thenReturn(responseMap);
            })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            var expectedResponse = new GenericResponse();
            expectedResponse.setSuccess(false);
            expectedResponse.setErrorMessage("no project found");

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().deleteProject("2525", "675", httpServletRequest);

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals("no project found", responseEntity.getBody().getErrorMessage());
        }

    }

    @Test
    void deleteProject_exception_Test(){

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                Mockito.when(projectServiceMock.deleteProject(any(),any(),any(),any())).thenThrow(new IllegalArgumentException("Invalid accountID."));
            })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            var expectedResponse = new GenericResponse();
            expectedResponse.setSuccess(false);
            expectedResponse.setErrorMessage("Invalid accountID.");

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().deleteProject("", "879", httpServletRequest);

            Assertions.assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
            Assertions.assertEquals(expectedResponse, responseEntity.getBody());
        }

    }

    @Test
    void deleteProject_validTest(){

        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("project", "newProject");

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = mockStatic(AnnotationHelper.class);
            MockedConstruction<ProjectService> mock = Mockito.mockConstruction(ProjectService.class, (projectServiceMock, context) -> {
                Mockito.when(projectServiceMock.deleteProject(any(),any(),any(),any())).thenReturn(responseMap);
            })){

            Contact contact = new Contact();
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            HttpServletRequest httpServletRequest = Mockito.mock(HttpServletRequest.class);

            var expectedResponse = new GenericResponse();
            expectedResponse.setSuccess(true);
            expectedResponse.setData(responseMap);

            ResponseEntity<GenericResponse> responseEntity = new ProjectApi().deleteProject("212", "432", httpServletRequest);

            Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            Assertions.assertEquals(expectedResponse, responseEntity.getBody());
        }

    }

}
