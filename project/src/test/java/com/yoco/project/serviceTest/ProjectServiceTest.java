package com.yoco.project.serviceTest;

import com.yoco.commons.dataservices.impl.ProjectImpl;
import com.yoco.commons.dataservices.impl.ProjectPeopleImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.entity.ProjectPeopleJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.project.enums.PROJECT_ERROR_RESPONSE;
import com.yoco.project.helper.ProjectHelper;
import com.yoco.project.helper.ProjectTaskHelper;
import com.yoco.project.service.ProjectService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

class ProjectServiceTest {

    ProjectService projectService =  ProjectService.getProjectService();

    @Test
    void createProject_empty_accountId_test(){
        try{
            projectService.createProject("",null,"","","");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void createProject_null_accountId_test(){
        try{
            projectService.createProject(null,null,"","","");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void createProject_empty_payload_test(){
        try{
            projectService.createProject("accountId",null,"","","");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void createProject_null_payload_test(){
        try{
            projectService.createProject("accountId",null,null,"","");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void createProject_null_contact_test(){
        try{
            Map<String,Object> payload = new HashMap<>();
            payload.put("name","project");
            projectService.createProject("accountId",null,JsonUtil.getJson(payload),"","");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void createProject_no_billable_key_test() throws IOException {
        try(MockedStatic<ProjectHelper> projectHelperMockedStatic = Mockito.mockStatic(ProjectHelper.class);
            MockedStatic<ProjectTaskHelper> projectTaskHelperMockedStatic = Mockito.mockStatic(ProjectTaskHelper.class)){

            ProjectJDO projectJDO = new ProjectJDO();

            ProjectHelper projectHelper = mock(ProjectHelper.class);
            Mockito.when(projectHelper.createProjectJDO(anyString(),anyString(),any(boolean.class),anyString(),any(double.class),anyString(),anyString())).thenReturn(projectJDO);
            Mockito.when(projectHelper.createProjectPeopleAssociation(any(ProjectJDO.class),any())).thenReturn(projectJDO);
            Mockito.doNothing().when(projectHelper).associateClientToProject(any(ProjectJDO.class),anyString());
            projectHelperMockedStatic.when(ProjectHelper::getProjectHelper).thenReturn(projectHelper);

            ProjectTaskHelper projectTaskHelper = mock(ProjectTaskHelper.class);
            Mockito.doNothing().when(projectTaskHelper).initiateProjectOperationsTaskQueue(anyMap());
            projectTaskHelperMockedStatic.when(ProjectTaskHelper::getProjectTaskHelper).thenReturn(projectTaskHelper);

            Map<String,Object> payload = new HashMap<>();
            payload.put("name","project");

            Contact contact = new Contact();
            Map<String,Object> response = projectService.createProject("accountId",contact, JsonUtil.getJson(payload),"web","");

            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(response));
            Mockito.verify(projectHelper).createProjectJDO("accountId","project",false,"",0.0,"","");
            Mockito.verify(projectHelper).createProjectPeopleAssociation(any(ProjectJDO.class),any());
            Mockito.verify(projectHelper).associateClientToProject(any(ProjectJDO.class),anyString());
            Mockito.verify(projectTaskHelper).initiateProjectOperationsTaskQueue(anyMap());
        }
    }

    @Test
    void createProject_valid_non_billable_test() throws IOException {
        try(MockedStatic<ProjectHelper> projectHelperMockedStatic = Mockito.mockStatic(ProjectHelper.class);
            MockedStatic<ProjectTaskHelper> projectTaskHelperMockedStatic = Mockito.mockStatic(ProjectTaskHelper.class)){

            ProjectJDO projectJDO = new ProjectJDO();

            ProjectHelper projectHelper = mock(ProjectHelper.class);
            Mockito.when(projectHelper.createProjectJDO(anyString(),anyString(),any(boolean.class),anyString(),any(double.class),anyString(),anyString())).thenReturn(projectJDO);
            Mockito.when(projectHelper.createProjectPeopleAssociation(any(ProjectJDO.class),any())).thenReturn(projectJDO);
            Mockito.doNothing().when(projectHelper).associateClientToProject(any(ProjectJDO.class),anyString());
            projectHelperMockedStatic.when(ProjectHelper::getProjectHelper).thenReturn(projectHelper);

            ProjectTaskHelper projectTaskHelper = mock(ProjectTaskHelper.class);
            Mockito.doNothing().when(projectTaskHelper).initiateProjectOperationsTaskQueue(anyMap());
            projectTaskHelperMockedStatic.when(ProjectTaskHelper::getProjectTaskHelper).thenReturn(projectTaskHelper);

            Map<String,Object> payload = new HashMap<>();
            payload.put("name","project");
            payload.put("billable",false);

            Contact contact = new Contact();
            Map<String,Object> response = projectService.createProject("accountId",contact, JsonUtil.getJson(payload),"web","");

            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(response));
            Mockito.verify(projectHelper).createProjectJDO("accountId","project",false,"",0.0,"","");
            Mockito.verify(projectHelper).createProjectPeopleAssociation(any(ProjectJDO.class),any());
            Mockito.verify(projectHelper).associateClientToProject(any(ProjectJDO.class),anyString());
            Mockito.verify(projectTaskHelper).initiateProjectOperationsTaskQueue(anyMap());
        }
    }

    @Test
    void createProject_valid_billable_test() throws IOException {
        try(MockedStatic<ProjectHelper> projectHelperMockedStatic = Mockito.mockStatic(ProjectHelper.class);
            MockedStatic<ProjectTaskHelper> projectTaskHelperMockedStatic = Mockito.mockStatic(ProjectTaskHelper.class)){

            ProjectJDO projectJDO = new ProjectJDO();

            ProjectHelper projectHelper = mock(ProjectHelper.class);
            Mockito.when(projectHelper.createProjectJDO(anyString(),anyString(),any(boolean.class),anyString(),any(double.class),anyString(),anyString())).thenReturn(projectJDO);
            Mockito.when(projectHelper.createProjectPeopleAssociation(any(ProjectJDO.class),any())).thenReturn(projectJDO);
            Mockito.doNothing().when(projectHelper).associateClientToProject(any(ProjectJDO.class),anyString());
            projectHelperMockedStatic.when(ProjectHelper::getProjectHelper).thenReturn(projectHelper);

            ProjectTaskHelper projectTaskHelper = mock(ProjectTaskHelper.class);
            Mockito.doNothing().when(projectTaskHelper).initiateProjectOperationsTaskQueue(anyMap());
            projectTaskHelperMockedStatic.when(ProjectTaskHelper::getProjectTaskHelper).thenReturn(projectTaskHelper);

            Map<String,Object> payload = new HashMap<>();
            payload.put("name","project");
            payload.put("billable",true);
            payload.put("currency","rupee");
            payload.put("rate","0.2");
            payload.put("plan","Hourly");
            payload.put("contacts","contact1,contact2");
            payload.put("clientID","clientId");

            Contact contact = new Contact();
            Map<String,Object> response = projectService.createProject("accountId",contact, JsonUtil.getJson(payload),"web","");

            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(response));
            Mockito.verify(projectHelper).createProjectJDO("accountId","project",true,"rupee",0.2,"Hourly","");
            Mockito.verify(projectHelper).createProjectPeopleAssociation(any(ProjectJDO.class),any());
            Mockito.verify(projectHelper).associateClientToProject(any(ProjectJDO.class),anyString());
            Mockito.verify(projectTaskHelper).initiateProjectOperationsTaskQueue(anyMap());
        }
    }

    @Test
    void enableProject_empty_accountId_test(){
        try{
            projectService.enableProject("","",null);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void enableProject_null_accountId_test(){
        try{
            projectService.enableProject(null,"",null);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void enableProject_empty_projectId_test(){
        try{
            projectService.enableProject("accountId","",null);
        }catch (Exception e){
            Assertions.assertEquals(PROJECT_ERROR_RESPONSE.INVALID_PROJECT_ID.value(),e.getMessage());
        }
    }

    @Test
    void enableProject_null_projectId_test(){
        try{
            projectService.enableProject("accountId",null,null);
        }catch (Exception e){
            Assertions.assertEquals(PROJECT_ERROR_RESPONSE.INVALID_PROJECT_ID.value(),e.getMessage());
        }
    }

    @Test
    void enableProject_null_contact_test(){
        try{
            projectService.enableProject("accountId","projectId",null);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void enableProject_null_project_test() throws IOException {
        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class);
            MockedStatic<ProjectTaskHelper> projectTaskHelperMockedStatic = Mockito.mockStatic(ProjectTaskHelper.class)){

            ProjectImpl projectImplMock = mock(ProjectImpl.class);
            Mockito.when(projectImplMock.getProject(anyString())).thenReturn(null);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(projectImplMock);

            ProjectTaskHelper projectTaskHelper = mock(ProjectTaskHelper.class);
            projectTaskHelperMockedStatic.when(ProjectTaskHelper::getProjectTaskHelper).thenReturn(projectTaskHelper);

            Contact contact = new Contact();
            Map<String,Object> response = projectService.enableProject("accountId","projectId",contact);

            Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
            Mockito.verify(projectImplMock).getProject("projectId");
            Mockito.verify(projectTaskHelper,times(0)).initiateProjectOperationsTaskQueue(anyMap());
        }
    }

    @Test
    void enableProject_valid_test() throws IOException {
        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class);
            MockedStatic<ProjectTaskHelper> projectTaskHelperMockedStatic = Mockito.mockStatic(ProjectTaskHelper.class)){

            ProjectJDO projectJDO = new ProjectJDO();
            projectJDO.setProjectId("projectId");

            ProjectImpl projectImplMock = mock(ProjectImpl.class);
            Mockito.when(projectImplMock.getProject(anyString())).thenReturn(projectJDO);
            Mockito.when(projectImplMock.saveProject(any(ProjectJDO.class))).thenReturn(projectJDO);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(projectImplMock);

            ProjectTaskHelper projectTaskHelper = mock(ProjectTaskHelper.class);
            Mockito.doNothing().when(projectTaskHelper).initiateProjectOperationsTaskQueue(anyMap());
            projectTaskHelperMockedStatic.when(ProjectTaskHelper::getProjectTaskHelper).thenReturn(projectTaskHelper);

            Contact contact = new Contact();
            Map<String,Object> response = projectService.enableProject("accountId","projectId",contact);

            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(response));
            Assertions.assertEquals(projectJDO,response.get("project"));
            Mockito.verify(projectImplMock).getProject("projectId");
            Mockito.verify(projectImplMock).saveProject(any(ProjectJDO.class));
            Mockito.verify(projectTaskHelper).initiateProjectOperationsTaskQueue(anyMap());
        }
    }

    @Test
    void getAllProjectDetailsOfAnAccount_nullAccountIDTest(){

        try{

            new ProjectService().getAllProjectDetailsOfAnAccount(null, "all", "cursor", 50);

        }catch (IllegalArgumentException illegalArgumentException){

            assertEquals("Invalid accountID.", illegalArgumentException.getMessage());
        }
    }

    @Test
    void getAllProjectDetailsOfAnAccount_emptyAccountIDTest(){

        try{

            new ProjectService().getAllProjectDetailsOfAnAccount("", "all", "cursor", 50);

        }catch (IllegalArgumentException illegalArgumentException){

            assertEquals("Invalid accountID.", illegalArgumentException.getMessage());
        }
    }

    @Test
    void getAllProjectDetailsOfAnAccount_activeProjectType_validTest(){

        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class);
            MockedStatic<ProjectHelper> projectHelperMockedStatic = Mockito.mockStatic(ProjectHelper.class)){

            List<String>listOfContacts = new ArrayList<>();
            listOfContacts.add(0, "123");

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setUniquepin("1234");
            projectObject.setProjectId("2343");
            projectObject.setProjectName("newProject");
            projectObject.setIsDefault(false);
            projectObject.setContacts(listOfContacts);

            List<ProjectJDO> projectList = new ArrayList<>();
            projectList.add(0, projectObject);

            Map<String, Object> listOfprojects = new HashMap<>();
            listOfprojects.put("projects", projectList);

            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("cursor", null);
            expectedMap.put("projects", projectList);

            ProjectImpl projectMock = mock(ProjectImpl.class);
            Mockito.when(projectMock.getAllProjectsInAccount(anyString(),anyString(),anyString(),anyInt())).thenReturn(listOfprojects);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(projectMock);

            ProjectHelper projectHelperMock = mock(ProjectHelper.class);
            Mockito.when(projectHelperMock.setProjectAssociatedContactToTheList(any(),any())).thenReturn(projectList);
            projectHelperMockedStatic.when(ProjectHelper::getProjectHelper).thenReturn(projectHelperMock);

            Map<String, Object>responseMap = new ProjectService().getAllProjectDetailsOfAnAccount("1234", "active", "cursor", 50);

            assertEquals(expectedMap,responseMap);

        }
    }

    @Test
    void getAllProjectDetailsOfAnAccount_inActiveProjectType_validTest(){

        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class);
            MockedStatic<ProjectHelper> projectHelperMockedStatic = Mockito.mockStatic(ProjectHelper.class)){

            List<String>listOfContacts = new ArrayList<>();
            listOfContacts.add(0, "123");

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setUniquepin("34");
            projectObject.setProjectId("43");
            projectObject.setProjectName("ProjectExperiment");
            projectObject.setIsDefault(false);
            projectObject.setContacts(listOfContacts);

            List<ProjectJDO> projectList = new ArrayList<>();
            projectList.add(0, projectObject);

            Map<String, Object> listOfprojects = new HashMap<>();
            listOfprojects.put("projects", projectList);

            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("cursor", null);
            expectedMap.put("projects", projectList);

            ProjectImpl projectMock = mock(ProjectImpl.class);
            Mockito.when(projectMock.getAllProjectsInAccount(anyString(),anyString(),anyString(),anyInt())).thenReturn(listOfprojects);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(projectMock);

            ProjectHelper projectHelperMock = mock(ProjectHelper.class);
            Mockito.when(projectHelperMock.setProjectAssociatedContactToTheList(any(),any())).thenReturn(projectList);
            projectHelperMockedStatic.when(ProjectHelper::getProjectHelper).thenReturn(projectHelperMock);

            Map<String, Object>responseMap = new ProjectService().getAllProjectDetailsOfAnAccount("34", "inactive", "cursor", 50);

            assertEquals(expectedMap,responseMap);

        }
    }

    @Test
    void getAllProjectDetailsOfAnAccount_validTest(){

        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class);
            MockedStatic<ProjectHelper> projectHelperMockedStatic = Mockito.mockStatic(ProjectHelper.class)){

            List<String>listOfContacts = new ArrayList<>();
            listOfContacts.add(0, "123");

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setUniquepin("653");
            projectObject.setProjectId("89");
            projectObject.setProjectName("ProjectExpt");
            projectObject.setIsDefault(false);
            projectObject.setContacts(listOfContacts);

            List<ProjectJDO> projectList = new ArrayList<>();
            projectList.add(0, projectObject);

            Map<String, Object> listOfprojects = new HashMap<>();
            listOfprojects.put("projects", projectList);

            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("cursor", null);
            expectedMap.put("projects", projectList);

            ProjectImpl projectMock = mock(ProjectImpl.class);
            Mockito.when(projectMock.getAllProjectsInAccount(anyString(),anyString(),anyString(),anyInt())).thenReturn(listOfprojects);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(projectMock);

            ProjectHelper projectHelperMock = mock(ProjectHelper.class);
            Mockito.when(projectHelperMock.setProjectAssociatedContactToTheList(any(),any())).thenReturn(projectList);
            projectHelperMockedStatic.when(ProjectHelper::getProjectHelper).thenReturn(projectHelperMock);

            Map<String, Object>responseMap = new ProjectService().getAllProjectDetailsOfAnAccount("653", "all", "cursor", 200);

            assertEquals(expectedMap,responseMap);

        }
    }

    @Test
    void getAllProjectsAssociatedToTheUser_nullContactIDTest(){

        try{
            new ProjectService().getAllProjectsAssociatedToTheUser("867", null, null, 1);
        }catch (IllegalArgumentException e){
            assertEquals("Invalid contactID.", e.getMessage());
        }

    }

    @Test
    void getAllProjectsAssociatedToTheUser_nullAccountIDTest(){

        try{
            new ProjectService().getAllProjectsAssociatedToTheUser(null, "376", null, 1);
        }catch (IllegalArgumentException e){
            assertEquals("Invalid accountID.", e.getMessage());
        }

    }

    @Test
    void getAllProjectsAssociatedToTheUser_emptyContactIDTest(){

        try{
            new ProjectService().getAllProjectsAssociatedToTheUser("49", null, null, 1);
        }catch (IllegalArgumentException e){
            assertEquals("Invalid contactID.", e.getMessage());
        }

    }

    @Test
    void getAllProjectsAssociatedToTheUser_emptyAccountIDTest(){

        try{
            new ProjectService().getAllProjectsAssociatedToTheUser("", "376", null, 1);
        }catch (IllegalArgumentException e){
            assertEquals("Invalid accountID.", e.getMessage());
        }

    }

    @Test
    void getAllProjectsAssociatedToTheUser_validTest(){

        try(MockedStatic<ProjectHelper> projectHelperMockedStatic = Mockito.mockStatic(ProjectHelper.class)){

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setUniquepin("005");
            projectObject.setProjectId("24");
            projectObject.setProjectName("ProjectTest");

            List<ProjectJDO> projectTestList = new ArrayList<>();
            projectTestList.add(0, projectObject);

            Map<String, Object> projectMap = new HashMap<>();
            projectMap.put("projects", projectTestList);

            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("projects", projectTestList);

            ProjectHelper projectHelperMock = mock(ProjectHelper.class);
            Mockito.when(projectHelperMock.getAllProjectsAndItsDetailsAssociatedToTheUser(any(),any(),any(), anyInt())).thenReturn(projectMap);
            projectHelperMockedStatic.when(ProjectHelper::getProjectHelper).thenReturn(projectHelperMock);

            Map<String,Object>responseMap = new ProjectService().getAllProjectsAssociatedToTheUser("005", "376", null, 1);

            assertEquals(expectedMap, responseMap);

        }

    }

    @Test
    void getAllProjectsAssociatedToTheUser_withLimit_validTest(){

        try(MockedStatic<ProjectHelper> projectHelperMockedStatic = Mockito.mockStatic(ProjectHelper.class)){

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setUniquepin("455");
            projectObject.setProjectId("24");
            projectObject.setProjectName("ProjectTest");

            List<ProjectJDO> projectTestList = new ArrayList<>();
            projectTestList.add(0, projectObject);
            projectTestList.add(1, projectObject);

            Map<String, Object> projectMap = new HashMap<>();
            projectMap.put("projects", projectTestList);
            projectMap.put("cursor", null);

            Map<String, Object>expectedMap = new HashMap<>();
            expectedMap.put("projects", projectTestList);
            expectedMap.put("cursor", null);

            ProjectHelper projectHelperMock = mock(ProjectHelper.class);
            Mockito.when(projectHelperMock.getAllProjectsAndItsDetailsAssociatedToTheUser(any(),any(),any(), anyInt())).thenReturn(projectMap);
            projectHelperMockedStatic.when(ProjectHelper::getProjectHelper).thenReturn(projectHelperMock);

            Map<String, Object>responseMap = new ProjectService().getAllProjectsAssociatedToTheUser("455", "903", null, 20);

            assertEquals(expectedMap,responseMap);

        }

    }

    @Test
    void deleteProject_nullProjectID_Test() throws IOException {

        try{
            Contact contact = new Contact();
            contact.setId("276");
            contact.setFirstName("new");

            ProjectService.getProjectService().deleteProject("876", null, contact, "2323");
        }catch(Exception e){
            assertEquals(PROJECT_ERROR_RESPONSE.INVALID_PROJECT_ID.value(), e.getMessage());
        }
    }

    @Test
    void deleteProject_emptyprojectID_Test() throws IOException {

        try{
            Contact contact = new Contact();
            contact.setId("732");
            contact.setLastName("experiment");

            ProjectService.getProjectService().deleteProject("876", "", contact, "2323");
        }catch(Exception e){
            assertEquals(PROJECT_ERROR_RESPONSE.INVALID_PROJECT_ID.value(), e.getMessage());
        }
    }

    @Test
    void deleteProject_nullAccountID_Test() throws IOException {

        try{
            Contact contact = new Contact();
            contact.setId("545");

            ProjectService.getProjectService().deleteProject(null, "454", contact, "2323");
        }catch(Exception e){
            assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(), e.getMessage());
        }
    }

    @Test
    void deleteProject_emptyAccountID_Test() throws IOException {

        try{
            Contact contact = new Contact();
            contact.setId("302");

            ProjectService.getProjectService().deleteProject("", "454", contact, "2323");
        }catch(Exception e){
            assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(), e.getMessage());
        }
    }

    @Test
    void deleteProject_validProjectObjectResponse_Test() throws IOException {

        ProjectJDO projectJDO = new ProjectJDO("123", "newProject", false, "INR", 2.00, "No plan", "454");

        try(MockedConstruction<ProjectImpl> projectmock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.getProject(anyString())).thenReturn(projectJDO);
        });
            MockedConstruction<ProjectPeopleImpl> projectPeopleMock = Mockito.mockConstruction(ProjectPeopleImpl.class,(projectPeopleImpl,context1) ->{
                Mockito.when(projectPeopleImpl.getProjectAssociationByProjectID(any(), any())).thenReturn(new ArrayList<>()); });
            MockedConstruction<ProjectHelper> projectHelperMock = Mockito.mockConstruction(ProjectHelper.class,(projectHelper,context2) ->{
                Mockito.when(projectHelper.setContactDetailsInsideProjectObject(any(), any())).thenReturn(projectJDO);
            });
            MockedConstruction<ProjectTaskHelper> projectTaskHelperMock = Mockito.mockConstruction(ProjectTaskHelper.class,(projectTaskHelper,context2) ->{ })){

            Contact contact = new Contact();
            contact.setId("234");
            contact.setFirstName("test");
            contact.setLastName("expt");

            Map<String, Object> responseMap = ProjectService.getProjectService().deleteProject("123", "454", contact, "2323");

            assertEquals(projectJDO, responseMap.get("project"));
        }
    }

    @Test
    void deleteProject_nullProjectObjectResponse_Test() throws IOException {

        List<String>contacts = new ArrayList<>();
        contacts.add("234");
        contacts.add("244");

        ProjectJDO projectJDO = null;

        try(MockedConstruction<ProjectImpl> projectmock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.getProject(anyString())).thenReturn(projectJDO);
        });
            MockedConstruction<ProjectPeopleImpl> projectPeopleMock = Mockito.mockConstruction(ProjectPeopleImpl.class,(projectPeopleImpl,context1) ->{
                Mockito.when(projectPeopleImpl.getProjectAssociationByProjectID(any(), any())).thenReturn(new ArrayList<>()); });
            MockedConstruction<ProjectHelper> projectHelperMock = Mockito.mockConstruction(ProjectHelper.class,(projectHelper,context2) ->{
                Mockito.when(projectHelper.setContactDetailsInsideProjectObject(any(), any())).thenReturn(projectJDO);
            });
            MockedConstruction<ProjectTaskHelper> projectTaskHelperMock = Mockito.mockConstruction(ProjectTaskHelper.class,(projectTaskHelper,context2) ->{ })){

            Contact contact = new Contact();
            contact.setId("234");
            contact.setFirstName("test");
            contact.setLastName("expt");

            Map<String, Object> responseMap = ProjectService.getProjectService().deleteProject("123", "454", contact, "2323");

            assertEquals(projectJDO, responseMap.get("project"));
        }
    }

    @Test
    void deleteProject_valid_Test() throws IOException {

        List<String> contacts = new ArrayList<>();
        contacts.add("234");
        contacts.add("244");

        ProjectJDO projectJDO = new ProjectJDO("123", "newProject", false, "INR", 2.00, "No plan", "454");
        ProjectPeopleJDO projectPeopleJDOOne = new ProjectPeopleJDO("123", "454", "234", "newProject");
        ProjectPeopleJDO projectPeopleJDOTwo = new ProjectPeopleJDO("123", "454", "244", "newProject");
        ProjectJDO projectObjectWithContacts = new ProjectJDO();
        projectObjectWithContacts.setProjectName("newProject");
        projectObjectWithContacts.setProjectId("454");
        projectObjectWithContacts.setUniquepin("123");
        projectObjectWithContacts.setContacts(contacts);

        List<ProjectPeopleJDO> projectPeopleList = new ArrayList<>();
        projectPeopleList.add(projectPeopleJDOOne);
        projectPeopleList.add(projectPeopleJDOTwo);

        try (MockedConstruction<ProjectImpl> projectmock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.getProject(anyString())).thenReturn(projectJDO);
        });
             MockedConstruction<ProjectPeopleImpl> projectPeopleMock = Mockito.mockConstruction(ProjectPeopleImpl.class, (projectPeopleImpl, context1) -> {
                 Mockito.when(projectPeopleImpl.getProjectAssociationByProjectID(any(), any())).thenReturn(projectPeopleList);
             });
             MockedConstruction<ProjectHelper> projectHelperMock = Mockito.mockConstruction(ProjectHelper.class, (projectHelper, context2) -> {
                 Mockito.when(projectHelper.setContactDetailsInsideProjectObject(any(), any())).thenReturn(projectObjectWithContacts);
             });
             MockedConstruction<ProjectTaskHelper> projectTaskHelperMock = Mockito.mockConstruction(ProjectTaskHelper.class, (projectTaskHelper, context2) -> {
             })) {

            Contact contact = new Contact();
            contact.setId("234");
            contact.setFirstName("test");
            contact.setLastName("expt");

            Map<String, Object> responseMap = ProjectService.getProjectService().deleteProject("123", "454", contact, "2323");

            assertEquals(projectObjectWithContacts, responseMap.get("project"));
        }
    }

    @Test
    void updateProject_nullPayload_Test() throws IOException {

        try{
            ProjectService.getProjectService().updateProject("764", null, "2323", null);
        }catch(Exception e){
            assertEquals(PROJECT_ERROR_RESPONSE.INVALID_PROJECT_ID.value(), e.getMessage());
        }
    }

    @Test
    void updateProject_emptyPayload_Test() throws IOException {

        try{
            ProjectService.getProjectService().updateProject("876", "", "23","");
        }catch(Exception e){
            assertEquals(PROJECT_ERROR_RESPONSE.INVALID_PROJECT_ID.value(), e.getMessage());
        }
    }

    @Test
    void updateProject_nullProjectID_Test() throws IOException {

        try{
            ProjectService.getProjectService().updateProject("764", null, "2323", "payload");
        }catch(Exception e){
            assertEquals(PROJECT_ERROR_RESPONSE.INVALID_PROJECT_ID.value(), e.getMessage());
        }
    }

    @Test
    void updateProject_emptyprojectID_Test() throws IOException {

        try{
            ProjectService.getProjectService().updateProject("876", "", "23","payload");
        }catch(Exception e){
            assertEquals(PROJECT_ERROR_RESPONSE.INVALID_PROJECT_ID.value(), e.getMessage());
        }
    }

    @Test
    void updateProject_nullAccountID_Test() throws IOException {

        try{
            ProjectService.getProjectService().updateProject(null, "454", "2979","payload");
        }catch(Exception e){
            assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(), e.getMessage());
        }
    }

    @Test
    void updateProject_emptyAccountID_Test() throws IOException {

        try{
            ProjectService.getProjectService().updateProject("", "975", "2323","payload");
        }catch(Exception e){
            assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(), e.getMessage());
        }
    }

    @Test
    void update_projectNameChange_Test() throws IOException {

        List<String>contacts = new ArrayList<>();
        contacts.add("234");
        contacts.add("244");

        ProjectJDO projectJDO = new ProjectJDO("65", "newProject", false, "INR" , 2.00, "No plan", "454");
        ProjectPeopleJDO projectPeopleJDOOne = new ProjectPeopleJDO("65", "3454", "234", "newProject");
        ProjectPeopleJDO projectPeopleJDOTwo = new ProjectPeopleJDO("65", "3454", "244", "newProject");
        ProjectJDO projectObjectWithContacts = new ProjectJDO();
        projectObjectWithContacts.setProjectName("newProject");
        projectObjectWithContacts.setProjectId("3454");
        projectObjectWithContacts.setUniquepin("65");
        projectObjectWithContacts.setContacts(contacts);

        ProjectJDO updatedProject = new ProjectJDO();
        updatedProject.setUniquepin("65");
        updatedProject.setProjectId("3454");
        updatedProject.setProjectName("test Project");
        updatedProject.setContacts(contacts);

        List<ProjectPeopleJDO> projectPeopleList = new ArrayList<>();
        projectPeopleList.add(projectPeopleJDOOne);
        projectPeopleList.add(projectPeopleJDOTwo);

        try(MockedConstruction<ProjectImpl> projectmock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.getProject(anyString())).thenReturn(projectJDO);
        });
            MockedConstruction<ProjectPeopleImpl> projectPeopleMock = Mockito.mockConstruction(ProjectPeopleImpl.class,(projectPeopleImpl,context1) ->{
                Mockito.when(projectPeopleImpl.getProjectAssociationByProjectID(any(), any())).thenReturn(projectPeopleList); });
            MockedConstruction<ProjectHelper> projectHelperMock = Mockito.mockConstruction(ProjectHelper.class,(projectHelper,context2) ->{
                Mockito.when(projectHelper.setContactDetailsInsideProjectObject(any(), any())).thenReturn( projectObjectWithContacts);
                Mockito.when(projectHelper.updateProjectAndItsAssociatedDetails(any(), any(), any(), any())).thenReturn(updatedProject );
            });

            MockedConstruction<ProjectTaskHelper> projectTaskHelperMock = Mockito.mockConstruction(ProjectTaskHelper.class,(projectTaskHelper,context2) ->{ })){

            String payload = "{\"payload\":\"project\"}";

            Map<String, Object> responseMap = ProjectService.getProjectService().updateProject("65","3454","7524", payload);

            assertEquals(updatedProject, responseMap.get("project"));
        }
    }

    @Test
    void update_valid_Test() throws IOException {

        List<String>contacts = new ArrayList<>();
        contacts.add("234");
        contacts.add("244");

        ProjectJDO projectJDO = new ProjectJDO("123", "newProject", false, "INR" , 2.00, "No plan", "454");
        ProjectPeopleJDO projectPeopleJDOOne = new ProjectPeopleJDO("123", "2354", "234", "newProject");
        ProjectPeopleJDO projectPeopleJDOTwo = new ProjectPeopleJDO("123", "2354", "244", "newProject");
        ProjectJDO projectObjectWithContacts = new ProjectJDO();
        projectObjectWithContacts.setProjectName("newProject");
        projectObjectWithContacts.setProjectId("2354");
        projectObjectWithContacts.setUniquepin("6565");
        projectObjectWithContacts.setContacts(contacts);

        List<ProjectPeopleJDO> projectPeopleList = new ArrayList<>();
        projectPeopleList.add(projectPeopleJDOOne);
        projectPeopleList.add(projectPeopleJDOTwo);

        try(MockedConstruction<ProjectImpl> projectmock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.getProject(anyString())).thenReturn(projectJDO);
        });
            MockedConstruction<ProjectPeopleImpl> projectPeopleMock = Mockito.mockConstruction(ProjectPeopleImpl.class,(projectPeopleImpl,context1) ->{
                Mockito.when(projectPeopleImpl.getProjectAssociationByProjectID(any(), any())).thenReturn(projectPeopleList); });
            MockedConstruction<ProjectHelper> projectHelperMock = Mockito.mockConstruction(ProjectHelper.class,(projectHelper,context2) ->{
                Mockito.when(projectHelper.setContactDetailsInsideProjectObject(any(), any())).thenReturn( projectObjectWithContacts);
                Mockito.when(projectHelper.updateProjectAndItsAssociatedDetails(any(), any(), any(), any())).thenReturn( projectObjectWithContacts);
            });

            MockedConstruction<ProjectTaskHelper> projectTaskHelperMock = Mockito.mockConstruction(ProjectTaskHelper.class,(projectTaskHelper,context2) ->{ })){

            List<String>addedList = new ArrayList<>();
            addedList.add("123");
            addedList.add("876");
            List<String>deletedList = new ArrayList<>();
            deletedList.add("1");
            deletedList.add("6");
            Map<String, Object>listMap = new HashMap<>();
            listMap.put("addedContacts",addedList);
            listMap.put("deletedContacts", deletedList);
            String payload = JsonUtil.getJson(listMap);

            Map<String, Object> responseMap = ProjectService.getProjectService().updateProject("6565","2354","7524", payload);

            assertEquals(projectObjectWithContacts, responseMap.get("project"));
        }
    }
}


