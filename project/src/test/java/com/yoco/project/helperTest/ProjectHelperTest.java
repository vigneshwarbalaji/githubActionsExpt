package com.yoco.project.helperTest;

import com.yoco.client.helper.ClientHelper;
import com.yoco.client.service.ClientService;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.*;
import com.yoco.commons.entity.*;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.utils.events.ClockUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.project.helper.ProjectFullMetricHelper;
import com.yoco.project.helper.ProjectHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.*;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

class ProjectHelperTest {

    ProjectHelper projectHelper = ProjectHelper.getProjectHelper();

    @Test
    void createProjectJDO_test(){

        try(MockedStatic<ProjectImpl> projectMockedStatic = mockStatic(ProjectImpl.class)){

            var projectJDO = new ProjectJDO("accountId","name",false,"currency",0.0,"plan","projectId");

            ProjectImpl projectImplMock = mock(ProjectImpl.class);
            Mockito.when(projectImplMock.saveProject(any(ProjectJDO.class))).thenReturn(projectJDO);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(projectImplMock);

            ProjectJDO response =  projectHelper.createProjectJDO("accountId","name",false,"currency",0.0,"plan","projectId");

            Assertions.assertEquals(response,projectJDO);
            Mockito.verify(projectImplMock).saveProject(any(ProjectJDO.class));
        }
    }

    @Test
    void createProjectPeopleJDO_test(){

        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = mockStatic(ProjectPeopleImpl.class)){

            var projectJDO = new ProjectJDO("accountId","name",false,"currency",0.0,"plan","projectId");
            var projectPeopleJDO = new ProjectPeopleJDO(projectJDO.getUniquepin(),projectJDO.getProjectId(),"contactId",projectJDO.getProjectName());
            List<ProjectPeopleJDO>projectPeopleList = new ArrayList<>();
            projectPeopleList.add(projectPeopleJDO);

            ProjectPeopleImpl projectPeopleMock = mock(ProjectPeopleImpl.class);
            Mockito.when(projectPeopleMock.saveProjectAssociations(anyList())).thenReturn(projectPeopleList);
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectPeopleMock);

            List<ProjectPeopleJDO> response =  projectHelper.createProjectPeopleJDO(projectJDO,"contactId");

            Assertions.assertEquals(response,projectPeopleList);
            Mockito.verify(projectPeopleMock).saveProjectAssociations(anyList());
        }
    }

    @Test
    void associateClientToProject_valid_test(){
        try(MockedStatic<ClientImpl> clientMockedStatic = mockStatic(ClientImpl.class);
            MockedStatic<ClientHelper> clientHelperMockedStatic = mockStatic(ClientHelper.class)){

            var projectJDO = new ProjectJDO("accountId","name",false,"currency",0.0,"plan","projectId");

            Client client = new Client();
            ClientImpl clientImplMock = mock(ClientImpl.class);
            Mockito.when(clientImplMock.getByID(anyString())).thenReturn(client);

            ClientHelper clientHelperMock = mock(ClientHelper.class);
            Mockito.when(clientHelperMock.updateClientProjects(anyMap(),any(Client.class))).thenReturn(true);
            clientHelperMockedStatic.when(ClientHelper::getClientHelper).thenReturn(clientHelperMock);

            Mockito.when(clientImplMock.saveClient(any(Client.class))).thenReturn(client);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientImplMock);

            projectHelper.associateClientToProject(projectJDO,"clientId");

            Mockito.verify(clientImplMock).getByID("clientId");
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.PROJECTS.value(),projectJDO.getProjectId());
            Mockito.verify(clientHelperMock).updateClientProjects(payloadMap,client);
            Mockito.verify(clientImplMock).saveClient(client);
        }


    }

    @Test
    void associateClientToProject_false_test(){
        try(MockedStatic<ClientImpl> clientMockedStatic = mockStatic(ClientImpl.class);
            MockedStatic<ClientHelper> clientHelperMockedStatic = mockStatic(ClientHelper.class)){
            var projectJDO = new ProjectJDO("accountId","name",false,"currency",0.0,"plan","projectId");

            Client client = new Client();
            ClientImpl clientImplMock = mock(ClientImpl.class);
            Mockito.when(clientImplMock.getByID(anyString())).thenReturn(client);

            ClientHelper clientHelperMock = mock(ClientHelper.class);
            Mockito.when(clientHelperMock.updateClientProjects(anyMap(),any(Client.class))).thenReturn(false);
            clientHelperMockedStatic.when(ClientHelper::getClientHelper).thenReturn(clientHelperMock);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientImplMock);

            projectHelper.associateClientToProject(projectJDO,"clientId");

            Mockito.verify(clientImplMock).getByID("clientId");
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.PROJECTS.value(),projectJDO.getProjectId());
            Mockito.verify(clientHelperMock).updateClientProjects(payloadMap,client);
        }

    }

    @Test
    void associateClientToProject_empty_clientObj_test(){

        try(MockedStatic<ClientImpl> clientMockedStatic = mockStatic(ClientImpl.class);
            MockedStatic<ClientHelper> clientHelperMockedStatic = mockStatic(ClientHelper.class)){

            var projectJDO = new ProjectJDO("accountId","name",false,"currency",0.0,"plan","projectId");

            Client client = new Client();
            ClientImpl clientImplMock = mock(ClientImpl.class);
            Mockito.when(clientImplMock.getByID(anyString())).thenReturn(null);

            ClientHelper clientHelperMock = mock(ClientHelper.class);
            clientHelperMockedStatic.when(ClientHelper::getClientHelper).thenReturn(clientHelperMock);

            Mockito.when(clientImplMock.saveClient(any(Client.class))).thenReturn(client);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientImplMock);

            projectHelper.associateClientToProject(projectJDO,"clientId");

            Mockito.verify(clientImplMock).getByID("clientId");
            Mockito.verifyNoInteractions(clientHelperMock);
        }

    }

    @Test
    void associateClientToProject_empty_clientId_test(){
        try(MockedStatic<ClientImpl> clientMockedStatic = mockStatic(ClientImpl.class)){
            var projectJDO = new ProjectJDO("accountId","name",false,"currency",0.0,"plan","projectId");

            ClientImpl clientImplMock = mock(ClientImpl.class);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientImplMock);

            projectHelper.associateClientToProject(projectJDO,"");
            Mockito.verifyNoInteractions(clientImplMock);
        }
    }

    @Test
    void createProjectPeopleAssociation_valid_test(){
        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = mockStatic(ProjectPeopleImpl.class)){
            var projectJDO = new ProjectJDO("accountId","name",false,"currency",0.0,"plan","projectId");
            var projectPeopleJDO = new ProjectPeopleJDO(projectJDO.getUniquepin(),projectJDO.getProjectId(),"contactId",projectJDO.getProjectName());
            List<ProjectPeopleJDO>projectPeopleList = new ArrayList<>();
            projectPeopleList.add(projectPeopleJDO);

            ProjectPeopleImpl projectPeopleMock = mock(ProjectPeopleImpl.class);
            Mockito.when(projectPeopleMock.getProjectsAssociated(anyString(),anyString(),anyString())).thenReturn(null);
            Mockito.when(projectPeopleMock.saveProjectAssociations(anyList())).thenReturn(projectPeopleList);
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectPeopleMock);

            String[] staffIds = "contactId,".split(",");
            ProjectJDO response = projectHelper.createProjectPeopleAssociation(projectJDO,staffIds);

            List<String> contacts =  new ArrayList<>();
            contacts.add("contactId");
            Assertions.assertEquals(contacts,response.getContacts());
            Mockito.verify(projectPeopleMock).getProjectsAssociated(projectJDO.getUniquepin(),"contactId",projectJDO.getProjectId());
            Mockito.verify(projectPeopleMock).saveProjectAssociations(anyList());
        }
    }

    @Test
    void createProjectPeopleAssociation_association_exists_test(){
        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = mockStatic(ProjectPeopleImpl.class)){

            var projectJDO = new ProjectJDO("accountId","name",false,"currency",0.0,"plan","projectId");
            var projectPeopleJDO = new ProjectPeopleJDO(projectJDO.getUniquepin(),projectJDO.getProjectId(),"contactId",projectJDO.getProjectName());

            ProjectPeopleImpl projectPeopleMock = mock(ProjectPeopleImpl.class);
            Mockito.when(projectPeopleMock.getProjectsAssociated(anyString(),anyString(),anyString())).thenReturn(projectPeopleJDO);
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectPeopleMock);

            String[] staffIds = "contactId, ".split(",");
            ProjectJDO response = projectHelper.createProjectPeopleAssociation(projectJDO,staffIds);

            Assertions.assertTrue(ObjUtils.isEmptyList(response.getContacts()));
            Mockito.verify(projectPeopleMock).getProjectsAssociated(projectJDO.getUniquepin(),"contactId",projectJDO.getProjectId());
        }
    }

    @Test
    void createProjectPeopleAssociation_no_staff_test(){
        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = mockStatic(ProjectPeopleImpl.class)){

            var projectJDO = new ProjectJDO("accountId","name",false,"currency",0.0,"plan","projectId");

            ProjectPeopleImpl projectPeopleMock = mock(ProjectPeopleImpl.class);
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectPeopleMock);

            String[] staffIds = new String[0];
            ProjectJDO response = projectHelper.createProjectPeopleAssociation(projectJDO,staffIds);

            Assertions.assertTrue(ObjUtils.isEmptyList(response.getContacts()));
            Mockito.verifyNoInteractions(projectPeopleMock);
        }
    }

    @Test
    void setProjectAssociatedContactToTheList_emptyProjectPeopleResponseTest(){

        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = mockStatic(ProjectPeopleImpl.class)){

            ProjectPeopleImpl projectpeopleMock = mock(ProjectPeopleImpl.class);
            Mockito.when(projectpeopleMock.getProjectAssociationByProjectID(anyString(),anyString())).thenReturn(new ArrayList<>());
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectpeopleMock);

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setUniquepin("123");
            projectObject.setProjectId("454");
            projectObject.setProjectName("exptProject");

            List<ProjectJDO> projectList = new ArrayList<>();
            projectList.add(0, projectObject);

            List<ProjectJDO> expectedList = new ArrayList<>();
            expectedList.add(0, projectObject);

            List<ProjectJDO>responseList = projectHelper.setProjectAssociatedContactToTheList("123", projectList);

            Assertions.assertEquals(expectedList, responseList);

        }

    }

    @Test
    void setProjectAssociatedContactToTheList_singleContactIDvalidTest(){

        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = mockStatic(ProjectPeopleImpl.class)){

            ProjectPeopleJDO projectPeopleObject1 = new ProjectPeopleJDO();
            projectPeopleObject1.setUniquepin("123");
            projectPeopleObject1.setProjectName("exptProject");
            projectPeopleObject1.setProjectId("454");
            projectPeopleObject1.setPeopleId("34");

            List<ProjectPeopleJDO> projectPeopleList = new ArrayList<>();
            projectPeopleList.add(0, projectPeopleObject1);

            ProjectJDO projectObject1 = new ProjectJDO();
            projectObject1.setUniquepin("123");
            projectObject1.setProjectId("454");
            projectObject1.setProjectName("exptProject");

            List<ProjectJDO> projectList = new ArrayList<>();
            projectList.add(0, projectObject1);

            List<String> contactList = new ArrayList<>();
            contactList.add(0, "34");

            ProjectJDO expectedProjectObject1 = new ProjectJDO();
            expectedProjectObject1.setUniquepin("123");
            expectedProjectObject1.setProjectId("454");
            expectedProjectObject1.setProjectName("exptProject");
            expectedProjectObject1.setContacts(contactList);

            List<ProjectJDO> expectedList = new ArrayList<>();
            expectedList.add(0, expectedProjectObject1);

            ProjectPeopleImpl projectpeopleMock = mock(ProjectPeopleImpl.class);
            Mockito.when(projectpeopleMock.getProjectAssociationByProjectID(anyString(),anyString())).thenReturn(projectPeopleList);
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectpeopleMock);

            List<ProjectJDO>responseList = projectHelper.setProjectAssociatedContactToTheList("123", projectList);

            Assertions.assertEquals(expectedList,responseList);
        }

    }

    @Test
    void setProjectAssociatedContactToTheList_multiplePeopleIDValidTest(){

        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = mockStatic(ProjectPeopleImpl.class)){

            ProjectPeopleJDO projectPeopleObject1 = new ProjectPeopleJDO();
            projectPeopleObject1.setUniquepin("123");
            projectPeopleObject1.setProjectName("exptProject");
            projectPeopleObject1.setProjectId("454");
            projectPeopleObject1.setPeopleId("34");

            ProjectPeopleJDO projectPeopleObject2 = new ProjectPeopleJDO();
            projectPeopleObject2.setUniquepin("123");
            projectPeopleObject2.setProjectName("exptProject");
            projectPeopleObject2.setProjectId("454");
            projectPeopleObject2.setPeopleId("23");

            List<ProjectPeopleJDO> projectPeopleList = new ArrayList<>();
            projectPeopleList.add(0, projectPeopleObject1);
            projectPeopleList.add(1, projectPeopleObject2);

            ProjectJDO projectObject1 = new ProjectJDO();
            projectObject1.setUniquepin("123");
            projectObject1.setProjectId("454");
            projectObject1.setProjectName("exptProject");

            List<ProjectJDO> projectList = new ArrayList<>();
            projectList.add(0, projectObject1);

            List<String> contactList = new ArrayList<>();
            contactList.add(0, "34");
            contactList.add(1, "23");

            ProjectJDO expectedProjectObject1 = new ProjectJDO();
            expectedProjectObject1.setUniquepin("123");
            expectedProjectObject1.setProjectId("454");
            expectedProjectObject1.setProjectName("exptProject");
            expectedProjectObject1.setContacts(contactList);

            List<ProjectJDO> expectedList = new ArrayList<>();
            expectedList.add(0, expectedProjectObject1);

            ProjectPeopleImpl projectpeopleMock = mock(ProjectPeopleImpl.class);
            Mockito.when(projectpeopleMock.getProjectAssociationByProjectID(anyString(),anyString())).thenReturn(projectPeopleList);
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectpeopleMock);

            List<ProjectJDO>responseList = projectHelper.setProjectAssociatedContactToTheList("123", projectList);

            Assertions.assertEquals(expectedList,responseList);
        }

    }

    @Test
    void setContactDetailsInsideProjectObject_ifProjectPeopleListIsEmpty_validTest(){

        ProjectJDO projectObject = new ProjectJDO();
        projectObject.setUniquepin("545");
        projectObject.setProjectId("3432");
        projectObject.setProjectName("Project");

        List<ProjectPeopleJDO>projectPeopleList = new ArrayList<>();

        ProjectJDO expectedObject = new ProjectJDO();
        expectedObject.setUniquepin("545");
        expectedObject.setProjectId("3432");
        expectedObject.setProjectName("Project");
        expectedObject.setContacts(new ArrayList<>());

        ProjectJDO responseObject = projectHelper.setContactDetailsInsideProjectObject(projectObject,projectPeopleList);

        Assertions.assertEquals(expectedObject, responseObject);

    }

    @Test
    void setContactDetailsInsideProjectObject_emptyProjectPeopleListAsInput_Test(){

        ProjectJDO projectObject = new ProjectJDO();
        projectObject.setUniquepin("123");
        projectObject.setProjectId("454");
        projectObject.setProjectName("exptProject");

        ProjectJDO expectedObject = projectObject;

        ProjectJDO responseObject = projectHelper.setContactDetailsInsideProjectObject(projectObject,new ArrayList<>());

        Assertions.assertEquals(expectedObject, responseObject);

    }

    @Test
    void setContactDetailsInsideProjectObject_oneContactAssociatedProject_validTest(){

        ProjectJDO projectObject = new ProjectJDO();
        projectObject.setUniquepin("123");
        projectObject.setProjectId("454");
        projectObject.setProjectName("exptProject");

        List<String>contactList = new ArrayList<>();
        contactList.add(0, "34");

        ProjectJDO expectedObject = new ProjectJDO();
        expectedObject.setUniquepin("123");
        expectedObject.setProjectId("454");
        expectedObject.setContacts(contactList);
        expectedObject.setProjectName("exptProject");

        ProjectPeopleJDO projectPeopleObject = new ProjectPeopleJDO();
        projectPeopleObject.setUniquepin("123");
        projectPeopleObject.setProjectName("exptProject");
        projectPeopleObject.setProjectId("454");
        projectPeopleObject.setPeopleId("34");

        List<ProjectPeopleJDO> projectPeopleList = new ArrayList<>();
        projectPeopleList.add(0, projectPeopleObject);

        ProjectJDO responseObject = projectHelper.setContactDetailsInsideProjectObject(projectObject,projectPeopleList);

        Assertions.assertEquals(expectedObject, responseObject);

    }

    @Test
    void setContactDetailsInsideProjectObject_ifTypeIsActive_validTest(){

        ProjectJDO projectObject = new ProjectJDO();
        projectObject.setUniquepin("73");
        projectObject.setProjectId("987");
        projectObject.setProjectName("Project");

        ProjectPeopleJDO projectPeopleObject1 = new ProjectPeopleJDO();
        projectPeopleObject1.setUniquepin("73");
        projectPeopleObject1.setProjectName("Project");
        projectPeopleObject1.setProjectId("987");
        projectPeopleObject1.setPeopleId("343");
        projectPeopleObject1.setIsDeleted(false);

        ProjectPeopleJDO projectPeopleObject2 = new ProjectPeopleJDO();
        projectPeopleObject2.setUniquepin("73");
        projectPeopleObject2.setProjectName("Project");
        projectPeopleObject2.setProjectId("987");
        projectPeopleObject2.setPeopleId("43");
        projectPeopleObject2.setIsDeleted(false);

        List<ProjectPeopleJDO>projectPeopleList = new ArrayList<>();
        projectPeopleList.add(projectPeopleObject1);
        projectPeopleList.add(projectPeopleObject2);

        List<String>contactList = new ArrayList<>();
        contactList.add("343");
        contactList.add("43");

        ProjectJDO expectedObject = new ProjectJDO();
        expectedObject.setUniquepin("73");
        expectedObject.setProjectId("987");
        expectedObject.setProjectName("Project");
        expectedObject.setContacts(contactList);

        ProjectJDO responseObject = projectHelper.setContactDetailsInsideProjectObject(projectObject,projectPeopleList);

        Assertions.assertEquals(expectedObject, responseObject);

    }

    @Test
    void setContactDetailsInsideProjectObject_multipleContactAssociatedProject_validTest(){

        ProjectJDO projectObject = new ProjectJDO();
        projectObject.setUniquepin("123");
        projectObject.setProjectId("454");
        projectObject.setProjectName("exptProject");

        List<String>contactList = new ArrayList<>();
        contactList.add(0, "34");
        contactList.add(1, "4343");

        ProjectJDO expectedObject = new ProjectJDO();
        expectedObject.setUniquepin("123");
        expectedObject.setProjectId("454");
        expectedObject.setContacts(contactList);
        expectedObject.setProjectName("exptProject");

        ProjectPeopleJDO projectPeopleObject1 = new ProjectPeopleJDO();
        projectPeopleObject1.setUniquepin("123");
        projectPeopleObject1.setProjectName("exptProject");
        projectPeopleObject1.setProjectId("454");
        projectPeopleObject1.setPeopleId("34");

        ProjectPeopleJDO projectPeopleObject2 = new ProjectPeopleJDO();
        projectPeopleObject2.setUniquepin("123");
        projectPeopleObject2.setProjectName("exptProject");
        projectPeopleObject2.setProjectId("454");
        projectPeopleObject2.setPeopleId("4343");

        List<ProjectPeopleJDO> projectPeopleList = new ArrayList<>();
        projectPeopleList.add(0, projectPeopleObject1);
        projectPeopleList.add(1, projectPeopleObject2);

        ProjectJDO responseObject = projectHelper.setContactDetailsInsideProjectObject(projectObject,projectPeopleList);

        Assertions.assertEquals(expectedObject, responseObject);

    }

    @Test
    void getAllProjectsAndItsDetailsAssociatedToTheUserWithoutCursor_validTest(){

        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = mockStatic(ProjectPeopleImpl.class);
            MockedStatic<ProjectImpl> projectMockedStatic = mockStatic(ProjectImpl.class)){

            ProjectPeopleJDO projectPeopleObject1 = new ProjectPeopleJDO();
            projectPeopleObject1.setUniquepin("324");
            projectPeopleObject1.setProjectName("expt");
            projectPeopleObject1.setProjectId("4");
            projectPeopleObject1.setPeopleId("4");

            ProjectPeopleJDO projectPeopleObject2 = new ProjectPeopleJDO();
            projectPeopleObject2.setUniquepin("324");
            projectPeopleObject2.setProjectName("newproject");
            projectPeopleObject2.setProjectId("3");
            projectPeopleObject2.setPeopleId("4356");

            List<ProjectPeopleJDO> projectPeopleList = new ArrayList<>();
            projectPeopleList.add(projectPeopleObject1);
            projectPeopleList.add(projectPeopleObject2);

            CollectionResponse<ProjectPeopleJDO> projectPeopleJDOList = new CollectionResponse<>();
            projectPeopleJDOList.setItems(projectPeopleList);

            ProjectPeopleImpl projectpeopleMock = mock(ProjectPeopleImpl.class);
            Mockito.when(projectpeopleMock.getProjectsAssociatedToUser(any(),any(),any(),anyInt())).thenReturn(projectPeopleJDOList);
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectpeopleMock);

            ProjectJDO projectObject1 = new ProjectJDO();
            projectObject1.setUniquepin("324");
            projectObject1.setProjectName("expt");
            projectObject1.setProjectId("4");

            ProjectJDO projectObject2 = new ProjectJDO();
            projectObject2.setUniquepin("324");
            projectObject2.setProjectName("newproject");
            projectObject2.setProjectId("3");

            List<ProjectJDO> projectList = new ArrayList<>();
            projectList.add(0, projectObject1);
            projectList.add(1, projectObject2);

            ProjectImpl projectMock = mock(ProjectImpl.class);
            Mockito.when(projectMock.get(any(), anyIterable())).thenReturn(Collections.singletonList(projectList));
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(projectMock);

            Map<String, Object> expectedMap = new HashMap<>();
            expectedMap.put("projects", Collections.singletonList(projectList));

            Map<String, Object>responseMap = projectHelper.getAllProjectsAndItsDetailsAssociatedToTheUser("125", "579", null, 1);

            Assertions.assertEquals(expectedMap.get("projects"), responseMap.get("projects"));

        }

    }

    @Test
    void getAllProjectsAndItsDetailsAssociatedToTheUser_validTest(){

        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = mockStatic(ProjectPeopleImpl.class);
            MockedStatic<ProjectImpl> projectMockedStatic = mockStatic(ProjectImpl.class)){

            ProjectPeopleJDO projectPeopleObject1 = new ProjectPeopleJDO();
            projectPeopleObject1.setUniquepin("123");
            projectPeopleObject1.setProjectName("exptProject");
            projectPeopleObject1.setProjectId("454");
            projectPeopleObject1.setPeopleId("34");

            List<ProjectPeopleJDO> projectPeopleList = new ArrayList<>();
            projectPeopleList.add(projectPeopleObject1);

            CollectionResponse<ProjectPeopleJDO> projectPeopleJDOList = new CollectionResponse<>();
            projectPeopleJDOList.setItems(projectPeopleList);
            projectPeopleJDOList.setCursor("newCursor");

            ProjectPeopleImpl projectpeopleMock = mock(ProjectPeopleImpl.class);
            Mockito.when(projectpeopleMock.getProjectsAssociatedToUser(any(),any(),any(),anyInt())).thenReturn(projectPeopleJDOList);
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectpeopleMock);

            ProjectJDO projectObject1 = new ProjectJDO();
            projectObject1.setUniquepin("123");
            projectObject1.setProjectName("exptProject");
            projectObject1.setProjectId("454");

            ProjectJDO projectObject2 = new ProjectJDO();
            projectObject2.setUniquepin("123");
            projectObject2.setProjectName("Project");
            projectObject2.setProjectId("427");

            List<ProjectJDO> projectList = new ArrayList<>();
            projectList.add(0, projectObject1);
            projectList.add(1, projectObject2);

            ProjectImpl projectMock = mock(ProjectImpl.class);
            Mockito.when(projectMock.get(any(), anyIterable())).thenReturn(Collections.singletonList(projectList));
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(projectMock);

            Map<String, Object> expectedMap = new HashMap<>();
            expectedMap.put("projects", Collections.singletonList(projectList));
            expectedMap.put("projectPeopleList", projectPeopleList);
            expectedMap.put("cursor", "newCursor");

            Map<String, Object>responseMap = projectHelper.getAllProjectsAndItsDetailsAssociatedToTheUser("125", "579", null, 1);

            Assertions.assertEquals(expectedMap, responseMap);

        }

    }

    @Test
    void updateProjectAndItsAssociatedDetails_withoutAnyPayload_Test(){

        ProjectJDO proObject = new ProjectJDO();
        proObject.setProjectId("692");
        proObject.setProjectName("exptProject");
        proObject.setUniquepin("123");

        try(MockedConstruction<ProjectImpl> mock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.saveProject(any())).thenReturn(proObject);
        })){


            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setProjectId("692");
            projectObject.setUniquepin("123");
            projectObject.setProjectName("exptProject");

            Map<String, Object>payloadMap = new HashMap<>();

            ProjectJDO expectedObject = new ProjectJDO();
            expectedObject.setProjectId("692");
            expectedObject.setUniquepin("123");
            expectedObject.setProjectName("exptProject");

            var responseObject = projectHelper.updateProjectAndItsAssociatedDetails("123", projectObject, payloadMap, "322");

            Assertions.assertEquals(expectedObject, responseObject);

        }

    }

    @Test
    void updateProjectAndItsAssociatedDetails_updateProjectName_Test(){

        ProjectJDO proObject = new ProjectJDO();
        proObject.setProjectId("692");
        proObject.setProjectName("exptProject");
        proObject.setUniquepin("123");

        try(MockedConstruction<ProjectImpl> mock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.saveProject(any())).thenReturn(proObject);
        })){


            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setProjectId("692");
            projectObject.setProjectName("Project");

            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("name","exptProject");


            ProjectJDO expectedObject = new ProjectJDO();
            expectedObject.setProjectId("692");
            expectedObject.setUniquepin("123");
            expectedObject.setProjectName("exptProject");

            var responseObject = projectHelper.updateProjectAndItsAssociatedDetails("123", projectObject, payloadMap, "566");

            Assertions.assertEquals(expectedObject, responseObject);

        }

    }

    @Test
    void updateProjectAndItsAssociatedDetails_updateBillable_valid_Test(){

        ProjectJDO proObject = new ProjectJDO();
        proObject.setProjectId("692");
        proObject.setUniquepin("123");
        proObject.setBillable(true);

        try(MockedConstruction<ProjectImpl> mock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.saveProject(any())).thenReturn(proObject);
        });
            MockedStatic<ProjectFullMetricHelper> projectFullMetricHelperMockedStatic = Mockito.mockStatic(ProjectFullMetricHelper.class)
        ){


            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setProjectId("692");

            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("billable",true);

            ProjectJDO expectedObject = new ProjectJDO();
            expectedObject.setProjectId("692");
            expectedObject.setUniquepin("123");
            expectedObject.setBillable(true);

            var responseObject = projectHelper.updateProjectAndItsAssociatedDetails("123", projectObject, payloadMap, "445");

            Assertions.assertEquals(expectedObject, responseObject);

        }

    }

    @Test
    void updateProjectAndItsAssociatedDetails_updateRate_valid_Test(){

        ProjectJDO proObject = new ProjectJDO();
        proObject.setProjectId("692");
        proObject.setRate(2.00);
        proObject.setUniquepin("123");

        try(MockedConstruction<ProjectImpl> mock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.saveProject(any())).thenReturn(proObject);
        })){


            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setProjectId("692");
            projectObject.setProjectName("Pro");

            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("rate", "2.00");

            ProjectJDO expectedObject = new ProjectJDO();
            expectedObject.setProjectId("692");
            expectedObject.setUniquepin("123");
            expectedObject.setRate(2.00);

            var responseObject = projectHelper.updateProjectAndItsAssociatedDetails("123", projectObject, payloadMap, "46754");

            Assertions.assertEquals(expectedObject, responseObject);

        }

    }

    @Test
    void updateProjectAndItsAssociatedDetails_updateCurrency_valid_Test(){

        ProjectJDO proObject = new ProjectJDO();
        proObject.setProjectId("692");
        proObject.setUniquepin("123");
        proObject.setCurrency("INR");

        try(MockedConstruction<ProjectImpl> mock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.saveProject(any())).thenReturn(proObject);
        })){


            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setProjectId("692");
            projectObject.setProjectName("projectTest");

            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("currency", "INR");

            ProjectJDO expectedObject = new ProjectJDO();
            expectedObject.setProjectId("692");
            expectedObject.setUniquepin("123");
            expectedObject.setCurrency("INR");

            var responseObject = projectHelper.updateProjectAndItsAssociatedDetails("123", projectObject, payloadMap, "865");

            Assertions.assertEquals(expectedObject, responseObject);

        }

    }

    @Test
    void updateProjectAndItsAssociatedDetails_updatePlan_valid_Test(){

        ProjectJDO proObject = new ProjectJDO();
        proObject.setProjectId("692");
        proObject.setUniquepin("123");
        proObject.setPlan("Basic");

        try(MockedConstruction<ProjectImpl> mock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
            Mockito.when(projectImplMock.saveProject(any())).thenReturn(proObject);
        })){


            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setProjectId("692");
            projectObject.setProjectName("testTest");

            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("plan", "Basic");

            ProjectJDO expectedObject = new ProjectJDO();
            expectedObject.setProjectId("692");
            expectedObject.setUniquepin("123");
            expectedObject.setPlan("Basic");

            var responseObject = projectHelper.updateProjectAndItsAssociatedDetails("123", projectObject, payloadMap,"223");

            Assertions.assertEquals(expectedObject, responseObject);

        }

    }

    @Test
    void updateProjectAndItsAssociatedDetails_updateProjectBasicDetailsAlongWithContacts_Test(){

        List<String> contactList = new ArrayList<>();
        contactList.add("23f23");

        ProjectJDO proObject = new ProjectJDO();
        proObject.setProjectId("692");
        proObject.setProjectName("experimentalProject");
        proObject.setRate(2.00);
        proObject.setUniquepin("123");
        proObject.setBillable(true);
        proObject.setCurrency("INR");
        proObject.setPlan("Basic");
        proObject.setContacts(contactList);

        var projectPeopleObject = new ProjectPeopleJDO();
        projectPeopleObject.setPeopleId("23f23");
        projectPeopleObject.setIsDeleted(false);
        projectPeopleObject.setUniquepin("242");
        projectPeopleObject.setProjectId("4342");

        try(MockedConstruction<ProjectImpl> mock = Mockito.mockConstruction(ProjectImpl.class, (projectImplMock, context) -> {
                Mockito.when(projectImplMock.saveProject(any())).thenReturn(proObject);
            });
            MockedConstruction<ProjectPeopleImpl> projectPeopleMock = Mockito.mockConstruction(ProjectPeopleImpl.class,(projectPeopleImpl,context1) ->{
                Mockito.when(projectPeopleImpl.getProjectsAssociated(any(), any(), any())).thenReturn(projectPeopleObject); });
            MockedStatic<ProjectFullMetricHelper> projectFullMetricHelperMockedStatic = Mockito.mockStatic(ProjectFullMetricHelper.class)
        ){

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setProjectId("692");
            projectObject.setProjectName("newProject");

            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("name","experimentalProject");
            payloadMap.put("billable",true);
            payloadMap.put("currency", "INR");
            payloadMap.put("plan", "Basic");
            payloadMap.put("rate", "2.00");
            payloadMap.put("deletedContacts","23f23,");
            payloadMap.put("addedContacts", "23f23,");

            ProjectJDO expectedObject = new ProjectJDO();
            expectedObject.setProjectId("692");
            expectedObject.setUniquepin("123");
            expectedObject.setProjectName("experimentalProject");
            expectedObject.setPlan("Basic");
            expectedObject.setCurrency("INR");
            expectedObject.setRate(2.00);
            expectedObject.setBillable(true);
            expectedObject.setContacts(contactList);

            var responseObject = projectHelper.updateProjectAndItsAssociatedDetails("123", projectObject, payloadMap, "6455");

            Assertions.assertEquals(expectedObject, responseObject);

        }

    }

    @Test
    void updateProjectName_emptyNamePayload_test(){
        var projectObject = new ProjectJDO();
        projectObject.setUniquepin("3232");
        projectObject.setProjectName("newProject");

        Map<String, Object>payloadMap = new HashMap<>();

        boolean response = projectHelper.updateProjectName(payloadMap, projectObject);

        Assertions.assertEquals(false, response);

    }

    @Test
    void updateProjectName_valid_test(){
        var projectObject = new ProjectJDO();
        projectObject.setUniquepin("3232");
        projectObject.setProjectName("newProject");

        Map<String, Object>payloadMap = new HashMap<>();
        payloadMap.put("name", "experTest");

        boolean response = projectHelper.updateProjectName(payloadMap, projectObject);

        Assertions.assertEquals(true, response);

    }

    @Test
    void updateProjectBillableDetails_emptyPayload_test(){

        var projectObject = new ProjectJDO();
        projectObject.setUniquepin("3t332");

        Map<String, Object>payloadMap = new HashMap<>();

        boolean response = projectHelper.updateProjectBillableDetails(payloadMap, projectObject);

        Assertions.assertEquals(false, response);

    }

    @Test
    void updateProjectBillableDetails_valid_test(){
        var projectObject = new ProjectJDO();
        projectObject.setUniquepin("3t332");
        projectObject.setProjectName("Project");
        projectObject.setBillable(false);

        Map<String, Object>payloadMap = new HashMap<>();
        payloadMap.put("billable", true);

        boolean response = projectHelper.updateProjectBillableDetails(payloadMap, projectObject);

        Assertions.assertEquals(true, response);

    }

    @Test
    void updateProjectCurrency_emptyPayload_test(){

        var projectObject = new ProjectJDO();
        projectObject.setUniquepin("3t332");

        Map<String, Object>payloadMap = new HashMap<>();

        boolean response = projectHelper.updateProjectCurrency(payloadMap, projectObject);

        Assertions.assertEquals(false, response);

    }

    @Test
    void updateCurrency_valid_test(){
        var projectObject = new ProjectJDO();
        projectObject.setUniquepin("3t332");
        projectObject.setProjectName("Project");
        projectObject.setCurrency("INR");

        Map<String, Object>payloadMap = new HashMap<>();
        payloadMap.put("currency", "INR");

        boolean response = projectHelper.updateProjectCurrency(payloadMap, projectObject);

        Assertions.assertEquals(true, response);

    }

    @Test
    void updateProjectRate_emptyPayload_test(){

        var projectObject = new ProjectJDO();
        projectObject.setUniquepin("32");

        Map<String, Object>payloadMap = new HashMap<>();

        boolean response = projectHelper.updateProjectRate(payloadMap, projectObject);

        Assertions.assertEquals(false, response);

    }

    @Test
    void updateProjectRate_valid_test(){
        var projectObject = new ProjectJDO();
        projectObject.setUniquepin("32");
        projectObject.setProjectName("Project");
        projectObject.setRate(2.00);

        Map<String, Object>payloadMap = new HashMap<>();
        payloadMap.put("rate", "2.00");

        boolean response = projectHelper.updateProjectRate(payloadMap, projectObject);

        Assertions.assertEquals(true, response);

    }

    @Test
    void updateProjectPlan_emptyPayload_test(){

        var projectObject = new ProjectJDO();
        projectObject.setUniquepin("3t3");

        Map<String, Object>payloadMap = new HashMap<>();

        boolean response = projectHelper.updateProjectPlan(payloadMap, projectObject);

        Assertions.assertEquals(false, response);

    }

    @Test
    void updateProjectPlan_valid_test(){
        var projectObject = new ProjectJDO();
        projectObject.setUniquepin("3t2");
        projectObject.setProjectName("Project");
        projectObject.setPlan("Basic");

        Map<String, Object>payloadMap = new HashMap<>();
        payloadMap.put("plan", "Basic");

        boolean response = projectHelper.updateProjectPlan(payloadMap, projectObject);

        Assertions.assertEquals(true, response);

    }

    @Test
    void updateProjectDeletedContacts_payloadWithoutKey_test(){

            var projectObject = new ProjectJDO();
            projectObject.setUniquepin("23f23");
            projectObject.setProjectName("Project");

            var expectedObject = projectObject;

            Map<String, Object>payloadMap = new HashMap<>();

            var response = projectHelper.updateProjectDeletedContacts(payloadMap, projectObject,"565");

            Assertions.assertEquals(null, response);

    }

    @Test
    void updateProjectDeletedContacts_nullResponse_test(){

        try(MockedConstruction<ProjectPeopleImpl> mock = Mockito.mockConstruction(ProjectPeopleImpl.class, (projectPeopleImplMock, context) -> {
            Mockito.when(projectPeopleImplMock.getProjectsAssociated(any(), any(),any())).thenReturn(null);
        })){

            List<String>contactList = new ArrayList<>();
            contactList.add("23f23");

            var projectObject = new ProjectJDO();
            projectObject.setUniquepin("23f23");
            projectObject.setProjectName("Project");
            projectObject.setContacts(contactList);

            var expectedObject = projectObject;

            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("deletedContacts", "23f23");

            var response = projectHelper.updateProjectDeletedContacts(payloadMap, projectObject, "65654");

            Assertions.assertEquals(expectedObject, response);

        }

    }

    @Test
    void updateProjectDeletedContacts_valid_test(){

        var projectPeopleObject = new ProjectPeopleJDO();
        projectPeopleObject.setPeopleId("23f23");
        projectPeopleObject.setIsDeleted(false);
        projectPeopleObject.setUniquepin("242");
        projectPeopleObject.setProjectId("4342");

        try(MockedConstruction<ProjectPeopleImpl> mock = Mockito.mockConstruction(ProjectPeopleImpl.class, (projectPeopleImplMock, context) -> {
            Mockito.when(projectPeopleImplMock.getProjectsAssociated(any(), any(),any())).thenReturn(projectPeopleObject);
        })){

            List<String>contactList = new ArrayList<>();
            contactList.add("23f23");

            var projectObject = new ProjectJDO();
            projectObject.setUniquepin("23f23");
            projectObject.setProjectName("Project");
            projectObject.setContacts(contactList);

            var expectedObject = projectObject;

            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("deletedContacts", "23f23");

            var response = projectHelper.updateProjectDeletedContacts(payloadMap, projectObject, "756");

            Assertions.assertEquals(expectedObject, response);

        }

    }

    @Test
    void updateProjectAddedContacts_payloadWithoutKey_test(){

        var projectObject = new ProjectJDO();
        projectObject.setUniquepin("23");
        projectObject.setProjectName("Proj");

        var expectedObject = projectObject;

        Map<String, Object>payloadMap = new HashMap<>();

        var response = projectHelper.updateProjectAddedContacts(payloadMap, projectObject);

        Assertions.assertEquals(null, response);

    }

    @Test
    void updateProjectAddedContacts_nullResponse_test(){

        try(MockedConstruction<ProjectPeopleImpl> mock = Mockito.mockConstruction(ProjectPeopleImpl.class, (projectPeopleImplMock, context) -> {
            Mockito.when(projectPeopleImplMock.getProjectsAssociated(any(), any(),any())).thenReturn(null);
        })){

            List<String>contactList = new ArrayList<>();
            contactList.add("283");

            var projectObject = new ProjectJDO();
            projectObject.setUniquepin("23f23");
            projectObject.setProjectName("Proj");
            projectObject.setContacts(contactList);

            var expectedObject = projectObject;

            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("addedContacts", "23");

            var response = projectHelper.updateProjectAddedContacts(payloadMap, projectObject);

            Assertions.assertEquals(expectedObject, response);

        }

    }

    @Test
    void updateProjectAddedContacts_valid_test(){

        var projectPeopleObject = new ProjectPeopleJDO();
        projectPeopleObject.setPeopleId("23f23");
        projectPeopleObject.setIsDeleted(false);
        projectPeopleObject.setUniquepin("242");
        projectPeopleObject.setProjectId("4342");

        try(MockedConstruction<ProjectPeopleImpl> mock = Mockito.mockConstruction(ProjectPeopleImpl.class, (projectPeopleImplMock, context) -> {
            Mockito.when(projectPeopleImplMock.getProjectsAssociated(any(), any(),any())).thenReturn(projectPeopleObject);
        })){

            List<String>contactList = new ArrayList<>();
            contactList.add("23f");

            var projectObject = new ProjectJDO();
            projectObject.setUniquepin("242");
            projectObject.setProjectName("Proj");
            projectObject.setContacts(contactList);

            var expectedObject = projectObject;

            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("addedContacts", "23f23");

            var response = projectHelper.updateProjectAddedContacts(payloadMap, projectObject);

            Assertions.assertEquals(expectedObject, response);

        }

    }

    @Test
    void updateProjectClient_emptyPayloadMap_test(){

            Map<String, Object>payloadMap = new HashMap<>();

            var project = new ProjectJDO();
            project.setUniquepin("3e32");
            project.setProjectId("3efe");

            boolean response = projectHelper.updateProjectClient(payloadMap, project);
            Assertions.assertEquals(false, response);

    }

    @Test
    void updateProjectClient_ifClientIDIsEmpty_test(){

        try(MockedConstruction<ClientImpl> mock = Mockito.mockConstruction(ClientImpl.class, (clientImplMock, context) -> {
            Mockito.when(clientImplMock.getAllClients(any(), any(),any())).thenReturn(null);
        })){
            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("clientID", "");

            var project = new ProjectJDO();
            project.setUniquepin("3e32");
            project.setProjectId("3efe");

            boolean response = projectHelper.updateProjectClient(payloadMap, project);
            Assertions.assertEquals(true, response);
        }

    }

    @Test
    void updateProjectClient_nullResponse_test(){

        try(MockedConstruction<ClientImpl> mock = Mockito.mockConstruction(ClientImpl.class, (clientImplMock, context) -> {
            Mockito.when(clientImplMock.getAllClients(any(), any(),any())).thenReturn(null);
        })){
            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("clientID", "485");

            var project = new ProjectJDO();
            project.setUniquepin("3e32");
            project.setProjectId("3efe");

            boolean response = projectHelper.updateProjectClient(payloadMap, project);
            Assertions.assertEquals(true, response);
        }

    }

    @Test
    void updateProjectClient_clientListIsEmpty_test(){

        HashMap<String, Object>clientResponseMap = new HashMap<>();
        clientResponseMap.put("clients", new ArrayList<>());

        try(MockedConstruction<ClientImpl> mock = Mockito.mockConstruction(ClientImpl.class, (clientImplMock, context) -> {
            Mockito.when(clientImplMock.getAllClients(any(), any(),any())).thenReturn(clientResponseMap);
        })){
            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("clientID", "485");

            var project = new ProjectJDO();
            project.setUniquepin("3e32");
            project.setProjectId("3efe");

            boolean response = projectHelper.updateProjectClient(payloadMap, project);
            Assertions.assertEquals(true, response);
        }

    }

    @Test
    void updateProjectClient_valid_test(){

        var client = new Client();
        client.setId("56u6");

        List<Client>clientList = new ArrayList<>();
        clientList.add(client);
        HashMap<String, Object>clientResponseMap = new HashMap<>();
        clientResponseMap.put("clients", clientList);

        try(MockedConstruction<ClientImpl> mock = Mockito.mockConstruction(ClientImpl.class, (clientImplMock, context) -> {
            Mockito.when(clientImplMock.getAllClients(any(), any(),any())).thenReturn(clientResponseMap);
        })){
            Map<String, Object>payloadMap = new HashMap<>();
            payloadMap.put("clientID", "485");

            var project = new ProjectJDO();
            project.setUniquepin("3e32");
            project.setProjectId("3efe");

            boolean response = projectHelper.updateProjectClient(payloadMap, project);
            Assertions.assertEquals(true, response);
        }

    }

    @Test
    void clockOutHandlerForProjectDeletedContacts_Exception_test(){

        Map<String, Object>activeEntryMap = new HashMap<>();
        activeEntryMap.put("report", "newObject");
        List<Map<String, Object>>activeEntryList = new ArrayList<>();
        activeEntryList.add(activeEntryMap);

        try(MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getActiveEntry(any(), any())).thenReturn(activeEntryList);
        });
            MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class,(userImpl, context1) ->{
                Mockito.when(userImpl.getUserWithoutContact(any(), any())).thenThrow(new IllegalArgumentException("Invalid Argument")); });
            MockedStatic<ClockUtil> ClockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class)
        ){
            projectHelper.clockOutHandlerForProjectDeletedContacts("13","52",null,"43");
            ClockUtilMockedStatic.verifyNoInteractions();
        }catch (Exception e){
            Assertions.assertEquals("Invalid Argument",e.getMessage() );
        }

    }

    @Test
    void clockOutHandlerForProjectDeletedContacts_withoutValidProjectID_test(){

        Map<String, Object>activeEntryMap = new HashMap<>();
        activeEntryMap.put("report", "newObject");
        List<Map<String, Object>>activeEntryList = new ArrayList<>();
        activeEntryList.add(activeEntryMap);

        try(MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getActiveEntry(any(), any())).thenReturn(activeEntryList);
        });
            MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class,(userImpl, context1) ->{
                Mockito.when(userImpl.getUserWithoutContact(any(), any())).thenThrow(new IllegalArgumentException("Invalid Argument")); });
            MockedStatic<ClockUtil> ClockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class)
        ){
            projectHelper.clockOutHandlerForProjectDeletedContacts("13","52","43","43");
            ClockUtilMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void clockOutHandlerForProjectDeletedContacts_activeEntryListIsEmpty_test(){

        Map<String, Object>activeEntryMap = new HashMap<>();
        List<Map<String, Object>>activeEntryList = new ArrayList<>();
        activeEntryList.add(activeEntryMap);

        try(MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getActiveEntry(any(), any())).thenReturn(activeEntryList);
        });
            MockedStatic<ClockUtil> ClockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class)
        ){
            projectHelper.clockOutHandlerForProjectDeletedContacts("13","52","43", "43");
            ClockUtilMockedStatic.verifyNoInteractions();
        }

    }

    @Test
    void clockOutHandlerForProjectDeletedContacts_ifUserObjectIsNull_test(){

        Map<String, Object>activeEntryMap = new HashMap<>();
        activeEntryMap.put("report", "newObject");
        activeEntryMap.put(SchedulingKeys.CALENDAR, "32");
        List<Map<String, Object>>activeEntryList = new ArrayList<>();
        activeEntryList.add(activeEntryMap);

        var userObject = new PeopleRelationJDO();
        userObject.setContactId("5");
        userObject.setUniquepin("1");

        var contactObject = new Contact();
        contactObject.setId("5");
        contactObject.setFirstName("test");

        var accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("5");

        try(MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getActiveEntry(any(), any())).thenReturn(activeEntryList);
        });
            MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class,(userImpl, context1) ->{
                Mockito.when(userImpl.getUserWithoutContact(any(), any())).thenReturn(null); });
            MockedConstruction<ContactImpl> contactMock = Mockito.mockConstruction(ContactImpl.class,(contactImpl, context2) ->{
                Mockito.when(contactImpl.getByID(any())).thenReturn(contactObject); });
            MockedConstruction<AccountImpl> accountMock = Mockito.mockConstruction(AccountImpl.class,(accountImpl, context3) ->{
                Mockito.when(accountImpl.getById(any())).thenReturn(accountObject); });
            MockedStatic<ClockUtil> ClockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class)
        ){
            projectHelper.clockOutHandlerForProjectDeletedContacts("1","5","32", "43");
            Map<String, Object> headersMap = new HashMap<>();
            headersMap.put("srcClientID", "43");
            headersMap.put("fromWhere", "");
            ClockUtilMockedStatic.verify(()->ClockUtil.clockOutHandler(accountObject, null, activeEntryList.get(0), headersMap));
        }

    }

    @Test
    void clockOutHandlerForProjectDeletedContacts_valid_test(){

        Map<String, Object>activeEntryMap = new HashMap<>();
        activeEntryMap.put("report", "newObject");
        activeEntryMap.put(SchedulingKeys.CALENDAR, "432");
        List<Map<String, Object>>activeEntryList = new ArrayList<>();
        activeEntryList.add(activeEntryMap);

        var userObject = new PeopleRelationJDO();
        userObject.setContactId("5342");
        userObject.setUniquepin("123");

        var contactObject = new Contact();
        contactObject.setId("5342");
        contactObject.setFirstName("test");

        var accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("5342");

        try(MockedConstruction<ReportImpl> reportMock = Mockito.mockConstruction(ReportImpl.class, (reportImplMock, context) -> {
            Mockito.when(reportImplMock.getActiveEntry(any(), any())).thenReturn(activeEntryList);
        });
            MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class,(userImpl, context1) ->{
                Mockito.when(userImpl.getUserWithoutContact(any(), any())).thenReturn(userObject); });
            MockedConstruction<ContactImpl> contactMock = Mockito.mockConstruction(ContactImpl.class,(contactImpl, context2) ->{
                Mockito.when(contactImpl.getByID(any())).thenReturn(contactObject); });
            MockedConstruction<AccountImpl> accountMock = Mockito.mockConstruction(AccountImpl.class,(accountImpl, context3) ->{
                Mockito.when(accountImpl.getById(any())).thenReturn(accountObject); });
            MockedStatic<ClockUtil> ClockUtilMockedStatic = Mockito.mockStatic(ClockUtil.class)
        ){
            projectHelper.clockOutHandlerForProjectDeletedContacts("123","5342","432", "432");
            Map<String, Object> headersMap = new HashMap<>();
            headersMap.put("srcClientID", "432");
            headersMap.put("fromWhere", "");
            ClockUtilMockedStatic.verify(()->ClockUtil.clockOutHandler(accountObject, userObject, activeEntryList.get(0), headersMap));
        }

    }

}
