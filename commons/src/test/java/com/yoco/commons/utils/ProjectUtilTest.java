package com.yoco.commons.utils;

import com.yoco.commons.dataservices.impl.ProjectImpl;
import com.yoco.commons.dataservices.impl.ProjectPeopleImpl;
import com.yoco.commons.dataservices.objectify.OfyService;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.entity.ProjectPeopleJDO;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.CollectionResponse;
import org.checkerframework.checker.units.qual.A;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.*;

import static org.mockito.ArgumentMatchers.*;

class ProjectUtilTest {
    @Test
    void getAllProjectsForUserInAnAccount_nullAccountID_test(){
        Assertions.assertTrue(ProjectUtil.getAllProjectsForUserInAnAccount(null,"123").isEmpty());
    }

    @Test
    void getAllProjectsForUserInAnAccount_emptyAccountID_test(){
        Assertions.assertTrue(ProjectUtil.getAllProjectsForUserInAnAccount("","123").isEmpty());
    }

    @Test
    void getAllProjectsForUserInAnAccount_nullcontactID_test(){
        Assertions.assertTrue(ProjectUtil.getAllProjectsForUserInAnAccount("123",null).isEmpty());
    }

    @Test
    void getAllProjectsForUserInAnAccount_emptyContactID_test(){
        Assertions.assertTrue(ProjectUtil.getAllProjectsForUserInAnAccount("123","").isEmpty());
    }

    @Test
    void getAllProjectsForUserInAnAccount_emptyAssociations_test(){
        try(MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class)){
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectsForUserInAnAccount("123","234")).thenCallRealMethod();
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectAssociationsForUserInAnAccount("123","234")).thenReturn(new ArrayList<>());
            Assertions.assertTrue(ProjectUtil.getAllProjectsForUserInAnAccount("123","234").isEmpty());
        }
    }

    @Test
    void getAllProjectsForUserInAnAccount_validAssociations_test(){
        try(MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);
            MockedStatic<ProjectImpl> projectImplMockedStatic = Mockito.mockStatic(ProjectImpl.class)){
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectsForUserInAnAccount("123","234")).thenCallRealMethod();
            ProjectPeopleJDO pp1 = new ProjectPeopleJDO();
            pp1.setProjectId("prj1");
            ProjectPeopleJDO pp2 = new ProjectPeopleJDO();
            pp2.setProjectId("prj2");
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectAssociationsForUserInAnAccount("123","234")).thenReturn(new ArrayList(){{add(pp1);add(pp2);}});
            ProjectImpl projectImplMock = Mockito.mock(ProjectImpl.class);
            Mockito.when(projectImplMock.get(eq(ProjectJDO.class),any(Set.class))).thenReturn(new ArrayList(){{add(new ProjectJDO());}});
            projectImplMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(projectImplMock);
            Assertions.assertEquals(1,ProjectUtil.getAllProjectsForUserInAnAccount("123","234").size());
        }
    }

    @Test
    void getAllProjectAssociationsForUserInAnAccount_nullAccountID_test(){
        Assertions.assertTrue(ProjectUtil.getAllProjectAssociationsForUserInAnAccount(null,"123").isEmpty());
    }

    @Test
    void getAllProjectAssociationsForUserInAnAccount_emptyAccountID_test(){
        Assertions.assertTrue(ProjectUtil.getAllProjectAssociationsForUserInAnAccount("","123").isEmpty());
    }

    @Test
    void getAllProjectAssociationsForUserInAnAccount_nullcontactID_test(){
        Assertions.assertTrue(ProjectUtil.getAllProjectAssociationsForUserInAnAccount("123",null).isEmpty());
    }

    @Test
    void getAllProjectAssociationsForUserInAnAccount_emptyContactID_test(){
        Assertions.assertTrue(ProjectUtil.getAllProjectAssociationsForUserInAnAccount("123","").isEmpty());
    }

    @Test
    void getAllProjectAssociationsForUserInAnAccount_noCursor_test(){
        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = Mockito.mockStatic(ProjectPeopleImpl.class)){
            ProjectPeopleImpl projectPeopleImplMock = Mockito.mock(ProjectPeopleImpl.class);
            CollectionResponse<ProjectPeopleJDO> response = new CollectionResponse<>();
            response.setCursor(null);
            response.setItems(new ArrayList(){{add(new ProjectPeopleJDO());}});
            Mockito.when(projectPeopleImplMock.getProjectsAssociatedToUser("123","234","",1000)).thenReturn(response);
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectPeopleImplMock);
            Assertions.assertEquals(1,ProjectUtil.getAllProjectAssociationsForUserInAnAccount("123","234").size());
        }
    }

    @Test
    void getAllProjectAssociationsForUserInAnAccount_validCursor_test(){
        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = Mockito.mockStatic(ProjectPeopleImpl.class)){
            ProjectPeopleImpl projectPeopleImplMock = Mockito.mock(ProjectPeopleImpl.class);
            CollectionResponse<ProjectPeopleJDO> response1 = new CollectionResponse<>();
            response1.setCursor("cursor");
            response1.setItems(new ArrayList(){{add(new ProjectPeopleJDO());}});

            CollectionResponse<ProjectPeopleJDO> response2 = new CollectionResponse<>();
            response2.setCursor(null);
            response2.setItems(new ArrayList(){{add(new ProjectPeopleJDO());}});

            Mockito.when(projectPeopleImplMock.getProjectsAssociatedToUser("123","234","",1000)).thenReturn(response1);
            Mockito.when(projectPeopleImplMock.getProjectsAssociatedToUser("123","234","cursor",1000)).thenReturn(response2);
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectPeopleImplMock);
            Assertions.assertEquals(2,ProjectUtil.getAllProjectAssociationsForUserInAnAccount("123","234").size());
        }
    }

    @Test
    void disassociateUserFromAllProjectsInAccount_noAssociations_test(){
        try(MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);
        MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = Mockito.mockStatic(ProjectPeopleImpl.class)){
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectAssociationsForUserInAnAccount("accID","contactID")).thenReturn(List.of());
            projectUtilMockedStatic.when(()->ProjectUtil.disassociateUserFromAllProjectsInAccount("accID","contactID")).thenCallRealMethod();
            ProjectUtil.disassociateUserFromAllProjectsInAccount("accID","contactID");
            projectPeopleMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void disassociateUserFromAllProjectsInAccount_ValidAssociations_test(){
        try(MockedConstruction<OfyService> ofyServiceMockedConstruction = Mockito.mockConstruction(OfyService.class, (ofyServiceMock, context) -> {
            Mockito.doNothing().when(ofyServiceMock).delete(any(List.class));
        });
                MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);){
            ProjectPeopleJDO projectPeopleJDOMock = new ProjectPeopleJDO();
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectAssociationsForUserInAnAccount("accID","contactID")).thenReturn(List.of(projectPeopleJDOMock));
            projectUtilMockedStatic.when(()->ProjectUtil.disassociateUserFromAllProjectsInAccount("accID","contactID")).thenCallRealMethod();
            ProjectUtil.disassociateUserFromAllProjectsInAccount("accID","contactID");
            Assertions.assertEquals(1,ofyServiceMockedConstruction.constructed().size());
        }
    }

    @Test
    void getAllProjectAssociationsUnderAccount_nullAccountID_test(){
        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = Mockito.mockStatic(ProjectPeopleImpl.class)){
            ProjectUtil.getAllProjectAssociationsUnderAccount(null);
            projectPeopleMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void getAllProjectAssociationsUnderAccount_validAccountID_test(){
        try(MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = Mockito.mockStatic(ProjectPeopleImpl.class)){
            ProjectPeopleImpl projectPeopleImplMock = Mockito.mock(ProjectPeopleImpl.class);
            CollectionResponse<ProjectPeopleJDO> collectionResponseWithCursor = new CollectionResponse<>();
            collectionResponseWithCursor.setItems(List.of(new ProjectPeopleJDO()));
            collectionResponseWithCursor.setCursor("cursor");
            CollectionResponse<ProjectPeopleJDO> collectionResponseWithoutCursor = new CollectionResponse<>();
            collectionResponseWithoutCursor.setItems(List.of(new ProjectPeopleJDO()));
            collectionResponseWithoutCursor.setCursor(null);
            Mockito.when(projectPeopleImplMock.getAllProjectAssociationsUnderAccount("accID",1000,"")).thenReturn(collectionResponseWithCursor);
            Mockito.when(projectPeopleImplMock.getAllProjectAssociationsUnderAccount("accID",1000,"cursor")).thenReturn(collectionResponseWithoutCursor);
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectPeopleImplMock);
            Assertions.assertEquals(2,ProjectUtil.getAllProjectAssociationsUnderAccount("accID").size());
        }
    }

    @Test
    void deleteAllProjectAssociationsUnderAccount_test(){
        try(MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);
            MockedStatic<ProjectPeopleImpl> projectPeopleMockedStatic = Mockito.mockStatic(ProjectPeopleImpl.class)){
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectAssociationsUnderAccount("accID")).thenReturn(List.of(new ProjectPeopleJDO(),new ProjectPeopleJDO()));
            projectUtilMockedStatic.when(()->ProjectUtil.deleteAllProjectAssociationsUnderAccount("accID")).thenCallRealMethod();
            ProjectPeopleImpl projectPeople = Mockito.mock(ProjectPeopleImpl.class);
            projectPeopleMockedStatic.when(ProjectPeopleImpl::getProjectPeopleImplInstance).thenReturn(projectPeople);
            ProjectUtil.deleteAllProjectAssociationsUnderAccount("accID");
            Mockito.verify(projectPeople).deleteProjectAssociations(List.of(new ProjectPeopleJDO(),new ProjectPeopleJDO()));
        }
    }

    @Test
    void getAllProjectsUnderAccount_nullAccountID_test(){
        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class)){
            ProjectUtil.getAllProjectsUnderAccount(null);
            projectMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void getAllProjectsUnderAccount_validAccountID_test(){
        try(MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class)){
            ProjectImpl projectImplMock = Mockito.mock(ProjectImpl.class);
            Map<String,Object> responseWithCursor = new HashMap<>();
            responseWithCursor.put("projects",List.of(new ProjectJDO()));
            responseWithCursor.put("cursor","cursor");
            Map<String,Object> responseWithoutCursor = new HashMap<>();
            responseWithoutCursor.put("projects",List.of(new ProjectJDO()));
            responseWithoutCursor.put("cursor",null);
            Mockito.when(projectImplMock.getAllProjectsInAccount("accID","active","",1000)).thenReturn(responseWithCursor);
            Mockito.when(projectImplMock.getAllProjectsInAccount("accID","active","cursor",1000)).thenReturn(responseWithoutCursor);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(projectImplMock);
            Assertions.assertEquals(2,ProjectUtil.getAllProjectsUnderAccount("accID").size());
        }
    }

    @Test
    void archiveAllProjectsUnderAccount_valid_test(){
        try(MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class);
            MockedStatic<ProjectImpl> projectMockedStatic = Mockito.mockStatic(ProjectImpl.class)){
            ProjectJDO prj1 = new ProjectJDO();
            ProjectJDO prj2 = new ProjectJDO();
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectsUnderAccount("accID")).thenReturn(List.of(prj1,prj2));
            projectUtilMockedStatic.when(()->ProjectUtil.archiveAllProjectsUnderAccount("accID")).thenCallRealMethod();
            ProjectImpl projectMock = Mockito.mock(ProjectImpl.class);
            projectMockedStatic.when(ProjectImpl::getProjectImplInstance).thenReturn(projectMock);
            ProjectUtil.archiveAllProjectsUnderAccount("accID");
            prj1.setIsDeleted(true);
            prj2.setIsDeleted(true);
            Mockito.verify(projectMock).saveCollection(List.of(prj1,prj2));
        }
    }

    @Test
    void getAllProjectIdsAssociatedToUser_valid_test(){
        try(MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class)){
            ProjectPeopleJDO prj1 = new ProjectPeopleJDO();
            prj1.setProjectId("id1");
            ProjectPeopleJDO prj2 = new ProjectPeopleJDO();
            prj2.setProjectId("id2");
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectAssociationsForUserInAnAccount(anyString(),anyString())).thenReturn(List.of(prj1,prj2));
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectIdsAssociatedToUser(anyString(),anyString())).thenCallRealMethod();
            Assertions.assertEquals(List.of("id1","id2"),ProjectUtil.getAllProjectIdsAssociatedToUser("accID","contId"));
        }
    }

    @Test
    void getAllProjectIdsAssociatedToUser_no_projects_test(){
        try(MockedStatic<ProjectUtil> projectUtilMockedStatic = Mockito.mockStatic(ProjectUtil.class)){
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectAssociationsForUserInAnAccount(anyString(),anyString())).thenReturn(List.of());
            projectUtilMockedStatic.when(()->ProjectUtil.getAllProjectIdsAssociatedToUser(anyString(),anyString())).thenCallRealMethod();
            Assertions.assertEquals(List.of(),ProjectUtil.getAllProjectIdsAssociatedToUser("accID","contId"));
        }
    }

}
