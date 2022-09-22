package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.objectify.LocalDatastoreExtension;
import com.yoco.commons.dataservices.objectify.ObjectifyExtension;
import com.yoco.commons.dataservices.objectify.OfyService;
import com.yoco.commons.entity.ProjectPeopleJDO;
import com.yoco.commons.modal.CollectionResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import java.util.ArrayList;
import java.util.List;

@ExtendWith({
        LocalDatastoreExtension.class,
        ObjectifyExtension.class})
class ProjectPeopleImplTest extends OfyService {

    @Test
    void getProjectAssociationByProjectID_validTest(){

        var projectPeopleObject = new ProjectPeopleJDO("232", "67", "90", "justAnotherProject");
        List<ProjectPeopleJDO>projectPeopleList = new ArrayList<>();
        projectPeopleList.add(projectPeopleObject);
        ProjectPeopleImpl.getProjectPeopleImplInstance().saveProjectAssociations(projectPeopleList);

        Assertions.assertEquals(projectPeopleList,
                ProjectPeopleImpl.getProjectPeopleImplInstance().getProjectAssociationByProjectID("232","67"));

        delete(projectPeopleObject);

    }

    @Test
    void getProjectsAssociatedToUser_validTest(){

        var expectedProjectPeopleObject = new ProjectPeopleJDO("232", "67", "90", "justAnotherProject");
        expectedProjectPeopleObject.setIsDeleted(false);
        List<ProjectPeopleJDO>projectPeopleList = new ArrayList<>();
        projectPeopleList.add(expectedProjectPeopleObject);

        ProjectPeopleImpl.getProjectPeopleImplInstance().saveProjectAssociations(projectPeopleList);

        CollectionResponse<ProjectPeopleJDO>projectList = ProjectPeopleImpl.getProjectPeopleImplInstance().getProjectsAssociatedToUser("232","90",null, 1);

        List<ProjectPeopleJDO>expectedList = new ArrayList<>();
        expectedList.add(expectedProjectPeopleObject);

        Assertions.assertEquals(expectedList,
                projectList.getItems());

        delete(expectedProjectPeopleObject);

    }

    @Test
    void getProjectsAssociated_validTest(){

        var expectedProjectPeopleObject = new ProjectPeopleJDO("232", "67", "90", "justAnotherProject");
        expectedProjectPeopleObject.setIsDeleted(false);
        List<ProjectPeopleJDO>projectPeopleList = new ArrayList<>();
        projectPeopleList.add(expectedProjectPeopleObject);

        ProjectPeopleImpl.getProjectPeopleImplInstance().saveProjectAssociations(projectPeopleList);

        Assertions.assertEquals(expectedProjectPeopleObject,
                ProjectPeopleImpl.getProjectPeopleImplInstance().getProjectsAssociated("232","90","67"));

        delete(expectedProjectPeopleObject);

    }

    @Test
    void deleteProjectAssociations_validTest(){

        var expectedProjectPeopleObject = new ProjectPeopleJDO("232", "67", "90", "justAnotherProject");
        List<ProjectPeopleJDO>projectPeopleList = new ArrayList<>();
        projectPeopleList.add(expectedProjectPeopleObject);

        ProjectPeopleImpl.getProjectPeopleImplInstance().saveProjectAssociations(projectPeopleList);

        Assertions.assertEquals(projectPeopleList,
                ProjectPeopleImpl.getProjectPeopleImplInstance().deleteProjectAssociations(projectPeopleList));

        delete(expectedProjectPeopleObject);
    }

    @Test
    void getAllProjectAssociationsUnderAccount_valid_test(){
        ProjectPeopleImpl projectPeopleImplInstance = ProjectPeopleImpl.getProjectPeopleImplInstance();
        var ppj1 = new ProjectPeopleJDO("accID", "234", "123", "test");
        var ppj2 = new ProjectPeopleJDO("accID", "234", "123", "test1");
        projectPeopleImplInstance.save(ppj1);
        projectPeopleImplInstance.save(ppj2);
        CollectionResponse<ProjectPeopleJDO> actual = projectPeopleImplInstance.getAllProjectAssociationsUnderAccount("accID",50,null);
        Assertions.assertEquals(2,actual.getItems().size());
        Assertions.assertTrue(actual.getItems().contains(ppj1));
        Assertions.assertTrue(actual.getItems().contains(ppj2));
        ofy().delete().entities(actual.getItems()).now();
    }
}
