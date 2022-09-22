package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.objectify.LocalDatastoreExtension;
import com.yoco.commons.dataservices.objectify.ObjectifyExtension;
import com.yoco.commons.dataservices.objectify.OfyService;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.utils.ObjUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@ExtendWith({
        LocalDatastoreExtension.class,
        ObjectifyExtension.class})
class ProjectImplTest extends OfyService {

    @Test
    void getProject_validTest(){

        var expectedProjectObject = new ProjectJDO("123", "newProject", false, "dollar", 0.0, "noPlan", "543");

        ProjectImpl.getProjectImplInstance().saveProject(expectedProjectObject);
        ProjectJDO projectResponse = new ProjectImpl().getProject("543");

        Assertions.assertEquals(expectedProjectObject, projectResponse);

        delete(expectedProjectObject);

    }

    @Test
    void getAllProjectsInAccount_ifCursorValueIsNotNull(){

        var projectJDO1 = new ProjectJDO("13", "newProject", false, "dollar", 0.0, "noPlan", "543");
        var projectJDO2 = new ProjectJDO("13", "Project", false, "dollar", 0.0, "noPlan", "53");

        ProjectImpl.getProjectImplInstance().saveProject(projectJDO1);
        ProjectImpl.getProjectImplInstance().saveProject(projectJDO2);
        Map<String, Object> responseMap = new ProjectImpl().getAllProjectsInAccount("13", "all", null, 1);

        Assertions.assertEquals(true, !ObjUtils.isNull(responseMap.get("cursor")));

        delete(projectJDO1);
        delete(projectJDO2);

    }

    @Test
    void getAllProjectsInAccount_withActiveProjectTypeTest(){

        var projectJDO1 = new ProjectJDO("123", "newProject", false, "dollar", 0.0, "noPlan", "543");
        projectJDO1.setIsDeleted(false);

        List<ProjectJDO> expectedList = new ArrayList<>();
        expectedList.add(0, projectJDO1);

        Map<String, Object>expectedMap = new HashMap<>();
        expectedMap.put("projects", expectedList);

        ProjectImpl.getProjectImplInstance().saveProject(projectJDO1);
        Map<String, Object> responseMap = new ProjectImpl().getAllProjectsInAccount("123", "active", null, 2);

        Assertions.assertEquals(expectedMap, responseMap);

        delete(projectJDO1);

    }

    @Test
    void getAllProjectsInAccount_withInActiveProjectTypeTest(){

        var projectJDO1 = new ProjectJDO("123", "newProject", false, "dollar", 0.0, "noPlan", "543");
        projectJDO1.setIsDeleted(true);

        List<ProjectJDO> expectedList = new ArrayList<>();
        expectedList.add(0, projectJDO1);

        Map<String, Object>expectedMap = new HashMap<>();
        expectedMap.put("projects", expectedList);

        ProjectImpl.getProjectImplInstance().saveProject(projectJDO1);
        Map<String, Object> responseMap = new ProjectImpl().getAllProjectsInAccount("123", "inactive", null, 2);

        Assertions.assertEquals(expectedMap, responseMap);

        delete(projectJDO1);

    }

    @Test
    void getAllProjectsInAccount_validTest(){

        var projectJDO1 = new ProjectJDO("123", "newProject", false, "dollar", 0.0, "noPlan", "543");

        List<ProjectJDO> expectedList = new ArrayList<>();
        expectedList.add(0, projectJDO1);

        Map<String, Object>expectedMap = new HashMap<>();
        expectedMap.put("projects", expectedList);

        ProjectImpl.getProjectImplInstance().saveProject(projectJDO1);
        Map<String, Object> responseMap = new ProjectImpl().getAllProjectsInAccount("123", "all", null, 2);

        Assertions.assertEquals(expectedMap, responseMap);

        delete(projectJDO1);

    }

}
