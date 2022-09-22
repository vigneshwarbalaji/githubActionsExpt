package com.yoco.commons.entity;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class ProjectPeopleJDOTest {

    @Test
    void ProjectPeopleJDO_test(){
        ProjectPeopleJDO projectPeopleJDO = new ProjectPeopleJDO("accountId","projectId","contactId","name");
        Assertions.assertNotNull(projectPeopleJDO.getProjectPeopleId());
        Assertions.assertNotNull(projectPeopleJDO.getDateAddedLongTime());
        Assertions.assertNotNull(projectPeopleJDO.getDateAdded());
        Assertions.assertFalse(projectPeopleJDO.getIsDeleted());
        Assertions.assertEquals("accountId",projectPeopleJDO.getUniquepin());
        Assertions.assertEquals("projectId",projectPeopleJDO.getProjectId());
        Assertions.assertEquals("contactId",projectPeopleJDO.getPeopleId());
        Assertions.assertEquals("name",projectPeopleJDO.getProjectName());
    }
}
