package com.yoco.commons.entity;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class TaskJDOTest {

    @Test
    void TaskJDO_test(){
        TaskJDO taskJDO = new TaskJDO("entryId","taskId","taskDesc","taskSrc");
        Assertions.assertEquals("entryId",taskJDO.getId());
        Assertions.assertEquals("taskId",taskJDO.getTaskID());
        Assertions.assertEquals("taskDesc",taskJDO.getTaskDescription());
        Assertions.assertEquals("taskSrc",taskJDO.getTaskSource());
    }
}
