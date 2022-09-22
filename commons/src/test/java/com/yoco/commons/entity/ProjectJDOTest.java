package com.yoco.commons.entity;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class ProjectJDOTest {

   @Test
    void empty_projectId_test(){
       ProjectJDO projectJDO = new ProjectJDO("accountId","name",false,"",0.0,"","");
       Assertions.assertNotNull(projectJDO.getProjectId());
       Assertions.assertNotNull(projectJDO.getDateAddedLongTime());
       Assertions.assertNotNull(projectJDO.getDateAdded());
       Assertions.assertFalse(projectJDO.getIsDeleted());
       Assertions.assertEquals("name",projectJDO.getProjectName());
       Assertions.assertFalse(projectJDO.getIsDefault());
       Assertions.assertEquals("accountId",projectJDO.getUniquepin());
       Assertions.assertFalse(projectJDO.getBillable());
       Assertions.assertEquals("",projectJDO.getCurrency());
       Assertions.assertEquals(0.0,projectJDO.getRate());
       Assertions.assertEquals("",projectJDO.getPlan());
   }

   @Test
   void valid_projectId_test(){
      ProjectJDO projectJDO = new ProjectJDO("accountId","name",true,"Rupee",0.2,"Hourly","id");
      Assertions.assertEquals("id",projectJDO.getProjectId());
      Assertions.assertNotNull(projectJDO.getDateAddedLongTime());
      Assertions.assertNotNull(projectJDO.getDateAdded());
      Assertions.assertFalse(projectJDO.getIsDeleted());
      Assertions.assertEquals("name",projectJDO.getProjectName());
      Assertions.assertFalse(projectJDO.getIsDefault());
      Assertions.assertEquals("accountId",projectJDO.getUniquepin());
      Assertions.assertTrue(projectJDO.getBillable());
      Assertions.assertEquals("Rupee",projectJDO.getCurrency());
      Assertions.assertEquals(0.2,projectJDO.getRate());
      Assertions.assertEquals("Hourly",projectJDO.getPlan());
   }
}
