package com.yoco.commons.entity;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class UserClockinSubStatusJDOTest {

     @Test
     void UserClockinSubStatusJDO_eventLongTime_nonZero_test(){
        UserClockinSubStatusJDO userClockinSubStatusJDO = new UserClockinSubStatusJDO("accountId","contactId",
                "projectId","test@gmail.com","activity","activityType",1641548481000L);
       Assertions.assertNotNull(userClockinSubStatusJDO.getUserClockInSubStatusID());
       Assertions.assertNotNull(userClockinSubStatusJDO.getEventLongTime());
       Assertions.assertNotNull(userClockinSubStatusJDO.getEventDate());
       Assertions.assertEquals("accountId",userClockinSubStatusJDO.getUniquepin());
       Assertions.assertEquals("projectId",userClockinSubStatusJDO.getProjectId());
       Assertions.assertEquals("contactId",userClockinSubStatusJDO.getContactId());
       Assertions.assertEquals("test@gmail.com",userClockinSubStatusJDO.getUserLogIn());
       Assertions.assertEquals("activity",userClockinSubStatusJDO.getSubStatus());
       Assertions.assertEquals("activityType",userClockinSubStatusJDO.getStatusType());
       Assertions.assertEquals("",userClockinSubStatusJDO.getConnectionId());
       Assertions.assertEquals(1641548481000L,userClockinSubStatusJDO.getReceivedLongTime());
     }

    @Test
    void UserClockinSubStatusJDO_eventLongTime_zero_test(){
        UserClockinSubStatusJDO userClockinSubStatusJDO = new UserClockinSubStatusJDO("accountId","contactId",
                "projectId","test@gmail.com","activity","activityType",0L);
        Assertions.assertNotNull(userClockinSubStatusJDO.getUserClockInSubStatusID());
        Assertions.assertNotNull(userClockinSubStatusJDO.getEventLongTime());
        Assertions.assertNotNull(userClockinSubStatusJDO.getEventDate());
        Assertions.assertEquals("accountId",userClockinSubStatusJDO.getUniquepin());
        Assertions.assertEquals("projectId",userClockinSubStatusJDO.getProjectId());
        Assertions.assertEquals("contactId",userClockinSubStatusJDO.getContactId());
        Assertions.assertEquals("test@gmail.com",userClockinSubStatusJDO.getUserLogIn());
        Assertions.assertEquals("activity",userClockinSubStatusJDO.getSubStatus());
        Assertions.assertEquals("activityType",userClockinSubStatusJDO.getStatusType());
        Assertions.assertEquals("",userClockinSubStatusJDO.getConnectionId());
        Assertions.assertNotNull(userClockinSubStatusJDO.getReceivedLongTime());
    }

}
