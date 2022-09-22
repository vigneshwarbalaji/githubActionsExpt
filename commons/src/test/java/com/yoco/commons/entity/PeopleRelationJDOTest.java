package com.yoco.commons.entity;

import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.modal.user.ProUserInfo;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class PeopleRelationJDOTest {

    @Test
    void PeopleRelationJDO_test(){
        ProUserInfo userInfo = new ProUserInfo("accountId","contactId","test@gmail.com","admin","112","123");
        PeopleRelationJDO userPro = new PeopleRelationJDO(userInfo,"parentId", DateConstants.ZONE_ID_IST,"(GMT+05:30) Asia/Kolkata (India Time)",false,false);
        Assertions.assertNotNull(userPro.getId());
        Assertions.assertFalse(userPro.isDelete());
        Assertions.assertFalse(userPro.isDefault());
        Assertions.assertNotNull(userPro.getDateAddedLongTime());
        Assertions.assertNotNull(userPro.getDateAdded());
        Assertions.assertNotNull(userPro.getDateModified());
        Assertions.assertFalse(userPro.isToBeMonitoredForExcessClockedIn());
        Assertions.assertEquals(0L,userPro.getCanAccessReportsSince());
        Assertions.assertEquals(0L,userPro.getHireDate());
        Assertions.assertEquals(0L,userPro.getStartDate());
        Assertions.assertEquals(0L,userPro.getRelieveDate());
        Assertions.assertEquals("parentId",userPro.getParentContactId());
        Assertions.assertEquals("contactId",userPro.getContactId());
        Assertions.assertEquals("accountId",userPro.getUniquepin());
        Assertions.assertEquals("test@gmail.com",userPro.getEmailID());
        Assertions.assertEquals("admin",userPro.getRole());
        Assertions.assertEquals("112",userPro.getLogin());
        Assertions.assertEquals("123",userPro.getRfId());
        Assertions.assertEquals(DateConstants.ZONE_ID_IST,userPro.getTimeZone());
        Assertions.assertEquals("(GMT+05:30) Asia/Kolkata (India Time)",userPro.getTimeZoneDisplayName());
    }
}