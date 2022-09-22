package com.yoco;

import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.utils.JsonUtil;

public class MockPRO {

    public static PeopleRelationJDO getMockPRO(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setUniquepin("accountId");
        userPro.setContactId("contactId");
        userPro.setEmailID("test@gmail.com");
        userPro.setParentContactId("pContactId");
        userPro.setRole(Skillset.ROLE_STAFF);
        userPro.setDelete(false);
        userPro.setDefault(true);
        userPro.setDateAddedLongTime(1499072216599L);
        userPro.setDateModified(1641816293211L);
        userPro.setTimeZone("Asia/Kolkata");
        userPro.setTimeZoneDisplayName("(GMT+05:30) Asia/Kolkata (India Time)");
        userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForStaff("accountId",false)));
        return userPro;
    }

}
