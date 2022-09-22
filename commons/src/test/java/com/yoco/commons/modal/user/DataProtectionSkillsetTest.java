package com.yoco.commons.modal.user;

import com.google.appengine.api.datastore.Text;
import com.yoco.commons.utils.JsonUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class DataProtectionSkillsetTest {

    @Test
    void getDefaultUserDataProtectionSkillset_test(){
        Text response = DataProtectionSkillset.getDefaultUserDataProtectionSkillset();

        DataProtectionSkillset dataProtectionSkillset = new DataProtectionSkillset(true,true,true,true,DataProtectionSkillset.getUserDefaultProfileSkillset());
        Text expected = new Text (JsonUtil.getJson(dataProtectionSkillset));

        Assertions.assertEquals(expected,response);
    }
}