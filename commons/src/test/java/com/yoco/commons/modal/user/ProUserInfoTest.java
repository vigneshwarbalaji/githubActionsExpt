package com.yoco.commons.modal.user;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class ProUserInfoTest {

    @Test
    void ProUserInfo_test(){
        ProUserInfo proUserInfo = new ProUserInfo("accountId","contactId","emailId","role","empId","rfId");
        Assertions.assertEquals("accountId",proUserInfo.getAccountID());
        Assertions.assertEquals("contactId",proUserInfo.getContactID());
        Assertions.assertEquals("emailId",proUserInfo.getEmailID());
        Assertions.assertEquals("role",proUserInfo.getRole());
        Assertions.assertEquals("empId",proUserInfo.getEmpID());
        Assertions.assertEquals("rfId",proUserInfo.getRfID());
    }
}