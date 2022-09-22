package com.yoco.user.helper;

import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.AccountUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.util.ArrayList;
import java.util.List;

class UserDownloadHelperTest {
    @Test
    void getIsDeleteQueryValue_statusActive_test(){
        Assertions.assertEquals(Boolean.FALSE,UserDownloadHelper.getIsDeleteQueryValue("active"));
    }

    @Test
    void getIsDeleteQueryValue_statusInActive_test(){
        Assertions.assertEquals(Boolean.TRUE,UserDownloadHelper.getIsDeleteQueryValue("Inactive"));
    }

    @Test
    void getIsDeleteQueryValue_statusBoth_test(){
        Assertions.assertNull(UserDownloadHelper.getIsDeleteQueryValue(""));
    }

    @Test
    void generateStaffDataCsvFileName_InActiveStaff_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accountUtilMockedStatic.when(()->AccountUtil.getCompanyDisplayName("accID")).thenReturn("Company");
            Assertions.assertEquals("StaffDataReport_Company_Inactive.csv",UserDownloadHelper.generateStaffDataCsvFileName("accID",Boolean.TRUE));
        }
    }

    @Test
    void generateStaffDataCsvFileName_ActiveStaff_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accountUtilMockedStatic.when(()->AccountUtil.getCompanyDisplayName("accID")).thenReturn("Company");
            Assertions.assertEquals("StaffDataReport_Company_Active.csv",UserDownloadHelper.generateStaffDataCsvFileName("accID",Boolean.FALSE));
        }
    }

    @Test
    void generateStaffDataCsvFileName_BothStaff_test(){
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accountUtilMockedStatic.when(()->AccountUtil.getCompanyDisplayName("accID")).thenReturn("Company");
            Assertions.assertEquals("StaffDataReport_Company_ActiveInactive.csv",UserDownloadHelper.generateStaffDataCsvFileName("accID",null));
        }
    }

    @Test
    void isFieldIncludedInOutputFields_nulField_test(){
        Assertions.assertFalse(UserDownloadHelper.isFieldIncludedInOutputFields("",null));
    }

    @Test
    void isFieldIncludedInOutputFields_emptyField_test(){
        Assertions.assertFalse(UserDownloadHelper.isFieldIncludedInOutputFields("",""));
    }

    @Test
    void isFieldIncludedInOutputFields_nullOutputField_test(){
        Assertions.assertFalse(UserDownloadHelper.isFieldIncludedInOutputFields(null,"status"));
    }

    @Test
    void isFieldIncludedInOutputFields_emptyOutputField_test(){
        Assertions.assertFalse(UserDownloadHelper.isFieldIncludedInOutputFields("","status"));
    }

    @Test
    void isFieldIncludedInOutputFields_NotPresentInOutputField_test(){
        Assertions.assertFalse(UserDownloadHelper.isFieldIncludedInOutputFields("outputfield,","status"));
    }

    @Test
    void isFieldIncludedInOutputFields_PresentInOutputField_test(){
        Assertions.assertTrue(UserDownloadHelper.isFieldIncludedInOutputFields("outputfield,status,emp","status"));
    }

    @Test
    void getUserReportHeader_onlyEmailField_test(){
        Assertions.assertEquals("Email\r\n",UserDownloadHelper.getUserReportHeader(false,false));
    }

    @Test
    void getUserReportHeader_statusField_test(){
        Assertions.assertEquals("Email,Status\r\n",UserDownloadHelper.getUserReportHeader(true,false));
    }

    @Test
    void getUserReportHeader_employeeIDField_test(){
        Assertions.assertEquals("Email,EmployeeID\r\n",UserDownloadHelper.getUserReportHeader(false,true));
    }

    @Test
    void getUserReportHeader_AllFields_test(){
        Assertions.assertEquals("Email,Status,EmployeeID\r\n",UserDownloadHelper.getUserReportHeader(true,true));
    }

   @Test
   void generateStaffDataCsv_AllFields_test(){
        PeopleRelationJDO pro1 = new PeopleRelationJDO();
        pro1.setEmailID("email1");
        pro1.setDelete(true);
        PeopleRelationJDO pro2 = new PeopleRelationJDO();
        pro2.setEmailID("email2");
        pro2.setDelete(false);
        pro2.setLogin("123");
        List<PeopleRelationJDO> stafflist = new ArrayList<>(){{
            add(pro1);
            add(pro2);
        }};
        String actual = UserDownloadHelper.generateStaffDataCsv(stafflist,"status,empID");
        Assertions.assertEquals("Email,Status,EmployeeID\r\nemail1,Inactive,-\r\nemail2,Active,123\r\n",actual);
    }

    @Test
    void generateStaffDataCsv_EmailEmployeeIDFields_test(){
        PeopleRelationJDO pro1 = new PeopleRelationJDO();
        pro1.setEmailID("email1");
        PeopleRelationJDO pro2 = new PeopleRelationJDO();
        pro2.setEmailID("email2");
        pro2.setLogin("123");
        List<PeopleRelationJDO> stafflist = new ArrayList<>(){{
            add(pro1);
            add(pro2);
        }};
        String actual = UserDownloadHelper.generateStaffDataCsv(stafflist,"empID");
        Assertions.assertEquals("Email,EmployeeID\r\nemail1,-\r\nemail2,123\r\n",actual);
    }

    @Test
    void generateStaffDataCsv_EmailStatusFields_test(){
        PeopleRelationJDO pro1 = new PeopleRelationJDO();
        pro1.setEmailID("email1");
        pro1.setDelete(true);
        PeopleRelationJDO pro2 = new PeopleRelationJDO();
        pro2.setEmailID("email2");
        pro2.setDelete(false);
        List<PeopleRelationJDO> stafflist = new ArrayList<>(){{
            add(pro1);
            add(pro2);
        }};
        String actual = UserDownloadHelper.generateStaffDataCsv(stafflist,"status");
        Assertions.assertEquals("Email,Status\r\nemail1,Inactive\r\nemail2,Active\r\n",actual);
    }

    @Test
    void generateStaffDataCsv_EmailField_test(){
        PeopleRelationJDO pro1 = new PeopleRelationJDO();
        pro1.setEmailID("email1");
        PeopleRelationJDO pro2 = new PeopleRelationJDO();
        pro2.setEmailID("email2");
        List<PeopleRelationJDO> stafflist = new ArrayList<>(){{
            add(pro1);
            add(pro2);
        }};
        String actual = UserDownloadHelper.generateStaffDataCsv(stafflist,"");
        Assertions.assertEquals("Email\r\nemail1\r\nemail2\r\n",actual);
    }
}
