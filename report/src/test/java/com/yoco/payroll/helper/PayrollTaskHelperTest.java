package com.yoco.payroll.helper;

import com.yoco.MockEvent;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class PayrollTaskHelperTest {

    @Test
    void handlePayrollApproval_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<PayrollHelper> payrollHelperMockedStatic = Mockito.mockStatic(PayrollHelper.class)){
            PayrollHelper payrollHelperMock = Mockito.mock(PayrollHelper.class);
            Mockito.when(payrollHelperMock.getPayrollEvents(anyList())).thenReturn(null);
            payrollHelperMockedStatic.when(PayrollHelper::getInstance).thenReturn(payrollHelperMock);
            Contact contact = new Contact();
            PayrollTaskHelper.handlePayrollApproval(contact,List.of("id1"),"HH MM");
            Mockito.verify(payrollHelperMock).getPayrollEvents(List.of("id1"));
        }
    }

    @Test
    void handlePayrollApproval_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<PayrollHelper> payrollHelperMockedStatic = Mockito.mockStatic(PayrollHelper.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class);
            MockedStatic<PayrollEmailHelper> payrollEmailHelperMockedStatic = Mockito.mockStatic(PayrollEmailHelper.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){

            Map<String,Object> event = MockEvent.getRawEvent();
            List<Map<String, Object>> payrollEventsList = new ArrayList<>();
            payrollEventsList.add(event);
            PayrollHelper payrollHelperMock = Mockito.mock(PayrollHelper.class);
            Mockito.when(payrollHelperMock.getPayrollEvents(anyList())).thenReturn(payrollEventsList);
            payrollHelperMockedStatic.when(PayrollHelper::getInstance).thenReturn(payrollHelperMock);

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accId");
            userPro.setDelete(false);
            userPro.setContactId("contId");
            userPro.setEmailID("test@gmail.com");
            userPro.setTimeZone(DateConstants.ZONE_ID_IST);
            Contact contact = new Contact();
            contact.setId("contId");
            userPro.setContact(contact);

            reportsUtilMockedStatic.when(()-> ReportsUtil.extractUserProFromEvent(anyMap())).thenReturn(userPro);
            reportsUtilMockedStatic.when(()-> ReportsUtil.getIDFromEvent(anyMap())).thenCallRealMethod();

            PayrollEmailHelper payrollEmailHelper = Mockito.mock(PayrollEmailHelper.class);
            Map<String,Object> sessionMap = new HashMap<>();
            sessionMap.put("session","sessionDetails");
            sessionMap.put("duration",0l);
            Mockito.when(payrollEmailHelper.buildSessionContentForMail(anyMap(),anyString(),anyString())).thenReturn(sessionMap);
            Mockito.doNothing().when(payrollEmailHelper).sendApprovalMail(anyString(),any(Contact.class),any(Contact.class),anyString(),anyString());
            payrollEmailHelperMockedStatic.when(PayrollEmailHelper::getInstance).thenReturn(payrollEmailHelper);

            activityUtilMockedStatic.when(()-> ActivityUtil.saveActivities(anyList())).thenAnswer((Answer<Void>) invocation -> null);

            PayrollTaskHelper.handlePayrollApproval(contact,List.of("id1"),"HH MM");

            Mockito.verify(payrollHelperMock).getPayrollEvents(List.of("id1"));
            reportsUtilMockedStatic.verify(()-> ReportsUtil.extractUserProFromEvent(event));
            Mockito.verify(payrollEmailHelper).buildSessionContentForMail(event,"HH MM",DateConstants.ZONE_ID_IST);
            activityUtilMockedStatic.verify(()-> ActivityUtil.saveActivities(anyList()),times(1));
            Mockito.verify(payrollEmailHelper).sendApprovalMail("accId",contact,contact,"sessionDetails","0h 00m");
        }
    }

    @Test
    void handlePayrollApproval_deletedUser_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<PayrollHelper> payrollHelperMockedStatic = Mockito.mockStatic(PayrollHelper.class);
            MockedStatic<ReportsUtil> reportsUtilMockedStatic = Mockito.mockStatic(ReportsUtil.class);
            MockedStatic<PayrollEmailHelper> payrollEmailHelperMockedStatic = Mockito.mockStatic(PayrollEmailHelper.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){

            Map<String,Object> event = MockEvent.getRawEvent();
            List<Map<String, Object>> payrollEventsList = new ArrayList<>();
            payrollEventsList.add(event);
            PayrollHelper payrollHelperMock = Mockito.mock(PayrollHelper.class);
            Mockito.when(payrollHelperMock.getPayrollEvents(anyList())).thenReturn(payrollEventsList);
            payrollHelperMockedStatic.when(PayrollHelper::getInstance).thenReturn(payrollHelperMock);

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accId");
            userPro.setDelete(true);
            userPro.setContactId("contId");
            userPro.setEmailID("test@gmail.com");
            userPro.setTimeZone(DateConstants.ZONE_ID_IST);
            Contact contact = new Contact();
            contact.setId("contId");
            userPro.setContact(contact);

            reportsUtilMockedStatic.when(()-> ReportsUtil.extractUserProFromEvent(anyMap())).thenReturn(userPro);
            reportsUtilMockedStatic.when(()-> ReportsUtil.getIDFromEvent(anyMap())).thenCallRealMethod();

            PayrollEmailHelper payrollEmailHelper = Mockito.mock(PayrollEmailHelper.class);
            payrollEmailHelperMockedStatic.when(PayrollEmailHelper::getInstance).thenReturn(payrollEmailHelper);

            activityUtilMockedStatic.when(()-> ActivityUtil.saveActivities(anyList())).thenAnswer((Answer<Void>) invocation -> null);

            PayrollTaskHelper.handlePayrollApproval(contact,List.of("id1"),"HH MM");

            Mockito.verify(payrollHelperMock).getPayrollEvents(List.of("id1"));
            reportsUtilMockedStatic.verify(()-> ReportsUtil.extractUserProFromEvent(event));
            Mockito.verifyNoInteractions(payrollEmailHelper);
            activityUtilMockedStatic.verify(()-> ActivityUtil.saveActivities(anyList()),times(1));
        }
    }

}