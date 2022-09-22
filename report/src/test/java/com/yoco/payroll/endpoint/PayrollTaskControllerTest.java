package com.yoco.payroll.endpoint;

import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.payroll.helper.PayrollTaskHelper;
import com.yoco.payroll.helper.adminupdate.PayrollAdminUpdateTaskHelper;
import freemarker.template.TemplateException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.mockito.ArgumentMatchers.*;

class PayrollTaskControllerTest {

    @Test
    void handlePayrollApproval_valid_test() throws IOException {
        try(MockedStatic<PayrollTaskHelper> payrollTaskHelperMockedStatic = Mockito.mockStatic(PayrollTaskHelper.class)) {
            payrollTaskHelperMockedStatic.when(()-> PayrollTaskHelper.handlePayrollApproval(any(Contact.class),anyList(),anyString())).thenAnswer((Answer<Void>) invocation -> null);
            Map<String,Object> payload = new HashMap<>();
            Contact adminContact = new Contact();
            payload.put("adminContact",adminContact);
            payload.put("eventIds", List.of("id1","id2"));
            payload.put("timeFormat","HH MM");
            new PayrollTaskController().handlePayrollApproval(CloudTaskUtil.convertObjectToByteArray(payload));
            payrollTaskHelperMockedStatic.verify(()->PayrollTaskHelper.handlePayrollApproval(adminContact, List.of("id1","id2"),"HH MM"));
        }
    }

    @Test
    void handlePayrollApproval_exception_test() throws IOException {
        try(MockedStatic<PayrollTaskHelper> payrollTaskHelperMockedStatic = Mockito.mockStatic(PayrollTaskHelper.class)) {
            payrollTaskHelperMockedStatic.when(()-> PayrollTaskHelper.handlePayrollApproval(any(Contact.class),anyList(),anyString())).thenThrow(new IOException());
            Map<String,Object> payload = new HashMap<>();
            Contact adminContact = new Contact();
            payload.put("adminContact",adminContact);
            payload.put("eventIds", List.of("id1","id2"));
            payload.put("timeFormat","HH MM");
            new PayrollTaskController().handlePayrollApproval(CloudTaskUtil.convertObjectToByteArray(payload));
            payrollTaskHelperMockedStatic.verify(()->PayrollTaskHelper.handlePayrollApproval(adminContact, List.of("id1","id2"),"HH MM"));
        }
    }

    @Test
    void handleAdminUpdateEntry_valid_shouldCallHandleAdminUpdateMethod_test() throws IOException, TemplateException {
        try(MockedConstruction<PayrollAdminUpdateTaskHelper> payrollAdminUpdateTaskHelperMockedConstruction = Mockito.mockConstruction(PayrollAdminUpdateTaskHelper.class)) {
            Map<String,Object> payload = new HashMap<>();
            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            payload.put("adminPro",adminPro);
            payload.put("userPro", userPro);
            payload.put("adjustment",adjustmentDTO);
            new PayrollTaskController().handleAdminUpdateEntry(CloudTaskUtil.convertObjectToByteArray(payload));
            Mockito.verify(payrollAdminUpdateTaskHelperMockedConstruction.constructed().get(0)).handleAdminUpdate(adminPro,userPro,adjustmentDTO);
        }
    }

    @Test
    void handleAdminUpdateEntry_ExceptionThrown_ShouldNotCallAdminUpdateMethod_test() throws IOException {
        try(MockedConstruction<PayrollAdminUpdateTaskHelper> payrollAdminUpdateTaskHelperMockedConstruction = Mockito.mockConstruction(PayrollAdminUpdateTaskHelper.class)) {
            new PayrollTaskController().handleAdminUpdateEntry(CloudTaskUtil.convertObjectToByteArray(List.of()));
            Assertions.assertEquals(0,payrollAdminUpdateTaskHelperMockedConstruction.constructed().size());
        }
    }
}