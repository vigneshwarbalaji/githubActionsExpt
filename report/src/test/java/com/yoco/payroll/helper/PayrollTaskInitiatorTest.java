package com.yoco.payroll.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.constants.EventConstants;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.List;
import java.util.Map;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class PayrollTaskInitiatorTest {

    @Test
    void initiateApprovePayrollQueue_test() throws IOException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAppUrl).thenReturn("https://url");
            taskCreatorMockedStatic.when(() -> TaskCreator.createPostTask(anyString(),anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            Contact adminContact = new Contact();
            List<String> entryIds =  List.of("id1","id2");
            PayrollTaskInitiator.initiateApprovePayrollQueue(adminContact,entryIds,"HH MM");

            var byteOut = new ByteArrayOutputStream();
            var out = new ObjectOutputStream(byteOut);
            out.writeObject(Map.of("adminContact",adminContact,"eventIds",entryIds,"timeFormat","HH MM"));
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("payroll-operation","https://url/task/payroll/approve",byteOut.toByteArray()));
        }
    }

    @Test
    void initiateAdminUpdateHandlerQueue_ShouldCallCreatePostTaskMethod_test() throws IOException {
        try(MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class);
            MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){
            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAppUrl).thenReturn("url");
            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            PayrollTaskInitiator.initiateAdminUpdateHandlerQueue(adminPro,userPro,adjustmentDTO);
            Map<String,Object> payloadMap = Map.of("adminPro", adminPro, "userPro", userPro, EventConstants.ADJUSTMENT,adjustmentDTO);
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("payroll-operation","url/task/payroll/admin-update/handler", CloudTaskUtil.convertObjectToByteArray(payloadMap)));
        }
    }

}