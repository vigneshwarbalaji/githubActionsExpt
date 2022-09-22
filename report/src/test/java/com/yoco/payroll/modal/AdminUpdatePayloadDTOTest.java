package com.yoco.payroll.modal;

import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.utils.DateUtil;
import com.yoco.payroll.helper.adminupdate.PayrollAdminUpdateHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

class AdminUpdatePayloadDTOTest {
    @Test
    void AdminUpdatePayloadDTO_invalidPayload_shouldThrowAnException(){
        try{
            new AdminUpdatePayloadDTO(null,"eID","123",new PeopleRelationJDO());
        }catch(Exception e){
            Assertions.assertEquals("Payload cannot be empty or null.",e.getMessage());
        }
    }

    @Test
    void AdminUpdatePayloadDTO_nullMessage_shouldThrowAnException(){
        try{
            new AdminUpdatePayloadDTO("{\"message\":null}","eID","123",new PeopleRelationJDO());
        }catch(Exception e){
            Assertions.assertEquals("Invalid description",e.getMessage());
        }
    }

    @Test
    void AdminUpdatePayloadDTO_nullIntime_shouldThrowAnException(){
        try{
            new AdminUpdatePayloadDTO("{\"message\":\"test\"}","eID","123",new PeopleRelationJDO());
        }catch(Exception e){
            Assertions.assertEquals("Invalid date format",e.getMessage());
        }
    }

    @Test
    void AdminUpdatePayloadDTO_IntimeEqualsToOuttime_shouldThrowAnException(){
        PeopleRelationJDO requesterPro = new PeopleRelationJDO();
        requesterPro.setTimeZone("Asia/Kolkata");
        try{
            new AdminUpdatePayloadDTO("{\"message\":\"test\",\"inTime\":\"10-Sep-2022 12:00:00 AM\",\"outTime\":\"10-Sep-2022 12:00:00 AM\"}","eID","123",requesterPro);
        }catch(Exception e){
            Assertions.assertEquals("Out time must be greater than in time",e.getMessage());
        }
    }

    @Test
    void AdminUpdatePayloadDTO_IntimeGreaterThanOuttime_shouldThrowAnException(){
        PeopleRelationJDO requesterPro = new PeopleRelationJDO();
        requesterPro.setTimeZone("Asia/Kolkata");
        try{
            new AdminUpdatePayloadDTO("{\"message\":\"test\",\"inTime\":\"10-Sep-2022 12:10:00 AM\",\"outTime\":\"10-Sep-2022 12:00:00 AM\"}","eID","123",requesterPro);
        }catch(Exception e){
            Assertions.assertEquals("Out time must be greater than in time",e.getMessage());
        }
    }

    @Test
    void AdminUpdatePayloadDTO_OuttimeGreaterThanCurrentTime_shouldThrowAnException(){
        PeopleRelationJDO requesterPro = new PeopleRelationJDO();
        requesterPro.setTimeZone("Asia/Kolkata");
        try{
            String outTime = DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY_HH_MM_SS_A,DateUtil.getCurrentTime()+10000L,"Asia/Kolkata");
            new AdminUpdatePayloadDTO("{\"message\":\"test\",\"inTime\":\"10-Sep-2022 12:10:00 AM\",\"outTime\":\""+outTime+"\"}","eID","123",requesterPro);
        }catch(Exception e){
            Assertions.assertEquals("Adjusted time must be earlier than current time.",e.getMessage());
        }
    }

    @Test
    void AdminUpdatePayloadDTO_valid_adminReupdateRequestFalse() throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO requesterPro = new PeopleRelationJDO();
        requesterPro.setTimeZone("Asia/Kolkata");
        try(MockedConstruction<PayrollAdminUpdateHelper> payrollAdminUpdateHelperMockedConstruction = Mockito.mockConstruction(PayrollAdminUpdateHelper.class,(mock,context)->{
            Mockito.when(mock.validateAuthorizationAndGetEvent("eID","123",requesterPro,false)).thenReturn(Map.of());
        })){
            AdminUpdatePayloadDTO payloadDTO = new AdminUpdatePayloadDTO("{\"message\":\"test\",\"inTime\":\"10-Sep-2022 12:10:00 AM\",\"outTime\":\"10-Sep-2022 01:10:00 AM\"}","eID","123",requesterPro);
            Assertions.assertEquals(Map.of(),payloadDTO.getEvent());
            Assertions.assertFalse(payloadDTO.isReUpdateRequest());
        }
    }

    @Test
    void AdminUpdatePayloadDTO_valid_adminReupdateRequestTrue() throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO requesterPro = new PeopleRelationJDO();
        requesterPro.setTimeZone("Asia/Kolkata");
        try(MockedConstruction<PayrollAdminUpdateHelper> payrollAdminUpdateHelperMockedConstruction = Mockito.mockConstruction(PayrollAdminUpdateHelper.class,(mock,context)->{
            Mockito.when(mock.validateAuthorizationAndGetEvent("eID","123",requesterPro,true)).thenReturn(Map.of());
        })){
            AdminUpdatePayloadDTO payloadDTO = new AdminUpdatePayloadDTO("{\"message\":\"test\",\"inTime\":\"10-Sep-2022 12:10:00 AM\",\"outTime\":\"10-Sep-2022 01:10:00 AM\",\"isReUpdateRequest\":true}","eID","123",requesterPro);
            Assertions.assertEquals(Map.of(),payloadDTO.getEvent());
            Assertions.assertTrue(payloadDTO.isReUpdateRequest());
        }
    }
}
