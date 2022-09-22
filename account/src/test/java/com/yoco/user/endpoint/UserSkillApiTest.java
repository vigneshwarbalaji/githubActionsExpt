package com.yoco.user.endpoint;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.user.service.UserSkillService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;

class UserSkillApiTest {

    @ParameterizedTest
    @NullAndEmptySource
    void modifyWeeklySkill_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyWeeklySkill(any(),any(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyWeeklySkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null,COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyWeeklySkill_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyWeeklySkill(any(),any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyWeeklySkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyWeeklySkill_Exception_test(){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyWeeklySkill(any(),any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyWeeklySkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyWeeklySkill_success_false_test(){
        Map<String,Object> mockResp = new HashMap<>();
        mockResp.put(Commons.SUCCESS,false);
        mockResp.put(Commons.MESSAGE,"errorMessage");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyWeeklySkill(any(),any(),any())).thenReturn(mockResp);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyWeeklySkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "errorMessage");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }


    @ParameterizedTest
    @NullAndEmptySource
    void modifyClockSkill_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyClockSkill(any(),any(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyClockSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null,COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyClockSkill_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyClockSkill(any(),any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyClockSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyClockSkill_Exception_test(){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyClockSkill(any(),any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyClockSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyClockSkill_success_false_test(){
        Map<String,Object> mockResp = new HashMap<>();
        mockResp.put(Commons.SUCCESS,false);
        mockResp.put(Commons.MESSAGE,"errorMessage");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyClockSkill(any(),any(),any())).thenReturn(mockResp);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyClockSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "errorMessage");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void modifyForceClockOutSkill_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyForceClockOutSkill(any(),any(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyForceClockOutSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null,COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyForceClockOutSkill_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyForceClockOutSkill(any(),any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyForceClockOutSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyForceClockOutSkill_Exception_test(){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyForceClockOutSkill(any(),any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyForceClockOutSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyForceClockOutSkill_success_false_test(){
        Map<String,Object> mockResp = new HashMap<>();
        mockResp.put(Commons.SUCCESS,false);
        mockResp.put(Commons.MESSAGE,"errorMessage");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyForceClockOutSkill(any(),any(),any())).thenReturn(mockResp);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyForceClockOutSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "errorMessage");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void modifyReportSkill_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyReportSkill(any(),any(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyReportSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null,COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyReportSkill_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyReportSkill(any(),any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyReportSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyReportSkill_Exception_test(){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyReportSkill(any(),any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyReportSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyReportSkill_success_false_test(){
        Map<String,Object> mockResp = new HashMap<>();
        mockResp.put(Commons.SUCCESS,false);
        mockResp.put(Commons.MESSAGE,"errorMessage");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyReportSkill(any(),any(),any())).thenReturn(mockResp);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyReportSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "errorMessage");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void modifyAdjustmentSkill_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyAdjustmentSkill(any(),any(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyAdjustmentSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null,COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyAdjustmentSkill_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyAdjustmentSkill(any(),any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyAdjustmentSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyAdjustmentSkill_Exception_test(){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyAdjustmentSkill(any(),any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyAdjustmentSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyAdjustmentSkill_success_false_test(){
        Map<String,Object> mockResp = new HashMap<>();
        mockResp.put(Commons.SUCCESS,false);
        mockResp.put(Commons.MESSAGE,"errorMessage");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyAdjustmentSkill(any(),any(),any())).thenReturn(mockResp);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyAdjustmentSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "errorMessage");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void modifyActivitySkill_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyActivitySkill(any(),any(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyActivitySkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null,COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyActivitySkill_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyActivitySkill(any(),any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyActivitySkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyActivitySkill_Exception_test(){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyActivitySkill(any(),any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyActivitySkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyActivitySkill_success_false_test(){
        Map<String,Object> mockResp = new HashMap<>();
        mockResp.put(Commons.SUCCESS,false);
        mockResp.put(Commons.MESSAGE,"errorMessage");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyActivitySkill(any(),any(),any())).thenReturn(mockResp);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyActivitySkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "errorMessage");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void modifyConfirmHoursSkill_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyConfirmHoursSkill(any(),any(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyConfirmHoursSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null,COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyConfirmHoursSkill_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyConfirmHoursSkill(any(),any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyConfirmHoursSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyConfirmHoursSkill_Exception_test(){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyConfirmHoursSkill(any(),any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyConfirmHoursSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyConfirmHoursSkill_success_false_test(){
        Map<String,Object> mockResp = new HashMap<>();
        mockResp.put(Commons.SUCCESS,false);
        mockResp.put(Commons.MESSAGE,"errorMessage");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyConfirmHoursSkill(any(),any(),any())).thenReturn(mockResp);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyConfirmHoursSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "errorMessage");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void modifyTeamReportSkill_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyTeamReportSkill(any(),any(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyTeamReportSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null,COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyTeamReportSkill_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyTeamReportSkill(any(),any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyTeamReportSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyTeamReportSkill_Exception_test(){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyTeamReportSkill(any(),any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyTeamReportSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyTeamReportSkill_success_false_test(){
        Map<String,Object> mockResp = new HashMap<>();
        mockResp.put(Commons.SUCCESS,false);
        mockResp.put(Commons.MESSAGE,"errorMessage");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyTeamReportSkill(any(),any(),any())).thenReturn(mockResp);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyTeamReportSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "errorMessage");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void modifyReminderSkill_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyReminderSkill(any(),any(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyReminderSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null,COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyReminderSkill_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyReminderSkill(any(),any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyReminderSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyReminderSkill_Exception_test(){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyReminderSkill(any(),any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyReminderSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void modifyReminderSkill_success_false_test(){
        Map<String,Object> mockResp = new HashMap<>();
        mockResp.put(Commons.SUCCESS,false);
        mockResp.put(Commons.MESSAGE,"errorMessage");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.modifyReminderSkill(any(),any(),any())).thenReturn(mockResp);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().modifyReminderSkill("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "errorMessage");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void updateReminderTime_nullResponse_test(Map<String,Object> mockMap){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.updateReminderTime(any(),any(),any())).thenReturn(mockMap);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().updateReminderTime("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null,COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updateReminderTime_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.updateReminderTime(any(),any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().updateReminderTime("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updateReminderTime_Exception_test(){
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.updateReminderTime(any(),any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().updateReminderTime("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void updateReminderTime_success_false_test(){
        Map<String,Object> mockResp = new HashMap<>();
        mockResp.put(Commons.SUCCESS,false);
        mockResp.put(Commons.MESSAGE,"errorMessage");
        try (MockedConstruction<UserSkillService> mock = Mockito.mockConstruction(UserSkillService.class, (userSkillServiceMock, context) -> {
            Mockito.when(userSkillServiceMock.updateReminderTime(any(),any(),any())).thenReturn(mockResp);
        })){
            ResponseEntity<GenericResponse> responseEntity = new UserSkillApi().updateReminderTime("accountId", "contactId","payload");
            var genericResponse = new GenericResponse(false, null, "errorMessage");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

}