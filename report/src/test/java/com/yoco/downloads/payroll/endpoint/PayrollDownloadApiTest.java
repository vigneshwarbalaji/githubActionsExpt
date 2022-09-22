package com.yoco.downloads.payroll.endpoint;

import com.yoco.commons.modal.GenericResponse;
import com.yoco.downloads.payroll.service.PayrollDownloadService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;

class PayrollDownloadApiTest {

    @Test
    void downloadPayrollEvents_validResponse_test(){
        Map<String,Object> mockResponse = new HashMap<>();
        mockResponse.put("data","dummy");
        try (MockedConstruction<PayrollDownloadService> mock = Mockito.mockConstruction(PayrollDownloadService.class, (payrollDownloadServiceMock, context) -> {
            Mockito.when(payrollDownloadServiceMock.downloadPayrollEvents(any(),any(),any(),any(),any(),any())).thenReturn(mockResponse);
        })){
            ResponseEntity<GenericResponse> responseEntity = new PayrollDownloadApi().downloadPayrollEvents("accountId", "confirmed","zone","06/20/2022","06/20/2022","");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.setData(mockResponse);
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.OK,responseEntity.getStatusCode());
            Assertions.assertEquals(200,responseEntity.getStatusCode().value());
        }
    }

    @Test
    void downloadPayrollEvents_exception_test(){
        try (MockedConstruction<PayrollDownloadService> mock = Mockito.mockConstruction(PayrollDownloadService.class, (payrollDownloadServiceMock, context) -> {
            Mockito.when(payrollDownloadServiceMock.downloadPayrollEvents(any(),any(),any(),any(),any(),any())).thenThrow(new IllegalArgumentException("exception"));
        })){
            ResponseEntity<GenericResponse> responseEntity = new PayrollDownloadApi().downloadPayrollEvents("accountId", "confirmed","zone","06/20/2022","06/20/2022","");
            var genericResponse = new GenericResponse(false, null, "exception");
            Assertions.assertEquals(genericResponse,responseEntity.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,responseEntity.getStatusCode());
            Assertions.assertEquals(400,responseEntity.getStatusCode().value());
        }
    }

}