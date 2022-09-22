package com.yoco.downloads.payroll.service;

import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.PayrollImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;

class PayrollDownloadServiceTest {

    PayrollDownloadService payrollDownloadService = new PayrollDownloadService();

    @ParameterizedTest
    @NullAndEmptySource
    void downloadPayrollEvents_invalid_status_test(String testValue){
        try {
            payrollDownloadService.downloadPayrollEvents("accId","zone",testValue,"from","to","");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_STATUS.value(),e.getMessage());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void downloadPayrollEvents_invalid_fromDate_test(String testValue){
        try {
            payrollDownloadService.downloadPayrollEvents("accId","zone","payrollStatus",testValue,"to","");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.FROM_AND_TO_DATE_MANDATORY.value(),e.getMessage());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void downloadPayrollEvents_invalid_toDate_test(String testValue){
        try {
            payrollDownloadService.downloadPayrollEvents("accId","zone","payrollStatus","06/30/2022",testValue,"");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.FROM_AND_TO_DATE_MANDATORY.value(),e.getMessage());
        }
    }

    @Test
    void downloadPayrollEvents_valid_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<PayrollImpl> reportMockedStatic = Mockito.mockStatic(PayrollImpl.class)){
            PayrollImpl reportImplMock = Mockito.mock(PayrollImpl.class);
            Mockito.when(reportImplMock.getPayrollEvents(anyString(), anyString(),anyString(),anyString(),anyString(), anyInt())).thenReturn(Map.of());
            reportMockedStatic.when(PayrollImpl::getInstance).thenReturn(reportImplMock);
            Map<String,Object> response = payrollDownloadService.downloadPayrollEvents("accId","zone","payrollStatus","06/30/2022","06/30/2022","");
            Assertions.assertEquals(Map.of(SchedulingKeys.ENTRIES,List.of()),response);
        }
    }

}