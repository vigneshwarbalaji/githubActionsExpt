package com.yoco.downloads.payroll.service;

import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.dataservices.impl.PayrollImpl;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.RangeValidator;
import com.yoco.commons.validations.Validator;
import com.yoco.downloads.payroll.helper.PayrollDownloadHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

@Slf4j
@Service
public class PayrollDownloadService {

    public Map<String,Object> downloadPayrollEvents(String accountID, String zone, String status, String from, String to,String cursor) throws IOException, NoSuchAlgorithmException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(status), COMMON_ERROR_RESPONSE.INVALID_STATUS.value());
        RangeValidator.validateRange(DateConstants.BY_DATE, from, to);

        var rangeInfoDTO =  DateUtil.getRangeDetails(zone,DateConstants.BY_DATE,from,to,accountID);
        String fromDateTimeText = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getFromDateEpochMilliseconds(),zone);
        String toDateTimeText   = DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getToDateEpochMilliseconds(),zone);

        Map<String,Object> payrollEventsResp = PayrollImpl.getInstance().getPayrollEvents(accountID, status, fromDateTimeText, toDateTimeText, cursor, 1000);

        return PayrollDownloadHelper.getPayrollEvents(payrollEventsResp,zone,status);
    }
}
