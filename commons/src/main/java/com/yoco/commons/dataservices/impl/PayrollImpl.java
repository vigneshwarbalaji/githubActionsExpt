package com.yoco.commons.dataservices.impl;

import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.dao.PayrollDao;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.events.SchedulingEngineUtil;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

public class PayrollImpl implements PayrollDao {

    public static PayrollImpl getInstance(){
        return new PayrollImpl();
    }

    @Override
    public Map<String,Object> getPayrollEvents(String accountID, String paymentStatus,String startDateTime,String endDateTime,String cursor, int limit) throws IOException, NoSuchAlgorithmException {
        Map<String, Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.MERCHANT_ID, accountID);
        queryMap.put(SchedulingKeys.BRAND, DcmConstants.YOCO_BRAND_ID);
        queryMap.put(SchedulingKeys.PAYMENT_STATUS, paymentStatus);
        queryMap.put(SchedulingKeys.START_DATE_TIME, startDateTime);
        queryMap.put(SchedulingKeys.END_DATE_TIME, endDateTime);

        if(limit > 0)
            queryMap.put(SchedulingKeys.LIMIT, limit);

        if(!ObjUtils.isNullOrEmpty(cursor)){
            queryMap.put(SchedulingKeys.CURSOR_STR,cursor);
        }

        return SchedulingEngineUtil.getEventByQueryString(queryMap);
    }

    @Override
    public Map<String, Object> getPayrollEventsBasedOnStatus(String accountID, String status, String paymentStatus, String startDateTime, String endDateTime) throws IOException, NoSuchAlgorithmException {
        Map<String, Object> queryMap = new HashMap<>();
        queryMap.put(SchedulingKeys.PAYMENT_STATUS, paymentStatus);
        queryMap.put(SchedulingKeys.START_DATE_TIME, startDateTime);
        queryMap.put(SchedulingKeys.END_DATE_TIME, endDateTime);
        queryMap.put(SchedulingKeys.BRAND, DcmConstants.YOCO_BRAND_ID);
        queryMap.put(SchedulingKeys.MERCHANT_ID, accountID);
        queryMap.put(SchedulingKeys.STATUS, status);
        return SchedulingEngineUtil.getEventByQueryString(queryMap);
    }
}
