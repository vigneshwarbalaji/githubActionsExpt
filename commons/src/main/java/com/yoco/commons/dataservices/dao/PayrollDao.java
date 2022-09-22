package com.yoco.commons.dataservices.dao;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

public interface PayrollDao {
    Map<String,Object> getPayrollEvents(String accountID, String paymentStatus, String startDateTime, String endDateTime, String cursor, int limit) throws IOException, NoSuchAlgorithmException;
    Map<String,Object> getPayrollEventsBasedOnStatus(String accountID, String status, String paymentStatus, String startDateTime, String endDateTime) throws IOException, NoSuchAlgorithmException;
}
