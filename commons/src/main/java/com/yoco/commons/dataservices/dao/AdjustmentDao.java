package com.yoco.commons.dataservices.dao;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;

public interface AdjustmentDao {

    Map<String, Object> getAllAdjustments(String accountID, String contactID, String startDate, String endDate, String status, String cursor) throws IOException, NoSuchAlgorithmException;

    Map<String, Object> getOverLapEntries(String accountID, List<String> calendarIds, String contactID,
                                          String startTime, String endTime, String entryIds) throws IOException, NoSuchAlgorithmException;
}
