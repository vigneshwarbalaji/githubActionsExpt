package com.yoco.commons.dataservices.dao;

import com.yoco.commons.modal.user.UserDTO;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeoutException;

public interface ReportDao {
    Map<String, Object> getEntryByID(String entryID) throws IOException, NoSuchAlgorithmException;
    List<Map<String,Object>> getUserEntriesByTaskID(String accountID, String contactID, String taskID) throws IOException, TimeoutException, NoSuchAlgorithmException;
    List<Map<String,Object>> getActiveEntry(String accountID, String contactID) throws IOException, NoSuchAlgorithmException;
    List<Map<String,Object>> getEntries(String accountID, String contactID, String start, String end, String order, boolean isDeleted) throws IOException, NoSuchAlgorithmException;
    Map<String, Object> getLastClockedOutEntry(String accountID, String contactID) throws IOException, NoSuchAlgorithmException;
    List<Map<String,Object>> getEntriesForCurrentDay(UserDTO userPro) throws IOException, NoSuchAlgorithmException;
    List<Map<String,Object>> getAllClockedInEntriesForAccount(String accountID) throws IOException, NoSuchAlgorithmException;
    List<Map<String,Object>> deleteEvents(List<String> eventIDs) throws IOException, NoSuchAlgorithmException;
    Map<String,Object> getAllEntriesOfAccount(String accountID,String startDateTime,String endDateTime,int limit,String cursor) throws IOException, NoSuchAlgorithmException;
    Map<String, Object> getEventsByIds(List<String> entryIDs) throws IOException, NoSuchAlgorithmException;
    Map<String, Object> revertEvent(String entryID) throws IOException, NoSuchAlgorithmException;
    Map<String, Object> getPreviousClockedOutEntryFromGivenTime(String accountID, String contactID, String endDateTime) throws IOException, NoSuchAlgorithmException;
    Map<String,Object> updateEntries(String accountID,List<Map<String,Object>> eventsList) throws IOException, NoSuchAlgorithmException;
}
