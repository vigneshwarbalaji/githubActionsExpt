package com.yoco.commons.dataservices.dao;

import com.yoco.commons.entity.FcmJDO;
import com.yoco.commons.modal.CollectionResponse;
import java.util.List;
import java.util.Set;

public interface FcmDao {

     FcmJDO saveFcm(FcmJDO fcmJDO);

     List<FcmJDO> getAllDevicesOfContact(String contactId, int limit, String cursor);

     List<FcmJDO> getAllDevicesOfContactUnderAccount(String accountId, String contactId, int limit, String cursor);

     List<String> getFcmDeviceToken(Set<String> contacts, String accountID);

     FcmJDO getFcmDeviceInfoById(String deviceId);

     void deleteDevice(FcmJDO fcmObject);
     
     CollectionResponse<FcmJDO> getAllDevicesUnderAccount(String accountId, int limit, String cursor);
}
