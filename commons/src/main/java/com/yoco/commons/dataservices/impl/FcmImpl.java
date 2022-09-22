package com.yoco.commons.dataservices.impl;

import com.googlecode.objectify.cmd.Query;
import com.yoco.commons.dataservices.dao.FcmDao;
import com.yoco.commons.dataservices.objectify.OfyService;
import com.yoco.commons.entity.FcmJDO;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.utils.ObjUtils;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import java.util.*;

@Slf4j
public class FcmImpl extends OfyService implements FcmDao {

    @NoArgsConstructor
    private enum FCM_CONSTANTS {
        CONTACT_ID("contactID"),
        ACCOUNT_ID("accountID");

        private String value;

        FCM_CONSTANTS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    public static FcmImpl getFcmImplInstance(){
        return new FcmImpl();
    }

    public FcmJDO saveFcm(FcmJDO fcmJDO){
        save(fcmJDO);
        return fcmJDO;
    }

    public List<FcmJDO> getAllDevicesOfContact(String contactId, int limit, String cursor){

        List<FcmJDO> response = new ArrayList<>();

        Query<FcmJDO> query = ofy().load().type(FcmJDO.class)
                .filter(FCM_CONSTANTS.CONTACT_ID.value(),contactId);

        CollectionResponse<FcmJDO> collectionResponse = fetchCursorQuery(query, limit, cursor);

        if(!ObjUtils.isNull(collectionResponse)){
            response.addAll(collectionResponse.getItems());
        }

        return response;
    }

    public List<FcmJDO> getAllDevicesOfContactUnderAccount(String accountId, String contactId, int limit, String cursor){

        log.info(" accountID " + accountId + " contactId " + contactId + " limit " + limit);

        List<FcmJDO> response = new ArrayList<>();

        Query<FcmJDO> query = ofy().load().type(FcmJDO.class)
                .filter(FCM_CONSTANTS.ACCOUNT_ID.value(),accountId)
                .filter(FCM_CONSTANTS.CONTACT_ID.value(), contactId);

        CollectionResponse<FcmJDO> collectionResponse = fetchCursorQuery(query, limit, cursor);

        if(!ObjUtils.isNull(collectionResponse)){
            response.addAll(collectionResponse.getItems());
        }

        return response;
    }

    @Override
    public List<String> getFcmDeviceToken(Set<String> contacts, String accountID) {

        List<String> fcmDeviceTokenList = new ArrayList<>();

        List<FcmJDO> fcmJDOList = new ArrayList<>();

        contacts.stream().forEach( contact -> {
            List<FcmJDO> lFcmList = getAllDevicesOfContactUnderAccount(accountID,contact,50,null);
            if (!ObjUtils.isEmptyList(lFcmList)) {
                fcmJDOList.addAll(lFcmList);
            }
        });

        if(!ObjUtils.isEmptyList(fcmJDOList)){
            fcmJDOList.stream().forEach( fcmJDO -> fcmDeviceTokenList.add(fcmJDO.getFcmDeviceToken()) );
        }

        return fcmDeviceTokenList;
    }

    @Override
    public FcmJDO getFcmDeviceInfoById(String deviceId) {
        return get(FcmJDO.class, deviceId);
    }

    @Override
    public void deleteDevice(FcmJDO fcmObject) {
        delete(fcmObject);
    }

    @Override
    public CollectionResponse<FcmJDO> getAllDevicesUnderAccount(String accountId, int limit, String cursor){
        Query<FcmJDO> query = ofy().load().type(FcmJDO.class)
                .filter(FCM_CONSTANTS.ACCOUNT_ID.value(),accountId);
        return fetchCursorQuery(query, limit, cursor);
    }
}
