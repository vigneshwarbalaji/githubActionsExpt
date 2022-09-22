package com.yoco.commons.services;

import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.dataservices.dao.FcmDao;
import com.yoco.commons.dataservices.impl.FcmImpl;
import com.yoco.commons.entity.FcmJDO;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.json.JSONException;
import org.json.JSONObject;


import java.io.IOException;
import java.util.*;

@Slf4j
public class FCMService {
    public static FCMService getFCMService() {
        return new FCMService();
    }

    @NoArgsConstructor
    public enum FCM_SERVICE_CONSTANTS {
        IS_DATA_UPDATED("isDataUpdated"),
        ERROR_MESSAGE("Error Message"),
        IS_FCM_TOKEN_AVAILABLE ("isFCMDeviceTokenAvailable"),
        FCM_CLOCK_IN_OUT("clockInOut"),
        FCM_TIME_ZONE("timeZone"),
        FCM_PROJECT("project"),
        FCM_PROFILE("profile"),
        FCM_DELETE_ACCOUNT("deleteAccount"),
        FCM_DEVICE_TOKEN("fcmDeviceToken");

        private String value;

        FCM_SERVICE_CONSTANTS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    private  static final HashMap<String, String> COLLAPSE_KEY = new HashMap<>();

    static {
        COLLAPSE_KEY.put(FCM_SERVICE_CONSTANTS.FCM_CLOCK_IN_OUT.value(), "CLOCK_STATUS");
        COLLAPSE_KEY.put(FCM_SERVICE_CONSTANTS.FCM_TIME_ZONE.value(), "TIME_ZONE");
        COLLAPSE_KEY.put(FCM_SERVICE_CONSTANTS.FCM_PROJECT.value(), "PROJECT");
        COLLAPSE_KEY.put(FCM_SERVICE_CONSTANTS.FCM_PROFILE.value(), "PROFILE");
    }

    static FcmDao fcmDao = FcmImpl.getFcmImplInstance();

    public Map<String, Object> updateFcmSyncObject(String accountID, Set<String> contacts, String syncKey, Object syncValue) {

        Map<String,Object> response = new HashMap<>();

        try{
            log.info(" in updateFcmSyncObject ");
            List<FcmJDO> fcmJDOList = this.getFcmSyncList(accountID,contacts,syncKey);

            response = this.updateFcmSyncList(fcmJDOList,syncKey,syncValue);

        }catch (Exception e){
            log.info(e.getMessage());
            response.put(FCM_SERVICE_CONSTANTS.IS_DATA_UPDATED.value(),false);
            response.put(FCM_SERVICE_CONSTANTS.ERROR_MESSAGE.value(),e.getMessage());
        }
        return response;
    }

    public List<FcmJDO> getFcmSyncList(String accountID, Set<String> contacts, String syncKey){

        List<FcmJDO> fcmJDOList = new ArrayList<>();
        var limit = 50;

        if (FCM_SERVICE_CONSTANTS.FCM_PROFILE.value().equalsIgnoreCase(syncKey)){
            contacts.stream().forEach( contact -> {
                List<FcmJDO> fcmList =  fcmDao.getAllDevicesOfContact(contact,limit,null);
                if(!ObjUtils.isEmptyList(fcmList)){
                    fcmJDOList.addAll(fcmList);
                }
            });
        }else{
            contacts.stream().forEach( contact -> {
                List<FcmJDO> fcmList =  fcmDao.getAllDevicesOfContactUnderAccount(accountID,contact,limit,null);
                if(!ObjUtils.isEmptyList(fcmList)){
                    fcmJDOList.addAll(fcmList);
                }
            });
        }

        return fcmJDOList;
    }

    public Map<String,Object> updateFcmSyncList(List<FcmJDO> fcmJDOList, String syncKey, Object syncValue) throws IOException,JSONException {

        Map<String,Object> response = new HashMap<>();

        if (!ObjUtils.isEmptyList(fcmJDOList)) {

            var lSyncAsString = "";

            log.info(" fcmJdo size :: " + fcmJDOList.size());

            for (FcmJDO fcmJDO : fcmJDOList) {

                log.info(" deviceModel " + fcmJDO.getDeviceModel());

                lSyncAsString = fcmJDO.getSyncObject();

                Set<Object> lKeyValues;
                JSONObject lSyncObject = ObjUtils.isNullOrEmpty(lSyncAsString) ?  new JSONObject() : new JSONObject(lSyncAsString);

                if (!ObjUtils.isNullOrEmpty(lSyncAsString) && syncValue instanceof List && lSyncObject.has(syncKey)) {
                    log.info(" key exists ");
                    lKeyValues = JsonUtil.convertJsonToSet(lSyncObject.get(syncKey).toString());
                    lKeyValues.addAll((List<Object>)syncValue);
                    lSyncObject.put(syncKey, lKeyValues);
                }else {
                    lSyncObject.put(syncKey, syncValue);
                }

                fcmJDO.setSyncObject(lSyncObject.toString());
                fcmDao.saveFcm(fcmJDO);
            }

            response.put(FCM_SERVICE_CONSTANTS.IS_DATA_UPDATED.value(),true);

        }else{
            log.info("There is no device available for these users ");
            response.put(FCM_SERVICE_CONSTANTS.IS_DATA_UPDATED.value(),false);
            response.put(FCM_SERVICE_CONSTANTS.ERROR_MESSAGE.value(),"Seems The user dose not have any device info to store");
        }

        return response;
    }

    public Map<String,Object> getFcmDeviceToken(Set<String> contacts, String accountID){

        Map<String, Object> response = new HashMap<>();

        try {
            log.info(" came inside fcm device token ");
            response.put(FCM_SERVICE_CONSTANTS.IS_FCM_TOKEN_AVAILABLE.value(), false);

            if (ObjUtils.isNullOrEmpty(contacts)) {
                response.put(FCM_SERVICE_CONSTANTS.ERROR_MESSAGE.value(),"You sent contactID as Null");
            }else{
                List<String> fcmDeviceToken = fcmDao.getFcmDeviceToken(contacts,accountID);
                log.info(" deviceToken list " + fcmDeviceToken);

                if (!ObjUtils.isEmptyList(fcmDeviceToken)) {
                    response.put(FCM_SERVICE_CONSTANTS.IS_FCM_TOKEN_AVAILABLE.value(), true);
                    response.put(FCM_SERVICE_CONSTANTS.FCM_DEVICE_TOKEN.value(), fcmDeviceToken);
                } else {
                    response.put(FCM_SERVICE_CONSTANTS.ERROR_MESSAGE.value(), "There is no FcmDeviceToken Available for this user");
                }
            }
        }catch (Exception e){
            response.put(FCM_SERVICE_CONSTANTS.ERROR_MESSAGE.value(), e.getMessage());
        }

        log.info(" fcm device token " + response);
        return response;
    }

    private boolean isSourceMobile(String clientKey){
        if(ObjUtils.isNullOrEmpty(clientKey)){
            return false;
        }
        String clientValue = ClientSource.getClientIdMap().get(clientKey);
        return (ClientSource.CLIENT_SOURCE_CONSTANTS.IOS.value().equalsIgnoreCase(clientValue)
                    || ClientSource.CLIENT_SOURCE_CONSTANTS.ANDROID.value().equalsIgnoreCase(clientValue)
        );
    }

    public void notifyFCM(String accountID, Set<String> contactID, String activity, boolean syncFCM, Object syncValue, String clientKey){

        try{

            log.info("accountID : "+accountID+" contactID : "+contactID+" activity : "+activity+" syncFCM : "+syncFCM+" syncValue : "+syncValue + " src client id " + clientKey);

            if(!isSourceMobile(clientKey)){

                if(syncFCM && !ObjUtils.isNull(syncValue)){
                    updateFcmSyncObject(accountID,contactID,activity,syncValue);
                }

                Map<String, Object> deviceTokenInfoMap = getFcmDeviceToken( contactID, accountID );

                if( (boolean) deviceTokenInfoMap.get(FCM_SERVICE_CONSTANTS.IS_FCM_TOKEN_AVAILABLE.value())){
                    log.info("device token available");
                    publishToFCM(activity, (List<String>) deviceTokenInfoMap.get(FCM_SERVICE_CONSTANTS.FCM_DEVICE_TOKEN.value()));
                }
            }

        }catch (Exception e){
            log.info("exception in notifying FCM : " + e.getMessage());
        }
    }

    public void publishToFCM(String activity, List<String> deviceTokens){
        try{
            if(ObjUtils.isNullOrEmpty(deviceTokens)){
                return;
            }
            var url = "https://fcm.googleapis.com/fcm/send";
            String fcmKey = "key="+ CommonAppProperties.getFcmKey();

            HashMap<String, String> activityMap = new HashMap<>();
            activityMap.put("notify",activity );

            HashMap<String, Object> payload = new HashMap<>();
            payload.put("data", activityMap );
            payload.put("registration_ids",deviceTokens);

            if (!FCM_SERVICE_CONSTANTS.FCM_DELETE_ACCOUNT.value().equalsIgnoreCase(activity)){

                if( FCM_SERVICE_CONSTANTS.FCM_CLOCK_IN_OUT.value().equalsIgnoreCase(activity)){
                    payload.put("time_to_live", 60); // only for clock in and out
                }

                payload.put("collapse_key", COLLAPSE_KEY.get( activity));
            }

            log.info(" payload " + JsonUtil.getJson(payload));

            var headers = new String[]{UrlFetcher.CONTENT_TYPE,UrlFetcher.APPLICATION_JSON, UrlFetcher.AUTHORIZATION,fcmKey };

            Map<String,Object> response = UrlFetcher.sendPostRequest(url,payload,headers,UrlFetcher.getHttpClientInstance());

            log.info(" response : " + response);

        }catch (IOException e){
            log.info(" exception " + e.getMessage());
        }
    }
}
