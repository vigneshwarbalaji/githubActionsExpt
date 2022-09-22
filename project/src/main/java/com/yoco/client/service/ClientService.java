package com.yoco.client.service;

import com.yoco.client.enums.CLIENT_ERROR_RESPONSE;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.client.helper.ClientFullMetricHelper;
import com.yoco.client.helper.ClientHelper;
import com.yoco.commons.entity.Client;
import com.yoco.commons.dataservices.dao.ClientDao;
import com.yoco.commons.dataservices.impl.ClientImpl;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.security.InvalidParameterException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@Component
public class ClientService {

    @NoArgsConstructor
    public enum CLIENT_PAYLOAD_FIELDS {
        NAME("name"),
        EMAIL("email"),
        PHONE("phone"),
        COMPANY("company"),
        PROJECTS("projects"),
        CLIENT("client");

        private String value;

        CLIENT_PAYLOAD_FIELDS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    public static ClientService getClientService () {
        return new ClientService();
    }

    public Map<String, Object> createClient (String accountID, String loggedInUsedID,  String payload) throws InvalidParameterException, NoSuchAlgorithmException {

        Map<String, Object> responseMap = new HashMap<>();

        Map<String, Object> payloadMap =  JsonUtil.convertJsonToMap(payload);

        String name = payloadMap.containsKey(CLIENT_PAYLOAD_FIELDS.NAME.value())  ?  payloadMap.get(CLIENT_PAYLOAD_FIELDS.NAME.value()).toString() : "";
        String email = payloadMap.containsKey(CLIENT_PAYLOAD_FIELDS.EMAIL.value())  ?  payloadMap.get(CLIENT_PAYLOAD_FIELDS.EMAIL.value()).toString() : "";

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(name), CLIENT_ERROR_RESPONSE.INVALID_NAME.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(email), COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.value());

        boolean isClientExist = ClientImpl.getClientImplInstance().getClientByEmailForAnAccount(accountID, email);

        log.info("Inside service " + isClientExist);

        if(!isClientExist) {

            String phone = payloadMap.containsKey(CLIENT_PAYLOAD_FIELDS.PHONE.value())  ?  payloadMap.get(CLIENT_PAYLOAD_FIELDS.PHONE.value()).toString() : "";
            String company = payloadMap.containsKey(CLIENT_PAYLOAD_FIELDS.COMPANY.value())  ?  payloadMap.get(CLIENT_PAYLOAD_FIELDS.COMPANY.value()).toString() : "";

            var objClient = new Client(name, email, phone, company, accountID, loggedInUsedID);

            objClient = ClientImpl.getClientImplInstance().saveClient(objClient);

            responseMap.put(CLIENT_PAYLOAD_FIELDS.CLIENT.value(), objClient);

            ClientFullMetricHelper.clientCreationMetric( accountID );

        } else {
            throw new InvalidParameterException( CLIENT_ERROR_RESPONSE.CLIENT_ALREADY_EXIST.value() );
        }

        return responseMap;
    }

    public Map<String, Object> getAll (String accountID, String projectID, String cursor) {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());

        return ClientImpl.getClientImplInstance().getAllClients(accountID, projectID, cursor);

    }

    public Map<String,Object> updateClient(String accountID, String clientID, String payload) throws InvalidParameterException{

        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());

        Map<String, Object> responseMap = new HashMap<>();

        var clientObj = ClientImpl.getClientImplInstance().getByID(clientID);

        if(!ObjUtils.isNull(clientObj)){

            log.info(" client exists ");

            Map<String, Object> payloadMap =  JsonUtil.convertJsonToMap(payload);

            Map<String,Object> updateResp = ClientHelper.getClientHelper().updateClientHelper(payloadMap,clientObj,accountID);

            if(Boolean.TRUE.equals(updateResp.get("isClientObjUpdated"))){
                clientObj = (Client) updateResp.get("clientObj");
                clientObj.setDateModified(DateUtil.getCurrentTime());

                var updatedClientObj = ClientImpl.getClientImplInstance().saveClient(clientObj);
                responseMap.put(CLIENT_PAYLOAD_FIELDS.CLIENT.value(), updatedClientObj);
            }
        }

        return responseMap;
    }

    public Map<String, Object> getByID (String clientID) {

        Map<String, Object> responseMap = new HashMap<>();

        Validator.checkArgument(ObjUtils.isNullOrEmpty(clientID), CLIENT_ERROR_RESPONSE.INVALID_CLIENT_ID.value());

        var objClient = ClientImpl.getClientImplInstance().getByID(clientID);

        if(!ObjUtils.isNull(objClient))
            responseMap.put(CLIENT_PAYLOAD_FIELDS.CLIENT.value(), objClient);

        return responseMap;
    }

    public Map<String, Object> deleteClient (String clientID, String accountID, String loggedInUser) {

        Map<String, Object> responseMap = new HashMap<>();

        Validator.checkArgument(ObjUtils.isNullOrEmpty(clientID), CLIENT_ERROR_RESPONSE.INVALID_CLIENT_ID.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(loggedInUser), CLIENT_ERROR_RESPONSE.INVALID_CONTACT_ID.value());

        var objClient = ClientImpl.getClientImplInstance().getByID(clientID);

        if(!ObjUtils.isNull(objClient) && accountID.equalsIgnoreCase(objClient.getUniquePin())) {

            objClient.setDeletedBy(loggedInUser);
            objClient.setStatus(ClientDao.Status.INACTIVE.toString());
            objClient.setDeletedDate(System.currentTimeMillis());

            objClient = ClientImpl.getClientImplInstance().saveClient(objClient);

            responseMap.put("client", objClient);

            ClientFullMetricHelper.clientDeletionMetric(accountID);
        }

        return responseMap;
    }

}
