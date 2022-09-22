package com.yoco.client.helper;

import com.yoco.client.enums.CLIENT_ERROR_RESPONSE;
import com.yoco.client.service.ClientService;
import com.yoco.commons.entity.Client;
import com.yoco.commons.dataservices.impl.ClientImpl;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import java.security.InvalidParameterException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ClientHelper {

    public static ClientHelper getClientHelper(){
        return new ClientHelper();
    }

    public Map<String,Object> updateClientHelper(Map<String, Object> payloadMap,Client clientObj, String accountID){

        Map<String,Object> response = new HashMap<>();
        boolean isClientNameUpdated = this.updateClientName(payloadMap,clientObj);
        boolean isClientEmailUpdated = this.updateClientEmail(payloadMap,clientObj,accountID);
        boolean isClientPhoneUpdated = this.updateClientPhoneNumber(payloadMap,clientObj);
        boolean isClientCompanyUpdated  = this.updateClientCompany(payloadMap,clientObj);
        boolean isClientProjectsUpdated = this.updateClientProjects(payloadMap,clientObj);

        response.put("isClientObjUpdated", (isClientNameUpdated || isClientEmailUpdated || isClientPhoneUpdated
                || isClientCompanyUpdated || isClientProjectsUpdated));
        response.put("clientObj",clientObj);
        return response;
    }

    public boolean updateClientName(Map<String, Object> payloadMap,Client clientObj){

        String name  = payloadMap.containsKey(ClientService.CLIENT_PAYLOAD_FIELDS.NAME.value())  ?  Validator.sanitizeText(payloadMap.get(ClientService.CLIENT_PAYLOAD_FIELDS.NAME.value()).toString()) : "";

        if(!ObjUtils.isNullOrEmpty(name)){
            clientObj.setName(name);
           return true;
        }

        return false;
    }

    public boolean updateClientEmail(Map<String, Object> payloadMap,Client clientObj,String accountID){

        String email = payloadMap.containsKey(ClientService.CLIENT_PAYLOAD_FIELDS.EMAIL.value())  ? payloadMap.get(ClientService.CLIENT_PAYLOAD_FIELDS.EMAIL.value()).toString() : "";

        if(!ObjUtils.isNullOrEmpty(email)){

            if(ClientImpl.getClientImplInstance().getClientByEmailForAnAccount(accountID,email)){
                throw new InvalidParameterException( CLIENT_ERROR_RESPONSE.CLIENT_ALREADY_EXIST.value() );
            }else{
                clientObj.setEmail(email);
               return true;
            }
        }
        return false;
    }

    public boolean updateClientPhoneNumber(Map<String, Object> payloadMap,Client clientObj){

        if(payloadMap.containsKey(ClientService.CLIENT_PAYLOAD_FIELDS.PHONE.value())){
            String phone =  Validator.sanitizeText(payloadMap.get(ClientService.CLIENT_PAYLOAD_FIELDS.PHONE.value()).toString());
            clientObj.setPhone(phone);
           return true;
        }
        return false;
    }

    public boolean updateClientCompany(Map<String, Object> payloadMap,Client clientObj ){

        if(payloadMap.containsKey(ClientService.CLIENT_PAYLOAD_FIELDS.COMPANY.value())){
            String company =  Validator.sanitizeText(payloadMap.get(ClientService.CLIENT_PAYLOAD_FIELDS.COMPANY.value()).toString());
            clientObj.setCompany(company);
            return true;
        }
        return false;
    }

    public boolean updateClientProjects( Map<String, Object> payloadMap,Client clientObj ){

        if(payloadMap.containsKey(ClientService.CLIENT_PAYLOAD_FIELDS.PROJECTS.value())) {

            String[] projectIDs = payloadMap.get(ClientService.CLIENT_PAYLOAD_FIELDS.PROJECTS.value()).toString().split(",");

            List<String> prjList = clientObj.getProjects();

            for (String prjId : projectIDs) {

                if (!ObjUtils.isBlank(prjId)) {

                    if (prjList.contains(prjId)) {
                        prjList.remove(prjId);
                    } else {
                        prjList.add(prjId);
                    }
                }
            }

            clientObj.setProjects(prjList);
            return true;
        }
        return false;
    }

}
