package com.yoco.commons.utils.client;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.dataservices.dao.ClientDao;
import com.yoco.commons.dataservices.impl.ClientImpl;
import com.yoco.commons.entity.Client;
import com.yoco.commons.utils.ObjUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ClientUtil {
    private ClientUtil(){}
    private static final String CLIENTS_KEY = "clients";
    public static void deactivateAllClientsUnderAccount(String accountID, Long dateDeletedLongTime, String primaryAdminContactID) {
        List<Client> clientsUnderAccount = getAllClientsUnderAccount(accountID);
        if(!ObjUtils.isNullOrEmpty(clientsUnderAccount)){
            for(Client client : clientsUnderAccount){
                client.setDeletedBy(primaryAdminContactID);
                client.setStatus(ClientDao.Status.INACTIVE.toString());
                client.setDeletedDate(dateDeletedLongTime);
            }
            ClientImpl.getClientImplInstance().saveCollection(clientsUnderAccount);
        }
    }

    public static List<Client> getAllClientsUnderAccount(String accountID) {
        if(ObjUtils.isNullOrEmpty(accountID)){
            return new ArrayList<>();
        }
        var clientImpl = ClientImpl.getClientImplInstance();
        List<Client> clients = new ArrayList<>();
        var isQueryIncomplete = true;
        Map<String,Object> clientResponse;
        var cursor = "";
        while(isQueryIncomplete){
            clientResponse  = clientImpl.getAllClients(accountID,"",cursor);
            clients.addAll((List<Client>)clientResponse.get(CLIENTS_KEY));
            if(!ObjUtils.isNullOrEmpty((String) clientResponse.get(Commons.CURSOR))){
                cursor = (String) clientResponse.get(Commons.CURSOR);
            }else{
                isQueryIncomplete = false;
            }
        }
        return clients;
    }
}
