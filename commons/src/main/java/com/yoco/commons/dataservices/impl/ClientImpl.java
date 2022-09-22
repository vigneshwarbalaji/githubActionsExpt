package com.yoco.commons.dataservices.impl;

import com.googlecode.objectify.cmd.Query;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.entity.Client;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.dataservices.dao.ClientDao;
import com.yoco.commons.dataservices.objectify.OfyService;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;


@Slf4j
public class ClientImpl extends OfyService implements ClientDao {

    public static ClientImpl getClientImplInstance(){
        return new ClientImpl();
    }

    @Override
    public Client getByID(String clientID) {
        return get(Client.class, clientID);
    }

    @Override
    public boolean getClientByEmailForAnAccount(String accountID, String emailID) {

        var          client       = ofy().load().type(Client.class)
                .filter("uniquePin",accountID)
                .filter("email",emailID)
                .filter("status", Status.ACTIVE.toString()).first().now();

        return !ObjUtils.isNull(client);
    }

    @Override
    public Client saveClient(Client clientObj) {

        if( !ObjUtils.isNull(clientObj)) {
           ofy().save().entity(clientObj).now();
           return clientObj;
        } else {
            return new Client();
        }
    }

    @Override
    public HashMap<String, Object> getAllClients(String accountID, String projectID, String cursor) {

        HashMap<String, Object> responseMap = new HashMap<>();

        Query<Client> query = ofy().load().type(Client.class)
                .filter("uniquePin",accountID)
                .filter("status", Status.ACTIVE.toString());

        if(!ObjUtils.isNullOrEmpty(projectID))
            query = query.filter("projects", projectID);

        CollectionResponse<Client> collectionResponse = fetchCursorQuery(query, 200, cursor);

        responseMap.put("clients", collectionResponse.getItems());

        if(!ObjUtils.isNullOrEmpty(collectionResponse.getCursor())) {
            responseMap.put(Commons.CURSOR, collectionResponse.getCursor());
        }

        return responseMap;
    }
}
