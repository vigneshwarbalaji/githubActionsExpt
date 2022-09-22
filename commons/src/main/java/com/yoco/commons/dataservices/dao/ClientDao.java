package com.yoco.commons.dataservices.dao;

import com.yoco.commons.entity.Client;

import java.util.HashMap;

public interface ClientDao {

    enum Status
    {
        ACTIVE, INACTIVE, DELETED
    }

    boolean getClientByEmailForAnAccount(String accountID, String emailID);

    Client saveClient(Client obj);

    HashMap<String, Object> getAllClients(String accountID, String projectID, String cursor);

    Client getByID(String clientID);

}
