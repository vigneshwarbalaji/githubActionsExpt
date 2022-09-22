package com.yoco.commons.dataservices.impl;

import com.googlecode.objectify.cmd.Query;
import com.yoco.commons.dataservices.dao.IntegrationsDao;
import com.yoco.commons.dataservices.objectify.OfyService;
import com.yoco.commons.entity.IntegrationsJDO;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.utils.ObjUtils;
import java.util.ArrayList;
import java.util.List;

public class IntegrationsImpl extends OfyService implements IntegrationsDao {

    public static final String UNIQUE_PIN = "uniquePin";
    public static final String CONTACT_ID = "contactID";
    public static final String INTEGRATION_TYPE = "integrationType";

    public static IntegrationsImpl getIntegrationsImplInstance(){
        return new IntegrationsImpl();
    }

    @Override
    public IntegrationsJDO getIntegrationByType(String accountID, String contactID, String integrationType) {
        if(ObjUtils.isNullOrEmpty(accountID) || ObjUtils.isNullOrEmpty(contactID) || ObjUtils.isNullOrEmpty(integrationType)){
            return null;
        }
        return ofy().load().type(IntegrationsJDO.class)
                    .filter(UNIQUE_PIN, accountID)
                    .filter(CONTACT_ID, contactID)
                    .filter(INTEGRATION_TYPE, integrationType).first().now();
    }

    @Override
    public List<IntegrationsJDO> getAllIntegrations(String accountID, String contactID){
        if(ObjUtils.isNullOrEmpty(accountID) || ObjUtils.isNullOrEmpty(contactID) ){
            return new ArrayList<>();
        }
        return ofy().load().type(IntegrationsJDO.class)
                .filter(UNIQUE_PIN, accountID)
                .filter(CONTACT_ID, contactID).list();
    }

    @Override
    public void saveIntegration(IntegrationsJDO integrationJDO){
        if (ObjUtils.isNull(integrationJDO)) {
            return;
        }
        save(integrationJDO);
    }

    @Override
    public CollectionResponse<IntegrationsJDO> getAllIntegrationsUnderAccount(String accountId, int limit, String cursor){
        Query<IntegrationsJDO> query = ofy().load().type(IntegrationsJDO.class)
                .filter(UNIQUE_PIN,accountId);
        return fetchCursorQuery(query, limit, cursor);
    }
}
