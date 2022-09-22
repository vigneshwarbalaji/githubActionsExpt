package com.yoco.commons.dataservices.dao;

import com.yoco.commons.entity.IntegrationsJDO;
import com.yoco.commons.modal.CollectionResponse;
import java.util.List;

public interface IntegrationsDao {
    IntegrationsJDO getIntegrationByType (String accountID, String contactID, String integrationType);

    List<IntegrationsJDO> getAllIntegrations(String accountID, String contactID);

     void saveIntegration (IntegrationsJDO integrationJDO);

    CollectionResponse<IntegrationsJDO> getAllIntegrationsUnderAccount(String accountID, int limit, String cursor);
}
