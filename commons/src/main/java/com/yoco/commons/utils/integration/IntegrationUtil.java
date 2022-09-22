package com.yoco.commons.utils.integration;

import com.yoco.commons.dataservices.impl.IntegrationsImpl;
import com.yoco.commons.entity.IntegrationsJDO;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.utils.ObjUtils;
import java.util.ArrayList;
import java.util.List;

public class IntegrationUtil {
    private IntegrationUtil(){}

    public static void deleteAllIntegrationsInAccount(String accountID){
        List<IntegrationsJDO> integrationsUnderAccount = getAllIntegrationsUnderAccount(accountID);
        if(!ObjUtils.isNullOrEmpty(integrationsUnderAccount)){
            IntegrationsImpl.getIntegrationsImplInstance().delete((List)integrationsUnderAccount);
        }
    }

    public static List<IntegrationsJDO> getAllIntegrationsUnderAccount(String accountID){
        if(ObjUtils.isNullOrEmpty(accountID)){
            return new ArrayList<>();
        }
        var integrationsImpl = IntegrationsImpl.getIntegrationsImplInstance();
        List<IntegrationsJDO> integrations= new ArrayList<>();
        var isQueryIncomplete = true;
        CollectionResponse<IntegrationsJDO> integrationsCollectionResponse;
        var cursor = "";
        while(isQueryIncomplete){
            integrationsCollectionResponse  = integrationsImpl.getAllIntegrationsUnderAccount(accountID,1000,cursor);
            integrations.addAll(integrationsCollectionResponse.getItems());
            if(!ObjUtils.isNullOrEmpty(integrationsCollectionResponse.getCursor())){
                cursor = integrationsCollectionResponse.getCursor();
            }else{
                isQueryIncomplete = false;
            }
        }
        return integrations;
    }
}
