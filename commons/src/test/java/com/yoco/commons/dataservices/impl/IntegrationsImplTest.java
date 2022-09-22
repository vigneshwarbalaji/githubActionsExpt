package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.objectify.LocalDatastoreExtension;
import com.yoco.commons.dataservices.objectify.ObjectifyExtension;
import com.yoco.commons.entity.IntegrationsJDO;
import com.yoco.commons.modal.CollectionResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.ArrayList;

import static com.yoco.commons.dataservices.objectify.OfyService.ofy;

@ExtendWith({
        LocalDatastoreExtension.class,
        ObjectifyExtension.class})
class IntegrationsImplTest {
    @Test
    void getIntegrationByType_nullAccountID_test(){
        Assertions.assertNull(IntegrationsImpl.getIntegrationsImplInstance().getIntegrationByType(null,"234","slack"));
    }
    @Test
    void getIntegrationByType_emptyAccountID_test(){
        Assertions.assertNull(IntegrationsImpl.getIntegrationsImplInstance().getIntegrationByType("","234","slack"));
    }
    @Test
    void getIntegrationByType_nullContactID_test(){
        Assertions.assertNull(IntegrationsImpl.getIntegrationsImplInstance().getIntegrationByType("123",null,"slack"));
    }
    @Test
    void getIntegrationByType_emptyContactID_test(){
        Assertions.assertNull(IntegrationsImpl.getIntegrationsImplInstance().getIntegrationByType("123","","slack"));
    }
    @Test
    void getIntegrationByType_nullType_test(){
        Assertions.assertNull(IntegrationsImpl.getIntegrationsImplInstance().getIntegrationByType("123","234",null));
    }
    @Test
    void getIntegrationByType_emptyType_test(){
        Assertions.assertNull(IntegrationsImpl.getIntegrationsImplInstance().getIntegrationByType("123","234",""));
    }
    @Test
    void getIntegrationByType_notFound_test(){
        Assertions.assertNull(IntegrationsImpl.getIntegrationsImplInstance().getIntegrationByType("123","234","slack"));
    }
    @Test
    void getIntegrationByType_vaild_test(){
        IntegrationsImpl integrationsImpl = IntegrationsImpl.getIntegrationsImplInstance();
        IntegrationsJDO result = new IntegrationsJDO("123","234","slack","{}",true);
        integrationsImpl.saveIntegration(null);
        integrationsImpl.saveIntegration(result);
        Assertions.assertEquals(result,integrationsImpl.getIntegrationByType("123","234","slack"));
        integrationsImpl.delete(result);
    }

    @Test
    void getAllIntegrations_nullAccountID_test(){
        Assertions.assertEquals(new ArrayList<>(),IntegrationsImpl.getIntegrationsImplInstance().getAllIntegrations(null,"234"));
    }
    @Test
    void getAllIntegrations_emptyAccountID_test(){
        Assertions.assertEquals(new ArrayList<>(),IntegrationsImpl.getIntegrationsImplInstance().getAllIntegrations("","234"));
    }
    @Test
    void getAllIntegrations_nullContactID_test(){
        Assertions.assertEquals(new ArrayList<>(),IntegrationsImpl.getIntegrationsImplInstance().getAllIntegrations("123",null));
    }
    @Test
    void getAllIntegrations_emptyContactID_test(){
        Assertions.assertEquals(new ArrayList<>(),IntegrationsImpl.getIntegrationsImplInstance().getAllIntegrations("123",""));
    }
    @Test
    void getAllIntegrations_notFound_test(){
        Assertions.assertEquals(new ArrayList<>(),IntegrationsImpl.getIntegrationsImplInstance().getAllIntegrations("123","234"));
    }
    @Test
    void getAllIntegrations_vaild_test(){
        IntegrationsImpl integrationsImpl = IntegrationsImpl.getIntegrationsImplInstance();
        IntegrationsJDO result = new IntegrationsJDO("123","234","slack","{}",true);
        integrationsImpl.saveIntegration(result);
        Assertions.assertEquals(new ArrayList(){{add(result);}},integrationsImpl.getAllIntegrations("123","234"));
        integrationsImpl.delete(result);
    }

    @Test
    void getAllIntegrationsUnderAccount_valid_test(){
        IntegrationsImpl integrationsImpl = IntegrationsImpl.getIntegrationsImplInstance();
        IntegrationsJDO int1 = new IntegrationsJDO("accID","234","slack","{}",true);
        IntegrationsJDO int2 = new IntegrationsJDO("accID","234","slack","{}",true);
        integrationsImpl.saveIntegration(int1);
        integrationsImpl.saveIntegration(int2);
        CollectionResponse<IntegrationsJDO> actual = integrationsImpl.getAllIntegrationsUnderAccount("accID",50,null);
        Assertions.assertEquals(2,actual.getItems().size());
        Assertions.assertTrue(actual.getItems().contains(int1));
        Assertions.assertTrue(actual.getItems().contains(int2));
        ofy().delete().entities(actual.getItems()).now();
    }
}
