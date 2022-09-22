package com.yoco.commons.utils;

import com.yoco.commons.dataservices.impl.IntegrationsImpl;
import com.yoco.commons.entity.IntegrationsJDO;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.utils.integration.IntegrationUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.util.List;

class IntegrationUtilTest {
    @Test
    void getAllIntegrationsUnderAccount_nullAccID_test(){
        try(MockedStatic<IntegrationsImpl> integrationsMockedStatic = Mockito.mockStatic(IntegrationsImpl.class)){
            IntegrationUtil.getAllIntegrationsUnderAccount(null);
            integrationsMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void getAllIntegrationsUnderAccount_validAccID_test(){
        try(MockedStatic<IntegrationsImpl> integrationsMockedStatic = Mockito.mockStatic(IntegrationsImpl.class)){
            IntegrationsImpl integrationsMock = Mockito.mock(IntegrationsImpl.class);
            integrationsMockedStatic.when(IntegrationsImpl::getIntegrationsImplInstance).thenReturn(integrationsMock);
            CollectionResponse<IntegrationsJDO> respWithCursor = new CollectionResponse<>();
            respWithCursor.setItems(List.of(new IntegrationsJDO(),new IntegrationsJDO()));
            respWithCursor.setCursor("cursor");
            CollectionResponse<IntegrationsJDO> respWithoutCursor = new CollectionResponse<>();
            respWithoutCursor.setItems(List.of(new IntegrationsJDO(),new IntegrationsJDO()));
            respWithoutCursor.setCursor(null);
            Mockito.when(integrationsMock.getAllIntegrationsUnderAccount("accID",1000,"")).thenReturn(respWithCursor);
            Mockito.when(integrationsMock.getAllIntegrationsUnderAccount("accID",1000,"cursor")).thenReturn(respWithoutCursor);
            Assertions.assertEquals(4,IntegrationUtil.getAllIntegrationsUnderAccount("accID").size());
        }
    }

    @Test
    void deleteAllIntegrationsInAccount_noIntegrations_Test(){
        try(MockedStatic<IntegrationUtil> integrationUtilMockedStatic = Mockito.mockStatic(IntegrationUtil.class);
            MockedStatic<IntegrationsImpl> integrationsMockedStatic = Mockito.mockStatic(IntegrationsImpl.class)){
            integrationUtilMockedStatic.when(()->IntegrationUtil.getAllIntegrationsUnderAccount("accID")).thenReturn(List.of());
            integrationUtilMockedStatic.when(()->IntegrationUtil.deleteAllIntegrationsInAccount("accID")).thenCallRealMethod();
            IntegrationUtil.deleteAllIntegrationsInAccount("accID");
            integrationsMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void deleteAllIntegrationsInAccount_valid_Test(){
        try(MockedStatic<IntegrationUtil> integrationUtilMockedStatic = Mockito.mockStatic(IntegrationUtil.class);
            MockedStatic<IntegrationsImpl> integrationsMockedStatic = Mockito.mockStatic(IntegrationsImpl.class)){
            IntegrationsImpl integrationsMock = Mockito.mock(IntegrationsImpl.class);
            integrationsMockedStatic.when(IntegrationsImpl::getIntegrationsImplInstance).thenReturn(integrationsMock);
            integrationUtilMockedStatic.when(()->IntegrationUtil.getAllIntegrationsUnderAccount("accID")).thenReturn(List.of(new IntegrationsJDO(),new IntegrationsJDO()));
            integrationUtilMockedStatic.when(()->IntegrationUtil.deleteAllIntegrationsInAccount("accID")).thenCallRealMethod();
            IntegrationUtil.deleteAllIntegrationsInAccount("accID");
            Mockito.verify(integrationsMock).delete(List.of(new IntegrationsJDO(),new IntegrationsJDO()));
        }
    }
}