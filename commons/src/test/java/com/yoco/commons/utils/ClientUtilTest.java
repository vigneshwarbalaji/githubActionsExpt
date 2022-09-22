package com.yoco.commons.utils;

import com.yoco.commons.dataservices.dao.ClientDao;
import com.yoco.commons.dataservices.impl.ClientImpl;
import com.yoco.commons.entity.Client;
import com.yoco.commons.utils.client.ClientUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.util.HashMap;
import java.util.List;

class ClientUtilTest {
    @Test
    void getAllClientsUnderAccount_nullAccountID_test(){
        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){
            ClientUtil.getAllClientsUnderAccount(null);
            clientMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void getAllClientsUnderAccount_validAccountID_test(){
        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){
            ClientImpl clientMock = Mockito.mock(ClientImpl.class);
            HashMap<String,Object> respWithCursor = new HashMap(){{put("clients", List.of(new Client(),new Client()));put("cursor","cursor");}};
            HashMap<String,Object> respWithoutCursor = new HashMap(){{put("clients", List.of(new Client(),new Client()));put("cursor",null);}};
            Mockito.when(clientMock.getAllClients("accID","","")).thenReturn(respWithCursor);
            Mockito.when(clientMock.getAllClients("accID","","cursor")).thenReturn(respWithoutCursor);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);
            Assertions.assertEquals(4,ClientUtil.getAllClientsUnderAccount("accID").size());
        }
    }

    @Test
    void deactivateAllClientsUnderAccount_valid_test(){
        try(MockedStatic<ClientUtil> clientUtilMockedStatic = Mockito.mockStatic(ClientUtil.class);
            MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){
            ClientImpl clientMock = Mockito.mock(ClientImpl.class);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);
            Client c1 = new Client();
            Client c2 = new Client();
            clientUtilMockedStatic.when(()->ClientUtil.getAllClientsUnderAccount("accID")).thenReturn(List.of(c1,c2));
            clientUtilMockedStatic.when(()->ClientUtil.deactivateAllClientsUnderAccount("accID",1L,"123")).thenCallRealMethod();
            ClientUtil.deactivateAllClientsUnderAccount("accID",1L,"123");
            c1.setDeletedBy("123");
            c1.setStatus(ClientDao.Status.INACTIVE.toString());
            c1.setDeletedDate(1L);
            c2.setDeletedBy("123");
            c2.setStatus(ClientDao.Status.INACTIVE.toString());
            c2.setDeletedDate(1L);
            Mockito.verify(clientMock).saveCollection(List.of(c1,c2));
        }
    }
}
