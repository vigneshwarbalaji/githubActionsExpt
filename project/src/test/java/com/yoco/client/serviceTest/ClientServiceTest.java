package com.yoco.client.serviceTest;


import com.yoco.client.enums.CLIENT_ERROR_RESPONSE;
import com.yoco.client.helper.ClientFullMetricHelper;
import com.yoco.client.helper.ClientHelper;
import com.yoco.client.service.ClientService;
import com.yoco.commons.dataservices.impl.ClientImpl;
import com.yoco.commons.entity.Client;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import org.jose4j.json.internal.json_simple.JSONObject;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mock;

class ClientServiceTest {

    ClientService clientService = ClientService.getClientService();
    HashMap<String, Object> createClientPayload = new HashMap<>();

    @Test
    void updateClient_noSuchClient_test(){
        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){
            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getByID(anyString())).thenReturn(null);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String,Object> resp = clientService.updateClient("accountId","clientId","payload");
            assertTrue(ObjUtils.isNullOrEmpty(resp));
        }
    }

    @Test
    void updateClient_InValid_test(){
        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class);
            MockedStatic<ClientHelper> clientHelperMockedStatic = Mockito.mockStatic(ClientHelper.class)){

            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getByID(anyString())).thenReturn(new Client());
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            ClientHelper clientHelper = mock(ClientHelper.class);
            Mockito.when(clientHelper.updateClientHelper(anyMap(),any(),anyString())).thenReturn(new HashMap<>());
            clientHelperMockedStatic.when(ClientHelper::getClientHelper).thenReturn(clientHelper);

            Map<String,Object> payload = new HashMap<>();
            payload.put("name","");
            Map<String,Object> resp = clientService.updateClient("accountId","clientId",new JSONObject(payload).toJSONString());

            assertTrue(ObjUtils.isNullOrEmpty(resp));
        }

    }

    @Test
    void updateClient_valid_test(){
        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class);
            MockedStatic<ClientHelper> clientHelperMockedStatic = Mockito.mockStatic(ClientHelper.class)) {
            Client client = new Client();
            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getByID(anyString())).thenReturn(client);
            Mockito.when(clientMock.saveClient(any(Client.class))).thenReturn(client);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String, Object> mockResp = new HashMap<>();
            mockResp.put("isClientObjUpdated", true);
            mockResp.put("clientObj", client);

            ClientHelper clientHelper = mock(ClientHelper.class);
            Mockito.when(clientHelper.updateClientHelper(anyMap(), any(), anyString())).thenReturn(mockResp);
            clientHelperMockedStatic.when(ClientHelper::getClientHelper).thenReturn(clientHelper);

            Map<String, Object> payload = new HashMap<>();
            payload.put("name", "cName");
            Map<String, Object> resp = clientService.updateClient("accountId", "clientId", new JSONObject(payload).toJSONString());

            assertTrue(resp.containsKey("client"));
        }
    }

    @Test
    void getAllClients_null_accountID_test(){

        try{
            Map<String,Object> resp = clientService.getAll(null,"projectID","");
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        }

    }

    @Test
    void getAllClients_empty_accountID_test(){

        try{
            Map<String,Object> resp = clientService.getAll("","projectID","");
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        }

    }

    @Test
    void getAllClients_success_empty_clients_test(){

        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){

            HashMap<String, Object> response = new HashMap<>();

            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getAllClients(anyString(),anyString(), anyString())).thenReturn(response);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String,Object> resp = clientService.getAll("1234","projectID","");

            assertTrue(!resp.containsKey("clients"));

        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        }

    }

    @Test
    void getAllClients_success_with_clients_test(){

        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){

            HashMap<String, Object> response = new HashMap<>();
            response.put("clients", "clients");

            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getAllClients(anyString(),anyString(), anyString())).thenReturn(response);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String,Object> resp = clientService.getAll("1234","projectID","");

            assertTrue(resp.containsKey("clients"));

        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        }

    }

    @Test
    void getClientByID_null_clientID_test(){

        try{
            Map<String,Object> resp = clientService.getByID(null);
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), CLIENT_ERROR_RESPONSE.INVALID_CLIENT_ID.value());
        }

    }

    @Test
    void getClientByID_empty_clientID_test(){

        try{
            Map<String,Object> resp = clientService.getByID("");
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), CLIENT_ERROR_RESPONSE.INVALID_CLIENT_ID.value());
        }

    }

    @Test
    void getClientByID_null_client_obj_test() {

        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){
            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getByID(anyString())).thenReturn(null);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String,Object> resp = clientService.getByID("1234");

            assertTrue(ObjUtils.isNullOrEmpty(resp));
        }
    }

    @Test
    void getClientByID_valid_client_obj_test() {
        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){
            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getByID(anyString())).thenReturn(new Client());
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String,Object> resp = clientService.getByID("1234");

            assertTrue(resp.containsKey("client"));
        }
    }

    @Test
    void deleteClient_null_clientID_test(){

        try{
            clientService.deleteClient(null,"1234", "1234");
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), CLIENT_ERROR_RESPONSE.INVALID_CLIENT_ID.value());
        }

    }

    @Test
    void deleteClient_empty_clientID_test(){

        try{
            clientService.deleteClient("","1234", "1234");;
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), CLIENT_ERROR_RESPONSE.INVALID_CLIENT_ID.value());
        }

    }

    @Test
    void deleteClient_null_accountID_test(){

        try{
            clientService.deleteClient("1234",null, "1234");
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        }

    }

    @Test
    void deleteClient_empty_accountID_test(){

        try{
            clientService.deleteClient("1234","", "1234");;
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        }

    }

    @Test
    void deleteClient_null_contactID_test(){

        try{
            clientService.deleteClient("1234","1234", null);
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), CLIENT_ERROR_RESPONSE.INVALID_CONTACT_ID.value());
        }

    }

    @Test
    void deleteClient_empty_contactID_test(){

        try{
            clientService.deleteClient("1234","1234", "");;
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), CLIENT_ERROR_RESPONSE.INVALID_CONTACT_ID.value());
        }
    }

    @Test
    void deleteClient_null_client_test(){

        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){

            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getByID(anyString())).thenReturn(null);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String, Object> resp = clientService.deleteClient("1234", "1234", "1234");

            assertTrue(ObjUtils.isNullOrEmpty(resp));

        } catch (Exception e) {
           e.getMessage();
        }
    }

    @Test
    void deleteClient_client_with_different_uniquePin_test(){

        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){

            Client objClient = new Client();
            objClient.setUniquePin("5678");

            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getByID(anyString())).thenReturn(objClient);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String, Object> resp = clientService.deleteClient("1234", "1234", "1234");

            assertTrue(ObjUtils.isNullOrEmpty(resp));

        } catch (Exception e) {
            e.getMessage();
        }
    }

    @Test
    void deleteClient_success_test(){

        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){

            Client objClient = new Client();
            objClient.setUniquePin("1234");

            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getByID(anyString())).thenReturn(objClient);
            Mockito.when(clientMock.saveClient(any(Client.class))).thenReturn(objClient);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String, Object> resp = clientService.deleteClient("1234", "1234", "1234");

            assertTrue(!ObjUtils.isNullOrEmpty(resp));
            assertTrue(resp.containsKey("client"));

        } catch (Exception e) {
            e.getMessage();
        }
    }

    @Test
    void createClient_empty_accountID_test() {
        try{
            clientService.createClient("","1234", JsonUtil.getJson(createClientPayload));;
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        }
    }

    @Test
    void createClient_null_accountID_test() {
        try{
            clientService.createClient(null,"1234", JsonUtil.getJson(createClientPayload));;
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        }
    }

    @Test
    void createClient_empty_payload_for_name_test() {
        try{
            clientService.createClient("1234","1234", JsonUtil.getJson(createClientPayload));;
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), CLIENT_ERROR_RESPONSE.INVALID_NAME.value());
        }
    }

    @Test
    void createClient_empty_name_in_payload_test() {
        try{
            createClientPayload.put("name", "");
            clientService.createClient("1234","1234", JsonUtil.getJson(createClientPayload));;
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), CLIENT_ERROR_RESPONSE.INVALID_NAME.value());
        }
    }

    @Test
    void createClient_null_name_test() {
        try{
            createClientPayload.put("name", null);
            clientService.createClient("1234","1234", JsonUtil.getJson(createClientPayload));;
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), CLIENT_ERROR_RESPONSE.INVALID_NAME.value());
        }
    }

    @Test
    void createClient_empty_email_in_payload_test() {
        try{
            createClientPayload.put("name", "name");
            createClientPayload.put("email", "");
            clientService.createClient("1234","1234", JsonUtil.getJson(createClientPayload));;
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.value());
        }
    }

    @Test
    void createClient_null_email_test() {
        try{
            createClientPayload.put("name", "name");
            createClientPayload.put("email", null);
            clientService.createClient("1234","1234", JsonUtil.getJson(createClientPayload));;
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), COMMON_ERROR_RESPONSE.INVALID_EMAIL_ID.value());
        }
    }

    @Test
    void createClient_client_exists_test() {
        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){

            createClientPayload.put("name", "name");
            createClientPayload.put("email", "email");

            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getClientByEmailForAnAccount(anyString(), anyString())).thenReturn(true);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            clientService.createClient("1234","1234", JsonUtil.getJson(createClientPayload));;
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), CLIENT_ERROR_RESPONSE.CLIENT_ALREADY_EXIST.value());
        }
    }

    @Test
    void createClient_success_test() {
        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class);
            MockedStatic<ClientFullMetricHelper> clientFullMetricHelperMockedStatic = Mockito.mockStatic(ClientFullMetricHelper.class)){

            createClientPayload.put("name", "name");
            createClientPayload.put("email", "email");

            Client client = new Client();
            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getClientByEmailForAnAccount(anyString(), anyString())).thenReturn(false);
            Mockito.when(clientMock.saveClient(any(Client.class))).thenReturn(client);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String, Object> resp = clientService.createClient("1234","1234", JsonUtil.getJson(createClientPayload));

            assertTrue(!ObjUtils.isNullOrEmpty(resp));
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), CLIENT_ERROR_RESPONSE.CLIENT_ALREADY_EXIST.value());
        }
    }

    @Test
    void createClient_valid_phone_test() {
        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class);
            MockedStatic<ClientFullMetricHelper> clientFullMetricHelperMockedStatic = Mockito.mockStatic(ClientFullMetricHelper.class)){

            createClientPayload.put("name", "name");
            createClientPayload.put("email", "email");
            createClientPayload.put("phone","phone");
            createClientPayload.put("company","company");

            Client client = new Client();
            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getClientByEmailForAnAccount(anyString(), anyString())).thenReturn(false);
            Mockito.when(clientMock.saveClient(any(Client.class))).thenReturn(client);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String, Object> resp = clientService.createClient("1234","1234", JsonUtil.getJson(createClientPayload));

            assertTrue(!ObjUtils.isNullOrEmpty(resp));
        } catch (Exception e) {
            Assertions.assertEquals(e.getMessage(), CLIENT_ERROR_RESPONSE.CLIENT_ALREADY_EXIST.value());
        }
    }
}
