package com.yoco.client.helperTest;

import com.yoco.client.enums.CLIENT_ERROR_RESPONSE;
import com.yoco.client.helper.ClientHelper;
import com.yoco.client.service.ClientService;
import com.yoco.commons.dataservices.impl.ClientImpl;
import com.yoco.commons.entity.Client;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;

class ClientHelperTest {

    ClientHelper clientHelper = ClientHelper.getClientHelper();

    Client client = new Client();

    @Test
    void updateClientHelper_empty_payload_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        Map<String,Object> resp = clientHelper.updateClientHelper(payloadMap,client,"accountId");
        assertFalse((Boolean) resp.get("isClientObjUpdated"));
    }

    @Test
    void updateClientHelper_clientNameUpdate_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.NAME.value(),"Client A");
        Map<String,Object> resp = clientHelper.updateClientHelper(payloadMap,client,"accountId");
        assertTrue((Boolean) resp.get("isClientObjUpdated"));
    }

    @Test
    void updateClientHelper_clientEmailUpdate_test(){
        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){
            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getClientByEmailForAnAccount(anyString(),anyString())).thenReturn(false);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.EMAIL.value(),"test@gmail.com");
            Map<String,Object> resp = clientHelper.updateClientHelper(payloadMap,client,"accountId");
            assertTrue((Boolean) resp.get("isClientObjUpdated"));
        }
    }

    @Test
    void updateClientHelper_clientPhoneUpdate_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.PHONE.value(),"123");
        Map<String,Object> resp = clientHelper.updateClientHelper(payloadMap,client,"accountId");
        assertTrue((Boolean) resp.get("isClientObjUpdated"));
    }

    @Test
    void updateClientHelper_clientCompanyUpdate_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.COMPANY.value(),"A");
        Map<String,Object> resp = clientHelper.updateClientHelper(payloadMap,client,"accountId");
        assertTrue((Boolean) resp.get("isClientObjUpdated"));
    }

    @Test
    void updateClientHelper_clientProjectUpdate_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.PROJECTS.value(),"project B");
        Map<String,Object> resp = clientHelper.updateClientHelper(payloadMap,client,"accountId");
        assertTrue((Boolean) resp.get("isClientObjUpdated"));
    }

    @Test
    void updateClientName_false_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        boolean isClientNameUpdated =  clientHelper.updateClientName(payloadMap,client);
        assertFalse(isClientNameUpdated);
    }

    @Test
    void updateClientName_true_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.NAME.value(),"Client A");
        boolean isClientNameUpdated =  clientHelper.updateClientName(payloadMap,client);
        assertTrue(isClientNameUpdated);
        assertEquals("Client A",client.getName());
    }

    @Test
    void updateClientEmail_false_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        boolean isClientMailUpdated = clientHelper.updateClientEmail(payloadMap,client,"accountID");
        assertFalse(isClientMailUpdated);
    }

    @Test
    void updateClientEmail_mail_exists_test() {
        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){
            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getClientByEmailForAnAccount(anyString(),anyString())).thenReturn(true);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.EMAIL.value(),"test@gmail.com");
            clientHelper.updateClientEmail(payloadMap,client,"accountID");
        }catch (Exception e){
            assertEquals(CLIENT_ERROR_RESPONSE.CLIENT_ALREADY_EXIST.value(),e.getMessage());
        }
    }

    @Test
    void updateClientEmail_mail_not_exists_test() {
        try(MockedStatic<ClientImpl> clientMockedStatic = Mockito.mockStatic(ClientImpl.class)){
            ClientImpl clientMock = mock(ClientImpl.class);
            Mockito.when(clientMock.getClientByEmailForAnAccount(anyString(),anyString())).thenReturn(false);
            clientMockedStatic.when(ClientImpl::getClientImplInstance).thenReturn(clientMock);

            Map<String, Object> payloadMap = new HashMap<>();
            payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.EMAIL.value(),"test@gmail.com");
            boolean isClientMailUpdated = clientHelper.updateClientEmail(payloadMap,client,"accountID");
            assertTrue(isClientMailUpdated);
        }
    }

    @Test
    void updateClientPhoneNumber_false_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        boolean isClientPhoneUpdated = clientHelper.updateClientPhoneNumber(payloadMap,client);
        assertFalse(isClientPhoneUpdated);
    }

    @Test
    void updateClientPhoneNumber_true_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.PHONE.value(),"123");
        boolean isClientPhoneUpdated = clientHelper.updateClientPhoneNumber(payloadMap,client);
        assertTrue(isClientPhoneUpdated);
        assertEquals("123",client.getPhone());
    }

    @Test
    void updateClientCompany_false_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        boolean isClientCompanyUpdated = clientHelper.updateClientCompany(payloadMap,client);
        assertFalse(isClientCompanyUpdated);
    }

    @Test
    void updateClientCompany_true_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.COMPANY.value(),"A");
        boolean isClientCompanyUpdated = clientHelper.updateClientCompany(payloadMap,client);
        assertTrue(isClientCompanyUpdated);
        assertEquals("A",client.getCompany());
    }

    @Test
    void updateClientProjects_false_test(){
        Map<String, Object> payloadMap = new HashMap<>();
        boolean isClientProjectsUpdated = clientHelper.updateClientProjects(payloadMap,client);
        assertFalse(isClientProjectsUpdated);
    }

    @Test
    void updateClientProjects_addProject_test(){
        List<String> project = new ArrayList<>();
        project.add("project A");
        client.setProjects(project);
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.PROJECTS.value(),"project B");
        boolean isClientProjectsUpdated = clientHelper.updateClientProjects(payloadMap,client);
        assertTrue(isClientProjectsUpdated);
        assertEquals(2,client.getProjects().size());
        List<String> expectedProjects = new ArrayList<>();
        expectedProjects.add("project A");
        expectedProjects.add("project B");
        assertEquals(expectedProjects,client.getProjects());
    }

    @Test
    void updateClientProjects_removeProject_test(){
        List<String> project = new ArrayList<>();
        project.add("project A");
        project.add("project B");
        client.setProjects(project);
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.PROJECTS.value(),"project A");
        boolean isClientProjectsUpdated = clientHelper.updateClientProjects(payloadMap,client);
        assertTrue(isClientProjectsUpdated);
        assertEquals(1,client.getProjects().size());
        List<String> expectedProjects = new ArrayList<>();
        expectedProjects.add("project B");
        assertEquals(expectedProjects,client.getProjects());
    }

    @Test
    void updateClientProjects_blankProject_test(){
        List<String> project = new ArrayList<>();
        project.add("project A");
        project.add("project B");
        client.setProjects(project);
        Map<String, Object> payloadMap = new HashMap<>();
        payloadMap.put(ClientService.CLIENT_PAYLOAD_FIELDS.PROJECTS.value(),"");
        boolean isClientProjectsUpdated = clientHelper.updateClientProjects(payloadMap,client);
        assertEquals(2,client.getProjects().size());
    }
}
