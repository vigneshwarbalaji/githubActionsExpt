package com.yoco.contact.service;

import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.utils.GaeUtils;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.contact.helper.hook.CreateContactHookHelper;
import com.yoco.contact.helper.hook.DeleteContactHookHelper;
import com.yoco.contact.helper.hook.UpdateContactHookHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class ContactHookServiceTest {

    ContactHookService contactHookService = new ContactHookService();

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"contact_created","contact_create","contact","dummy"})
    void validateAndExtractDTOFromRequest_invalid_event_type_test(String eventType){
        try{
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("event",eventType);
            contactHookService.validateAndExtractDTOFromRequest(payloadMap);
        }catch (Exception e){
            Assertions.assertEquals(" Not processing for event type : " + eventType,e.getMessage());
        }
    }

    @Test
    void validateAndExtractDTOFromRequest_invalid_appID_yoco_staging_test(){
        try{
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("event","contact_updated");
            payloadMap.put("data",Map.of("appID", GaeUtils.APP_ID_STAGING));
            contactHookService.validateAndExtractDTOFromRequest(payloadMap);
        }catch (Exception e){
            Assertions.assertEquals(" Not processing for appID : " + GaeUtils.APP_ID_STAGING,e.getMessage());
        }
    }

    @Test
    void validateAndExtractDTOFromRequest_invalid_appID_yoco_live_test(){
        try{
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("event","contact_deleted");
            payloadMap.put("data",Map.of("appID", GaeUtils.APP_ID_LIVE));
            contactHookService.validateAndExtractDTOFromRequest(payloadMap);
        }catch (Exception e){
            Assertions.assertEquals(" Not processing for appID : " + GaeUtils.APP_ID_LIVE,e.getMessage());
        }
    }

    @Test
    void validateAndExtractDTOFromRequest_invalid_contact_list_test(){
        try{
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("event","contact_deleted");
            payloadMap.put("data",Map.of("appID", "id","contact",new ArrayList<>()));
            contactHookService.validateAndExtractDTOFromRequest(payloadMap);
        }catch (Exception e){
            Assertions.assertEquals(" Not processing due to invalid id/login ",e.getMessage());
        }
    }

    @Test
    void validateAndExtractDTOFromRequest_invalid_contact_map_test(){
        try {
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("event","contact_deleted");
            List<Map<String,Object>> contactList = new ArrayList<>();
            contactList.add(new HashMap<>());
            payloadMap.put("data",Map.of("appID", "id","contact",contactList));
            contactHookService.validateAndExtractDTOFromRequest(payloadMap);
        }catch (Exception e){
            Assertions.assertEquals(" Not processing due to invalid id/login ",e.getMessage());
        }
    }

    @Test
    void validateAndExtractDTOFromRequest_invalid_contact_id_test(){
        try{
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("event","contact_deleted");
            Map<String,Object> contactMap = new HashMap<>();
            contactMap.put("id",null);
            List<Map<String,Object>> contactList = new ArrayList<>();
            contactList.add(contactMap);
            payloadMap.put("data",Map.of("appID", "id","contact",contactList));
            contactHookService.validateAndExtractDTOFromRequest(payloadMap);
        }catch (Exception e){
            Assertions.assertEquals(" Not processing due to invalid id/login ",e.getMessage());
        }
    }

    @Test
    void validateAndExtractDTOFromRequest_invalid_emailID_test(){
        try{
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("event","contact_deleted");
            Map<String,Object> contactMap = new HashMap<>();
            contactMap.put("id","cId");
            contactMap.put("login","test");
            List<Map<String,Object>> contactList = new ArrayList<>();
            contactList.add(contactMap);
            payloadMap.put("data",Map.of("appID", "id","contact",contactList));
            contactHookService.validateAndExtractDTOFromRequest(payloadMap);
        }catch (Exception e){
            Assertions.assertEquals(" Not processing due to invalid id/login ",e.getMessage());
        }
    }

    @Test
    void validateAndExtractDTOFromRequest_valid_test(){
        Map<String,Object> payloadMap = new HashMap<>();
        payloadMap.put("event","contact_deleted");
        Map<String,Object> contactMap = new HashMap<>();
        contactMap.put("id","cId");
        contactMap.put("login","test@gmail.com");
        contactMap.put("firstName","test");
        contactMap.put("lastName","");
        contactMap.put("deleted",false);
        contactMap.put("brandID","brandId");
        contactMap.put("photoID","photoId");
        contactMap.put("accountID","accountId");
        contactMap.put("linkedContacts",new ArrayList<>());
        contactMap.put("linkedAccounts",new ArrayList<>());
        contactMap.put("is_password_present",false);
        List<Map<String,Object>> contactList = new ArrayList<>();
        contactList.add(contactMap);
        payloadMap.put("data",Map.of("appID", "id","contact",contactList));
        Assertions.assertEquals(new DcmContactDTO(contactMap),contactHookService.validateAndExtractDTOFromRequest(payloadMap));
    }

    @Test
    void processFullHook_contact_create_test(){
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)) {
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("event","contact_created");
            Map<String,Object> contactMap = new HashMap<>();
            contactMap.put("id","cId");
            contactMap.put("login","test@gmail.com");
            contactMap.put("firstName","test");
            contactMap.put("lastName","");
            contactMap.put("deleted",false);
            contactMap.put("brandID","brandId");
            contactMap.put("photoID","photoId");
            contactMap.put("accountID","accountId");
            contactMap.put("linkedContacts",new ArrayList<>());
            contactMap.put("linkedAccounts",new ArrayList<>());
            contactMap.put("is_password_present",false);
            List<Map<String,Object>> contactList = new ArrayList<>();
            contactList.add(contactMap);
            payloadMap.put("data",Map.of("appID", "id","contact",contactList));
            contactHookService.processFullHook(JsonUtil.getJson(payloadMap));
            contactMockedStatic.verifyNoInteractions();
        }catch (Exception e){
            Assertions.assertEquals(" Not processing for event type : contact_created",e.getMessage());
        }
    }

    @Test
    void processFullHook_contact_create_test2() throws Exception {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<CreateContactHookHelper> contactHookHelperMockedStatic = Mockito.mockStatic(CreateContactHookHelper.class)) {
            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(null);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);
            CreateContactHookHelper contactHookHelperMock = Mockito.mock(CreateContactHookHelper.class);
            Mockito.doNothing().when(contactHookHelperMock).processContactCreation(any(DcmContactDTO.class));
            contactHookHelperMockedStatic.when(CreateContactHookHelper::getInstance).thenReturn(contactHookHelperMock);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("event","contact_updated");
            Map<String,Object> contactMap = new HashMap<>();
            contactMap.put("id","cId");
            contactMap.put("login","test@gmail.com");
            contactMap.put("firstName","test");
            contactMap.put("lastName","");
            contactMap.put("deleted",false);
            contactMap.put("brandID","brandId");
            contactMap.put("photoID","photoId");
            contactMap.put("accountID","accountId");
            contactMap.put("linkedContacts",new ArrayList<>());
            contactMap.put("linkedAccounts",new ArrayList<>());
            contactMap.put("is_password_present",false);
            List<Map<String,Object>> contactList = new ArrayList<>();
            contactList.add(contactMap);
            payloadMap.put("data",Map.of("appID", "id","contact",contactList));
            contactHookService.processFullHook(JsonUtil.getJson(payloadMap));
            Mockito.verify(contactImplMock).getByID("cId");
            Mockito.verify(contactHookHelperMock).processContactCreation(new DcmContactDTO(contactMap));
        }
    }

    @Test
    void processFullHook_contact_update_test() {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<UpdateContactHookHelper> contactHookHelperMockedStatic = Mockito.mockStatic(UpdateContactHookHelper.class)) {
            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact("cId","test@gmail.com","fName","","photoId",0l);
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);
            UpdateContactHookHelper contactHookHelperMock = Mockito.mock(UpdateContactHookHelper.class);
            Mockito.doNothing().when(contactHookHelperMock).processContactUpdation(any(DcmContactDTO.class),any(Contact.class));
            contactHookHelperMockedStatic.when(UpdateContactHookHelper::getInstance).thenReturn(contactHookHelperMock);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("event","contact_updated");
            Map<String,Object> contactMap = new HashMap<>();
            contactMap.put("id","cId");
            contactMap.put("login","test@gmail.com");
            contactMap.put("firstName","test");
            contactMap.put("lastName","");
            contactMap.put("deleted",false);
            contactMap.put("brandID","brandId");
            contactMap.put("photoID","photoId");
            contactMap.put("accountID","accountId");
            contactMap.put("linkedContacts",new ArrayList<>());
            contactMap.put("linkedAccounts",new ArrayList<>());
            contactMap.put("is_password_present",false);
            List<Map<String,Object>> contactList = new ArrayList<>();
            contactList.add(contactMap);
            payloadMap.put("data",Map.of("appID", "id","contact",contactList));
            contactHookService.processFullHook(JsonUtil.getJson(payloadMap));
            Mockito.verify(contactImplMock).getByID("cId");
            Mockito.verify(contactHookHelperMock).processContactUpdation(new DcmContactDTO(contactMap),contact);
        }
    }

    @Test
    void processFullHook_contact_delete_test() {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<DeleteContactHookHelper> contactHookHelperMockedStatic = Mockito.mockStatic(DeleteContactHookHelper.class)) {
            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            Contact contact = new Contact("cId","test@gmail.com","fName","","photoId",0l);
            Mockito.when(contactImplMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);
            DeleteContactHookHelper contactHookHelperMock = Mockito.mock(DeleteContactHookHelper.class);
            Mockito.doNothing().when(contactHookHelperMock).processContactDeletion(any(DcmContactDTO.class));
            contactHookHelperMockedStatic.when(DeleteContactHookHelper::getInstance).thenReturn(contactHookHelperMock);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("event","contact_deleted");
            Map<String,Object> contactMap = new HashMap<>();
            contactMap.put("id","cId");
            contactMap.put("login","test@gmail.com");
            contactMap.put("firstName","test");
            contactMap.put("lastName","");
            contactMap.put("deleted",true);
            contactMap.put("brandID","brandId");
            contactMap.put("photoID","photoId");
            contactMap.put("accountID","accountId");
            contactMap.put("linkedContacts",new ArrayList<>());
            contactMap.put("linkedAccounts",new ArrayList<>());
            contactMap.put("is_password_present",false);
            List<Map<String,Object>> contactList = new ArrayList<>();
            contactList.add(contactMap);
            payloadMap.put("data",Map.of("appID", "id","contact",contactList));
            contactHookService.processFullHook(JsonUtil.getJson(payloadMap));
            Mockito.verify(contactImplMock).getByID("cId");
            Mockito.verify(contactHookHelperMock).processContactDeletion(new DcmContactDTO(contactMap));
        }
    }

    @Test
    void processFullHook_exception_test() {
        try(MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("event","contact_created");
            payloadMap.put("data",null);
            contactHookService.processFullHook(JsonUtil.getJson(payloadMap));
            contactMockedStatic.verifyNoInteractions();
        }
    }

}