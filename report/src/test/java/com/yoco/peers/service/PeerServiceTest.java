package com.yoco.peers.service;

import com.yoco.MockEvent;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.ObjUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class PeerServiceTest {

    @Test
    void getClockedInUsers_empty_accountID_test() {
        try {
            new PeerService().getClockedInUsers("", new Contact());
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getClockedInUsers_null_accountID_test() {
        try {
            new PeerService().getClockedInUsers(null, new Contact());
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getClockedInUsers_null_contact_test() {
        try {
            new PeerService().getClockedInUsers("aaccountID",null);
        }catch(Exception e ){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value(),e.getMessage());
        }
    }

    @Test
    void getClockedInUsers_empty_events_test() throws Exception{
        PeopleRelationJDO objPro = new PeopleRelationJDO();
        Contact objContact = new Contact();
        objContact.setId("1234");

        try(MockedConstruction<UserImpl>objUser = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
            Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPro);
        });
            MockedConstruction<ReportImpl>objReport = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
                Mockito.when(objReportImpl.getAllClockedInEntriesForAccount(anyString())).thenReturn(new ArrayList<>());
            })) {
            Map<String, Object> resp = new PeerService().getClockedInUsers("accountID", objContact);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
        }
    }

    @Test
    void getClockedInUsers_empty_contactList_test() throws Exception{
        PeopleRelationJDO objPro = new PeopleRelationJDO();
        Contact objContact = new Contact();
        objContact.setId("1234");

        try(MockedConstruction<UserImpl>objUser = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
            Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPro);
        });
            MockedConstruction<ReportImpl>objReport = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
                Mockito.when(objReportImpl.getAllClockedInEntriesForAccount(anyString())).thenReturn(new ArrayList<>());
            });
            MockedConstruction<ContactImpl>objContactImp = Mockito.mockConstruction(ContactImpl.class, (objContactImpl, context)->{
                Mockito.when(objContactImpl.getContacts(any(List.class))).thenReturn(new ArrayList<>());
            })) {
            Map<String, Object> resp = new PeerService().getClockedInUsers("accountID", objContact);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
        }
    }

    @Test
    void getClockedInUsers_valid_test() throws Exception{
        PeopleRelationJDO objPro = new PeopleRelationJDO();
        objPro.setTimeZone("Asia/Kolkata");
        Contact objContact = new Contact();
        objContact.setId("da71cc4f-c8b7-46c3-bd86-c792559da0e1");
        objContact.setEmailID("test@gmail.com");
        List<Map<String, Object>> entries = new ArrayList<>();
        entries.add(MockEvent.getRawEvent());
        List<Contact> objRespContact = new ArrayList<>();
        objRespContact.add(objContact);

        try(MockedConstruction<UserImpl>objUser = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
            Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPro);
        });
            MockedConstruction<ReportImpl>objReport = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
                Mockito.when(objReportImpl.getAllClockedInEntriesForAccount(anyString())).thenReturn(entries);
            });
            MockedConstruction<ContactImpl>objContactImp = Mockito.mockConstruction(ContactImpl.class, (objContactImpl, context)->{
                Mockito.when(objContactImpl.getContacts(any(List.class))).thenReturn(objRespContact);
            })) {
            Map<String, Object> resp = new PeerService().getClockedInUsers("accountID", objContact);
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));
            Assertions.assertTrue(resp.containsKey(SchedulingKeys.ENTRIES));
        }
    }


}
