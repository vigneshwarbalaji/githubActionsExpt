package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.objectify.LocalDatastoreExtension;
import com.yoco.commons.dataservices.objectify.ObjectifyExtension;
import com.yoco.commons.entity.FcmJDO;
import com.yoco.commons.modal.CollectionResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static com.yoco.commons.dataservices.objectify.OfyService.ofy;

@ExtendWith({
        LocalDatastoreExtension.class,
        ObjectifyExtension.class})
class FcmImplTest {

    FcmImpl fcmImplInstance = FcmImpl.getFcmImplInstance();

    @Test
    void saveFcm_test(){
        Assertions.assertEquals(null,fcmImplInstance.saveFcm(null));
    }

    @Test
    void getAllDevicesOfContact_valid_test(){
        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setDeviceID("deviceId");
        fcmJDO.setContactID("contactId");
        fcmImplInstance.saveFcm(fcmJDO);
        List<FcmJDO> response =  fcmImplInstance.getAllDevicesOfContact("contactId",10,"");
        Assertions.assertEquals(fcmJDO,response.get(0));
        Assertions.assertEquals(1,response.size());
        ofy().delete().entity(fcmJDO).now();
    }

    @Test
    void getAllDevicesOfContactUnderAccount_valid_test(){
        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setDeviceID("deviceId");
        fcmJDO.setContactID("contactId");
        fcmJDO.setAccountID("accountId");
        fcmImplInstance.saveFcm(fcmJDO);
        List<FcmJDO> response =  fcmImplInstance.getAllDevicesOfContactUnderAccount("accountId","contactId",10,"");
        Assertions.assertEquals(fcmJDO,response.get(0));
        Assertions.assertEquals(1,response.size());
        ofy().delete().entity(fcmJDO).now();
    }

    @Test
    void getFcmDeviceToken_valid_test(){
        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setDeviceID("deviceId");
        fcmJDO.setContactID("contactId");
        fcmJDO.setAccountID("accountId");
        fcmJDO.setFcmDeviceToken("fcmDeviceToken");
        fcmImplInstance.saveFcm(fcmJDO);
        Set<String> contacts = new HashSet<>();
        contacts.add("contactId");
        List<String> response =  fcmImplInstance.getFcmDeviceToken(contacts,"accountId");
        Assertions.assertEquals("fcmDeviceToken",response.get(0));
        Assertions.assertEquals(1,response.size());
        ofy().delete().entity(fcmJDO).now();
    }

    @Test
    void getFcmDeviceToken_emptyList_test(){
        Set<String> contacts = new HashSet<>();
        contacts.add("contactId");
        List<String> response =  fcmImplInstance.getFcmDeviceToken(contacts,"accountId");
        Assertions.assertEquals(0,response.size());
    }

    @Test
    void getFcmDeviceInfoById_valid_test(){

        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setDeviceID("deviceId");
        fcmJDO.setContactID("contactId");
        fcmImplInstance.saveFcm(fcmJDO);
        Assertions.assertEquals(fcmJDO, FcmImpl.getFcmImplInstance().getFcmDeviceInfoById("deviceId"));
        FcmImpl.getFcmImplInstance().deleteDevice(fcmJDO);
    }

    void getAllDevicesUnderAccount_valid_test(){
        FcmJDO fcmJDO = new FcmJDO();
        fcmJDO.setDeviceID("1");
        fcmJDO.setAccountID("accID");
        FcmJDO fcmJDO1 = new FcmJDO();
        fcmJDO1.setDeviceID("2");
        fcmJDO1.setAccountID("accID");
        fcmImplInstance.saveFcm(fcmJDO);
        fcmImplInstance.saveFcm(fcmJDO1);
        CollectionResponse<FcmJDO> actual = fcmImplInstance.getAllDevicesUnderAccount("accID",50,null);
        Assertions.assertEquals(2,actual.getItems().size());
        Assertions.assertTrue(actual.getItems().contains(fcmJDO));
        Assertions.assertTrue(actual.getItems().contains(fcmJDO1));
        ofy().delete().entities(actual.getItems()).now();
    }
}
