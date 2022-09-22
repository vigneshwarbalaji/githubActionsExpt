package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.objectify.LocalDatastoreExtension;
import com.yoco.commons.dataservices.objectify.ObjectifyExtension;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.utils.ObjUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import static com.yoco.commons.dataservices.objectify.OfyService.ofy;

@ExtendWith({
        LocalDatastoreExtension.class,
        ObjectifyExtension.class})
class UserImplTest {

     UserImpl userImplInstance = UserImpl.getUserImplInstance();

    @Test
    void savePro_test(){
        Assertions.assertNull(userImplInstance.savePro(null));
    }

    @Test
    void getAllUsers_noUsers_test(){
        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setId("id2");
        userPro1.setUniquepin("accountId1");
        userPro1.setDelete(false);
        userImplInstance.savePro(userPro1);
        Map<String,Object> resp = userImplInstance.getAllUsers("accountId",null,0,"");
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
        ofy().delete().entity(userPro1).now();
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getAllUsers_invalidAccountId_test(String testValue){
        Map<String,Object> resp = userImplInstance.getAllUsers(testValue,null,0,"");
        Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
    }

    @Test
    void getAllUsers_valid_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setUniquepin("accountId");
        userPro.setDelete(false);

        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setId("id2");
        userPro1.setUniquepin("accountId");
        userPro1.setDelete(false);

        userImplInstance.savePro(userPro);
        userImplInstance.savePro(userPro1);

        Map<String,Object> resp = userImplInstance.getAllUsers("accountId",false,1,"");
        List<PeopleRelationJDO> proList = (List<PeopleRelationJDO>) resp.get("users");
        Assertions.assertEquals(userPro,proList.get(0));
        Assertions.assertTrue(resp.containsKey("cursor"));

        ofy().delete().entity(userPro).now();
        ofy().delete().entity(userPro1).now();
    }

    @Test
    void getAllUsersWithoutContact_valid_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setUniquepin("accountId");
        userPro.setDelete(false);

        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setId("id2");
        userPro1.setUniquepin("accountId");
        userPro1.setDelete(false);

        userImplInstance.savePro(userPro);
        userImplInstance.savePro(userPro1);

        List<PeopleRelationJDO> resp = userImplInstance.getAllUsers("accountId",false);
        List<PeopleRelationJDO> expectedList = new ArrayList<>();
        expectedList.add(userPro);
        expectedList.add(userPro1);

        Assertions.assertEquals(expectedList,resp);

        ofy().delete().entity(userPro).now();
        ofy().delete().entity(userPro1).now();
    }

    @Test
    void getAllUsersWithoutContact_fetchAll_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setUniquepin("accountId");
        userPro.setDelete(false);

        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setId("id2");
        userPro1.setUniquepin("accountId");
        userPro1.setDelete(true);

        userImplInstance.savePro(userPro);
        userImplInstance.savePro(userPro1);

        List<PeopleRelationJDO> resp = userImplInstance.getAllUsers("accountId",true);
        List<PeopleRelationJDO> expectedList = new ArrayList<>();
        expectedList.add(userPro);
        expectedList.add(userPro1);

        Assertions.assertEquals(expectedList,resp);

        ofy().delete().entity(userPro).now();
        ofy().delete().entity(userPro1).now();
    }

    @Test
    void getRecentlyUpdatedPRO_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setUniquepin("accountId");
        userPro.setDelete(false);
        userPro.setDateModified(1646384551000L);
        userImplInstance.savePro(userPro);

        List<PeopleRelationJDO> resp = userImplInstance.getRecentlyUpdatedPRO("accountId",1646384551000L);
        List<PeopleRelationJDO> expectedList = new ArrayList<>();
        expectedList.add(userPro);

        Assertions.assertEquals(expectedList,resp);

        ofy().delete().entity(userPro).now();
    }

    @Test
    void getUsersByRole_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setUniquepin("accountId");
        userPro.setDelete(false);
        userPro.setRole(Skillset.ROLE_ADMIN);
        userPro.setDateModified(1646384551000L);
        userImplInstance.savePro(userPro);

        PeopleRelationJDO userPro2 = new PeopleRelationJDO();
        userPro2.setId("id2");
        userPro2.setUniquepin("accountId");
        userPro2.setDelete(false);
        userPro2.setRole(Skillset.ROLE_ADMIN);
        userPro2.setDateModified(1646384551000L);
        userImplInstance.savePro(userPro2);

        PeopleRelationJDO userPro3 = new PeopleRelationJDO();
        userPro3.setId("id3");
        userPro3.setUniquepin("accountId");
        userPro3.setDelete(false);
        userPro3.setRole(Skillset.ROLE_STAFF);
        userPro3.setDateModified(1646384551000L);
        userImplInstance.savePro(userPro3);

        List<PeopleRelationJDO> resp = userImplInstance.getUsersByRole("accountId",Skillset.ROLE_ADMIN);
        List<PeopleRelationJDO> expectedList = new ArrayList<>();
        expectedList.add(userPro);
        expectedList.add(userPro2);

        Assertions.assertEquals(expectedList,resp);

        ofy().delete().entity(userPro).now();
        ofy().delete().entity(userPro2).now();
        ofy().delete().entity(userPro3).now();
    }

    @Test
    void getUserByEmailID_valid_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id3");
        userPro.setUniquepin("accountId");
        userPro.setEmailID("emailId");
        userImplInstance.savePro(userPro);
        Assertions.assertNotNull(userImplInstance.getUserByEmailID("accountId","emailId"));
        ofy().delete().entity(userPro).now();
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getUserByEmailID_empty_accountID_test(String testValue){
        Assertions.assertNull(userImplInstance.getUserByEmailID(testValue,"emailId"));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getUserByEmailID_empty_emailID_test(String testValue){
        Assertions.assertNull(userImplInstance.getUserByEmailID("accountId",testValue));
    }

    @Test
    void getContactsByKeys_test(){
        Contact contact1 = new Contact();
        contact1.setId("contact1");
        ContactImpl.getContactImplInstance().saveContact(contact1);

        Contact contact2 = new Contact();
        contact2.setId("contact2");
        ContactImpl.getContactImplInstance().saveContact(contact2);

        PeopleRelationJDO userPro1 = new PeopleRelationJDO();
        userPro1.setContactId("contact1");

        PeopleRelationJDO userPro2 = new PeopleRelationJDO();
        userPro2.setContactId("contact2");

        List<PeopleRelationJDO> proList = new ArrayList<>();
        proList.add(userPro1);
        proList.add(userPro2);

        List<Contact> resp = userImplInstance.getContactsByKeys(proList);

        Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));
        Assertions.assertEquals(contact1,resp.get(0));
        Assertions.assertEquals(contact2,resp.get(1));
        ofy().delete().entity(contact1).now();
        ofy().delete().entity(contact2).now();
    }

    @Test
    void getAllUserProsForUser_nullContactID_test(){
        Assertions.assertEquals(List.of(),new UserImpl().getAllUserProsForUser(null,false));
    }

    @Test
    void getAllUserProsForUser_ValidContactID_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setContactId("123");
        userPro.setDelete(false);
        userImplInstance.savePro(userPro);

        PeopleRelationJDO userPro2 = new PeopleRelationJDO();
        userPro2.setId("id2");
        userPro2.setContactId("123");
        userPro2.setDelete(false);
        userImplInstance.savePro(userPro2);

        PeopleRelationJDO userPro3 = new PeopleRelationJDO();
        userPro3.setId("id3");
        userPro3.setContactId("123");
        userPro3.setDelete(false);
        userImplInstance.savePro(userPro3);
        Assertions.assertEquals(List.of(userPro,userPro2,userPro3),new UserImpl().getAllUserProsForUser("123",false));
    }

    @Test
    void getAllUsersWithContact_noUsers_test(){
        Assertions.assertEquals(Map.of(),userImplInstance.getAllUsersWithContact("accID",false,100,null));
    }

    @Test
    void getAllUsersWithContact_validUsers_test() {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setContactId("123");
        userPro.setUniquepin("accID");
        userPro.setDelete(false);

        PeopleRelationJDO userPro2 = new PeopleRelationJDO();
        userPro2.setId("id2");
        userPro2.setContactId("234");
        userPro2.setUniquepin("accID");
        userPro2.setDelete(false);

        userImplInstance.savePros(List.of(userPro, userPro2));


        Contact c1 = new Contact();
        c1.setId("123");
        Contact c2 = new Contact();
        c2.setId("234");
        ContactImpl.getContactImplInstance().saveCollection(List.of(c1, c2));
        userPro.setContact(c1);
        userPro2.setContact(c2);
        Assertions.assertEquals(List.of(userPro, userPro2), userImplInstance.getAllUsersWithContact("accID", false, 100, null).get("users"));
        ofy().delete().entity(c1).now();
        ofy().delete().entity(c2).now();
        ofy().delete().entities(List.of(userPro, userPro2)).now();
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getUserWithoutContact_invalid_accountID_test(String testValue){
        Assertions.assertNull(userImplInstance.getUserWithoutContact(testValue,"emailId"));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getUserWithoutContact_invalid_contactID_test(String testValue){
        Assertions.assertNull(userImplInstance.getUserWithoutContact("accountId",testValue));
    }

    @Test
    void getUserWithoutContact_valid_test() {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setContactId("123");
        userPro.setUniquepin("567");
        userPro.setDelete(false);
        userImplInstance.savePro(userPro);
        Assertions.assertNotNull(userImplInstance.getUserWithoutContact("567","123"));
        ofy().delete().entity(userPro).now();
    }

    @ParameterizedTest
    @NullAndEmptySource
    void getDefaultUserPro_invalid_contactID_test(String testValue){
        Assertions.assertNull(userImplInstance.getDefaultUserPro(testValue));
    }

    @Test
    void getDefaultUserPro_valid_test() {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setContactId("123");
        userPro.setDelete(false);
        userPro.setDefault(true);
        userImplInstance.savePro(userPro);
        Assertions.assertNotNull(userImplInstance.getDefaultUserPro("123"));
        ofy().delete().entity(userPro).now();
    }


    @ParameterizedTest
    @NullAndEmptySource
    void getDefaultUserProWithEmail_invalid_emailID_test(String testValue){
        Assertions.assertNull(userImplInstance.getDefaultUserProWithEmail(testValue));
    }

    @Test
    void getDefaultUserProWithEmail_valid_test() {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setEmailID("123@gmail.com");
        userPro.setDelete(false);
        userPro.setDefault(true);
        userImplInstance.savePro(userPro);
        Assertions.assertNotNull(userImplInstance.getDefaultUserProWithEmail("123@gmail.com"));
        ofy().delete().entity(userPro).now();
    }

    @Test
    void getAllUserProsForUserByEmail_nullEmailID_test(){
        Assertions.assertEquals(List.of(),userImplInstance.getAllUserProsForUserByEmail(null,false));
    }

    @Test
    void getAllUserProsForUserByEmail_noEntities_test(){
        Assertions.assertEquals(List.of(),userImplInstance.getAllUserProsForUserByEmail("email",null));
    }

    @Test
    void getAllUserProsForUserByEmail_validEntities_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setEmailID("email");
        userPro.setDelete(false);
        userImplInstance.savePro(userPro);
        PeopleRelationJDO userPro2 = new PeopleRelationJDO();
        userPro2.setId("id1");
        userPro2.setEmailID("email");
        userPro2.setDelete(true);
        userImplInstance.savePro(userPro2);
        PeopleRelationJDO userPro3 = new PeopleRelationJDO();
        userPro3.setId("id2");
        userPro3.setEmailID("email");
        userPro3.setDelete(false);
        userImplInstance.savePro(userPro3);
        Assertions.assertEquals(List.of(userPro,userPro3),userImplInstance.getAllUserProsForUserByEmail("email",false));
        userImplInstance.delete(List.of(userPro,userPro2,userPro3));
    }
}
