package com.yoco.commons.modal.user;

import com.yoco.commons.constants.StoragePublicUrl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.modal.contact.ContactDTO;
import com.yoco.commons.utils.JsonUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.mockito.ArgumentMatchers.anyList;

class UserDTOTest {

    public PeopleRelationJDO getMockPRO(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setId("id");
        userPro.setUniquepin("accountId");
        userPro.setContactId("contactId");
        userPro.setEmailID("test@gmail.com");
        userPro.setParentContactId("pContactId");
        userPro.setRole(Skillset.ROLE_STAFF);
        userPro.setDelete(false);
        userPro.setDefault(true);
        userPro.setDateAddedLongTime(1499072216599L);
        userPro.setDateModified(1641816293211L);
        userPro.setTimeZoneDisplayName("(GMT+05:30) Asia/Kolkata (India Time)");
        userPro.setSkillsets(JsonUtil.getJson(Skillset.generateSkillsetForStaff("accountId",false)));
        return userPro;
    }

    @ParameterizedTest
    @NullAndEmptySource
    void UserDTO_nullAndEmpty_Skills_test(String mockValue){
        PeopleRelationJDO pro = getMockPRO();
        pro.setSkillsets(mockValue);
        UserDTO userDTO = new UserDTO(pro,null);
        Assertions.assertNotNull(userDTO);
        Assertions.assertEquals("id",userDTO.getId());
        Assertions.assertEquals("contactId",userDTO.getContactID());
        Assertions.assertEquals("accountId",userDTO.getAccountID());
        Assertions.assertEquals("test@gmail.com",userDTO.getEmailID());
        Assertions.assertEquals("pContactId",userDTO.getParentContactID());
        Assertions.assertEquals(Skillset.ROLE_STAFF,userDTO.getRole());
        Assertions.assertFalse(userDTO.isDeleted());
        Assertions.assertTrue(userDTO.isDefault());
        Assertions.assertEquals(1499072216599L,userDTO.getDateAdded());
        Assertions.assertEquals(1641816293211L,userDTO.getDateModified());
        Assertions.assertEquals("",userDTO.getEmpID());
        Assertions.assertEquals("(GMT+05:30) Asia/Kolkata (India Time)",userDTO.getTimezone());
        Assertions.assertEquals("",userDTO.getRfID());
        Assertions.assertNull(userDTO.getSkills());
        Assertions.assertNull(userDTO.getData_view_permission());
        Assertions.assertNull(userDTO.getContact());
        Assertions.assertNull(userDTO.getPermissions());
    }

    @Test
    void UserDTO_valid_test(){
        PeopleRelationJDO pro = getMockPRO();
        pro.setLogin("login");
        pro.setRfId("rfId");
        pro.setDataProtectionSkillset(DataProtectionSkillset.getDefaultUserDataProtectionSkillset().getValue());
        Contact contact = new Contact("contactId","test@gmail.com","test","", StoragePublicUrl.DUMMY_USER_PIC_URL,0L);
        pro.setContact(contact);
        Set<String> permissions = new HashSet<>();
        permissions.add(IAMPermission.CLOCK.toString());
        UserDTO userDTO = new UserDTO(pro,permissions);
        Assertions.assertNotNull(userDTO);
        Assertions.assertEquals("id",userDTO.getId());
        Assertions.assertEquals("contactId",userDTO.getContactID());
        Assertions.assertEquals("accountId",userDTO.getAccountID());
        Assertions.assertEquals("test@gmail.com",userDTO.getEmailID());
        Assertions.assertEquals("pContactId",userDTO.getParentContactID());
        Assertions.assertEquals(Skillset.ROLE_STAFF,userDTO.getRole());
        Assertions.assertFalse(userDTO.isDeleted());
        Assertions.assertTrue(userDTO.isDefault());
        Assertions.assertEquals(1499072216599L,userDTO.getDateAdded());
        Assertions.assertEquals(1641816293211L,userDTO.getDateModified());
        Assertions.assertEquals("login",userDTO.getEmpID());
        Assertions.assertEquals("(GMT+05:30) Asia/Kolkata (India Time)",userDTO.getTimezone());
        Assertions.assertEquals("rfId",userDTO.getRfID());
        Assertions.assertNotNull(userDTO.getSkills());
        Assertions.assertNotNull(userDTO.getData_view_permission());
        Assertions.assertEquals(new ContactDTO(contact),userDTO.getContact());
        Assertions.assertEquals(permissions,userDTO.getPermissions());
    }

    @Test
    void generateUserDTOList_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){

            Contact contact1 = new Contact();
            contact1.setId("contactId");

            Contact contact2 = new Contact();
            contact2.setId("contactId2");

            List<Contact> contactList = new ArrayList<>();
            contactList.add(contact1);
            contactList.add(contact2);

            UserImpl user = Mockito.mock(UserImpl.class);
            Mockito.when(user.getContactsByKeys(anyList())).thenReturn(contactList);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(user);

            List<PeopleRelationJDO> userProList = new ArrayList<>();
            userProList.add(getMockPRO());

            PeopleRelationJDO user2 = getMockPRO();
            user2.setContactId("contactId2");

            userProList.add(user2);

            List<UserDTO> response = new UserDTO().generateUserDTOList(userProList);

            UserDTO userDTO1 = new UserDTO(getMockPRO(),null);
            userDTO1.setContact(new ContactDTO(contact1));
            UserDTO userDTO2 = new UserDTO(user2,null);
            userDTO2.setContact(new ContactDTO(contact2));

            List<UserDTO> expected = new ArrayList<>();
            expected.add(userDTO1);
            expected.add(userDTO2);

            Assertions.assertEquals(expected,response);
        }
    }

    @Test
    void generateUserDTOList_empty_contact_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){

            Contact contact1 = new Contact();
            contact1.setId("contactId2");

            List<Contact> contactList = new ArrayList<>();
            contactList.add(contact1);

            UserImpl user = Mockito.mock(UserImpl.class);
            Mockito.when(user.getContactsByKeys(anyList())).thenReturn(contactList);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(user);

            List<PeopleRelationJDO> userProList = new ArrayList<>();
            userProList.add(getMockPRO());

            PeopleRelationJDO user2 = getMockPRO();
            user2.setContactId("");

            userProList.add(user2);

            List<UserDTO> response = new UserDTO().generateUserDTOList(userProList);

            UserDTO userDTO1 = new UserDTO(getMockPRO(),null);
            userDTO1.setContact(null);

            List<UserDTO> expected = new ArrayList<>();
            expected.add(userDTO1);

            Assertions.assertEquals(expected,response);
        }
    }

    @Test
    void generateUserDTOList_exception_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){

            Contact contact1 = new Contact();
            contact1.setId("contactId2");

            List<Contact> contactList = new ArrayList<>();
            contactList.add(contact1);

            UserImpl user = Mockito.mock(UserImpl.class);
            Mockito.when(user.getContactsByKeys(anyList())).thenThrow(new IllegalArgumentException());
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(user);

            List<PeopleRelationJDO> userProList = new ArrayList<>();
            userProList.add(getMockPRO());

            Assertions.assertEquals(new ArrayList<>(),new UserDTO().generateUserDTOList(userProList));
        }
    }

    @Test
    void generateUserDTOList_empty_list_test(){
        Assertions.assertEquals(new ArrayList<>(),new UserDTO().generateUserDTOList(null));
    }

}
