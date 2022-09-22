package com.yoco.contact.helper.hook;

import com.fullauth.api.manage.exception.FullAuthApiException;
import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.user.helper.UserTaskInitiator;
import com.yoco.user.helper.staff.UserStaffHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class DeleteContactHookHelperTest {

    DeleteContactHookHelper deleteContactHookHelper = DeleteContactHookHelper.getInstance();

    @Test
    void processContactDeletion_no_pros_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class)){
            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getAllUserProsForUser(anyString(),anyBoolean())).thenReturn(null);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            deleteContactHookHelper.processContactDeletion(dcmContactDTO);
            Mockito.verify(userMock,times(1)).getAllUserProsForUser("contId",false);
        }
    }

    @Test
    void processContactDeletion_no_policies_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){
            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getAllUserProsForUser(anyString(),anyBoolean())).thenReturn(List.of(new PeopleRelationJDO()));
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(null);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            deleteContactHookHelper.processContactDeletion(dcmContactDTO);
            Mockito.verify(userMock,times(1)).getAllUserProsForUser("contId",false);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"),times(1));
        }
    }

    @Test
    void processContactDeletion_exception_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){
            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getAllUserProsForUser(anyString(),anyBoolean())).thenReturn(List.of(new PeopleRelationJDO()));
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenThrow(new IOException("exception"));
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            deleteContactHookHelper.processContactDeletion(dcmContactDTO);
            Mockito.verify(userMock,times(1)).getAllUserProsForUser("contId",false);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"),times(1));
        }
    }

    @Test
    void processContactDeletion_no_superAdmin_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("123");
            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getAllUserProsForUser(anyString(),anyBoolean())).thenReturn(List.of(userPro));
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setPermissions(Set.of(IAMPermission.ACTIVITY.toString()));
            accessPolicy.setResource("account/123");
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(List.of(accessPolicy));

            UserStaffHelper userStaffHelperMock = Mockito.mock(UserStaffHelper.class);
            Mockito.doNothing().when(userStaffHelperMock).updateAndSaveProForUserDisabling(any(PeopleRelationJDO.class));
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelperMock);

            PeopleRelationJDO primaryPro = new PeopleRelationJDO();
            primaryPro.setContactId("111");
            primaryPro.setUniquepin("123");
            userPROUtilMockedStatic.when(()-> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(primaryPro);
            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateDeleteUserQueue(any(PeopleRelationJDO.class),any(PeopleRelationJDO.class),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            accessManagerMockedStatic.when(()-> AccessManager.deleteAccessPolicies(anySet())).thenAnswer((Answer<Void>) invocation -> null);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            deleteContactHookHelper.processContactDeletion(dcmContactDTO);
            Mockito.verify(userMock,times(1)).getAllUserProsForUser("contId",false);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"),times(1));
            Mockito.verify(userStaffHelperMock,times(1)).updateAndSaveProForUserDisabling(userPro);
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getPrimaryAdmin("123"),times(1));
            userTaskInitiatorMockedStatic.verify(()-> UserTaskInitiator.initiateDeleteUserQueue(primaryPro,userPro,DeleteContactHookHelper.FULL_SYNC_DELETE),times(1));
            accessManagerMockedStatic.verify(()-> AccessManager.deleteAccessPolicies(Set.of("123")));
        }
    }


    @Test
    void processContactDeletion_superAdmin_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("123");
            userPro.setEmailID("test@gmail.com");
            UserImpl userMock = Mockito.mock(UserImpl.class);
            List<PeopleRelationJDO> proList = new ArrayList<>();
            proList.add(userPro);
            Mockito.when(userMock.getAllUserProsForUser(anyString(),anyBoolean())).thenReturn(proList);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setPermissions(Set.of(IAMPermission.SUPER_ADMIN.toString()));
            accessPolicy.setResource("account/123");
            List<AccessPolicy> policyList = new ArrayList<>();
            policyList.add(accessPolicy);
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(policyList);

            accessManagerMockedStatic.when(()-> AccessManager.deleteAccessPolicies(anySet())).thenAnswer((Answer<Void>) invocation -> null);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            deleteContactHookHelper.processContactDeletion(dcmContactDTO);
            Mockito.verify(userMock,times(1)).getAllUserProsForUser("contId",false);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"),times(1));
            activityUtilMockedStatic.verify(()-> ActivityUtil.saveActivity("123","contId",
                    "YoCo_Delete_FullSync","test@gmail.com",
                    "MailID test@gmail.com is PrimaryAdmin so not marking existing PRO as deleted",
                    ActivityUtil.ACTIVITIES.DUMMY.value()),times(1));
        }
    }

    @Test
    void processContactDeletion_initiateDeleteUser_Exception_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("123");
            UserImpl userMock = Mockito.mock(UserImpl.class);
            Mockito.when(userMock.getAllUserProsForUser(anyString(),anyBoolean())).thenReturn(List.of(userPro));
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setPermissions(Set.of(IAMPermission.ACTIVITY.toString()));
            accessPolicy.setResource("account/123");
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(List.of(accessPolicy));

            UserStaffHelper userStaffHelperMock = Mockito.mock(UserStaffHelper.class);
            Mockito.doNothing().when(userStaffHelperMock).updateAndSaveProForUserDisabling(any(PeopleRelationJDO.class));
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelperMock);

            PeopleRelationJDO primaryPro = new PeopleRelationJDO();
            primaryPro.setContactId("111");
            primaryPro.setUniquepin("123");
            userPROUtilMockedStatic.when(()-> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(primaryPro);
            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateDeleteUserQueue(any(PeopleRelationJDO.class),any(PeopleRelationJDO.class),anyString())).thenThrow(new IOException("exception"));

            accessManagerMockedStatic.when(()-> AccessManager.deleteAccessPolicies(anySet())).thenAnswer((Answer<Void>) invocation -> null);
            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            deleteContactHookHelper.processContactDeletion(dcmContactDTO);
            Mockito.verify(userMock,times(1)).getAllUserProsForUser("contId",false);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"),times(1));
            Mockito.verify(userStaffHelperMock,times(1)).updateAndSaveProForUserDisabling(userPro);
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getPrimaryAdmin("123"),times(1));
            userTaskInitiatorMockedStatic.verify(()-> UserTaskInitiator.initiateDeleteUserQueue(primaryPro,userPro,DeleteContactHookHelper.FULL_SYNC_DELETE),times(1));
            accessManagerMockedStatic.verify(()-> AccessManager.deleteAccessPolicies(Set.of("123")));
        }
    }

    @Test
    void processContactDeletion_mixedRoles_test(){
        try(MockedStatic<UserImpl> userMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<UserStaffHelper> userStaffHelperMockedStatic = Mockito.mockStatic(UserStaffHelper.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<UserTaskInitiator> userTaskInitiatorMockedStatic = Mockito.mockStatic(UserTaskInitiator.class)){

            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("123");
            userPro.setEmailID("test@gmail.com");
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setUniquepin("1233");
            userPro2.setEmailID("test2@gmail.com");
            UserImpl userMock = Mockito.mock(UserImpl.class);
            List<PeopleRelationJDO> proList = new ArrayList<>();
            proList.add(userPro);
            proList.add(userPro2);
            Mockito.when(userMock.getAllUserProsForUser(anyString(),anyBoolean())).thenReturn(proList);
            userMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userMock);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setPermissions(Set.of(IAMPermission.SUPER_ADMIN.toString()));
            accessPolicy.setResource("account/123");

            AccessPolicy accessPolicy2 = new AccessPolicy();
            accessPolicy2.setPermissions(Set.of(IAMPermission.ADMIN.toString()));
            accessPolicy2.setResource("account/123");

            List<AccessPolicy> policyList = new ArrayList<>();
            policyList.add(accessPolicy);
            policyList.add(accessPolicy2);
            accessManagerMockedStatic.when(()-> AccessManager.getAllActiveAccessPoliciesOfUser(anyString())).thenReturn(policyList);

            UserStaffHelper userStaffHelperMock = Mockito.mock(UserStaffHelper.class);
            Mockito.doNothing().when(userStaffHelperMock).updateAndSaveProForUserDisabling(any(PeopleRelationJDO.class));
            userStaffHelperMockedStatic.when(UserStaffHelper::getInstance).thenReturn(userStaffHelperMock);

            PeopleRelationJDO primaryPro = new PeopleRelationJDO();
            primaryPro.setContactId("111");
            primaryPro.setUniquepin("123");
            userPROUtilMockedStatic.when(()-> UserPROUtil.getPrimaryAdmin(anyString())).thenReturn(primaryPro);
            userTaskInitiatorMockedStatic.when(()-> UserTaskInitiator.initiateDeleteUserQueue(any(PeopleRelationJDO.class),any(PeopleRelationJDO.class),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            DcmContactDTO dcmContactDTO = new DcmContactDTO();
            dcmContactDTO.setId("contId");
            deleteContactHookHelper.processContactDeletion(dcmContactDTO);
            Mockito.verify(userMock,times(1)).getAllUserProsForUser("contId",false);
            accessManagerMockedStatic.verify(()-> AccessManager.getAllActiveAccessPoliciesOfUser("contId"),times(1));
            activityUtilMockedStatic.verify(()-> ActivityUtil.saveActivity("123","contId",
                    "YoCo_Delete_FullSync","test@gmail.com",
                    "MailID test@gmail.com is PrimaryAdmin so not marking existing PRO as deleted",
                    ActivityUtil.ACTIVITIES.DUMMY.value()),times(1));
            Mockito.verify(userStaffHelperMock,times(1)).updateAndSaveProForUserDisabling(userPro2);
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getPrimaryAdmin("1233"),times(1));
            userTaskInitiatorMockedStatic.verify(()-> UserTaskInitiator.initiateDeleteUserQueue(primaryPro,userPro2,DeleteContactHookHelper.FULL_SYNC_DELETE),times(1));
            accessManagerMockedStatic.verify(()-> AccessManager.deleteAccessPolicies(Set.of("123")));
        }
    }

    @Test
    void skipSuperAdminProsDeletionProcess_multipleSuperAdmins_test(){
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accId1");
            userPro.setEmailID("test@gmail.com");
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setUniquepin("accId2");
            userPro2.setEmailID("test@gmail.com");

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setResource("account/accId1");
            AccessPolicy accessPolicy2 = new AccessPolicy();
            accessPolicy2.setResource("account/accId2");

            List<PeopleRelationJDO> proList = new ArrayList<>();
            proList.add(userPro);
            proList.add(userPro2);
            List<PeopleRelationJDO> result = deleteContactHookHelper.skipSuperAdminProsDeletionProcess("cont",proList,List.of(accessPolicy,accessPolicy2));
            Assertions.assertTrue(result.isEmpty());
            activityUtilMockedStatic.verify(()-> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),anyString(),anyString()),times(2));
        }
    }

    @Test
    void skipSuperAdminProsDeletionProcess_singleSuperAdmins_test(){
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accId1");
            userPro.setEmailID("test@gmail.com");
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setUniquepin("accId2");
            userPro2.setEmailID("test@gmail.com");

            List<PeopleRelationJDO> proList = new ArrayList<>();
            proList.add(userPro);
            proList.add(userPro2);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setResource("account/accId1");

            List<PeopleRelationJDO> result = deleteContactHookHelper.skipSuperAdminProsDeletionProcess("contId",proList,List.of(accessPolicy));
            Assertions.assertFalse(result.isEmpty());
            Assertions.assertEquals(1,result.size());
            Assertions.assertEquals(userPro2,result.get(0));
            activityUtilMockedStatic.verify(()-> ActivityUtil.saveActivity("accId1","contId",
                    "YoCo_Delete_FullSync","test@gmail.com",
                    "MailID test@gmail.com is PrimaryAdmin so not marking existing PRO as deleted",
                    ActivityUtil.ACTIVITIES.DUMMY.value()),times(1));
        }
    }

    @Test
    void skipSuperAdminProsDeletionProcess_noSuperAdmins_test(){
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accId2");
            userPro.setEmailID("test@gmail.com");
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setUniquepin("accId3");
            userPro2.setEmailID("test@gmail.com");

            List<PeopleRelationJDO> proList = new ArrayList<>();
            proList.add(userPro);
            proList.add(userPro2);

            AccessPolicy accessPolicy = new AccessPolicy();
            accessPolicy.setResource("account/accId1");

            List<PeopleRelationJDO> result = deleteContactHookHelper.skipSuperAdminProsDeletionProcess("contId",proList,List.of(accessPolicy));
            Assertions.assertFalse(result.isEmpty());
            Assertions.assertEquals(2,result.size());
            Assertions.assertEquals(List.of(userPro,userPro2),result);
            activityUtilMockedStatic.verifyNoInteractions();
        }
    }


    @Test
    void persistSuperAdminDeletionActivity_singlePro_test(){
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accId1");
            userPro.setEmailID("test@gmail.com");
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setUniquepin("accId2");
            userPro2.setEmailID("test@gmail.com");
            List<PeopleRelationJDO> result = deleteContactHookHelper.persistSuperAdminDeletionActivity("contId",List.of(userPro,userPro2),List.of("accId1"));
            activityUtilMockedStatic.verify(()-> ActivityUtil.saveActivity("accId1","contId",
                    "YoCo_Delete_FullSync","test@gmail.com",
                    "MailID test@gmail.com is PrimaryAdmin so not marking existing PRO as deleted",
                    ActivityUtil.ACTIVITIES.DUMMY.value()),times(1));
            Assertions.assertEquals(1,result.size());
            Assertions.assertEquals(userPro,result.get(0));
        }
    }

    @Test
    void persistSuperAdminDeletionActivity_multiplePro_test(){
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accId1");
            userPro.setEmailID("test@gmail.com");
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setUniquepin("accId2");
            userPro2.setEmailID("test@gmail.com");
            List<PeopleRelationJDO> result = deleteContactHookHelper.persistSuperAdminDeletionActivity("contId",List.of(userPro,userPro2),List.of("accId1","accId2"));
            activityUtilMockedStatic.verify(()-> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),anyString(),anyString()),times(2));
            Assertions.assertEquals(2,result.size());
            Assertions.assertEquals(List.of(userPro,userPro2),result);
        }
    }

    @Test
    void persistSuperAdminDeletionActivity_noPro_test(){
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accId1");
            userPro.setEmailID("test@gmail.com");
            PeopleRelationJDO userPro2 = new PeopleRelationJDO();
            userPro2.setUniquepin("accId2");
            userPro2.setEmailID("test@gmail.com");
            List<PeopleRelationJDO> result = deleteContactHookHelper.persistSuperAdminDeletionActivity("contId",List.of(userPro,userPro2),List.of("accId"));
            activityUtilMockedStatic.verifyNoInteractions();
            Assertions.assertEquals(0,result.size());
        }
    }


    @Test
    void deleteAccessPoliciesOfContact_no_policies_test() throws FullAuthApiException, IOException {
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){
            deleteContactHookHelper.deleteAccessPoliciesOfContact(List.of());
            accessManagerMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void deleteAccessPoliciesOfContact_valid_test() throws FullAuthApiException, IOException {
        try(MockedStatic<AccessManager> accessManagerMockedStatic = Mockito.mockStatic(AccessManager.class)){
            AccessPolicy accessPolicy1 = new AccessPolicy();
            accessPolicy1.setResource("account/123");
            AccessPolicy accessPolicy2 = new AccessPolicy();
            accessPolicy2.setResource("account/456");
            deleteContactHookHelper.deleteAccessPoliciesOfContact(List.of(accessPolicy1,accessPolicy2));
            accessManagerMockedStatic.verify(()-> AccessManager.deleteAccessPolicies(Set.of("123","456")),times(1));
        }
    }


}