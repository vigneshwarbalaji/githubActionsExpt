package com.yoco.contact.service;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.AppMode;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.FullAuthService;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.GaeUtils;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.constants.CommonConstants;
import com.yoco.contact.helper.ProfileHelper;
import com.yoco.user.service.UserSkillService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class ProfileServiceTest {

    ProfileService profileService = new ProfileService();

    @Test
    void getProfileImageUploadUrl_test() throws NoSuchAlgorithmException, IOException {
        try (MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
             MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
             MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)) {
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(), anyMap(), any(String[].class), any())).thenReturn(Map.of(Commons.SUCCESS, true));
            profileService.getProfileImageUploadUrl("id", "fname");
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(eq(DcmConstants.getFetchProfileImageUrl()),eq(Map.of("contactID","id","title","fname")),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token","User-Agent","AppEngine-Google; (+http://code.google.com/appengine; appid: s~staging-goclockin-dashboard)"}),isNull()),times(1));
        }
    }

    @Test
    void updateProfileImage_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(), anyString(), any(String[].class), any())).thenReturn(Map.of(Commons.SUCCESS, true));

            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(new PeopleRelationJDO());

            UserDTO userDTO = new UserDTO();
            ProfileHelper profileHelper = Mockito.mock(ProfileHelper.class);
            Mockito.when(profileHelper.profileImageUpdateHandler(any(PeopleRelationJDO.class),anyString())).thenReturn(userDTO);
            profileHelperMockedStatic.when(ProfileHelper::getInstance).thenReturn(profileHelper);

            Map<String,Object> response = profileService.updateProfileImage(JsonUtil.getJson(Map.of("accountID","123","contactID","111","profile_pic_url","url?s=112")));
            Assertions.assertNotNull(response);
            Assertions.assertEquals(userDTO,response.get(CommonConstants.USER_KEY));
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(eq(DcmConstants.getUpdateProfileImageUrl()),eq(JsonUtil.getJson(Map.of("accountID","123","contactID","111","profile_pic_url","url?s=112"))),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token","User-Agent","AppEngine-Google; (+http://code.google.com/appengine; appid: s~staging-goclockin-dashboard)"}),isNull()),times(1));
        }
    }


    @Test
    void updateProfileImage_InvalidDTO_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){

            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(), anyString(), any(String[].class), any())).thenReturn(Map.of(Commons.SUCCESS, true));

            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(new PeopleRelationJDO());

            ProfileHelper contactHelper = Mockito.mock(ProfileHelper.class);
            Mockito.when(contactHelper.profileImageUpdateHandler(any(PeopleRelationJDO.class),anyString())).thenReturn(null);
            profileHelperMockedStatic.when(ProfileHelper::getInstance).thenReturn(contactHelper);

            Map<String,Object> response = profileService.updateProfileImage(JsonUtil.getJson(Map.of("accountID","123","contactID","111","profile_pic_url","url?s=112")));
            Assertions.assertNotNull(response);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(eq(DcmConstants.getUpdateProfileImageUrl()),eq(JsonUtil.getJson(Map.of("accountID","123","contactID","111","profile_pic_url","url?s=112"))),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token","User-Agent","AppEngine-Google; (+http://code.google.com/appengine; appid: s~staging-goclockin-dashboard)"}),isNull()),times(1));
        }
    }

    @Test
    void updateProfileImage_InvalidPRO_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.STAGING);
            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(), anyString(), any(String[].class), any())).thenReturn(Map.of(Commons.SUCCESS, true));

            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(null);

            Map<String,Object> response = profileService.updateProfileImage(JsonUtil.getJson(Map.of("accountID","123","contactID","111","profile_pic_url","url?s=112")));
            Assertions.assertNotNull(response);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(eq(DcmConstants.getUpdateProfileImageUrl()),eq(JsonUtil.getJson(Map.of("accountID","123","contactID","111","profile_pic_url","url?s=112"))),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token","User-Agent","AppEngine-Google; (+http://code.google.com/appengine; appid: s~staging-goclockin-dashboard)"}),isNull()),times(1));
        }
    }

    @Test
    void updateProfileImage_dcmFailure_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<FullAuthService> fullAuthServiceMockedStatic = Mockito.mockStatic(FullAuthService.class);
            MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.STAGING);

            fullAuthServiceMockedStatic.when(FullAuthService::getServerAccessToken).thenReturn("token");
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            urlFetcherMockedStatic.when(() -> UrlFetcher.sendPostRequest(anyString(), anyString(), any(String[].class), any())).thenReturn(Map.of(Commons.SUCCESS, false));

            Map<String,Object> response = profileService.updateProfileImage(JsonUtil.getJson(Map.of("accountID","123","contactID","111","profile_pic_url","url?s=112")));
            Assertions.assertNotNull(response);
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(response));
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPostRequest(eq(DcmConstants.getUpdateProfileImageUrl()),eq(JsonUtil.getJson(Map.of("accountID","123","contactID","111","profile_pic_url","url?s=112"))),eq(new String[]{"Content-Type","application/json","Authorization","Bearer token","User-Agent","AppEngine-Google; (+http://code.google.com/appengine; appid: s~staging-goclockin-dashboard)"}),isNull()),times(1));
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void updateProfile_invalid_payload_test(String testValue){
        try{
            profileService.updateProfile(null,testValue);
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void updateProfile_invalid_payload_json_test(){
        try{
            profileService.updateProfile(null,"payload");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value(),e.getMessage());
        }
    }

    @Test
    void updateProfile_unauthorized_user_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setRole("");
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(userPro);
            Contact contact = new Contact();
            contact.setId("id");
            profileService.updateProfile(contact,JsonUtil.getJson(Map.of(CommonConstants.ACCOUNT_ID_KEY,"accId",CommonConstants.CONTACT_ID_KEY,"id2")));
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void updateProfile_empty_dcmResp_test(Map<String,Object> testValue) throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setRole("");
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(userPro);

            ProfileHelper profileHelperMock = Mockito.mock(ProfileHelper.class);
            Mockito.when(profileHelperMock.updateProfileInDcm(anyMap(),anyString(),anyString())).thenReturn(testValue);
            profileHelperMockedStatic.when(ProfileHelper::getInstance).thenReturn(profileHelperMock);

            Contact contact = new Contact();
            contact.setId("id");
            Assertions.assertEquals(Map.of(),profileService.updateProfile(contact,JsonUtil.getJson(Map.of(CommonConstants.ACCOUNT_ID_KEY,"accId",CommonConstants.CONTACT_ID_KEY,"id"))));
            Mockito.verify(profileHelperMock).updateProfileInDcm(Map.of(CommonConstants.ACCOUNT_ID_KEY,"accId",CommonConstants.CONTACT_ID_KEY,"id"),"accId","id");
        }
    }

    @Test
    void updateProfile_empty_dcmResp_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setRole("");
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(userPro);

            ProfileHelper profileHelperMock = Mockito.mock(ProfileHelper.class);
            Mockito.when(profileHelperMock.updateProfileInDcm(anyMap(),anyString(),anyString())).thenReturn(Map.of(Commons.SUCCESS,false));
            profileHelperMockedStatic.when(ProfileHelper::getInstance).thenReturn(profileHelperMock);

            Contact contact = new Contact();
            contact.setId("id");
            Assertions.assertEquals(Map.of(),profileService.updateProfile(contact,JsonUtil.getJson(Map.of(CommonConstants.ACCOUNT_ID_KEY,"accId",CommonConstants.CONTACT_ID_KEY,"id"))));
            Mockito.verify(profileHelperMock).updateProfileInDcm(Map.of(CommonConstants.ACCOUNT_ID_KEY,"accId",CommonConstants.CONTACT_ID_KEY,"id"),"accId","id");
        }
    }

    @Test
    void updateProfile_nullDTO_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setRole(Skillset.ROLE_ADMIN);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenCallRealMethod();

            ProfileHelper profileHelperMock = Mockito.mock(ProfileHelper.class);
            List<Map<String,Object>> contactList = new ArrayList<>();
            contactList.add(Map.of("id","id"));
            Mockito.when(profileHelperMock.updateProfileInDcm(anyMap(),anyString(),anyString())).thenReturn(Map.of(Commons.SUCCESS,true, ContactConstants.CONTACT,contactList));
            Mockito.when(profileHelperMock.profileUpdateHandler(any(PeopleRelationJDO.class),anyMap())).thenReturn(null);
            profileHelperMockedStatic.when(ProfileHelper::getInstance).thenReturn(profileHelperMock);

            Contact contact = new Contact();
            contact.setId("id");
            Assertions.assertEquals(Map.of(),profileService.updateProfile(contact,JsonUtil.getJson(Map.of(CommonConstants.ACCOUNT_ID_KEY,"accId",CommonConstants.CONTACT_ID_KEY,"id2"))));
            Mockito.verify(profileHelperMock).updateProfileInDcm(Map.of(CommonConstants.ACCOUNT_ID_KEY,"accId",CommonConstants.CONTACT_ID_KEY,"id2"),"accId","id2");
            Mockito.verify(profileHelperMock).profileUpdateHandler(userPro,Map.of("id","id"));
        }
    }

    @Test
    void updateProfile_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setRole(Skillset.ROLE_ADMIN);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(userPro);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserAnAdminInAccount(any(PeopleRelationJDO.class))).thenCallRealMethod();

            UserDTO userDTO = new UserDTO();

            ProfileHelper profileHelperMock = Mockito.mock(ProfileHelper.class);
            List<Map<String,Object>> contactList = new ArrayList<>();
            contactList.add(Map.of("id","id"));
            Mockito.when(profileHelperMock.updateProfileInDcm(anyMap(),anyString(),anyString())).thenReturn(Map.of(Commons.SUCCESS,true, ContactConstants.CONTACT,contactList));
            Mockito.when(profileHelperMock.profileUpdateHandler(any(PeopleRelationJDO.class),anyMap())).thenReturn(userDTO);
            profileHelperMockedStatic.when(ProfileHelper::getInstance).thenReturn(profileHelperMock);

            Contact contact = new Contact();
            contact.setId("id");
            Assertions.assertEquals(Map.of(UserSkillService.UPDATED_PRO,userDTO),profileService.updateProfile(contact,JsonUtil.getJson(Map.of(CommonConstants.ACCOUNT_ID_KEY,"accId",CommonConstants.CONTACT_ID_KEY,"id2"))));
            Mockito.verify(profileHelperMock).updateProfileInDcm(Map.of(CommonConstants.ACCOUNT_ID_KEY,"accId",CommonConstants.CONTACT_ID_KEY,"id2"),"accId","id2");
            Mockito.verify(profileHelperMock).profileUpdateHandler(userPro,Map.of("id","id"));
        }
    }

    @Test
    void updateProfile_valid_self_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<ProfileHelper> profileHelperMockedStatic = Mockito.mockStatic(ProfileHelper.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setRole(Skillset.ROLE_ADMIN);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(userPro);

            UserDTO userDTO = new UserDTO();

            ProfileHelper profileHelperMock = Mockito.mock(ProfileHelper.class);
            List<Map<String,Object>> contactList = new ArrayList<>();
            contactList.add(Map.of("id","id"));
            Mockito.when(profileHelperMock.updateProfileInDcm(anyMap(),anyString(),anyString())).thenReturn(Map.of(Commons.SUCCESS,true, ContactConstants.CONTACT,contactList));
            Mockito.when(profileHelperMock.profileUpdateHandler(any(PeopleRelationJDO.class),anyMap())).thenReturn(userDTO);
            profileHelperMockedStatic.when(ProfileHelper::getInstance).thenReturn(profileHelperMock);

            Contact contact = new Contact();
            contact.setId("id");
            Assertions.assertEquals(Map.of(UserSkillService.UPDATED_PRO,userDTO),profileService.updateProfile(contact,JsonUtil.getJson(Map.of(CommonConstants.ACCOUNT_ID_KEY,"accId",CommonConstants.CONTACT_ID_KEY,"id"))));
            Mockito.verify(profileHelperMock).updateProfileInDcm(Map.of(CommonConstants.ACCOUNT_ID_KEY,"accId",CommonConstants.CONTACT_ID_KEY,"id"),"accId","id");
            Mockito.verify(profileHelperMock).profileUpdateHandler(userPro,Map.of("id","id"));
        }
    }


}