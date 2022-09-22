package com.yoco.user.helper.staff;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.enums.error.DCM_ERROR_RESPONSE;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.utils.DcmUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;

class UserStaffDCMHelperTest {

    UserStaffDCMHelper dcmHelper = UserStaffDCMHelper.getInstance();

    public Map<String,Object> getDcmContactMap(){
        Map<String, Object> contactMap = new HashMap<>();
        contactMap.put("id","contactId");
        contactMap.put("firstName","fName");
        contactMap.put("lastName","lName");
        contactMap.put("deleted",false);
        contactMap.put("login","test@gmail.com");
        contactMap.put("brandID","brandID");
        contactMap.put("photoID","photoID");
        contactMap.put("accountID","accountId");
        contactMap.put("linkedContacts",new ArrayList<>());
        contactMap.put("linkedAccounts",new ArrayList<>());
        contactMap.put("is_password_present",false);
        return contactMap;
    }

    @ParameterizedTest
    @NullAndEmptySource
    void validateAndExtractContactFromDCM_failed_test(Map<String,Object> testValue) throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmUtil.fetchContactFromDcm(anyString())).thenReturn(testValue);
            Map<String,Object> resp =  dcmHelper.validateAndExtractContactFromDCM("emailId","accountId",null);
            Assertions.assertNotNull(resp);
            Assertions.assertFalse((Boolean) resp.get(Commons.SUCCESS));
            Assertions.assertEquals("Failed to upload the user, Please re-upload the user",resp.get(Commons.ERROR_RESPONSE));
        }
    }

    @Test
    void validateAndExtractContactFromDCM_contactExists_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put("contactExist",true);
            dcmUtilMockedStatic.when(()-> DcmUtil.fetchContactFromDcm(anyString())).thenReturn(mockResp);
            UserStaffDCMHelper userStaffDCMHelper = Mockito.spy(dcmHelper);
            Mockito.doReturn(null).when(userStaffDCMHelper).linkContactWithAccountInDcm(anyString(),anyMap());
            Assertions.assertNull(userStaffDCMHelper.validateAndExtractContactFromDCM("emailId","accountId",null));
            Mockito.verify(userStaffDCMHelper, times(1)).linkContactWithAccountInDcm("accountId",mockResp);
        }
    }

    @Test
    void validateAndExtractContactFromDCM_newContact_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            Map<String,Object> mockResp = new HashMap<>();
            mockResp.put("contactExist",false);
            dcmUtilMockedStatic.when(()-> DcmUtil.fetchContactFromDcm(anyString())).thenReturn(mockResp);
            UserStaffDCMHelper userStaffDCMHelper = Mockito.spy(dcmHelper);
            Mockito.doReturn(null).when(userStaffDCMHelper).createNewContactInDcm(anyString(),anyMap());
            Assertions.assertNull(userStaffDCMHelper.validateAndExtractContactFromDCM("emailId","accountId",new HashMap<>()));
            Mockito.verify(userStaffDCMHelper, times(1)).createNewContactInDcm("accountId",new HashMap<>());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    void linkContactWithAccountInDcm_invalidResp_test(Map<String,Object> testValue) {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmUtil.getActiveDefaultAccountFromDcm(anyString())).thenReturn("");
            dcmUtilMockedStatic.when(()-> DcmUtil.linkContactToAccount(anyString(),anyString())).thenReturn(testValue);
            Map<String,Object> payload = new HashMap<>();
            payload.put("contact",getDcmContactMap());
            Map<String,Object> resp = dcmHelper.linkContactWithAccountInDcm("accountId",payload);
            Assertions.assertNotNull(resp);
            Assertions.assertFalse((Boolean) resp.get(Commons.SUCCESS));
            Assertions.assertEquals(DCM_ERROR_RESPONSE.NOT_ALLOWED_IN_DCM.value(),resp.get(Commons.ERROR_RESPONSE));
        }
    }

    @Test
    void linkContactWithAccountInDcm_success_false_test() {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmUtil.getActiveDefaultAccountFromDcm(anyString())).thenReturn("");
            Map<String, Object> contactLinkResp = new HashMap<>();
            contactLinkResp.put(Commons.SUCCESS,false);
            dcmUtilMockedStatic.when(()-> DcmUtil.linkContactToAccount(anyString(),anyString())).thenReturn(contactLinkResp);
            Map<String,Object> payload = new HashMap<>();
            payload.put("contact",getDcmContactMap());
            Map<String,Object> resp = dcmHelper.linkContactWithAccountInDcm("accountId",payload);
            Assertions.assertNotNull(resp);
            Assertions.assertFalse((Boolean) resp.get(Commons.SUCCESS));
            Assertions.assertEquals(DCM_ERROR_RESPONSE.NOT_ALLOWED_IN_DCM.value(),resp.get(Commons.ERROR_RESPONSE));
            dcmUtilMockedStatic.verify(()-> DcmUtil.getActiveDefaultAccountFromDcm("test@gmail.com"));
        }
    }

    @Test
    void linkContactWithAccountInDcm_updateDefaultDomain_test() {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmUtil.getActiveDefaultAccountFromDcm(anyString())).thenReturn("");
            dcmUtilMockedStatic.when(()-> DcmUtil.updateDefaultAccountInDcm(anyString(),anyString())).thenReturn(new HashMap<>());
            Map<String, Object> contactLinkResp = new HashMap<>();
            contactLinkResp.put(Commons.SUCCESS,true);
            dcmUtilMockedStatic.when(()-> DcmUtil.linkContactToAccount(anyString(),anyString())).thenReturn(contactLinkResp);
            Map<String,Object> payload = new HashMap<>();
            Map<String,Object> dcmMap = getDcmContactMap();
            dcmMap.put("is_password_present",true);
            payload.put("contact",dcmMap);
            Map<String,Object> resp = dcmHelper.linkContactWithAccountInDcm("accountId",payload);
            Assertions.assertNotNull(resp);
            Assertions.assertTrue((Boolean) resp.get(Commons.SUCCESS));
            Assertions.assertTrue((Boolean) resp.get("markAsDefault"));
            Assertions.assertFalse((Boolean) resp.get("isNewUser"));
            Assertions.assertEquals(new DcmContactDTO(dcmMap),resp.get("dcmContact"));
            dcmUtilMockedStatic.verify(()-> DcmUtil.getActiveDefaultAccountFromDcm("test@gmail.com"));
            dcmUtilMockedStatic.verify(()-> DcmUtil.updateDefaultAccountInDcm("accountId","contactId"));
            dcmUtilMockedStatic.verify(()-> DcmUtil.linkContactToAccount("accountId","contactId"));
        }
    }

    @Test
    void linkContactWithAccountInDcm_valid_defaultDomain_test() {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmUtil.getActiveDefaultAccountFromDcm(anyString())).thenReturn("defaultAccount");
            Map<String, Object> contactLinkResp = new HashMap<>();
            contactLinkResp.put(Commons.SUCCESS,true);
            dcmUtilMockedStatic.when(()-> DcmUtil.linkContactToAccount(anyString(),anyString())).thenReturn(contactLinkResp);
            Map<String,Object> payload = new HashMap<>();
            payload.put("contact",getDcmContactMap());
            Map<String,Object> resp = dcmHelper.linkContactWithAccountInDcm("accountId",payload);
            Assertions.assertNotNull(resp);
            Assertions.assertTrue((Boolean) resp.get(Commons.SUCCESS));
            Assertions.assertFalse((Boolean) resp.get("markAsDefault"));
            Assertions.assertTrue((Boolean) resp.get("isNewUser"));
            Assertions.assertEquals(new DcmContactDTO(getDcmContactMap()),resp.get("dcmContact"));
            dcmUtilMockedStatic.verify(()-> DcmUtil.getActiveDefaultAccountFromDcm("test@gmail.com"));
            dcmUtilMockedStatic.verify(()-> DcmUtil.updateDefaultAccountInDcm("accountId","contactId"),times(0));
            dcmUtilMockedStatic.verify(()-> DcmUtil.linkContactToAccount("accountId","contactId"));
        }
    }

    @Test
    void linkContactWithAccountInDcm_exception_test() {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()-> DcmUtil.getActiveDefaultAccountFromDcm(anyString())).thenReturn("");
            dcmUtilMockedStatic.when(()-> DcmUtil.getActiveDefaultAccountFromDcm(anyString())).thenReturn("");
            dcmUtilMockedStatic.when(()-> DcmUtil.linkContactToAccount(anyString(),anyString())).thenThrow(new NoSuchAlgorithmException());
            Map<String,Object> payload = new HashMap<>();
            payload.put("contact",getDcmContactMap());
            Map<String,Object> resp = dcmHelper.linkContactWithAccountInDcm("accountId",payload);
            Assertions.assertNotNull(resp);
            Assertions.assertFalse((Boolean) resp.get(Commons.SUCCESS));
            Assertions.assertEquals(DCM_ERROR_RESPONSE.NOT_ALLOWED_IN_DCM.value(),resp.get(Commons.ERROR_RESPONSE));
        }
    }

    @Test
    void createNewContactInDcm_success_false_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){

            Map<String,Object> mockResponse = new HashMap<>();
            mockResponse.put(Commons.SUCCESS,false);
            dcmUtilMockedStatic.when(()-> DcmUtil.createUserInDcm(anyString(),anyString(),anyString(),anyString(),anyString(),anyString())).thenReturn(mockResponse);

            Map<String,Object> payload = new HashMap<>();
            payload.put(ContactConstants.EMAIL_ID,"test@gmail.com");
            payload.put(ContactConstants.FIRST_NAME,"");
            payload.put(ContactConstants.LAST_NAME,"");
            payload.put(ContactConstants.PHOTO_ID,"");

            Map<String,Object> resp = dcmHelper.createNewContactInDcm("accountId",payload);

            Assertions.assertNotNull(resp);
            Assertions.assertFalse((Boolean) resp.get(Commons.SUCCESS));
            Assertions.assertEquals(DCM_ERROR_RESPONSE.NOT_ALLOWED_IN_DCM.value(),resp.get(Commons.ERROR_RESPONSE));
        }
    }

    @Test
    void createNewContactInDcm_success_true_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){

            Map<String,Object> mockResponse = new HashMap<>();
            mockResponse.put(Commons.SUCCESS,true);
            mockResponse.put("contact",getDcmContactMap());
            dcmUtilMockedStatic.when(()-> DcmUtil.createUserInDcm(anyString(),anyString(),anyString(),anyString(),anyString(),anyString())).thenReturn(mockResponse);
            dcmUtilMockedStatic.when(()-> DcmUtil.updateDefaultAccountInDcm(anyString(),anyString())).thenReturn(new HashMap<>());

            Map<String,Object> payload = new HashMap<>();
            payload.put(ContactConstants.EMAIL_ID,"test@gmail.com");
            payload.put(ContactConstants.FIRST_NAME,"fName");
            payload.put(ContactConstants.LAST_NAME,"lName");
            payload.put(ContactConstants.PHOTO_ID,"photo");

            Map<String,Object> resp = dcmHelper.createNewContactInDcm("accountId",payload);

            Assertions.assertNotNull(resp);
            Assertions.assertTrue((Boolean) resp.get(Commons.SUCCESS));
            Assertions.assertTrue((Boolean) resp.get("markAsDefault"));
            Assertions.assertTrue((Boolean) resp.get("isNewUser"));
            Assertions.assertEquals(new DcmContactDTO(getDcmContactMap()),resp.get("dcmContact"));
            dcmUtilMockedStatic.verify(()-> DcmUtil.updateDefaultAccountInDcm("accountId","contactId"),times(1));
            dcmUtilMockedStatic.verify(()-> DcmUtil.createUserInDcm("accountId","test@gmail.com","","fName","lName","photo"),times(1));
        }
    }

}