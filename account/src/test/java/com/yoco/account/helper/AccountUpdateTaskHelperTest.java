package com.yoco.account.helper;

import com.yoco.account.modal.AccountUpdatePayloadDTO;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.DcmUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;

class AccountUpdateTaskHelperTest {
    @Test
    void handleAccountUpdate_valid_test() throws NoSuchAlgorithmException, IOException {
        Contact contact = new Contact();
        AccountUpdatePayloadDTO payloadDTO = new AccountUpdatePayloadDTO();
        try(MockedStatic<AccountUpdateTaskHelper> accountUpdateTaskHelperMockedStatic = Mockito.mockStatic(AccountUpdateTaskHelper.class)){
            accountUpdateTaskHelperMockedStatic.when(()->AccountUpdateTaskHelper.handleAccountUpdate("accID",contact,payloadDTO)).thenCallRealMethod();
            AccountUpdateTaskHelper.handleAccountUpdate("accID",contact,payloadDTO);
            accountUpdateTaskHelperMockedStatic.verify(()->AccountUpdateTaskHelper.saveAccountUpdateActivity("accID",contact,payloadDTO));
            accountUpdateTaskHelperMockedStatic.verify(()->AccountUpdateTaskHelper.updateDcmAccountTimeZone("accID",payloadDTO));
            accountUpdateTaskHelperMockedStatic.verify(()->AccountUpdateTaskHelper.updateDcmContactPhoneNumber("accID",contact,payloadDTO));
        }
    }

    @Test
    void updateAccountTimezone_nullTimezone_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            AccountUpdateTaskHelper.updateDcmAccountTimeZone("accID",new AccountUpdatePayloadDTO());
            dcmUtilMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void updateAccountTimezone_validTimezone_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            AccountUpdatePayloadDTO accountUpdatePayloadDTO = new AccountUpdatePayloadDTO();
            accountUpdatePayloadDTO.setTimeZone("Asia/Kolkata");
            AccountUpdateTaskHelper.updateDcmAccountTimeZone("accID",accountUpdatePayloadDTO);
            dcmUtilMockedStatic.verify(()->DcmUtil.updateDcmAccount("accID", Map.of("timeZone","Asia/Kolkata")));
        }
    }

    @Test
    void updateDcmContactPhoneNumber_nullPhoneNumber_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            AccountUpdateTaskHelper.updateDcmContactPhoneNumber("accID",new Contact(),new AccountUpdatePayloadDTO());
            dcmUtilMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void updateDcmContactPhoneNumber_validPhoneNumber_noOldPhoneNumber_test() throws NoSuchAlgorithmException, IOException {
        Contact contact = new Contact();
        contact.setId("123");
        contact.setEmailID("email");
        AccountUpdatePayloadDTO accountUpdatePayloadDTO = new AccountUpdatePayloadDTO();
        accountUpdatePayloadDTO.setPhoneNumber("919000090000");
        accountUpdatePayloadDTO.setPhoneNumberCountry("IN");
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            AccountUpdateTaskHelper.updateDcmContactPhoneNumber("accID",contact,accountUpdatePayloadDTO);
            dcmUtilMockedStatic.verify(()->DcmUtil.updateContactInDcm("accID",Map.of("id","123","linkedContactMethods", List.of(Map.of("value","919000090000","type","phone","countryCode","+91","nationalNumber","9000090000")))));
        }
    }

    @Test
    void updateDcmContactPhoneNumber_validPhoneNumber_ValidOldPhoneNumber_test() throws NoSuchAlgorithmException, IOException {
        Contact contact = new Contact();
        contact.setId("123");
        contact.setEmailID("email");
        AccountUpdatePayloadDTO accountUpdatePayloadDTO = new AccountUpdatePayloadDTO();
        accountUpdatePayloadDTO.setPhoneNumber("919000090000");
        accountUpdatePayloadDTO.setPhoneNumberCountry("IN");
        accountUpdatePayloadDTO.setOldPhoneNumber("old");
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class);
            MockedStatic<AccountUpdateTaskHelper> accountUpdateTaskHelperMockedStatic = Mockito.mockStatic(AccountUpdateTaskHelper.class)){
            accountUpdateTaskHelperMockedStatic.when(()->AccountUpdateTaskHelper.updateDcmContactPhoneNumber("accID",contact,accountUpdatePayloadDTO)).thenCallRealMethod();
            accountUpdateTaskHelperMockedStatic.when(()->AccountUpdateTaskHelper.getExistingContactMethodIdForOldPhoneNumber("email","old")).thenReturn("methodID");
            AccountUpdateTaskHelper.updateDcmContactPhoneNumber("accID",contact,accountUpdatePayloadDTO);
            dcmUtilMockedStatic.verify(()->DcmUtil.updateContactInDcm("accID",Map.of("id","123","linkedContactMethods", List.of(Map.of("id","methodID","value","919000090000","type","phone","countryCode","+91","nationalNumber","9000090000")))));
        }
    }

    @Test
    void getExistingContactMethodIdForOldPhoneNumber_exception_test(){
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            dcmUtilMockedStatic.when(()->DcmUtil.fetchContactFromDcm("email")).thenThrow(new IllegalArgumentException(""));
            Assertions.assertEquals("",AccountUpdateTaskHelper.getExistingContactMethodIdForOldPhoneNumber("email","old"));
        }
    }

    @Test
    void getExistingContactMethodIdForOldPhoneNumber_NoMatch_test(){
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            Map<String,Object> dcmResponse = Map.of("contact",Map.of("linkedContactMethods",List.of(Map.of("value","123"))));
            dcmUtilMockedStatic.when(()->DcmUtil.fetchContactFromDcm("email")).thenReturn(dcmResponse);
            Assertions.assertNull(AccountUpdateTaskHelper.getExistingContactMethodIdForOldPhoneNumber("email","old"));
        }
    }

    @Test
    void getExistingContactMethodIdForOldPhoneNumber_ValidMatch_test(){
        try(MockedStatic<DcmUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmUtil.class)){
            Map<String,Object> dcmResponse = Map.of("contact",Map.of("linkedContactMethods",List.of(Map.of("id","methodID","value","+91123"))));
            dcmUtilMockedStatic.when(()->DcmUtil.fetchContactFromDcm("email")).thenReturn(dcmResponse);
            Assertions.assertEquals("methodID",AccountUpdateTaskHelper.getExistingContactMethodIdForOldPhoneNumber("email","91123"));
        }
    }

    @Test
    void saveAccountUpdateActivity_nullActivityMessage_test(){
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){
            AccountUpdateTaskHelper.saveAccountUpdateActivity("accID",new Contact(),new AccountUpdatePayloadDTO());
            activityUtilMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void saveAccountUpdateActivity_validActivityMessage_test(){
        Contact contact = new Contact();
        contact.setEmailID("email");
        contact.setId("123");
        AccountUpdatePayloadDTO payloadDTO = new AccountUpdatePayloadDTO();
        payloadDTO.setActivityMessage("message");
        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class)){
            AccountUpdateTaskHelper.saveAccountUpdateActivity("accID",contact,payloadDTO);
            activityUtilMockedStatic.verify(()->ActivityUtil.saveActivity(eq("accID"),eq("123"),eq("CompanyProfileUpdate"),eq("email"),eq("email has updated account : message"),eq("DUMMY"),anyLong()));
        }
    }
}
