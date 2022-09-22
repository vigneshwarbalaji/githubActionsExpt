package com.yoco.account.helper;

import com.yoco.account.modal.AccountCreationPayloadDTO;
import com.yoco.commons.constants.UniversalSignupConstants;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.modal.user.ProUserInfo;
import com.yoco.commons.services.UrlFetcher;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Map;

class UniversalSignupHelperTest {
    @Test
    void createAccountAndValidateResponse_nullApiresponse_test(){
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<UniversalSignupConstants> universalSignupConstantsMockedStatic = Mockito.mockStatic(UniversalSignupConstants.class)){
                universalSignupConstantsMockedStatic.when(()->UniversalSignupConstants.getFreeSignupApiUrl()).thenReturn("url");
                urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url", Map.of(),new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON},null)).thenReturn(null);
                UniversalSignupHelper.createAccountAndValidateResponse(Map.of());
        }catch (Exception e){
            Assertions.assertEquals("Unable to create ",e.getMessage());
        }
    }

    @Test
    void createAccountAndValidateResponse_successFalseApiresponse_test(){
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<UniversalSignupConstants> universalSignupConstantsMockedStatic = Mockito.mockStatic(UniversalSignupConstants.class)){
            universalSignupConstantsMockedStatic.when(()->UniversalSignupConstants.getFreeSignupApiUrl()).thenReturn("url");
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url", Map.of(),new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON},null)).thenReturn(Map.of("success",false));
            UniversalSignupHelper.createAccountAndValidateResponse(Map.of());
        }catch (Exception e){
            Assertions.assertEquals("Unable to create ",e.getMessage());
        }
    }

    @Test
    void createAccountAndValidateResponse_alreadyCreatedTrueApiresponse_test(){
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<UniversalSignupConstants> universalSignupConstantsMockedStatic = Mockito.mockStatic(UniversalSignupConstants.class)){
            universalSignupConstantsMockedStatic.when(()->UniversalSignupConstants.getFreeSignupApiUrl()).thenReturn("url");
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url", Map.of(),new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON},null)).thenReturn(Map.of("success",true,"alreadyCreated",true));
            UniversalSignupHelper.createAccountAndValidateResponse(Map.of());
        }catch (Exception e){
            Assertions.assertEquals("Email is already registered. Please login",e.getMessage());
        }
    }

    @Test
    void createAccountAndValidateResponse_nullAccountMapApiresponse_test(){
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<UniversalSignupConstants> universalSignupConstantsMockedStatic = Mockito.mockStatic(UniversalSignupConstants.class)){
            universalSignupConstantsMockedStatic.when(()->UniversalSignupConstants.getFreeSignupApiUrl()).thenReturn("url");
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url", Map.of(),new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON},null)).thenReturn(Map.of("success",true,"alreadyCreated",false));
            UniversalSignupHelper.createAccountAndValidateResponse(Map.of());
        }catch (Exception e){
            Assertions.assertEquals("Unable to create ",e.getMessage());
        }
    }

    @Test
    void createAccountAndValidateResponse_nullContactMapApiresponse_test(){
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<UniversalSignupConstants> universalSignupConstantsMockedStatic = Mockito.mockStatic(UniversalSignupConstants.class)){
            universalSignupConstantsMockedStatic.when(()->UniversalSignupConstants.getFreeSignupApiUrl()).thenReturn("url");
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url", Map.of(),new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON},null)).thenReturn(Map.of("success",true,"alreadyCreated",false,"account",Map.of("key","value")));
            UniversalSignupHelper.createAccountAndValidateResponse(Map.of());
        }catch (Exception e){
            Assertions.assertEquals("Unable to create ",e.getMessage());
        }
    }

    @Test
    void createAccountAndValidateResponse_validApiresponse_test() throws IOException {
        try(MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class);
            MockedStatic<UniversalSignupConstants> universalSignupConstantsMockedStatic = Mockito.mockStatic(UniversalSignupConstants.class)){
            universalSignupConstantsMockedStatic.when(()->UniversalSignupConstants.getFreeSignupApiUrl()).thenReturn("url");
            Map<String,Object> expected = Map.of("success",true,"alreadyCreated",false,"account",Map.of("key","value"),"contact", List.of(Map.of()));
            urlFetcherMockedStatic.when(()->UrlFetcher.sendPostRequest("url", Map.of(),new String[]{UrlFetcher.CONTENT_TYPE, UrlFetcher.APPLICATION_JSON},null)).thenReturn(expected);
            Map<String,Object> actual = UniversalSignupHelper.createAccountAndValidateResponse(Map.of());
            Assertions.assertEquals(actual,expected);
        }
    }

    @Test
    void extractAndSaveSettingsJdoFromResponse_nullAccountID_test(){
        try{
            Map<String,Object> apiResponse = Map.of("account",Map.of());
            UniversalSignupHelper.extractAndSaveSettingsJdoFromResponse(apiResponse,new AccountCreationPayloadDTO());
        }catch (Exception e){
            Assertions.assertEquals("Unable to create ",e.getMessage());
        }
    }

    @Test
    void extractAndSaveSettingsJdoFromResponse_emptyAccountID_test(){
        try{
            Map<String,Object> apiResponse = Map.of("account",Map.of("id",""));
            UniversalSignupHelper.extractAndSaveSettingsJdoFromResponse(apiResponse,new AccountCreationPayloadDTO());
        }catch (Exception e){
            Assertions.assertEquals("Unable to create ",e.getMessage());
        }
    }

    @Test
    void extractAndSaveSettingsJdoFromResponse_valid_test(){
        try(MockedStatic<AccountImpl> accountImplMockedStatic = Mockito.mockStatic(AccountImpl.class)){
            Map<String,Object> apiResponse = Map.of("account",Map.of("id","accID","createdDate",1654108200000L));
            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            accountImplMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);
            AccountCreationPayloadDTO payloadDTO = Mockito.mock(AccountCreationPayloadDTO.class);
            Mockito.when(payloadDTO.getEmailID()).thenReturn("email");
            Mockito.when(payloadDTO.getAccountName()).thenReturn("accName");
            Mockito.when(payloadDTO.getTimeZone()).thenReturn("Asia/Kolkata");
            Mockito.when(payloadDTO.getSource()).thenReturn("web");
            Mockito.when(payloadDTO.getSrcRef()).thenReturn("src");
            Mockito.when(payloadDTO.getEmailID()).thenReturn("email");
            Mockito.when(payloadDTO.generateRegistrationInfoDetails()).thenReturn(Map.of());
            SettingsJDO expected = new SettingsJDO("accID","email","accName","Asia/Kolkata","web",Map.of(),"src");
            expected.setDateAddedLongTime(1654108200000L);
            expected.setDateAdded(new Date(1654108200000L));
            UniversalSignupHelper.extractAndSaveSettingsJdoFromResponse(apiResponse,payloadDTO);
            Mockito.verify(accountImplMock).saveAccount(expected);
        }
    }

    @Test
    void extractAndSaveUserProAndContactFromResponse_nullContactPersonMap_test(){
        try{
            UniversalSignupHelper.extractAndSaveUserProAndContactFromResponse(Map.of("contact",List.of()),new AccountCreationPayloadDTO(),new SettingsJDO());
        }catch (Exception e){
            Assertions.assertEquals("Unable to create ",e.getMessage());
        }
    }

    @Test
    void extractAndSaveUserProAndContactFromResponse_NullPhotoID_valid_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<ContactImpl> contactImplMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accID");
            account.setDateAddedLongTime(1654108200000L);
            AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
            payloadDTO.setEmailID("email");
            payloadDTO.setTimeZone("Asia/Kolkata");
            payloadDTO.setFirstName("first");
            payloadDTO.setLastName("last");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            contactImplMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);
            PeopleRelationJDO actual = UniversalSignupHelper.extractAndSaveUserProAndContactFromResponse(Map.of("contact",List.of(Map.of("category","company"),Map.of("category","person","id","123"))),payloadDTO,account);
            PeopleRelationJDO expected = new PeopleRelationJDO(new ProUserInfo("accID","123","email","admin","",""),"123","Asia/Kolkata","(GMT+05:30) Asia/Kolkata (India Time)",true,true);
            expected.setDateAddedLongTime(1654108200000L);
            expected.setDateModified(1654108200000L);
            expected.setDateAdded(new Date(1654108200000L));
            expected.setId(actual.getId());
            Contact expectedContact = new Contact("123","email","first","last","https://storage.googleapis.com/live-yoco/user.png",1654108200000L);
            expectedContact.setDateAddedLongTime(1654108200000L);
            expected.setContact(expectedContact);
            Mockito.verify(userImplMock).savePro(expected);
            Mockito.verify(contactImplMock).saveContact(expectedContact);
            Assertions.assertEquals(actual,expected);
        }
    }

    @Test
    void extractAndSaveUserProAndContactFromResponse_validPhotoID_valid_test(){
        try(MockedStatic<UserImpl> userImplMockedStatic = Mockito.mockStatic(UserImpl.class);
            MockedStatic<ContactImpl> contactImplMockedStatic = Mockito.mockStatic(ContactImpl.class)){
            SettingsJDO account = new SettingsJDO();
            account.setPeopleUniquePin("accID");
            account.setDateAddedLongTime(1654108200000L);
            AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
            payloadDTO.setEmailID("email");
            payloadDTO.setTimeZone("Asia/Kolkata");
            payloadDTO.setFirstName("first");
            payloadDTO.setLastName("last");
            payloadDTO.setPhotoID("photoID");
            UserImpl userImplMock = Mockito.mock(UserImpl.class);
            ContactImpl contactImplMock = Mockito.mock(ContactImpl.class);
            userImplMockedStatic.when(UserImpl::getUserImplInstance).thenReturn(userImplMock);
            contactImplMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactImplMock);
            PeopleRelationJDO actual = UniversalSignupHelper.extractAndSaveUserProAndContactFromResponse(Map.of("contact",List.of(Map.of("category","company"),Map.of("category","person","id","123"))),payloadDTO,account);
            PeopleRelationJDO expected = new PeopleRelationJDO(new ProUserInfo("accID","123","email","admin","",""),"123","Asia/Kolkata","(GMT+05:30) Asia/Kolkata (India Time)",true,true);
            expected.setDateAddedLongTime(1654108200000L);
            expected.setDateModified(1654108200000L);
            expected.setDateAdded(new Date(1654108200000L));
            expected.setId(actual.getId());
            Contact expectedContact = new Contact("123","email","first","last","photoID",1654108200000L);
            expectedContact.setDateAddedLongTime(1654108200000L);
            expected.setContact(expectedContact);
            Mockito.verify(userImplMock).savePro(expected);
            Mockito.verify(contactImplMock).saveContact(expectedContact);
            Assertions.assertEquals(actual,expected);
        }
    }
}
