package com.yoco.account.endpoint;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.account.service.AccountService;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.modal.account.AccountDTO;
import com.yoco.commons.modal.user.UserDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Map;
import static org.mockito.ArgumentMatchers.any;

class AccountApiTest {
    @Test
    void getAccount_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new AccountApi().getAccount("accID",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void getAccount_valid_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        AccountDTO mockResponse = new AccountDTO();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<AccountService> mock = Mockito.mockConstruction(AccountService.class, (accountServiceMock, context) -> {
                Mockito.when(accountServiceMock.getAccount("accID","123")).thenReturn(mockResponse);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new AccountApi().getAccount("accID",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add("account",mockResponse);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

    @Test
    void getAccountsForUser_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new AccountApi().getAllAccountsForUser("contactID",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void getAccountsForUser_valid_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        List<SettingsJDO> mockResponse = List.of(new SettingsJDO());
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<AccountService> mock = Mockito.mockConstruction(AccountService.class, (accountServiceMock, context) -> {
                Mockito.when(accountServiceMock.getAllAccountsForUser("contactID","123")).thenReturn(mockResponse);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new AccountApi().getAllAccountsForUser("contactID",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add("accounts",mockResponse);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

    @Test
    void getDefaultAccountIDForUser_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserAccessTokenFromRequest(
                    any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new AccountApi().getDefaultAccountProForUser("contactID",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void getDefaultAccountIDForUser_valid_test(){
        OauthAccessToken token = new OauthAccessToken();
        UserDTO userDTO = new UserDTO();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<AccountService> mock = Mockito.mockConstruction(AccountService.class, (accountServiceMock, context) -> {
                Mockito.when(accountServiceMock.getDefaultAccountProForUser("contactID",token)).thenReturn(userDTO);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserAccessTokenFromRequest(any(HttpServletRequest.class))).thenReturn(token);
            ResponseEntity<GenericResponse> actual = new AccountApi().getDefaultAccountProForUser("contactID",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add("user",userDTO);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

    @Test
    void setDefaultAccountForUser_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(
                    any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new AccountApi().setDefaultAccountForUser("contactID","accID",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void setDefaultAccountForUser_valid_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        UserDTO userDTO = new UserDTO();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<AccountService> mock = Mockito.mockConstruction(AccountService.class, (accountServiceMock, context) -> {
                Mockito.when(accountServiceMock.setDefaultAccountForUser("contactID","123","accID")).thenReturn(userDTO);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new AccountApi().setDefaultAccountForUser("contactID","accID",new MockHttpServletRequest());
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add("user",userDTO);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

    @Test
    void deleteAccount_Exception_test(){
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(
                    any(HttpServletRequest.class))).thenThrow(new IllegalArgumentException("Exception test"));
            ResponseEntity<GenericResponse> actual = new AccountApi().deleteAccount("contactID",new MockHttpServletRequest(),"payload");
            var genericResponse = new GenericResponse(false, null, "Exception test");
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.BAD_REQUEST,actual.getStatusCode());
        }
    }

    @Test
    void deleteAccount_valid_test(){
        Contact mockContact = new Contact();
        mockContact.setId("123");
        AccountDTO accountDTO = new AccountDTO();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<AccountService> mock = Mockito.mockConstruction(AccountService.class, (accountServiceMock, context) -> {
                Mockito.when(accountServiceMock.deleteAccount(mockContact,"accID","payload")).thenReturn(accountDTO);
            })){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new AccountApi().deleteAccount("accID",new MockHttpServletRequest(),"payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add("account",accountDTO);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

    @Test
    void createAccount_exception_test(){
        HttpServletRequest request = new MockHttpServletRequest();
        try(MockedConstruction<AccountService> mock = Mockito.mockConstruction(AccountService.class, (accountServiceMock, context) -> {
            Mockito.when(accountServiceMock.createAccount(request,"payload")).thenThrow(new IllegalArgumentException("test exception"));
        })){
            new AccountApi().createAccount(request, "payload");
        }catch (Exception e){
            Assertions.assertEquals("test exception", e.getMessage());
        }
    }

    @Test
    void createAccount_valid_test(){
        HttpServletRequest request = new MockHttpServletRequest();
        try(MockedConstruction<AccountService> mock = Mockito.mockConstruction(AccountService.class, (accountServiceMock, context) -> {
            Mockito.when(accountServiceMock.createAccount(request,"payload")).thenReturn(Map.of("success",true));
        })){
            ResponseEntity<GenericResponse> actual = new AccountApi().createAccount(request, "payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add("success",true);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

    @Test
    void processOneTapRequest_exception_test(){
        HttpServletRequest request = new MockHttpServletRequest();
        try(MockedConstruction<AccountService> mock = Mockito.mockConstruction(AccountService.class, (accountServiceMock, context) -> {
            Mockito.when(accountServiceMock.processOneTapRequest(request,"payload")).thenThrow(new IllegalArgumentException("test exception"));
        })){
            new AccountApi().processOneTapRequest(request, "payload");
        }catch (Exception e){
            Assertions.assertEquals("test exception", e.getMessage());
        }
    }

    @Test
    void processOneTapRequest_valid_test(){
        HttpServletRequest request = new MockHttpServletRequest();
        try(MockedConstruction<AccountService> mock = Mockito.mockConstruction(AccountService.class, (accountServiceMock, context) -> {
            Mockito.when(accountServiceMock.processOneTapRequest(request,"payload")).thenReturn(Map.of("success",true));
        })){
            ResponseEntity<GenericResponse> actual = new AccountApi().processOneTapRequest(request, "payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add("success",true);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }

    @Test
    void updateAccount_exception_test(){
        HttpServletRequest request = new MockHttpServletRequest();
        Contact mockContact = new Contact();
        try(MockedConstruction<AccountService> mock = Mockito.mockConstruction(AccountService.class, (accountServiceMock, context) -> {
            Mockito.when(accountServiceMock.updateAccount(mockContact,"accID","payload")).thenThrow(new IllegalArgumentException("test exception"));
        });
            MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenReturn(mockContact);
            new AccountApi().updateAccount(request,"accID", "payload");
        }catch (Exception e){
            Assertions.assertEquals("test exception", e.getMessage());
        }
    }

    @Test
    void updateAccount_valid_test(){
        HttpServletRequest request = new MockHttpServletRequest();
        Contact mockContact = new Contact();
        try(MockedConstruction<AccountService> mock = Mockito.mockConstruction(AccountService.class, (accountServiceMock, context) -> {
            Mockito.when(accountServiceMock.updateAccount(mockContact,"accID","payload")).thenReturn(Map.of("success",true));
        });
            MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class)){
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(request)).thenReturn(mockContact);
            ResponseEntity<GenericResponse> actual = new AccountApi().updateAccount(request,"accID", "payload");
            var genericResponse = new GenericResponse(true, null, null);
            genericResponse.add("success",true);
            Assertions.assertEquals(genericResponse,actual.getBody());
            Assertions.assertEquals(HttpStatus.OK,actual.getStatusCode());
        }
    }
}
