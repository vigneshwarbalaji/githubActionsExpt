package com.yoco.account.modal;

import com.yoco.commons.utils.TimeZoneUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.mock.web.MockHttpServletRequest;
import java.util.HashMap;
import java.util.Map;

class AccountCreationPayloadDTOTest {
    @Test
    void constructor_nullEmailTest(){
        try{
            new AccountCreationPayloadDTO(Map.of(),new MockHttpServletRequest(),"web");
        }catch (Exception e){
            Assertions.assertEquals("EmailID is invalid",e.getMessage());
        }
    }

    @Test
    void constructor_internalDomainTest(){
        try{
            new AccountCreationPayloadDTO(Map.of("emailID","email@anywhere.co"),new MockHttpServletRequest(),"web");
        }catch (Exception e){
            Assertions.assertEquals("Can not sign up with internal domains.",e.getMessage());
        }
    }

    @Test
    void constructor_nullUserNameTest(){
        try{
            new AccountCreationPayloadDTO(Map.of("emailID","email@gmail.co"),new MockHttpServletRequest(),"web");
        }catch (Exception e){
            Assertions.assertEquals("Username is invalid",e.getMessage());
        }
    }

    @Test
    void constructor_nullGoogleToken_nullOneTapToken_Test(){
        try{
            new AccountCreationPayloadDTO(Map.of("emailID","email@gmail.co","userName","name"),new MockHttpServletRequest(),"web");
        }catch (Exception e){
            Assertions.assertEquals("Unable to create ",e.getMessage());
        }
    }

    @Test
    void constructor_passwordInvalidTest(){
        try{
            new AccountCreationPayloadDTO(Map.of("emailID","email@gmail.co","userName","name","password","asd"),new MockHttpServletRequest(),"web");
        }catch (Exception e){
            Assertions.assertEquals("Password must contain 8 to 64 characters",e.getMessage());
        }
    }


    @Test
    void constructor_noOptionalFieldsTest(){
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO(Map.of("emailID","email@gmail.co","userName","name","password","asdasd123123"),new MockHttpServletRequest(),"");
        Assertions.assertEquals("email@gmail.co",payloadDTO.getEmailID());
        Assertions.assertEquals("name",payloadDTO.getUserName());
        Assertions.assertEquals("asdasd123123",payloadDTO.getPassword());
        Assertions.assertEquals("",payloadDTO.getGoogleToken());
        Assertions.assertEquals("",payloadDTO.getOneTapToken());
        Assertions.assertEquals("name",payloadDTO.getAccountName());
        Assertions.assertEquals("0",payloadDTO.getSysOffset());
        Assertions.assertEquals("free",payloadDTO.getPlan());
        Assertions.assertEquals("0.0",payloadDTO.getPrice());
        Assertions.assertEquals("",payloadDTO.getSrcRef());
        Assertions.assertEquals("",payloadDTO.getPhotoID());
        Assertions.assertEquals("",payloadDTO.getPhoneNumber());
        Assertions.assertEquals("",payloadDTO.getCountryCode());
        Assertions.assertEquals("web",payloadDTO.getSource());
        Assertions.assertEquals("127.0.0.1",payloadDTO.getIpAddress());
        Assertions.assertEquals(new GeoLocationDTO(new MockHttpServletRequest()),payloadDTO.getGeoLocation());
        Assertions.assertEquals("UTC",payloadDTO.getTimeZone());
    }

    @Test
    void constructor_validOptionalFieldsTest(){
        Map<String,Object> payload = new HashMap<>();
        payload.put("emailID","email@gmail.co");
        payload.put("userName","name");
        payload.put("googleAccessToken","token");
        payload.put("accountName","accName");
        payload.put("sysOffset",330);
        payload.put("plan","standard");
        payload.put("price","10.0");
        payload.put("srcRef","ref");
        payload.put("photoID","photo");
        payload.put("phoneNumber","999999999999");
        payload.put("countryCode","91");
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO(payload,new MockHttpServletRequest(),"web");
        Assertions.assertEquals("email@gmail.co",payloadDTO.getEmailID());
        Assertions.assertEquals("name",payloadDTO.getUserName());
        Assertions.assertEquals("",payloadDTO.getPassword());
        Assertions.assertEquals("token",payloadDTO.getGoogleToken());
        Assertions.assertEquals("accName",payloadDTO.getAccountName());
        Assertions.assertEquals("330",payloadDTO.getSysOffset());
        Assertions.assertEquals("standard",payloadDTO.getPlan());
        Assertions.assertEquals("10.0",payloadDTO.getPrice());
        Assertions.assertEquals("ref",payloadDTO.getSrcRef());
        Assertions.assertEquals("photo",payloadDTO.getPhotoID());
        Assertions.assertEquals("",payloadDTO.getPhoneNumber());
        Assertions.assertEquals("",payloadDTO.getCountryCode());
        Assertions.assertEquals("web",payloadDTO.getSource());
        Assertions.assertEquals("127.0.0.1",payloadDTO.getIpAddress());
        Assertions.assertEquals(new GeoLocationDTO(new MockHttpServletRequest()),payloadDTO.getGeoLocation());
        Assertions.assertEquals("Asia/Kolkata",payloadDTO.getTimeZone());
    }

    @Test
    void constructor_validOptionalFields_oneTapToken_Test(){
        Map<String,Object> payload = new HashMap<>();
        payload.put("emailID","email@gmail.co");
        payload.put("userName","name");
        payload.put("credential","oneTapToken");
        payload.put("accountName","accName");
        payload.put("sysOffset","330");
        payload.put("plan","standard");
        payload.put("price","10.0");
        payload.put("srcRef","ref");
        payload.put("photoID","photo");
        payload.put("phoneNumber","999999999999");
        payload.put("countryCode","91");
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO(payload,new MockHttpServletRequest(),"web");
        Assertions.assertEquals("email@gmail.co",payloadDTO.getEmailID());
        Assertions.assertEquals("name",payloadDTO.getUserName());
        Assertions.assertEquals("",payloadDTO.getPassword());
        Assertions.assertEquals("",payloadDTO.getGoogleToken());
        Assertions.assertEquals("oneTapToken",payloadDTO.getOneTapToken());
        Assertions.assertEquals("accName",payloadDTO.getAccountName());
        Assertions.assertEquals("330",payloadDTO.getSysOffset());
        Assertions.assertEquals("standard",payloadDTO.getPlan());
        Assertions.assertEquals("10.0",payloadDTO.getPrice());
        Assertions.assertEquals("ref",payloadDTO.getSrcRef());
        Assertions.assertEquals("photo",payloadDTO.getPhotoID());
        Assertions.assertEquals("",payloadDTO.getPhoneNumber());
        Assertions.assertEquals("",payloadDTO.getCountryCode());
        Assertions.assertEquals("web",payloadDTO.getSource());
        Assertions.assertEquals("127.0.0.1",payloadDTO.getIpAddress());
        Assertions.assertEquals(new GeoLocationDTO(new MockHttpServletRequest()),payloadDTO.getGeoLocation());
        Assertions.assertEquals("Asia/Kolkata",payloadDTO.getTimeZone());
    }

    @Test
    void setFirstAndLastName_onlyFirstname_test(){
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        payloadDTO.setUserName("name");
        payloadDTO.setFirstAndLastName();
        Assertions.assertEquals("name",payloadDTO.getFirstName());
        Assertions.assertEquals("",payloadDTO.getLastName());
    }

    @Test
    void setFirstAndLastName_validName_test(){
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        payloadDTO.setUserName("name last last2");
        payloadDTO.setFirstAndLastName();
        Assertions.assertEquals("name",payloadDTO.getFirstName());
        Assertions.assertEquals("last",payloadDTO.getLastName());
    }

    @Test
    void extractAndSetTimezone_validCoordinates_test(){
        try(MockedStatic<TimeZoneUtil> timeZoneUtilMockedStatic = Mockito.mockStatic(TimeZoneUtil.class)){
            timeZoneUtilMockedStatic.when(()->TimeZoneUtil.getTimezoneFromCoordinates("1","1")).thenReturn("Asia/Kolkata");
            AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
            GeoLocationDTO geoLocationDTO = new GeoLocationDTO();
            geoLocationDTO.setLatitude("1");
            geoLocationDTO.setLongitude("1");
            payloadDTO.setGeoLocation(geoLocationDTO);
            payloadDTO.extractAndSetTimezone();
            Assertions.assertEquals("Asia/Kolkata",payloadDTO.getTimeZone());
        }
    }

    @Test
    void extractAndSetPhoneNumber_invalid_number_test(){
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        payloadDTO.setPhoneNumber("123");
        payloadDTO.setCountryCode("789");
        payloadDTO.extractAndSetPhoneNumber();
        Assertions.assertEquals("",payloadDTO.getPhoneNumber());
        Assertions.assertEquals("",payloadDTO.getCountryCode());
    }

    @Test
    void extractAndSetPhoneNumber_valid_number_test(){
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        payloadDTO.setPhoneNumber("9999911111");
        payloadDTO.setCountryCode("IN");
        payloadDTO.extractAndSetPhoneNumber();
        Assertions.assertEquals("9999911111",payloadDTO.getPhoneNumber());
        Assertions.assertEquals("91",payloadDTO.getCountryCode());
    }


    @Test
    void generateRegistrationInfoDetails_emptyNumber_ValidCountryCode_test(){
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        payloadDTO.setPrice("0.0");
        payloadDTO.setPlan("free");
        payloadDTO.setIpAddress("1.1");
        payloadDTO.setPhoneNumber("");
        payloadDTO.setCountryCode("in");
        payloadDTO.setGeoLocation(new GeoLocationDTO(new MockHttpServletRequest()));
        Map<String,Object> actual = payloadDTO.generateRegistrationInfoDetails();
        Assertions.assertEquals(Map.of("price","0.0","plan","free","ipAddress","1.1","maxUsers","10","phone","","geoInfo", "{\"country\":\"\",\"city\":\"\",\"latlong\":\",\",\"region\":\"\"}"),actual);
    }
    @Test
    void generateRegistrationInfoDetails_valid_test(){
        AccountCreationPayloadDTO payloadDTO = new AccountCreationPayloadDTO();
        payloadDTO.setPrice("0.0");
        payloadDTO.setPlan("free");
        payloadDTO.setIpAddress("1.1");
        payloadDTO.setPhoneNumber("123");
        payloadDTO.setCountryCode("91");
        payloadDTO.setGeoLocation(new GeoLocationDTO(new MockHttpServletRequest()));
        Map<String,Object> actual = payloadDTO.generateRegistrationInfoDetails();
        Assertions.assertEquals(Map.of("price","0.0","plan","free","ipAddress","1.1","maxUsers","10","phone","91123","geoInfo", "{\"country\":\"\",\"city\":\"\",\"latlong\":\",\",\"region\":\"\"}"),actual);
    }
}
