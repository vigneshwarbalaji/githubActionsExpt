package com.yoco.contact.helper;

import com.yoco.commons.modal.contact.ContactDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.FCMService;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.constants.CommonConstants;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

class ContactTaskHandlerTest {

    public ContactTaskHandler contactTaskHandler = ContactTaskHandler.getInstance();

    @Test
    void handleProfileImageUpdate_Invalid_Action_test(){

        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){

            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"");

            contactTaskHandler.handleContactTaskHandler(taskMap);

            activityUtilMockedStatic.verifyNoInteractions();
            rtmServiceMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleProfileImageUpdate_valid_test(){

        try(MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<FCMService> fcmServiceMockedStatic = Mockito.mockStatic(FCMService.class)){

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString())).thenAnswer((Answer<Void>) invocation -> null);

            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(),anyString(),anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);

            FCMService fcmServiceMock = Mockito.mock(FCMService.class);
            Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            fcmServiceMockedStatic.when(FCMService::getFCMService).thenReturn(fcmServiceMock);

            UserDTO userDTO = new UserDTO();
            userDTO.setAccountID("accountId");
            userDTO.setContactID("contactId");

            ContactDTO contactDTO = new ContactDTO();
            contactDTO.setId("contactId");
            contactDTO.setEmailID("test@gmail.com");
            contactDTO.setPhotoID("photo2.png");
            contactDTO.setName("name");

            userDTO.setContact(contactDTO);

            String activity = "Contact update from DCM :test@gmail.com contactID : contactId. Details are : contact photoID : photo1.png DCM photoID : photo2.png";

            Map<String,Object> taskMap = new HashMap<>();
            taskMap.put(CommonConstants.ACTION,"profileUpdate");
            taskMap.put(CommonConstants.USER_KEY,userDTO);
            taskMap.put(CommonConstants.ACTIVITY,activity);

            contactTaskHandler.handleContactTaskHandler(taskMap);

            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("accountId","contactId","PROFILE_UPDATE", "test@gmail.com",
                    activity,"DUMMY"), times(1));

            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accountId","profile_update",CommonConstants.USER_KEY,userDTO));

            Mockito.verify(fcmServiceMock).notifyFCM("accountId", Set.of("contactId"),"profile",true,"name",null);

        }
    }

}