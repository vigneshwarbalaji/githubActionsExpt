package com.yoco.adjustment.helper;

import com.yoco.MockEvent;
import com.yoco.adjustment.helper.approve.ApproveAdjustmentEmailHelper;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.InAppReminderUtil;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class AdjustmentTaskHelperTest {

    @Test
    void handleApproveAdjustment_noUserPro_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AdjustmentChannelPublishHelper> adjustmentChannelPublishHelperMockedStatic = Mockito.mockStatic(AdjustmentChannelPublishHelper.class)){
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(null);
            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("adminPRO",adminPro);
            Map<String,Object> eventMap = new HashMap<>();
            eventMap.put(SchedulingKeys.MERCHANT,"account123");
            eventMap.put(SchedulingKeys.PROVIDER,new ArrayList<>(){{add("contactId");}});
            payloadMap.put("eventMap",eventMap);
            AdjustmentTaskHelper.handleApproveAdjustment(payloadMap);
            adjustmentChannelPublishHelperMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleApproveAdjustment_inActiveUserPro_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AdjustmentChannelPublishHelper> adjustmentChannelPublishHelperMockedStatic = Mockito.mockStatic(AdjustmentChannelPublishHelper.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<ApproveAdjustmentEmailHelper> approveAdjustmentEmailHelperMockedStatic = Mockito.mockStatic(ApproveAdjustmentEmailHelper.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setEmailID("user@gmail.com");
            userPro.setTimeZone(DateConstants.ZONE_ID_IST);
            userPro.setDelete(true);
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(userPro);

            AdjustmentChannelPublishHelper adjustmentChannelPublishHelper = Mockito.mock(AdjustmentChannelPublishHelper.class);
            Mockito.doNothing().when(adjustmentChannelPublishHelper).publishApproveAdjustment(any(ReportsDTO.class),any(AdjustmentDTO.class));
            adjustmentChannelPublishHelperMockedStatic.when(AdjustmentChannelPublishHelper::getInstance).thenReturn(adjustmentChannelPublishHelper);

            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO account = new SettingsJDO();
            Mockito.when(accountImplMock.getById(anyString())).thenReturn(account);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);

            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("adminPRO",adminPro);
            payloadMap.put("eventMap", MockEvent.getRawEvent());

            AdjustmentTaskHelper.handleApproveAdjustment(payloadMap);
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserProWithContact("123","111"));
            Mockito.verify(adjustmentChannelPublishHelper).publishApproveAdjustment(any(ReportsDTO.class),any(AdjustmentDTO.class));
            Mockito.verify(accountImplMock).getById("123");
            approveAdjustmentEmailHelperMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleApproveAdjustment_nullSettingsJDO_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AdjustmentChannelPublishHelper> adjustmentChannelPublishHelperMockedStatic = Mockito.mockStatic(AdjustmentChannelPublishHelper.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<ApproveAdjustmentEmailHelper> approveAdjustmentEmailHelperMockedStatic = Mockito.mockStatic(ApproveAdjustmentEmailHelper.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setEmailID("user@gmail.com");
            userPro.setTimeZone(DateConstants.ZONE_ID_IST);
            userPro.setDelete(false);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserProActive(any(PeopleRelationJDO.class))).thenCallRealMethod();
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(userPro);

            AdjustmentChannelPublishHelper adjustmentChannelPublishHelper = Mockito.mock(AdjustmentChannelPublishHelper.class);
            Mockito.doNothing().when(adjustmentChannelPublishHelper).publishApproveAdjustment(any(ReportsDTO.class),any(AdjustmentDTO.class));
            adjustmentChannelPublishHelperMockedStatic.when(AdjustmentChannelPublishHelper::getInstance).thenReturn(adjustmentChannelPublishHelper);

            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getById(anyString())).thenReturn(null);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);

            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("adminPRO",adminPro);
            payloadMap.put("eventMap", MockEvent.getRawEvent());

            AdjustmentTaskHelper.handleApproveAdjustment(payloadMap);
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserProWithContact("123","111"));
            Mockito.verify(adjustmentChannelPublishHelper).publishApproveAdjustment(any(ReportsDTO.class),any(AdjustmentDTO.class));
            Mockito.verify(accountImplMock).getById("123");
            approveAdjustmentEmailHelperMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleApproveAdjustment_activeUserPro_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AdjustmentChannelPublishHelper> adjustmentChannelPublishHelperMockedStatic = Mockito.mockStatic(AdjustmentChannelPublishHelper.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<ApproveAdjustmentEmailHelper> approveAdjustmentEmailHelperMockedStatic = Mockito.mockStatic(ApproveAdjustmentEmailHelper.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setEmailID("user@gmail.com");
            userPro.setTimeZone(DateConstants.ZONE_ID_IST);
            userPro.setDelete(false);
            Contact contact = new Contact();
            userPro.setContact(contact);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserProActive(any(PeopleRelationJDO.class))).thenCallRealMethod();
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(userPro);

            AdjustmentChannelPublishHelper adjustmentChannelPublishHelper = Mockito.mock(AdjustmentChannelPublishHelper.class);
            Mockito.doNothing().when(adjustmentChannelPublishHelper).publishApproveAdjustment(any(ReportsDTO.class),any(AdjustmentDTO.class));
            adjustmentChannelPublishHelperMockedStatic.when(AdjustmentChannelPublishHelper::getInstance).thenReturn(adjustmentChannelPublishHelper);

            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO account = new SettingsJDO();
            Mockito.when(accountImplMock.getById(anyString())).thenReturn(account);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);

            ApproveAdjustmentEmailHelper approveAdjustmentEmailHelper = Mockito.mock(ApproveAdjustmentEmailHelper.class);
            Mockito.doNothing().when(approveAdjustmentEmailHelper).adjustmentApprovedMailHandler(any(Contact.class),any(Contact.class),anyString(),any(AdjustmentDTO.class),any(SettingsJDO.class));
            approveAdjustmentEmailHelperMockedStatic.when(ApproveAdjustmentEmailHelper::getInstance).thenReturn(approveAdjustmentEmailHelper);

            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            adminPro.setContact(contact);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("adminPRO",adminPro);
            payloadMap.put("eventMap", MockEvent.getRawEvent());

            AdjustmentTaskHelper.handleApproveAdjustment(payloadMap);
            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserProWithContact("123","111"));
            Mockito.verify(adjustmentChannelPublishHelper).publishApproveAdjustment(any(ReportsDTO.class),any(AdjustmentDTO.class));
            Mockito.verify(accountImplMock).getById("123");
            Mockito.verify(approveAdjustmentEmailHelper).adjustmentApprovedMailHandler(any(Contact.class),any(Contact.class),anyString(),any(AdjustmentDTO.class),any(SettingsJDO.class));
        }
    }

    @Test
    void handleEditApproveAdjustment_nullPRO_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AdjustmentChannelPublishHelper> adjustmentChannelPublishHelperMockedStatic = Mockito.mockStatic(AdjustmentChannelPublishHelper.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<ApproveAdjustmentEmailHelper> approveAdjustmentEmailHelperMockedStatic = Mockito.mockStatic(ApproveAdjustmentEmailHelper.class);
            MockedStatic<InAppReminderUtil> inAppReminderUtilMockedStatic = Mockito.mockStatic(InAppReminderUtil.class)){

            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(null);

            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            Contact contact = new Contact();
            adminPro.setContact(contact);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("adminPRO",adminPro);
            payloadMap.put("eventMap", MockEvent.getRawEvent());

            AdjustmentTaskHelper.handleEditApproveAdjustment(payloadMap);

            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserProWithContact("123","111"));
            adjustmentChannelPublishHelperMockedStatic.verifyNoInteractions();
            accountMockedStatic.verifyNoInteractions();
            approveAdjustmentEmailHelperMockedStatic.verifyNoInteractions();
            inAppReminderUtilMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleEditApproveAdjustment_nullAccount_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AdjustmentChannelPublishHelper> adjustmentChannelPublishHelperMockedStatic = Mockito.mockStatic(AdjustmentChannelPublishHelper.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<ApproveAdjustmentEmailHelper> approveAdjustmentEmailHelperMockedStatic = Mockito.mockStatic(ApproveAdjustmentEmailHelper.class);
            MockedStatic<InAppReminderUtil> inAppReminderUtilMockedStatic = Mockito.mockStatic(InAppReminderUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setEmailID("user@gmail.com");
            userPro.setTimeZone(DateConstants.ZONE_ID_IST);
            userPro.setDelete(false);
            Contact contact = new Contact();
            userPro.setContact(contact);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserProActive(any(PeopleRelationJDO.class))).thenCallRealMethod();
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(userPro);

            AdjustmentChannelPublishHelper adjustmentChannelPublishHelper = Mockito.mock(AdjustmentChannelPublishHelper.class);
            Mockito.doNothing().when(adjustmentChannelPublishHelper).publishEditApproveAdjustment(any(ReportsDTO.class),any(AdjustmentDTO.class),anyString());
            Mockito.doNothing().when(adjustmentChannelPublishHelper).publishAdjustmentActionToAW(any(ReportsDTO.class),any(AdjustmentDTO.class));
            adjustmentChannelPublishHelperMockedStatic.when(AdjustmentChannelPublishHelper::getInstance).thenReturn(adjustmentChannelPublishHelper);

            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            Mockito.when(accountImplMock.getById(anyString())).thenReturn(null);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);

            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            adminPro.setContact(contact);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("adminPRO",adminPro);
            payloadMap.put("eventMap", MockEvent.getRawEvent());

            AdjustmentTaskHelper.handleEditApproveAdjustment(payloadMap);

            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserProWithContact("123","111"));
            Mockito.verify(adjustmentChannelPublishHelper).publishEditApproveAdjustment(any(ReportsDTO.class),any(AdjustmentDTO.class),anyString());
            Mockito.verify(adjustmentChannelPublishHelper).publishAdjustmentActionToAW(any(ReportsDTO.class),any(AdjustmentDTO.class));
            Mockito.verify(accountImplMock).getById("123");
            approveAdjustmentEmailHelperMockedStatic.verifyNoInteractions();
            inAppReminderUtilMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleEditApproveAdjustment_InActiveUserPro_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AdjustmentChannelPublishHelper> adjustmentChannelPublishHelperMockedStatic = Mockito.mockStatic(AdjustmentChannelPublishHelper.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<ApproveAdjustmentEmailHelper> approveAdjustmentEmailHelperMockedStatic = Mockito.mockStatic(ApproveAdjustmentEmailHelper.class);
            MockedStatic<InAppReminderUtil> inAppReminderUtilMockedStatic = Mockito.mockStatic(InAppReminderUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setEmailID("user@gmail.com");
            userPro.setTimeZone(DateConstants.ZONE_ID_IST);
            userPro.setDelete(true);
            Contact contact = new Contact();
            userPro.setContact(contact);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserProActive(any(PeopleRelationJDO.class))).thenCallRealMethod();
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(userPro);

            AdjustmentChannelPublishHelper adjustmentChannelPublishHelper = Mockito.mock(AdjustmentChannelPublishHelper.class);
            Mockito.doNothing().when(adjustmentChannelPublishHelper).publishEditApproveAdjustment(any(ReportsDTO.class),any(AdjustmentDTO.class),anyString());
            Mockito.doNothing().when(adjustmentChannelPublishHelper).publishAdjustmentActionToAW(any(ReportsDTO.class),any(AdjustmentDTO.class));
            adjustmentChannelPublishHelperMockedStatic.when(AdjustmentChannelPublishHelper::getInstance).thenReturn(adjustmentChannelPublishHelper);

            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO account = new SettingsJDO();
            Mockito.when(accountImplMock.getById(anyString())).thenReturn(account);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);

            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            adminPro.setContact(contact);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("adminPRO",adminPro);
            payloadMap.put("eventMap", MockEvent.getRawEvent());

            AdjustmentTaskHelper.handleEditApproveAdjustment(payloadMap);

            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserProWithContact("123","111"));
            Mockito.verify(adjustmentChannelPublishHelper).publishEditApproveAdjustment(any(ReportsDTO.class),any(AdjustmentDTO.class),anyString());
            Mockito.verify(adjustmentChannelPublishHelper).publishAdjustmentActionToAW(any(ReportsDTO.class),any(AdjustmentDTO.class));
            Mockito.verify(accountImplMock).getById("123");
            approveAdjustmentEmailHelperMockedStatic.verifyNoInteractions();
            inAppReminderUtilMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleEditApproveAdjustment_activeUserPro_test() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<AdjustmentChannelPublishHelper> adjustmentChannelPublishHelperMockedStatic = Mockito.mockStatic(AdjustmentChannelPublishHelper.class);
            MockedStatic<AccountImpl> accountMockedStatic = Mockito.mockStatic(AccountImpl.class);
            MockedStatic<ApproveAdjustmentEmailHelper> approveAdjustmentEmailHelperMockedStatic = Mockito.mockStatic(ApproveAdjustmentEmailHelper.class);
            MockedStatic<InAppReminderUtil> inAppReminderUtilMockedStatic = Mockito.mockStatic(InAppReminderUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setEmailID("user@gmail.com");
            userPro.setTimeZone(DateConstants.ZONE_ID_IST);
            userPro.setDelete(false);
            Contact contact = new Contact();
            userPro.setContact(contact);
            userPROUtilMockedStatic.when(()-> UserPROUtil.isUserProActive(any(PeopleRelationJDO.class))).thenCallRealMethod();
            userPROUtilMockedStatic.when(()-> UserPROUtil.getUserProWithContact(anyString(),anyString())).thenReturn(userPro);

            AdjustmentChannelPublishHelper adjustmentChannelPublishHelper = Mockito.mock(AdjustmentChannelPublishHelper.class);
            Mockito.doNothing().when(adjustmentChannelPublishHelper).publishEditApproveAdjustment(any(ReportsDTO.class),any(AdjustmentDTO.class),anyString());
            Mockito.doNothing().when(adjustmentChannelPublishHelper).publishAdjustmentActionToAW(any(ReportsDTO.class),any(AdjustmentDTO.class));
            adjustmentChannelPublishHelperMockedStatic.when(AdjustmentChannelPublishHelper::getInstance).thenReturn(adjustmentChannelPublishHelper);

            AccountImpl accountImplMock = Mockito.mock(AccountImpl.class);
            SettingsJDO account = new SettingsJDO();
            Mockito.when(accountImplMock.getById(anyString())).thenReturn(account);
            accountMockedStatic.when(AccountImpl::getAccountImplInstance).thenReturn(accountImplMock);

            ApproveAdjustmentEmailHelper approveAdjustmentEmailHelper = Mockito.mock(ApproveAdjustmentEmailHelper.class);
            Mockito.doNothing().when(approveAdjustmentEmailHelper).adjustmentEditApprovedMailHandler(any(Contact.class),any(Contact.class),anyString(),any(AdjustmentDTO.class),any(SettingsJDO.class));
            approveAdjustmentEmailHelperMockedStatic.when(ApproveAdjustmentEmailHelper::getInstance).thenReturn(approveAdjustmentEmailHelper);

            InAppReminderUtil inAppReminderUtil = Mockito.mock(InAppReminderUtil.class);
            Mockito.doNothing().when(inAppReminderUtil).handleInAppNotificationForAdjustment(any(AdjustmentDTO.class),any(PeopleRelationJDO.class),anyString());
            inAppReminderUtilMockedStatic.when(InAppReminderUtil::getInstance).thenReturn(inAppReminderUtil);

            PeopleRelationJDO adminPro = new PeopleRelationJDO();
            adminPro.setContact(contact);
            Map<String,Object> payloadMap = new HashMap<>();
            payloadMap.put("adminPRO",adminPro);
            payloadMap.put("eventMap", MockEvent.getRawEvent());

            AdjustmentTaskHelper.handleEditApproveAdjustment(payloadMap);

            userPROUtilMockedStatic.verify(()-> UserPROUtil.getUserProWithContact("123","111"));
            Mockito.verify(adjustmentChannelPublishHelper).publishEditApproveAdjustment(any(ReportsDTO.class),any(AdjustmentDTO.class),anyString());
            Mockito.verify(adjustmentChannelPublishHelper).publishAdjustmentActionToAW(any(ReportsDTO.class),any(AdjustmentDTO.class));
            Mockito.verify(accountImplMock).getById("123");
            Mockito.verify(approveAdjustmentEmailHelper).adjustmentEditApprovedMailHandler(any(Contact.class),any(Contact.class),anyString(),any(AdjustmentDTO.class),any(SettingsJDO.class));
            Mockito.verify(inAppReminderUtil).handleInAppNotificationForAdjustment(any(AdjustmentDTO.class),any(PeopleRelationJDO.class),anyString());
        }
    }

}