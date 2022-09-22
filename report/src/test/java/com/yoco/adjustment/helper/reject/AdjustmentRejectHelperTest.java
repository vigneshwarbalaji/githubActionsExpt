package com.yoco.adjustment.helper.reject;

import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;

class AdjustmentRejectHelperTest {
    @Test
    void validateAuthorizationAndGetAdjustmentEvent_adjustmentNotValid_ShouldThrowExceptionWithActionAlreadyTakenMessage() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportMock = Mockito.mock(ReportImpl.class);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportMock);
            Mockito.when(reportMock.getEntryByID("123")).thenReturn(Map.of());
            Throwable exception = Assertions.assertThrows(IllegalArgumentException.class,()->AdjustmentRejectHelper.validateAuthorizationAndGetAdjustmentEvent("123",null));
            Assertions.assertEquals("Someone already took an action on this adjustment.",exception.getMessage());
        }
    }

    @Test
    void validateAuthorizationAndGetAdjustmentEvent_userNotAuthorized_ShouldThrowExceptionWithUserNotAuthorizedMessage() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class)){
            ReportImpl reportMock = Mockito.mock(ReportImpl.class);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportMock);
            Mockito.when(reportMock.getEntryByID("123")).thenReturn(Map.of("metaData",Map.of("status","PENDING")));
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accID");
            Throwable exception = Assertions.assertThrows(IllegalArgumentException.class,()->AdjustmentRejectHelper.validateAuthorizationAndGetAdjustmentEvent("123",userPro));
            Assertions.assertEquals("User not authorized",exception.getMessage());
        }
    }

    @Test
    void validateAuthorizationAndGetAdjustmentEvent_validRun_ShouldReturnTheQueriedEvent() throws IOException, NoSuchAlgorithmException {
        try(MockedStatic<ReportImpl> reportMockedStatic = Mockito.mockStatic(ReportImpl.class);
            MockedStatic<PermissionManager> permissionManagerMockedStatic = Mockito.mockStatic(PermissionManager.class)){
            ReportImpl reportMock = Mockito.mock(ReportImpl.class);
            reportMockedStatic.when(ReportImpl::getReportImplInstance).thenReturn(reportMock);
            Map<String,Object> mockEvent = Map.of("metaData",Map.of("status","PENDING"),"merchant","accID","provider", List.of("123"));
            Mockito.when(reportMock.getEntryByID("123")).thenReturn(mockEvent);
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("accID");
            userPro.setContactId("123");
            permissionManagerMockedStatic.when(()->PermissionManager.checkUserHasAdjustmentEditAccess(userPro)).thenReturn(true);
            Assertions.assertEquals(mockEvent,AdjustmentRejectHelper.validateAuthorizationAndGetAdjustmentEvent("123",userPro));
        }
    }

    @Test
    void isUserAuthorizedToRejectAdjustment_EntryAccountIsNotSameAsLoggedInUserAccount_shouldReturnFalse_test(){
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setUniquepin("123");
        Assertions.assertFalse(AdjustmentRejectHelper.isUserAuthorizedToRejectAdjustment(Map.of("merchant","234"),userPro));
    }

    @Test
    void isUserAuthorizedToRejectAdjustment_LoggedInUserHasNoPermissions_shouldReturnFalse_test(){
        try(MockedStatic<PermissionManager> permissionManagerMockedStatic = Mockito.mockStatic(PermissionManager.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("123");
            permissionManagerMockedStatic.when(()->PermissionManager.checkUserHasAdjustmentEditAccess(userPro)).thenReturn(false);
            Assertions.assertFalse(AdjustmentRejectHelper.isUserAuthorizedToRejectAdjustment(Map.of("merchant","123"),userPro));
        }
    }

    @Test
    void isUserAuthorizedToRejectAdjustment_LoggedInUserHasValidPermissions_shouldReturnTrue_test(){
        try(MockedStatic<PermissionManager> permissionManagerMockedStatic = Mockito.mockStatic(PermissionManager.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setUniquepin("123");
            permissionManagerMockedStatic.when(()->PermissionManager.checkUserHasAdjustmentEditAccess(userPro)).thenReturn(true);
            Assertions.assertTrue(AdjustmentRejectHelper.isUserAuthorizedToRejectAdjustment(Map.of("merchant","123"),userPro));
        }
    }

    @Test
    void validateAndExtractMessage_invalidPayloadJson_ShouldThrowExceptionWithInvalidDescriptionMessage_test(){
        Throwable exception = Assertions.assertThrows(IllegalArgumentException.class,()->AdjustmentRejectHelper.validateAndExtractMessage(""));
        Assertions.assertEquals("Invalid description", exception.getMessage());
    }

    @Test
    void validateAndExtractMessage_emptyMessage_ShouldThrowExceptionWithInvalidDescriptionMessage_test(){
        Throwable exception = Assertions.assertThrows(IllegalArgumentException.class,()->AdjustmentRejectHelper.validateAndExtractMessage("{\"message\":\"\"}"));
        Assertions.assertEquals("Invalid description", exception.getMessage());
    }

    @Test
    void validateAndExtractMessage_validMessage_ShouldReturnSanitizedMessage_test(){
      Assertions.assertEquals("test message<",AdjustmentRejectHelper.validateAndExtractMessage("{\"message\":\"<script></script>test message&lt;\"}"));
    }

    @Test
    void updateEventForRejection_validRun_shouldUpdateMetaAndQueryableMetaDataOfEvent_test(){
        try(MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)){
            dateUtilMockedStatic.when(DateUtil::getCurrentTime).thenReturn(1L);
            Map<String,Object> event = Map.of("metaData",Map.of("additionalInfo",Map.of("adjustment_info",Map.of())));
            event = JsonUtil.convertJsonToMap(JsonUtil.getJson(event));
            AdjustmentRejectHelper.updateEventForRejection(event,"123","reason");
            Assertions.assertEquals(Map.of("queryableMeta",Map.of("status","REJECTED"),"metaData",Map.of("status","REJECTED","additionalInfo",Map.of("adjustment_info",Map.of("reject_info",Map.of("by","123","when",1L,"notes","reason"))))),event);
        }
    }
}
