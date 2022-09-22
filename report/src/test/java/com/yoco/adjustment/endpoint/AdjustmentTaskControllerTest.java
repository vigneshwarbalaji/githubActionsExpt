package com.yoco.adjustment.endpoint;

import com.yoco.adjustment.helper.AdjustmentTaskHelper;
import com.yoco.adjustment.helper.delete.AdjustmentDeleteTaskHelper;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.CloudTaskUtil;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyMap;

class AdjustmentTaskControllerTest {

    @Test
    void handleApproveAdjustment_valid_test() throws IOException {
        try(MockedStatic<AdjustmentTaskHelper> adjustmentTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentTaskHelper.class)) {
            adjustmentTaskHelperMockedStatic.when(()-> AdjustmentTaskHelper.handleApproveAdjustment(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            Map<String,Object> payload = new HashMap<>();
            payload.put("key","data");
            new AdjustmentTaskController().handleApproveAdjustment(CloudTaskUtil.convertObjectToByteArray(payload));
            adjustmentTaskHelperMockedStatic.verify(()->AdjustmentTaskHelper.handleApproveAdjustment(payload));
        }
    }

    @Test
    void handleApproveAdjustment_exception_test() throws IOException {
        try(MockedStatic<AdjustmentTaskHelper> adjustmentTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentTaskHelper.class)) {
            adjustmentTaskHelperMockedStatic.when(()-> AdjustmentTaskHelper.handleApproveAdjustment(anyMap())).thenThrow(new IOException());
            Map<String,Object> payload = new HashMap<>();
            payload.put("key","data");
            new AdjustmentTaskController().handleApproveAdjustment(CloudTaskUtil.convertObjectToByteArray(payload));
            adjustmentTaskHelperMockedStatic.verify(()->AdjustmentTaskHelper.handleApproveAdjustment(payload));
        }
    }

    @Test
    void handleEditApproveAdjustment_valid_test() throws IOException {
        try (MockedStatic<AdjustmentTaskHelper> adjustmentTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentTaskHelper.class)) {
            adjustmentTaskHelperMockedStatic.when(() -> AdjustmentTaskHelper.handleEditApproveAdjustment(anyMap())).thenAnswer((Answer<Void>) invocation -> null);
            Map<String, Object> payload = new HashMap<>();
            payload.put("key", "data");
            new AdjustmentTaskController().handleEditApproveAdjustment(CloudTaskUtil.convertObjectToByteArray(payload));
            adjustmentTaskHelperMockedStatic.verify(() -> AdjustmentTaskHelper.handleEditApproveAdjustment(payload));
        }
    }

    @Test
    void deleteAdjustmentHandler_ExceptionThrown_AdjustmentDeleteTaskHelperShouldNotHaveAnyInteraction(){
        try(MockedStatic<AdjustmentDeleteTaskHelper> adjustmentDeleteTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteTaskHelper.class);
        MockedStatic<CloudTaskUtil> cloudTaskUtilMockedStatic = Mockito.mockStatic(CloudTaskUtil.class)){
            cloudTaskUtilMockedStatic.when(()->CloudTaskUtil.convertByteArrayToObject(new byte[]{})).thenThrow(new IllegalArgumentException());
            new AdjustmentTaskController().deleteAdjustmentHandler(new byte[]{});
            adjustmentDeleteTaskHelperMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleEditApproveAdjustment_exception_test() throws IOException {
        try(MockedStatic<AdjustmentTaskHelper> adjustmentTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentTaskHelper.class)) {
            adjustmentTaskHelperMockedStatic.when(()-> AdjustmentTaskHelper.handleEditApproveAdjustment(anyMap())).thenThrow(new IOException());
            Map<String,Object> payload = new HashMap<>();
            payload.put("key","data");
            new AdjustmentTaskController().handleEditApproveAdjustment(CloudTaskUtil.convertObjectToByteArray(payload));
            adjustmentTaskHelperMockedStatic.verify(()->AdjustmentTaskHelper.handleEditApproveAdjustment(payload));
        }
    }

    @Test
    void deleteAdjustmentHandler_validPayload_AdjustmentDeleteTaskHelperShouldHaveBeenCalled(){
        try(MockedStatic<AdjustmentDeleteTaskHelper> adjustmentDeleteTaskHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteTaskHelper.class);
            MockedStatic<CloudTaskUtil> cloudTaskUtilMockedStatic = Mockito.mockStatic(CloudTaskUtil.class)){
            cloudTaskUtilMockedStatic.when(()->CloudTaskUtil.convertByteArrayToObject(new byte[]{})).thenReturn(Map.of("entries", List.of(),"adjustments",List.of(),"loggedInUserPro",new PeopleRelationJDO(),"entryContactPro",new PeopleRelationJDO()));
            new AdjustmentTaskController().deleteAdjustmentHandler(new byte[]{});
            adjustmentDeleteTaskHelperMockedStatic.verify(()->AdjustmentDeleteTaskHelper.handleAdjustmentDeletion(List.of(),List.of(),new PeopleRelationJDO(), new PeopleRelationJDO()));
        }
    }
}