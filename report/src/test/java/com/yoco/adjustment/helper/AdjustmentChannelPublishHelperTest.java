package com.yoco.adjustment.helper;

import com.yoco.adjustment.modal.AdjustmentPublishDTO;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.DateUtil;
import com.yoco.constants.EventConstants;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class AdjustmentChannelPublishHelperTest {

    AdjustmentChannelPublishHelper adjustmentChannelPublishHelper = AdjustmentChannelPublishHelper.getInstance();

    @Test
    void publishApproveAdjustment_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            rtmServiceMockedStatic.when(()-> RTMService.publishToChannel(anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);
            ReportsDTO reportsDTO = new ReportsDTO();
            reportsDTO.setAccountID("123");
            reportsDTO.setEmailID("test@gmail.com");
            reportsDTO.setContactID("111");
            reportsDTO.setId("id");
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentChannelPublishHelper.publishApproveAdjustment(reportsDTO,adjustmentDTO);
            AdjustmentPublishDTO adjustmentPublishDTO = new AdjustmentPublishDTO(reportsDTO,adjustmentDTO);
            adjustmentPublishDTO.setStatus("adj_approve");
            rtmServiceMockedStatic.verify(()-> RTMService.publishToChannel("123",adjustmentPublishDTO));
        }
    }

    @Test
    void publishEditApproveAdjustment_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            rtmServiceMockedStatic.when(()-> RTMService.publishToChannel(anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);
            ReportsDTO reportsDTO = new ReportsDTO();
            reportsDTO.setAccountID("123");
            reportsDTO.setEmailID("test@gmail.com");
            reportsDTO.setContactID("111");
            reportsDTO.setId("id");
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentDTO.setAdjustedInTime(1658912218000L);
            adjustmentChannelPublishHelper.publishEditApproveAdjustment(reportsDTO,adjustmentDTO, DateConstants.ZONE_ID_IST);
            AdjustmentPublishDTO adjustmentPublishDTO = new AdjustmentPublishDTO(reportsDTO,adjustmentDTO);
            adjustmentPublishDTO.setStatus("adj_approve");
            adjustmentPublishDTO.setCancelledAdjDate(DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY_HH_MM_SS_A, 1658912218000L,DateConstants.ZONE_ID_IST));
            rtmServiceMockedStatic.verify(()-> RTMService.publishToChannel("123",adjustmentPublishDTO));
        }
    }

    @Test
    void publishAdjustmentActionToAW_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            rtmServiceMockedStatic.when(()-> RTMService.publishToAW(anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);
            ReportsDTO reportsDTO = new ReportsDTO();
            reportsDTO.setAccountID("123");
            reportsDTO.setEmailID("test@gmail.com");
            reportsDTO.setContactID("111");
            reportsDTO.setId("id");
            AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
            adjustmentChannelPublishHelper.publishAdjustmentActionToAW(reportsDTO,adjustmentDTO);
            HashMap<String,Object> awPayload = new HashMap<>();
            awPayload.put("accountID","123");
            awPayload.put("action", "EDIT_AND_APPROVE_ADJUSTMENT");
            awPayload.put(EventConstants.ENTRY,reportsDTO);
            awPayload.put(EventConstants.ADJUSTMENT,adjustmentDTO);
            rtmServiceMockedStatic.verify(()-> RTMService.publishToAW("111",awPayload));
        }
    }

}