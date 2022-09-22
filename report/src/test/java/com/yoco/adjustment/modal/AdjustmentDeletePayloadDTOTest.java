package com.yoco.adjustment.modal;

import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import java.util.Map;

class AdjustmentDeletePayloadDTOTest {

    @Test
    void AdjustmentDeletePayloadDTO_emptyArgConstructor_test(){
        AdjustmentDeletePayloadDTO payloadDTO = new AdjustmentDeletePayloadDTO();
        Assertions.assertEquals(AdjustmentDeletePayloadDTO.class, payloadDTO.getClass());
        Assertions.assertNull(payloadDTO.getEventToDelete());
        Assertions.assertFalse(payloadDTO.isRejectAdjustmentRequest());
    }

    @Test
    void AdjustmentDeletePayloadDTO_constructor_messageNull_invalidTimeZone_test(){
        AdjustmentDeletePayloadDTO payload = new AdjustmentDeletePayloadDTO(Map.of(),true,new PeopleRelationJDO(),new PeopleRelationJDO(),"zone","",null);
        Assertions.assertEquals(Map.of(),payload.getEventToDelete());
        Assertions.assertEquals(DateFormats.DD_MMM_YYYY_HH_MM_SS_A,payload.getDateFormat());
        Assertions.assertTrue(payload.isShouldDeleteSubsets());
        Assertions.assertFalse(payload.isRejectAdjustmentRequest());
        Assertions.assertNull(payload.getTimezone());
        Assertions.assertEquals("",payload.getMessage());
    }
    @Test
    void AdjustmentDeletePayloadDTO_constructor_ValidMessage_ValidTimeZone_test(){
        AdjustmentDeletePayloadDTO payload = new AdjustmentDeletePayloadDTO(Map.of(),true,new PeopleRelationJDO(),new PeopleRelationJDO(),"Asia/Kolkata","","test");
        Assertions.assertEquals(Map.of(),payload.getEventToDelete());
        Assertions.assertEquals(DateFormats.DD_MMM_YYYY_HH_MM_SS_A,payload.getDateFormat());
        Assertions.assertTrue(payload.isShouldDeleteSubsets());
        Assertions.assertTrue(payload.isRejectAdjustmentRequest());
        Assertions.assertEquals("Asia/Kolkata",payload.getTimezone());
        Assertions.assertEquals("test",payload.getMessage());
    }
}
