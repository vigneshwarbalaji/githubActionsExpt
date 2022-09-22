package com.yoco.adjustment.service;

import com.yoco.adjustment.helper.AdjustmentHelper;
import com.yoco.adjustment.helper.delete.AdjustmentDeleteHelper;
import com.yoco.adjustment.helper.reject.AdjustmentRejectHelper;
import com.yoco.adjustment.modal.AdjustmentDeletePayloadDTO;
import com.yoco.adjustment.modal.GetAdjustmentPayloadDTO;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.constants.EventConstants;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.mockito.ArgumentMatchers.*;

class AdjustmentServiceTest {
    @Test
    void getAdjustments_emptyPayloadParameters_exception_test() throws IOException, NoSuchAlgorithmException {
        Map<String, Object> adjustmentMap = new HashMap<>();
        adjustmentMap.put("adjustment", "adjustment");
        List<Map<String, Object>> adjustmentsList = new ArrayList<>();
        adjustmentsList.add(adjustmentMap);
        Map<String, Object> implMap = new HashMap<>();
        implMap.put("eventList",adjustmentsList);
        try(MockedConstruction<AdjustmentHelper> adjustmentHelperMock = Mockito.mockConstruction(AdjustmentHelper.class, (adjustmentHelper, context)->{
        })) {
            Contact contact = new Contact();
            var getAdjustmentDto = new GetAdjustmentPayloadDTO("", "","","","",contact);
            new AdjustmentService().getAdjustments("accountID", "PENDING", getAdjustmentDto, "", "");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_RANGE.value(), e.getMessage());
        }
    }

    @Test
    void getAdjustments_invalidUserPRO_exception_test() {
        Map<String, Object> adjustmentMap = new HashMap<>();
        adjustmentMap.put("adjustment", "adjustment");
        List<Map<String, Object>> adjustmentsList = new ArrayList<>();
        adjustmentsList.add(adjustmentMap);
        Map<String, Object> implMap = new HashMap<>();
        implMap.put("eventList",adjustmentsList);
        try(MockedConstruction<AdjustmentHelper> adjustmentHelperMock = Mockito.mockConstruction(AdjustmentHelper.class, (adjustmentHelper, context)->{
        })) {
            Contact contact = new Contact();
            var getAdjustmentDto = new GetAdjustmentPayloadDTO("", DateConstants.CURRENT_WEEK,"false","false","",contact);
            new AdjustmentService().getAdjustments("accountID", "PENDING", getAdjustmentDto, "", "");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value(), e.getMessage());
        }
    }

    @Test
    void getAdjustments_invalidSkillSet_exception_test() {
        Map<String, Object> adjustmentMap = new HashMap<>();
        adjustmentMap.put("adjustment", "adjustment");
        List<Map<String, Object>> adjustmentsList = new ArrayList<>();
        adjustmentsList.add(adjustmentMap);
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("id");
        userPro.setUniquepin("123");
        Map<String, Object> implMap = new HashMap<>();
        implMap.put("eventList",adjustmentsList);
        try(MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
                Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
            });
            MockedConstruction<AdjustmentHelper> adjustmentHelperMock = Mockito.mockConstruction(AdjustmentHelper.class, (adjustmentHelper, context)->{
                Mockito.when(adjustmentHelper.verifyUserSkillSet(anyString(), any())).thenReturn(false);
            })) {

            Contact contact = new Contact();
            contact.setId("id");
            var getAdjustmentDto = new GetAdjustmentPayloadDTO("", DateConstants.CURRENT_WEEK,"false","false","",contact);
            new AdjustmentService().getAdjustments("accountID", "APPROVED", getAdjustmentDto,"","");
        }catch (Exception e){
            Assertions.assertEquals(EVENTS_ERROR_RESPONSE.NO_SKILL_SET.value(), e.getMessage());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "Asia/kolkata"})
    void getAdjustments_valid_test(String testValue) throws IOException, NoSuchAlgorithmException {
        Map<String, Object> adjustmentMap = new HashMap<>();
        adjustmentMap.put("adjustments", "adjustments");
        List<Map<String, Object>> adjustmentsList = new ArrayList<>();
        adjustmentsList.add(adjustmentMap);
        AdjustmentDTO adjustmentObject = new AdjustmentDTO();
        adjustmentObject.setAccountID("accountID");
        adjustmentObject.setContactID("contactID");
        List<AdjustmentDTO> convertedList = new ArrayList<>();
        convertedList.add(adjustmentObject);
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("id");
        userPro.setUniquepin("123");
        userPro.setTimeZone("Asia/Kolkata");
        Map<String, Object> implMap = new HashMap<>();
        implMap.put("eventList",adjustmentsList);
        try(MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
                Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
            });
            MockedConstruction<AdjustmentHelper> adjustmentHelperMock = Mockito.mockConstruction(AdjustmentHelper.class, (adjustmentHelper, context)->{
                Mockito.when(adjustmentHelper.convertEventListIntoAdjustmentList(anyList(), anyString(), anyString())).thenReturn(convertedList);
                Mockito.when(adjustmentHelper.verifyUserSkillSet(anyString(), any())).thenReturn(true);
                Mockito.when(adjustmentHelper.getAdjustmentsBasedOnStatus(anyString(), anyString(), anyString(), anyString(),anyString(), anyString())).thenReturn(implMap);
            })) {
            Contact contact = new Contact();
            contact.setId("id");
            var getAdjustmentDto = new GetAdjustmentPayloadDTO("", DateConstants.BY_DATE, "10/11/2021", "10/11/2021", testValue, contact);
            Map<String, Object> responseMap = new AdjustmentService().getAdjustments("accountID", "PENDING", getAdjustmentDto,"", "");
            Assertions.assertEquals(convertedList, responseMap.get(EventConstants.ADJUSTMENTS));
        }
    }

    @Test
    void deleteAdjustment_validRun_shouldCallAdjustmentDeleteHelperMethods() throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO loggedInUserPro = new PeopleRelationJDO();
        PeopleRelationJDO entryContactPro = new PeopleRelationJDO();
        try(MockedStatic<AdjustmentDeleteHelper> adjustmentDeleteHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteHelper.class);
            MockedStatic<AdjustmentHelper> adjustmentHelperMockedStatic = Mockito.mockStatic(AdjustmentHelper.class)){
            Contact loggedInContact = new Contact();
            loggedInContact.setId("123");
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.validateAuthorizationAndGetAdjustmentEvent("eID",loggedInUserPro)).thenReturn(Map.of());
            adjustmentHelperMockedStatic.when(()-> AdjustmentHelper.validateAndExtractLoggedInPro("accID",loggedInContact)).thenReturn(loggedInUserPro);
            adjustmentHelperMockedStatic.when(()-> AdjustmentHelper.getEntryContactPro(loggedInUserPro,Map.of())).thenReturn(entryContactPro);
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.validateAndDeleteAdjustment(new AdjustmentDeletePayloadDTO(Map.of(),false,loggedInUserPro,entryContactPro,"","",null))).thenReturn(new GenericResponse(true,null,null));
            AdjustmentService service = new AdjustmentService();
            Assertions.assertEquals(new GenericResponse(true,null,null),service.deleteAdjustment("accID","eID",false,loggedInContact,"",""));
        }
    }

    @Test
    void rejectAdjustment_validRun_shouldCallAdjustmentDeleteHelperMethods() throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO loggedInUserPro = new PeopleRelationJDO();
        PeopleRelationJDO entryContactPro = new PeopleRelationJDO();
        try(MockedStatic<AdjustmentDeleteHelper> adjustmentDeleteHelperMockedStatic = Mockito.mockStatic(AdjustmentDeleteHelper.class);
            MockedStatic<AdjustmentRejectHelper> adjustmentRejectHelperMockedStatic = Mockito.mockStatic(AdjustmentRejectHelper.class);
            MockedStatic<AdjustmentHelper> adjustmentHelperMockedStatic = Mockito.mockStatic(AdjustmentHelper.class)){
            Contact loggedInContact = new Contact();
            loggedInContact.setId("123");
            adjustmentRejectHelperMockedStatic.when(()->AdjustmentRejectHelper.validateAuthorizationAndGetAdjustmentEvent("eID",loggedInUserPro)).thenReturn(Map.of());
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentRejectHelper.validateAndExtractMessage("payload")).thenReturn("test");
            adjustmentHelperMockedStatic.when(()-> AdjustmentHelper.validateAndExtractLoggedInPro("accID",loggedInContact)).thenReturn(loggedInUserPro);
            adjustmentHelperMockedStatic.when(()-> AdjustmentHelper.getEntryContactPro(loggedInUserPro,Map.of())).thenReturn(entryContactPro);
            adjustmentDeleteHelperMockedStatic.when(()->AdjustmentDeleteHelper.validateAndDeleteAdjustment(new AdjustmentDeletePayloadDTO(Map.of(),false,loggedInUserPro,entryContactPro,"","","test"))).thenReturn(new GenericResponse(true,null,null));
            AdjustmentService service = new AdjustmentService();
            Assertions.assertEquals(new GenericResponse(true,null,null),service.rejectAdjustment("accID","eID",false,loggedInContact,"","","payload"));
        }
    }
}

