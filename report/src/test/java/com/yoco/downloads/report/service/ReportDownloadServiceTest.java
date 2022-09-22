package com.yoco.downloads.report.service;

import com.yoco.MockEvent;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.date.RangeInfoDTO;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyString;

class ReportDownloadServiceTest {

    @Test
    void downloadGlobal_empty_accountID_test() {
        try{
            new ReportDownloadService().downloadGlobal("", new Contact(), new HashMap<>(), "");
        } catch(Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(), e.getMessage());
        }
    }

    @Test
    void downloadGlobal_null_accountID_test() {
        try{
            new ReportDownloadService().downloadGlobal(null, new Contact(), new HashMap<>(), "");
        } catch(Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value(), e.getMessage());
        }
    }

    @Test
    void downloadGlobal_null_Contact_test() {
        try{
            new ReportDownloadService().downloadGlobal("acc", null, new HashMap<>(), "");
        } catch(Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value(), e.getMessage());
        }
    }

    @Test
    void downloadGlobal_invalid_range_empty_test() {
        Contact objCOntact = new Contact();
        objCOntact.setId("1234");
        HashMap<String, Object> payload = new HashMap<>();
        payload.put("range", "");
        payload.put("from", "06/07/2022");
        payload.put("to", "06/07/2022");
        try{
            new ReportDownloadService().downloadGlobal("acc", objCOntact, payload, "");
        } catch(Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_RANGE.value(), e.getMessage());
        }
    }

    @Test
    void downloadGlobal_invalid_range_null_test() {
        Contact objCOntact = new Contact();
        objCOntact.setId("1234");
        HashMap<String, Object> payload = new HashMap<>();
        payload.put("range", "");
        payload.put("from", "06/07/2022");
        payload.put("to", "06/07/2022");
        try{
            new ReportDownloadService().downloadGlobal("acc", objCOntact, payload, "");
        } catch(Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.INVALID_RANGE.value(), e.getMessage());
        }
    }

    @Test
    void downloadGlobal_invalid_range_from_empty_test() {
        Contact objCOntact = new Contact();
        objCOntact.setId("1234");
        HashMap<String, Object> payload = new HashMap<>();
        payload.put("range", "by_date");
        payload.put("from", "");
        payload.put("to", "06/07/2022");
        try{
            new ReportDownloadService().downloadGlobal("acc", objCOntact, payload, "");
        } catch(Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.FROM_AND_TO_DATE_MANDATORY.value(), e.getMessage());
        }
    }

    @Test
    void downloadGlobal_invalid_range_to_empty_test() {
        Contact objCOntact = new Contact();
        objCOntact.setId("1234");
        HashMap<String, Object> payload = new HashMap<>();
        payload.put("range", "by_date");
        payload.put("from", "06/07/2022");
        payload.put("to", "");
        try{
            new ReportDownloadService().downloadGlobal("acc", objCOntact, payload, "");
        } catch(Exception e) {
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.FROM_AND_TO_DATE_MANDATORY.value(), e.getMessage());
        }
    }

    @Test
    void downloadGlobal_null_userPRO_test() {
        Contact objCOntact = new Contact();
        objCOntact.setId("1234");
        HashMap<String, Object> payload = new HashMap<>();
        payload.put("range", "current_week");

        try(MockedConstruction<UserImpl> userImpl = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
            Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(null);
        })){
            Map<String, Object> resp = new ReportDownloadService().downloadGlobal("acc", objCOntact, payload, "");
            Assertions.assertTrue(ObjUtils.isNullOrEmpty(resp));
        } catch (IOException e) {
            e.printStackTrace();
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void downloadGlobal_valid_test_with_cursor_test() {
        Contact objCOntact = new Contact();
        objCOntact.setId("1234");
        HashMap<String, Object> payload = new HashMap<>();
        payload.put("range", "current_week");
        PeopleRelationJDO objPRO = new PeopleRelationJDO();
        objPRO.setTimeZone("Asia/Kolkata");

        HashMap<String, Object> event = new HashMap<>();
        List events = new ArrayList();
        events.add(MockEvent.getRawEvent());
        HashMap<String, Object> dataMap = new HashMap<>();
        dataMap.put("events", events);
        dataMap.put(Commons.CURSOR, "1234");
        event.put("data", dataMap);


        SettingsJDO objSetting = new SettingsJDO();
        objSetting.setDisplayDomainName("test");

        try(MockedConstruction<UserImpl> userImpl = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
                Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPRO);
        });
            MockedConstruction<ReportImpl> reportImpl = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
                Mockito.when(objReportImpl.getAllEntriesOfAccount("acc", null, null, 1000, "")).thenReturn(event);
        });
            MockedConstruction<AccountImpl> accountImpl = Mockito.mockConstruction(AccountImpl.class, (objaccountImpl, context)->{
                Mockito.when(objaccountImpl.getById(anyString())).thenReturn(objSetting);
            });
            MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)){

            RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
            rangeInfoDTO.setFromDateEpochMilliseconds(1658263373030l);
            rangeInfoDTO.setToDateEpochMilliseconds(1658263373030l);

            dateUtilMockedStatic.when(()->DateUtil.getRangeDetails("Asia/Kolkata", "current_week", null, null, "acc")).thenReturn(rangeInfoDTO);

            Map<String, Object> resp = new ReportDownloadService().downloadGlobal("acc", objCOntact, payload, "");
            System.out.println(resp);
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));
            Assertions.assertTrue(resp.containsKey(Commons.CURSOR));
            Assertions.assertFalse(resp.containsKey("fileName"));
        } catch (IOException e) {
            e.printStackTrace();
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Test
    void downloadGlobal_valid_test_without_cursor_test() {
        Contact objCOntact = new Contact();
        objCOntact.setId("1234");
        HashMap<String, Object> payload = new HashMap<>();
        payload.put("range", "current_week");
        PeopleRelationJDO objPRO = new PeopleRelationJDO();
        objPRO.setTimeZone("Asia/Kolkata");

        HashMap<String, Object> event = new HashMap<>();
        List events = new ArrayList();
        events.add(MockEvent.getRawEvent());
        HashMap<String, Object> dataMap = new HashMap<>();
        dataMap.put("events", events);
        event.put("data", dataMap);


        SettingsJDO objSetting = new SettingsJDO();
        objSetting.setDisplayDomainName("test");

        try(MockedConstruction<UserImpl> userImpl = Mockito.mockConstruction(UserImpl.class, (objUserImpl, context)->{
            Mockito.when(objUserImpl.getUserWithoutContact(anyString(), anyString())).thenReturn(objPRO);
        });
            MockedConstruction<ReportImpl> reportImpl = Mockito.mockConstruction(ReportImpl.class, (objReportImpl, context)->{
                Mockito.when(objReportImpl.getAllEntriesOfAccount("acc", null, null, 1000, "")).thenReturn(event);
            });
            MockedConstruction<AccountImpl> accountImpl = Mockito.mockConstruction(AccountImpl.class, (objaccountImpl, context)->{
                Mockito.when(objaccountImpl.getById(anyString())).thenReturn(objSetting);
            });
            MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)){

            RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
            rangeInfoDTO.setFromDateEpochMilliseconds(1658263373030l);
            rangeInfoDTO.setToDateEpochMilliseconds(1658263373030l);

            dateUtilMockedStatic.when(()->DateUtil.getRangeDetails("Asia/Kolkata", "current_week", null, null, "acc")).thenReturn(rangeInfoDTO);

            Map<String, Object> resp = new ReportDownloadService().downloadGlobal("acc", objCOntact, payload, "");
            System.out.println(resp);
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));
            Assertions.assertFalse(resp.containsKey(Commons.CURSOR));
            Assertions.assertTrue(resp.containsKey("fileName"));
        } catch (IOException e) {
            e.printStackTrace();
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }


}
