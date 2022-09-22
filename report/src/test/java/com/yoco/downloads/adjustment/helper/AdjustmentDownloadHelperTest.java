package com.yoco.downloads.adjustment.helper;

import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Font;
import com.itextpdf.text.FontFactory;
import com.itextpdf.text.pdf.PdfPTable;
import com.yoco.adjustment.helper.AdjustmentHelper;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.AdjustmentImpl;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.modal.date.RangeInfoDTO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.utils.AccountUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.constants.EventConstants;
import com.yoco.downloads.util.DownloadUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

import static com.itextpdf.text.FontFactory.HELVETICA;
import static org.mockito.ArgumentMatchers.*;

class AdjustmentDownloadHelperTest {
    @Test
    void getPdfHeadersForDownload_forFreeAccount_test() throws DocumentException {

        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accountUtilMockedStatic.when(() -> AccountUtil.isFreeAccount(any(SettingsJDO.class))).thenReturn(true);
            SettingsJDO accountObject = new SettingsJDO();
            accountObject.setPeopleUniquePin("212");
            Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
            PdfPTable pdf = AdjustmentDownloadHelper.getInstance().getPdfHeadersForDownload(accountObject, fontType, false);
            Assertions.assertEquals(14, pdf.getNumberOfColumns());
        }
    }

    @Test
    void getPdfHeadersForDownload_forActiveDecimalDuration_test() throws DocumentException {

        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accountUtilMockedStatic.when(() -> AccountUtil.isFreeAccount(any(SettingsJDO.class))).thenReturn(true);
            SettingsJDO accountObject = new SettingsJDO();
            accountObject.setPeopleUniquePin("212");
            Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
            PdfPTable pdf = AdjustmentDownloadHelper.getInstance().getPdfHeadersForDownload(accountObject, fontType, true);
            Assertions.assertEquals(14, pdf.getNumberOfColumns());
        }
    }

    @Test
    void getPdfHeadersForDownload_ifDecimalDurationIsNotActiveForEntreprisePlanAccount_test() throws DocumentException {

        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accountUtilMockedStatic.when(() -> AccountUtil.isFreeAccount(any(SettingsJDO.class))).thenReturn(false);
            SettingsJDO accountObject = new SettingsJDO();
            accountObject.setPeopleUniquePin("212");
            Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
            PdfPTable pdf = AdjustmentDownloadHelper.getInstance().getPdfHeadersForDownload(accountObject, fontType, false);
            Assertions.assertEquals(16, pdf.getNumberOfColumns());
        }
    }

    @Test
    void getPdfDocumentForDownload_valid_test() throws DocumentException, IOException, NoSuchAlgorithmException {

        SettingsJDO accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("accountID");
        accountObject.setDisplayTimeFormat(DownloadUtil.DECIMAL);
        accountObject.setDisplayDomainName("domainName");

        AdjustmentDTO adjustmentObject = new AdjustmentDTO();
        adjustmentObject.setAccountID("accountID");
        adjustmentObject.setContactID("contactID");
        adjustmentObject.setIsNew(true);
        adjustmentObject.setAdjustedInTime(12442342L);
        adjustmentObject.setAdjustedOutTime(2222222222L);
        adjustmentObject.setRequestMessage("Others - test");
        List<AdjustmentDTO>adjustmentDTOList = new ArrayList<>();
        adjustmentDTOList.add(adjustmentObject);

        List<Map<String, Object>> eventList = new ArrayList<>();

        Contact contact = new Contact();
        contact.setId("123");
        contact.setEmailID("newMailID@gmail.com");

        List<Contact>contactList = new ArrayList<>();
        contactList.add(contact);

        Map<String, Object> adjustmentMap = new HashMap<>();
        adjustmentMap.put(EventConstants.EVENT_LIST, eventList);
        adjustmentMap.put(Commons.CURSOR, "cursor");

        try(MockedConstruction<AccountImpl> accountImplMock = Mockito.mockConstruction(AccountImpl.class, (accountImpl, context)->{
            Mockito.when(accountImpl.getById(anyString())).thenReturn(accountObject);
        });
            MockedConstruction<AdjustmentHelper> adjustmentHelperMock = Mockito.mockConstruction(AdjustmentHelper.class, (adjustmentHelper, context)->{
                Mockito.when(adjustmentHelper.convertEventListIntoAdjustmentList(anyList(), anyString(), anyString())).thenReturn(adjustmentDTOList);
            });
            MockedConstruction<ContactImpl> contactImplMock = Mockito.mockConstruction(ContactImpl.class, (contactImpl, context)->{
                Mockito.when(contactImpl.getContacts(anyList())).thenReturn(contactList);
            });
            MockedConstruction<AdjustmentImpl> adjustmentImplMock = Mockito.mockConstruction(AdjustmentImpl.class, (adjustmentImpl, context)->{
                Mockito.when(adjustmentImpl.getAllAdjustments(anyString(), anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(adjustmentMap);
            })
        ){
            RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
            rangeInfoDTO.setFromDateEpochMilliseconds(1643817200000L);
            rangeInfoDTO.setToDateEpochMilliseconds(1643817200000L);
            Map<String, Object> responseMap = AdjustmentDownloadHelper.getInstance().getPdfDocumentForDownload("accountID", "PENDING", "02/02/2022", "02/02/2022", "Asia/Kolkata", rangeInfoDTO);
            Assertions.assertEquals("PENDING_AdjustmentsReport_domainName_ALL_02-Feb-2022", responseMap.get(DownloadUtil.FILE_NAME));
        }
    }

    @Test
    void initializeAndAddHeadersToThePdf_valid_test() throws DocumentException {
        float[] columnWidths = new float[]{4f, 6f, 4f, 4f, 4f, 4f, 4f, 6f, 4f, 4f, 4f, 4f, 4f, 6f, 5f, 6f};
        List<String>pdfHeaders = new ArrayList<>(Arrays.asList("Project","Email","ActualInDate","ActualInTime","ActualOutDate","ActualOutTime",
                "ActualDuration", "ActualDurationInDecimal", "AdjustedInDate", "AdjustedInTime",
                "AdjustedOutDate", "AdjustedOutTime", "AdjustedDuration", "AdjustedDurationInDecimal",
                "AdjustmentReason", "AdjustmentMessage"));
        Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
        PdfPTable pdf = AdjustmentDownloadHelper.getInstance().initializeAndAddHeadersToThePdf(16, columnWidths, pdfHeaders, fontType);
        Assertions.assertTrue(!ObjUtils.isNull(pdf));
    }

    @Test
    void getNumberOfColumnsRequired_ifDecimalDurationActive_test() throws DocumentException {
        int numberOfColumns = AdjustmentDownloadHelper.getInstance().getNumberOfColumnsRequired(true, false);
        Assertions.assertEquals(14, numberOfColumns);
    }

    @Test
    void getNumberOfColumnsRequired_isAccountOnFreePlan_test() throws DocumentException {
        int numberOfColumns = AdjustmentDownloadHelper.getInstance().getNumberOfColumnsRequired(false, true);
        Assertions.assertEquals(14, numberOfColumns);
    }

    @Test
    void getNumberOfColumnsRequired_ifDecimalDurationIsActiveAndItIsNotAFreePlan_test() throws DocumentException {
        int numberOfColumns = AdjustmentDownloadHelper.getInstance().getNumberOfColumnsRequired(false, false);
        Assertions.assertEquals(16, numberOfColumns);
    }

    @Test
    void getRequiredColumnWidths_ifDecimalDurationActive_test() throws DocumentException {
        float[] columnWidths = AdjustmentDownloadHelper.getInstance().getRequiredColumnWidths(true, false);
        Assertions.assertEquals(14, columnWidths.length);
    }

    @Test
    void getRequiredColumnWidths_isAccountOnFreePlan_test() throws DocumentException {
        float[] columnWidths = AdjustmentDownloadHelper.getInstance().getRequiredColumnWidths(false, true);
        Assertions.assertEquals(14, columnWidths.length);
    }

    @Test
    void getRequiredColumnWidths_ifDecimalDurationIsActiveAndItIsNotAFreePlan_test() throws DocumentException {
        float[] columnWidths = AdjustmentDownloadHelper.getInstance().getRequiredColumnWidths(false, false);
        Assertions.assertEquals(16, columnWidths.length);
    }

    @Test
    void getPdfHeadersForActiveDecimalDuration_ifDecimalDurationIsActive_test() throws DocumentException {
        List<String>pdfHeaders = new ArrayList<>(Arrays.asList("Project","Email","ActualInDate","ActualInTime","ActualOutDate","ActualOutTime",
                "ActualDuration", "ActualDurationInDecimal", "AdjustedInDate", "AdjustedInTime",
                "AdjustedOutDate", "AdjustedOutTime", "AdjustedDuration", "AdjustedDurationInDecimal",
                "AdjustmentReason", "AdjustmentMessage"));
        List<String>resultList = AdjustmentDownloadHelper.getInstance().getPdfHeadersForActiveDecimalDuration(true, pdfHeaders);
        Assertions.assertEquals(14, resultList.size());
    }

    @Test
    void getPdfHeadersForActiveDecimalDuration_ifDecimalDurationIsNotActive_test() throws DocumentException {
        List<String>pdfHeaders = new ArrayList<>(Arrays.asList("Project","Email","ActualInDate","ActualInTime","ActualOutDate","ActualOutTime",
                "ActualDuration", "ActualDurationInDecimal", "AdjustedInDate", "AdjustedInTime",
                "AdjustedOutDate", "AdjustedOutTime", "AdjustedDuration", "AdjustedDurationInDecimal",
                "AdjustmentReason", "AdjustmentMessage"));
        List<String>resultList = AdjustmentDownloadHelper.getInstance().getPdfHeadersForActiveDecimalDuration(false, pdfHeaders);
        Assertions.assertEquals(16, resultList.size());
    }

    @Test
    void getPdfHeadersForFreeAccount_ifDecimalDurationIsActive_test() throws DocumentException {
        List<String>pdfHeaders = new ArrayList<>(Arrays.asList("Project","Email","ActualInDate","ActualInTime","ActualOutDate","ActualOutTime",
                "ActualDuration", "ActualDurationInDecimal", "AdjustedInDate", "AdjustedInTime",
                "AdjustedOutDate", "AdjustedOutTime", "AdjustedDuration", "AdjustedDurationInDecimal",
                "AdjustmentReason", "AdjustmentMessage"));
        List<String>resultList = AdjustmentDownloadHelper.getInstance().getPdfHeadersForFreeAccount(true, pdfHeaders);
        Assertions.assertEquals(14, resultList.size());
    }

    @Test
    void getPdfHeadersForFreeAccount_ifDecimalDurationIsNotActive_test() throws DocumentException {
        List<String>pdfHeaders = new ArrayList<>(Arrays.asList("Project","Email","ActualInDate","ActualInTime","ActualOutDate","ActualOutTime",
                "ActualDuration", "ActualDurationInDecimal", "AdjustedInDate", "AdjustedInTime",
                "AdjustedOutDate", "AdjustedOutTime", "AdjustedDuration", "AdjustedDurationInDecimal",
                "AdjustmentReason", "AdjustmentMessage"));
        List<String>resultList = AdjustmentDownloadHelper.getInstance().getPdfHeadersForFreeAccount(false, pdfHeaders);
        Assertions.assertEquals(16, resultList.size());
    }

    @Test
    void getAdjustmentsDetailsForThePdf_isEmpty_test() throws IOException, NoSuchAlgorithmException {
        Map<String, Object>eventMap = new HashMap<>();
        eventMap.put("event", "event");
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        adjustmentDTO.setContactID("124");
        adjustmentDTO.setAccountID("232");
        List<AdjustmentDTO> adjustmentDTOList = new ArrayList<>();
        List<Map<String, Object>> newEventList = new ArrayList<>();
        newEventList.add(eventMap);
        Map<String, Object> newAdjustmentMap = new HashMap<>();
        try(MockedConstruction<AdjustmentImpl> adjustmentImplMock = Mockito.mockConstruction(AdjustmentImpl.class, (adjustmentImpl, context)->{
            Mockito.when(adjustmentImpl.getAllAdjustments(anyString(), anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(newAdjustmentMap);
        });
            MockedConstruction<AdjustmentHelper> adjustmentHelperMock = Mockito.mockConstruction(AdjustmentHelper.class, (adjustmentHelper, context)->{
                Mockito.when(adjustmentHelper.convertEventListIntoAdjustmentList(anyList(), anyString(), anyString())).thenReturn(adjustmentDTOList);
            })
        ){
            List<AdjustmentDTO> adjustmentList = AdjustmentDownloadHelper.getInstance().getAdjustmentsDetailsForThePdf("accountID", "PENDING", "02/02/2022", "02/02/2022", "Asia/Kolkata");
            Assertions.assertEquals(adjustmentDTOList, adjustmentList);
        }
    }

    @Test
    void getAdjustmentsDetailsForThePdf_cursorNull_test() throws IOException, NoSuchAlgorithmException {
        Map<String, Object>eventMap = new HashMap<>();
        eventMap.put("event", "event");
        AdjustmentDTO adjustmentDTO = new AdjustmentDTO();
        adjustmentDTO.setContactID("124");
        adjustmentDTO.setAccountID("232");
        List<AdjustmentDTO> adjustmentDTOList = new ArrayList<>();
        adjustmentDTOList.add(adjustmentDTO);
        List<Map<String, Object>> newEventList = new ArrayList<>();
        newEventList.add(eventMap);
        Map<String, Object> newAdjustmentMap = new HashMap<>();
        newAdjustmentMap.put(EventConstants.EVENT_LIST, newEventList);
        newAdjustmentMap.put(Commons.CURSOR, "");
        try(MockedConstruction<AdjustmentImpl> adjustmentImplMock = Mockito.mockConstruction(AdjustmentImpl.class, (adjustmentImpl, context)->{
            Mockito.when(adjustmentImpl.getAllAdjustments(anyString(), anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(newAdjustmentMap);
        });
        MockedConstruction<AdjustmentHelper> adjustmentHelperMock = Mockito.mockConstruction(AdjustmentHelper.class, (adjustmentHelper, context)->{
            Mockito.when(adjustmentHelper.convertEventListIntoAdjustmentList(anyList(), anyString(), anyString())).thenReturn(adjustmentDTOList);
        })
        ){
            List<AdjustmentDTO> adjustmentList = AdjustmentDownloadHelper.getInstance().getAdjustmentsDetailsForThePdf("accountID", "PENDING", "02/02/2022", "02/02/2022", "Asia/Kolkata");
            Assertions.assertEquals(adjustmentDTOList, adjustmentList);
        }
    }

    @Test
    void getAdjustmentsDetailsForThePdf_valid_test() throws IOException, NoSuchAlgorithmException {
        List<Map<String, Object>> eventList = new ArrayList<>();
        Map<String, Object> adjustmentMap = new HashMap<>();
        adjustmentMap.put(EventConstants.EVENT_LIST, eventList);
        adjustmentMap.put(Commons.CURSOR, "");
        try(MockedConstruction<AdjustmentImpl> adjustmentImplMock = Mockito.mockConstruction(AdjustmentImpl.class, (adjustmentImpl, context)->{
                Mockito.when(adjustmentImpl.getAllAdjustments(anyString(), anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(adjustmentMap);
        })
        ){
            List<AdjustmentDTO> adjustmentList = AdjustmentDownloadHelper.getInstance().getAdjustmentsDetailsForThePdf("accountID", "PENDING", "02/02/2022", "02/02/2022", "Asia/Kolkata");
            Assertions.assertEquals(new ArrayList<>(), adjustmentList);
        }
    }

    @Test
    void getBodyOfTheAdjustmentPdf_valid_test() {
        Contact contact = new Contact();
        contact.setId("27");
        contact.setEmailID("mail@mail.com");

        List<Contact>contactList = new ArrayList<>();
        contactList.add(contact);
        try(MockedConstruction<ContactImpl> contactImplMock = Mockito.mockConstruction(ContactImpl.class, (contactImpl, context)->{
            Mockito.when(contactImpl.getContacts(anyList())).thenReturn(contactList);
        })){
            SettingsJDO accountObject = new SettingsJDO();
            accountObject.setPeopleUniquePin("accountId");
            accountObject.setDisplayTimeFormat(DownloadUtil.DECIMAL);
            accountObject.setDisplayDomainName("randomDomain");
            AdjustmentDTO adjustmentObject = new AdjustmentDTO();
            adjustmentObject.setAccountID("accountId");
            adjustmentObject.setContactID("contactId");
            adjustmentObject.setIsNew(false);
            adjustmentObject.setOriginalInTime(1255L);
            adjustmentObject.setOriginalOutTime(3333L);
            adjustmentObject.setAdjustedInTime(1244L);
            adjustmentObject.setAdjustedOutTime(2222L);
            List<AdjustmentDTO>adjustmentList = new ArrayList<>();
            adjustmentList.add(adjustmentObject);
            adjustmentObject.setRequestMessage("Others - test test test");
            Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
            PdfPTable table = new PdfPTable(16);
            PdfPTable pdfResult = AdjustmentDownloadHelper.getInstance().getBodyOfTheAdjustmentPdf(accountObject, adjustmentList, table, fontType, false, "Asia/kolkata");
            Assertions.assertTrue(!ObjUtils.isNull(pdfResult));
        }
    }

    @Test
    void getMapOfMailIdsLinkedToContactIds_valid_test() {
        Contact contact = new Contact();
        contact.setId("56");
        contact.setEmailID("gmailID@gmail.com");

        List<Contact>contactList = new ArrayList<>();
        contactList.add(contact);
        try(MockedConstruction<ContactImpl> contactImplMock = Mockito.mockConstruction(ContactImpl.class, (contactImpl, context)->{
            Mockito.when(contactImpl.getContacts(anyList())).thenReturn(contactList);
        })){
            AdjustmentDTO adjustmentObject = new AdjustmentDTO();
            adjustmentObject.setAccountID("accountID");
            adjustmentObject.setContactID("56");
            adjustmentObject.setIsNew(true);
            adjustmentObject.setAdjustedInTime(12442L);
            adjustmentObject.setAdjustedOutTime(22222L);
            adjustmentObject.setRequestMessage("Others - test new test");
            List<AdjustmentDTO>adjustmentDTOList = new ArrayList<>();
            adjustmentDTOList.add(adjustmentObject);
            Map<String, Object>resultMap = AdjustmentDownloadHelper.getInstance().getMapOfMailIdsLinkedToContactIds(adjustmentDTOList);
            Assertions.assertEquals("gmailID@gmail.com", resultMap.get("56"));
        }
    }

    @Test
    void addDurationTimeAndDateDetailsToThePdfDocument_IfItIsNotANewAdjustment_test() {
        SettingsJDO accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("account");
        accountObject.setDisplayTimeFormat(DownloadUtil.DECIMAL);
        accountObject.setDisplayDomainName("domain");
        AdjustmentDTO adjustmentObject = new AdjustmentDTO();
        adjustmentObject.setAccountID("account");
        adjustmentObject.setContactID("contact");
        adjustmentObject.setIsNew(false);
        adjustmentObject.setOriginalInTime(124422L);
        adjustmentObject.setOriginalOutTime(333333333L);
        adjustmentObject.setAdjustedInTime(124423L);
        adjustmentObject.setAdjustedOutTime(22222222L);
        adjustmentObject.setRequestMessage("Others - test test");
        Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
        PdfPTable table = new PdfPTable(16);
        PdfPTable pdfResult = AdjustmentDownloadHelper.getInstance().addDurationTimeAndDateDetailsToThePdfDocument(accountObject, adjustmentObject, table, fontType, false, "Asia/Kolkata");
        Assertions.assertTrue(!ObjUtils.isNull(pdfResult));
    }

    @Test
    void addDurationTimeAndDateDetailsToThePdfDocument_IfItIsANewAdjustment_test() {
        SettingsJDO accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("accountID");
        accountObject.setDisplayTimeFormat(DownloadUtil.DECIMAL);
        accountObject.setDisplayDomainName("domainName");
        AdjustmentDTO adjustmentObject = new AdjustmentDTO();
        adjustmentObject.setAccountID("accountID");
        adjustmentObject.setContactID("contactID");
        adjustmentObject.setIsNew(true);
        adjustmentObject.setAdjustedInTime(12442342L);
        adjustmentObject.setAdjustedOutTime(2222222222L);
        adjustmentObject.setRequestMessage("Others - test");
        Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
        PdfPTable table = new PdfPTable(16);
        PdfPTable pdfResult = AdjustmentDownloadHelper.getInstance().addDurationTimeAndDateDetailsToThePdfDocument(accountObject, adjustmentObject, table, fontType, false, "Asia/Kolkata");
        Assertions.assertTrue(!ObjUtils.isNull(pdfResult));
    }

    @Test
    void handleNewAdjustmentForAddingDurationAndDecimalDurationToThePdf_ifItIsANewAdjustment_test() {
        SettingsJDO accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("account");
        accountObject.setDisplayTimeFormat(DownloadUtil.DECIMAL);
        accountObject.setDisplayDomainName("domain");
        PdfPTable table = new PdfPTable(16);
        Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
        PdfPTable pdfResult = AdjustmentDownloadHelper.getInstance().handleNewAdjustmentForAddingDurationAndDecimalDurationToThePdf(accountObject, true, table, fontType, true);
        Assertions.assertTrue(!ObjUtils.isNull(pdfResult));
    }

    @Test
    void handleNewAdjustmentForAddingDurationAndDecimalDurationToThePdf_ifItIsNotANewAdjustment_test() {
        SettingsJDO accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("acc");
        accountObject.setDisplayTimeFormat("HH:MM");
        accountObject.setDisplayDomainName("domain");
        PdfPTable table = new PdfPTable(14);
        Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
        PdfPTable pdfResult = AdjustmentDownloadHelper.getInstance().handleNewAdjustmentForAddingDurationAndDecimalDurationToThePdf(accountObject, false, table, fontType, true);
        Assertions.assertTrue(!ObjUtils.isNull(pdfResult));
    }

    @Test
    void handleEditedAdjustmentForAddingActualTimeAndDateToThePdf_ifItIsANewAdjustment_test() {
        SettingsJDO accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("111");
        accountObject.setDisplayTimeFormat("HH:MM");
        accountObject.setDisplayDomainName("newDomain");
        AdjustmentDTO adjustmentObject = new AdjustmentDTO();
        adjustmentObject.setAccountID("111");
        adjustmentObject.setContactID("222");
        adjustmentObject.setIsNew(true);
        adjustmentObject.setAdjustedInTime(111L);
        adjustmentObject.setAdjustedOutTime(555L);
        adjustmentObject.setRequestMessage("Others - test test");
        Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
        PdfPTable table = new PdfPTable(16);
        PdfPTable pdfResult = AdjustmentDownloadHelper.getInstance().handleEditedAdjustmentForAddingActualTimeAndDateToThePdf(accountObject, true,
                                                                                                table, fontType, true, adjustmentObject, "Asia/kolkata");
        Assertions.assertTrue(!ObjUtils.isNull(pdfResult));
    }

    @Test
    void handleEditedAdjustmentForAddingActualTimeAndDateToThePdf_ifItIsANotNewAdjustment_test() {
        SettingsJDO accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("323");
        accountObject.setDisplayTimeFormat("HH:MM");
        accountObject.setDisplayDomainName("DomainTest");
        AdjustmentDTO adjustmentObject = new AdjustmentDTO();
        adjustmentObject.setAccountID("323");
        adjustmentObject.setContactID("434");
        adjustmentObject.setIsNew(false);
        adjustmentObject.setAdjustedInTime(222L);
        adjustmentObject.setAdjustedOutTime(777L);
        adjustmentObject.setRequestMessage("Others - test test test");
        Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
        PdfPTable table = new PdfPTable(14);
        PdfPTable pdfResult = AdjustmentDownloadHelper.getInstance().handleEditedAdjustmentForAddingActualTimeAndDateToThePdf(accountObject, true,
                table, fontType, false, adjustmentObject, "Pacific/Samoa");
        Assertions.assertTrue(!ObjUtils.isNull(pdfResult));
    }

    @Test
    void addDurationAndDecimalDurationTextToThePdfDocument_ifDecimalDurationIsNotActiveAndAccountIsNotOnFreePlan_test() {
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)) {
            accountUtilMockedStatic.when(() -> AccountUtil.isFreeAccount(any())).thenReturn(true);
            SettingsJDO accountObject = new SettingsJDO();
            accountObject.setPeopleUniquePin("accountID");
            accountObject.setDisplayTimeFormat(DownloadUtil.DECIMAL);
            accountObject.setDisplayDomainName("domainName");
            String []cellContentArray = {"cellArray", "randomArray"};
            Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
            PdfPTable table = new PdfPTable(16);
            PdfPTable pdfResult = AdjustmentDownloadHelper.getInstance().addDurationAndDecimalDurationTextToThePdfDocument(accountObject, cellContentArray, table, fontType, true, 232323L);
            Assertions.assertEquals(16, pdfResult.getNumberOfColumns());
        }
    }

    @Test
    void addDurationAndDecimalDurationTextToThePdfDocument_ifAccountIsNotOnFreePlan_test() {
        try(MockedStatic<AccountUtil> accountUtilMockedStatic = Mockito.mockStatic(AccountUtil.class)){
            accountUtilMockedStatic.when(() -> AccountUtil.isFreeAccount(any())).thenReturn(false);
            SettingsJDO accountObject = new SettingsJDO();
            accountObject.setPeopleUniquePin("accountID");
            accountObject.setDisplayTimeFormat(DownloadUtil.DECIMAL);
            accountObject.setDisplayDomainName("domainName");
            String []cellContentArray = {"cell", "random"};
            Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
            PdfPTable table = new PdfPTable(16);
            PdfPTable pdfResult = AdjustmentDownloadHelper.getInstance().addDurationAndDecimalDurationTextToThePdfDocument(accountObject, cellContentArray, table, fontType, false, 232323L);
            Assertions.assertEquals(16, pdfResult.getNumberOfColumns());
        }
    }

    @Test
    void addActualAndAdjustedDateAndTimeToThePdf_valid_test() {
        SettingsJDO accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("accountID");
        accountObject.setDisplayTimeFormat(DownloadUtil.DECIMAL);
        accountObject.setDisplayDomainName("domainName");
        Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
        PdfPTable table = new PdfPTable(16);
        PdfPTable pdfResult = AdjustmentDownloadHelper.getInstance().addActualAndAdjustedDateAndTimeToThePdf(accountObject, 1234L, 2345L, table, "Asia/Kolkata", fontType, false);
        Assertions.assertTrue(!ObjUtils.isNull(pdfResult));
    }

    @Test
    void addAdjustmentReasonAndMessageToThePdfDocument_WithOutAdjustmentReason_test() {
        Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
        PdfPTable table = new PdfPTable(16);
        PdfPTable result = AdjustmentDownloadHelper.getInstance().addAdjustmentReasonAndMessageToThePdfDocument(fontType, "test - test", table);
        Assertions.assertEquals(16, result.getNumberOfColumns());
    }

    @Test
    void addAdjustmentReasonAndMessageToThePdfDocument_WithAdjustmentReason_test() {
        Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
        PdfPTable table = new PdfPTable(16);
        PdfPTable result = AdjustmentDownloadHelper.getInstance().addAdjustmentReasonAndMessageToThePdfDocument(fontType, "IT Issue - test", table);
        Assertions.assertEquals(16, result.getNumberOfColumns());
    }

    @Test
    void getFileName_ifStartDateDoesNotEqualsEndDate_test() {
        SettingsJDO accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("accountID");
        accountObject.setDisplayTimeFormat(DownloadUtil.DECIMAL);
        accountObject.setDisplayDomainName("domainName");
        RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
        rangeInfoDTO.setFromDateEpochMilliseconds(1643817200000L);
        rangeInfoDTO.setToDateEpochMilliseconds(1644076400000L);
        String fileName = AdjustmentDownloadHelper.getInstance().getFileName(accountObject, rangeInfoDTO, "PENDING", "Asia/kolkata");
        Assertions.assertEquals("PENDING_AdjustmentsReport_domainName_ALL_02-Feb-2022_to_05-Feb-2022.pdf", fileName);
    }

    @Test
    void getFileName_ifStartDateEqualsEndDate_test() {
        SettingsJDO accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("accountID");
        accountObject.setDisplayTimeFormat(DownloadUtil.DECIMAL);
        accountObject.setDisplayDomainName("domainName");
        RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
        rangeInfoDTO.setFromDateEpochMilliseconds(1643817200000L);
        rangeInfoDTO.setToDateEpochMilliseconds(1643817200000L);
        String fileName = AdjustmentDownloadHelper.getInstance().getFileName(accountObject, rangeInfoDTO, "PENDING", "Asia/Kolkata");
        Assertions.assertEquals("PENDING_AdjustmentsReport_domainName_ALL_02-Feb-2022", fileName);
    }

    @Test
    void getFileName_ifCurrentWeekDateIsGreaterThanCurrentDay_test() {
        SettingsJDO accountObject = new SettingsJDO();
        accountObject.setPeopleUniquePin("accountID");
        accountObject.setDisplayTimeFormat(DownloadUtil.DECIMAL);
        accountObject.setDisplayDomainName("domainName");
        RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
        rangeInfoDTO.setFromDateEpochMilliseconds(1643817200000L);
        rangeInfoDTO.setToDateEpochMilliseconds(3615810800000L);
        long currentMillis = DateUtil.getCurrentTime();
        var endDate= DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY, currentMillis,"Asia/Kolkata");
        String fileName = AdjustmentDownloadHelper.getInstance().getFileName(accountObject, rangeInfoDTO, "PENDING", "Asia/Kolkata");
        Assertions.assertEquals("PENDING_AdjustmentsReport_domainName_ALL_02-Feb-2022_to_"+endDate+".pdf", fileName);
    }
}
