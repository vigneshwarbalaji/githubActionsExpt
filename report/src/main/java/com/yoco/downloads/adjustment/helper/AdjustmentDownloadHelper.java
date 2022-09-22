package com.yoco.downloads.adjustment.helper;

import com.itextpdf.text.*;
import com.itextpdf.text.pdf.PdfPCell;
import com.itextpdf.text.pdf.PdfPTable;
import com.itextpdf.text.pdf.PdfWriter;
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
import lombok.extern.slf4j.Slf4j;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;

import static com.itextpdf.text.FontFactory.HELVETICA;

@Slf4j
public class AdjustmentDownloadHelper {

    public static AdjustmentDownloadHelper getInstance(){
        return new AdjustmentDownloadHelper();
    }

    private static final String[] ADJUSTMENT_REASONS = {"IT Issue","Shift Board Signup","Training","Remote Setup","Travel Time","Others"};

    public PdfPTable getPdfHeadersForDownload(SettingsJDO accountObject, Font fontType, boolean isDecimalDurationActive) throws DocumentException {
        boolean isThisAccountOnFreePlan = AccountUtil.isFreeAccount(accountObject);
        int numberOfColumns = getNumberOfColumnsRequired(isDecimalDurationActive, isThisAccountOnFreePlan);
        float[] columnWidths = getRequiredColumnWidths(isDecimalDurationActive, isThisAccountOnFreePlan);
        List<String>pdfHeaders = new ArrayList<>(Arrays.asList("Project","Email","ActualInDate","ActualInTime","ActualOutDate","ActualOutTime",
                "ActualDuration", "ActualDurationInDecimal", "AdjustedInDate", "AdjustedInTime",
                "AdjustedOutDate", "AdjustedOutTime", "AdjustedDuration", "AdjustedDurationInDecimal",
                "AdjustmentReason", "AdjustmentMessage"));
        pdfHeaders = getPdfHeadersForActiveDecimalDuration(isDecimalDurationActive, pdfHeaders);
        pdfHeaders = getPdfHeadersForFreeAccount(isThisAccountOnFreePlan, pdfHeaders);
        return initializeAndAddHeadersToThePdf(numberOfColumns, columnWidths, pdfHeaders, fontType);
    }

    public PdfPTable initializeAndAddHeadersToThePdf(int numberOfColumns, float [] columnWidths, List<String>pdfHeaders, Font fontType) throws DocumentException {
        PdfPTable table = new PdfPTable(numberOfColumns);
        table.setWidthPercentage(100);
        table.setSpacingAfter(100f);
        table.setSpacingBefore(100f);
        table.setWidths(columnWidths);
        for (var header : pdfHeaders) {
            table.addCell(new PdfPCell(new Paragraph(header, fontType)));
        }
        return table;
    }

    public int getNumberOfColumnsRequired(boolean isDecimalDurationActive, boolean isThisAccountOnFreePlan) {
        return (isDecimalDurationActive || isThisAccountOnFreePlan) ? 14 : 16;
    }

    public float[] getRequiredColumnWidths(boolean isDecimalDurationActive, boolean isThisAccountOnFreePlan){
        return (isDecimalDurationActive || isThisAccountOnFreePlan) ? new float[]{4f, 6f, 4f, 4f, 4f, 4f, 6f, 4f, 4f, 4f, 4f, 6f, 5f, 6f} :
                                                                      new float[]{4f, 6f, 4f, 4f, 4f, 4f, 4f, 6f, 4f, 4f, 4f, 4f, 4f, 6f, 5f, 6f};
    }

    public List<String> getPdfHeadersForActiveDecimalDuration(boolean isDecimalDurationActive, List<String>pdfHeaders){
        if(isDecimalDurationActive){
            pdfHeaders.remove("ActualDuration");
            pdfHeaders.remove("AdjustedDuration");
        }
        return pdfHeaders;
    }

    public List<String> getPdfHeadersForFreeAccount(boolean isThisAccountOnFreePlan, List<String>pdfHeaders){
        if(isThisAccountOnFreePlan){
            pdfHeaders.remove("ActualDurationInDecimal");
            pdfHeaders.remove("AdjustedDurationInDecimal");
        }
        return pdfHeaders;
    }

    public Map<String, Object> getPdfDocumentForDownload(String accountID, String status, String startDate, String endDate, String timeZone, RangeInfoDTO rangeInfoDTO) throws DocumentException, IOException, NoSuchAlgorithmException {
        Font fontType = FontFactory.getFont(HELVETICA, 8F, Font.NORMAL);
        fontType.setSize(10);
        SettingsJDO account = AccountImpl.getAccountImplInstance().getById(accountID);
        boolean isDecimalDurationActive = AccountUtil.isDecimalDurationActiveForTheGivenAccount(account);
        PdfPTable pdfDocument = getPdfHeadersForDownload(account, fontType, isDecimalDurationActive);
        List<AdjustmentDTO> adjustmentList = getAdjustmentsDetailsForThePdf(accountID, status, startDate, endDate, timeZone);
        pdfDocument = getBodyOfTheAdjustmentPdf(account, adjustmentList, pdfDocument, fontType, isDecimalDurationActive, timeZone);
        ByteArrayOutputStream pdfByteArray =  initializePdfDocumentWriter(pdfDocument);
        Map<String, Object> adjustmentsPdfMap = new HashMap<>();
        adjustmentsPdfMap.put(DownloadUtil.PDF_STREAM,pdfByteArray);
        adjustmentsPdfMap.put(DownloadUtil.FILE_NAME, getFileName(account, rangeInfoDTO, status, timeZone));
        return adjustmentsPdfMap;
    }

    public ByteArrayOutputStream initializePdfDocumentWriter(PdfPTable pdfDocument) throws DocumentException {
        final ByteArrayOutputStream pdfByteArray = new ByteArrayOutputStream();
        Document document = new Document();
        PdfWriter pdfWriter = PdfWriter.getInstance(document, pdfByteArray);
        Rectangle pageSize = new Rectangle(1600, 900);
        document.setPageSize(pageSize);
        document.open();
        document.add(pdfDocument);
        document.close();
        pdfWriter.close();
        return pdfByteArray;
    }

    public List<AdjustmentDTO> getAdjustmentsDetailsForThePdf(String accountID, String status, String startDate, String endDate, String timeZone) throws IOException, NoSuchAlgorithmException {
        List<Map<String, Object>> eventList = new ArrayList<>();
        List<Map<String, Object>> temporaryEventList;
        String cursor = "";
        do{
            Map<String, Object>eventListMap = new AdjustmentImpl().getAllAdjustments(accountID, "", startDate, endDate, status, cursor);
            temporaryEventList = (List<Map<String, Object>>) eventListMap.get(EventConstants.EVENT_LIST);
            if(eventListMap.containsKey(Commons.CURSOR)){
                cursor = (String) eventListMap.get(Commons.CURSOR);
            }
            if(!ObjUtils.isNullOrEmpty(temporaryEventList))
                eventList.addAll(temporaryEventList);
        }while(!ObjUtils.isNullOrEmpty(temporaryEventList) && !ObjUtils.isNullOrEmpty(cursor));
        return AdjustmentHelper.getInstance().convertEventListIntoAdjustmentList(eventList, timeZone, "");
    }

    public PdfPTable getBodyOfTheAdjustmentPdf(SettingsJDO accountObject, List<AdjustmentDTO> adjustmentList, PdfPTable pdfDocument,
                                               Font fontType, boolean isDecimalDurationActive, String timeZone){
        Map<String, Object>mailIdMap = getMapOfMailIdsLinkedToContactIds(adjustmentList);
        for (var adjustment : adjustmentList) {
            pdfDocument.addCell(new PdfPCell(new Paragraph(adjustment.getProjectName(), fontType)));
            pdfDocument.addCell(new PdfPCell(new Paragraph((String) mailIdMap.get(adjustment.getContactID()), fontType)));
            pdfDocument = addDurationTimeAndDateDetailsToThePdfDocument(accountObject, adjustment, pdfDocument, fontType, isDecimalDurationActive, timeZone);
            pdfDocument = addAdjustmentReasonAndMessageToThePdfDocument(fontType, adjustment.getRequestMessage(), pdfDocument);
        }
        return pdfDocument;
    }

    public Map<String, Object> getMapOfMailIdsLinkedToContactIds(List<AdjustmentDTO> adjustmentList){
        List<String> adjustmentContactIDs = adjustmentList.stream().distinct().map(AdjustmentDTO::getContactID).collect(Collectors.toList());
        List<Contact> contactObjectList = new ContactImpl().getContacts(adjustmentContactIDs);
        return contactObjectList.stream().collect(Collectors.toMap(Contact::getId,Contact::getEmailID));
    }

    public PdfPTable addDurationTimeAndDateDetailsToThePdfDocument(SettingsJDO accountObject, AdjustmentDTO adjustment, PdfPTable pdfTable,
                                                                   Font fontType, boolean isDecimalDurationActive, String timeZone) {
        boolean isThisANewAdjustment = adjustment.getIsNew();
        pdfTable = handleNewAdjustmentForAddingDurationAndDecimalDurationToThePdf(accountObject, isThisANewAdjustment, pdfTable, fontType, isDecimalDurationActive);
        pdfTable = handleEditedAdjustmentForAddingActualTimeAndDateToThePdf(accountObject, isThisANewAdjustment, pdfTable, fontType, isDecimalDurationActive, adjustment, timeZone);
        pdfTable = addActualAndAdjustedDateAndTimeToThePdf(accountObject, adjustment.getAdjustedInTime(), adjustment.getAdjustedOutTime(),
                pdfTable, timeZone, fontType, isDecimalDurationActive);
        return pdfTable;
    }

    public PdfPTable handleNewAdjustmentForAddingDurationAndDecimalDurationToThePdf(SettingsJDO accountObject, boolean isThisANewAdjustment, PdfPTable pdfTable,
                                                                                    Font fontType, boolean isDecimalDurationActive) {
        if(isThisANewAdjustment){
            long actualDuration = 0L;
            String [] cellContentArray = new String [4];
            Arrays.fill(cellContentArray,"---------------");
            pdfTable = addDurationAndDecimalDurationTextToThePdfDocument(accountObject, cellContentArray, pdfTable, fontType, isDecimalDurationActive, actualDuration);
        }

        return pdfTable;
    }

    public PdfPTable handleEditedAdjustmentForAddingActualTimeAndDateToThePdf(SettingsJDO accountObject, boolean isThisANewAdjustment, PdfPTable pdfTable,
                                                                                    Font fontType, boolean isDecimalDurationActive,  AdjustmentDTO adjustment, String timeZone) {
     if(!isThisANewAdjustment){
         pdfTable = addActualAndAdjustedDateAndTimeToThePdf(accountObject, adjustment.getOriginalInTime(), adjustment.getOriginalOutTime(),
                 pdfTable, timeZone, fontType, isDecimalDurationActive);
     }
     return pdfTable;
    }

    public PdfPTable addDurationAndDecimalDurationTextToThePdfDocument(SettingsJDO accountObject, String [] cellContentArray, PdfPTable pdfTable,
                                                                       Font fontType, boolean isDecimalDurationActive, long duration) {
        boolean isThisAccountOnFreePlan = AccountUtil.isFreeAccount(accountObject);
        for(var cellContent :cellContentArray){
            pdfTable.addCell(new PdfPCell(new Paragraph(cellContent, fontType)));
        }
        String durationText = DateUtil.getTimeFormatAsPerCompany(duration,accountObject.getDisplayTimeFormat());
        String durationDecimalText = DownloadUtil.getDecimalDuration(duration);
        if(!isDecimalDurationActive){
            pdfTable.addCell(new PdfPCell(new Paragraph(durationText,fontType)));
        }
        if(!isThisAccountOnFreePlan)
            pdfTable.addCell(new PdfPCell(new Paragraph(durationDecimalText,fontType)));
        return pdfTable;
    }

    public PdfPTable addActualAndAdjustedDateAndTimeToThePdf(SettingsJDO accountObject, long inTimeInMillis,
                                                             long outTimeInMillis, PdfPTable pdfTable, String timeZone,
                                                             Font fontType, boolean isDecimalDurationActive) {
        String inDate = DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY, inTimeInMillis, timeZone);
        String inTime = DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A, inTimeInMillis, timeZone);
        String outDate = DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY, outTimeInMillis, timeZone);
        String outTime = DateUtil.convertMillisToDateTimeText(DateFormats.HH_MM_SS_A, outTimeInMillis, timeZone);
        String [] cellContentArray = new String[]{inDate,inTime,outDate,outTime};
        long duration = outTimeInMillis - inTimeInMillis;
        pdfTable = addDurationAndDecimalDurationTextToThePdfDocument(accountObject, cellContentArray, pdfTable, fontType, isDecimalDurationActive, duration);
        return pdfTable;
    }

    public PdfPTable addAdjustmentReasonAndMessageToThePdfDocument(Font fontType, String requestMessage, PdfPTable pdfTable){
        String[] messageArray = requestMessage.split("-", 2);
        String adjustmentReason = messageArray[0].trim();
        String adjustmentMessage = messageArray[1].trim();
        if(!Arrays.asList(ADJUSTMENT_REASONS).contains(adjustmentReason)){
            adjustmentReason = "Others";
        }
        pdfTable.addCell(new PdfPCell(new Paragraph(adjustmentReason, fontType)));
        pdfTable.addCell(new PdfPCell(new Paragraph(adjustmentMessage, fontType)));
        return pdfTable;
    }

    public String getFileName(SettingsJDO accountObject, RangeInfoDTO rangeInfoDTO, String status, String timeZone) {
        var startDate= DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY, rangeInfoDTO.getFromDateEpochMilliseconds(), timeZone);
        long currentMillis = DateUtil.getCurrentTime();
        long endMillis = (rangeInfoDTO.getToDateEpochMilliseconds() > currentMillis) ? currentMillis : rangeInfoDTO.getToDateEpochMilliseconds();
        var endDate= DateUtil.convertMillisToDateTimeText(DateFormats.DD_MMM_YYYY, endMillis,timeZone);
        String fileName = status.toUpperCase() + "_AdjustmentsReport_"
                + accountObject.getDisplayDomainName().replace(" ","_")
                + "_ALL_";
        fileName += startDate;
        if(!startDate.equals(endDate))
            fileName+= "_to_" + endDate + ".pdf";
        return fileName;
    }
}
